library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(scales)
library(plotly)
source(here::here("app", "ingest.R"))
source(here::here("app", "table_dimensions.R"))
source(here::here("app", "home_test_tab.R"))

acs5 <- readRDS(here::here("app/data", "acs5.rds"))
acs1 <- readRDS(here::here("app/data", "acs1.rds"))
options(tigris_use_cache = TRUE)

# Beats and variables (WIP: now read from the sheet, one variable per ACS table)
coding <- vars |>
  separate_longer_delim(beats, delim = ",") |> # split column into rows
  filter(priority == TRUE) |>
  mutate(beats = str_to_title(str_squish(beats)),
         label = if_else(str_detect(tolower(paste(label, var_label)), "dollars"),
                         paste0(var_label, " ($)"), var_label))  # "($)" keeps the money formatting working

beats <- split(setNames(coding$name, coding$label), coding$beats)
all_vars <- unlist(unname(beats))
all_vars <- all_vars[!duplicated(all_vars)]

# Define UI for app 
ui <- page_sidebar(
  title = "Connecticut Mirror Data Dashboard",
  fillable = FALSE,

  sidebar = sidebar(
    selectInput("beat", "Beat", choices = c("All beats", names(beats))),
    selectizeInput(
      "variable", "Variable", choices = NULL, width = "100%",
      options = list(placeholder = "Type to search...",
                     openOnFocus = TRUE,
                     maxOptions = 200)
    )
  ),

  navset_tab(
     nav_panel("Home Test", home_test_ui("home_test")),
    nav_panel(
      "Home", 
      conditionalPanel("!input.variable", p("Pick a variable to get started")),
      leafletOutput(outputId = "ctmap_dynamic"),
      plotly::plotlyOutput(outputId = "time_series"),
      ),
    nav_panel(
      "Advanced", "Coming soon!"
    ),
    nav_panel(
      "Find variables", "Coming soon!"
    ),
    nav_panel(
      "Data", "", 
      card(max_height = 350, tableOutput(outputId = "town_table")),
      downloadButton(outputId = "download_table", label = "Download Data")
    )
  )
)

# Define server logic 
server <- function(input, output, session) {

  # Sidebar: Beat narrows the variable list; the Variable box is type-to-search.
  # server = TRUE keeps the browser fast even with thousands of variables.
  observeEvent(input$beat, {
    choices <- if (input$beat == "All beats") all_vars else beats[[input$beat]]
    updateSelectizeInput(session, "variable",
                         choices = choices,
                         selected = "",   # start empty so the placeholder shows
                         server = TRUE)
  })

  # Dynamic variable label
  var_label <- reactive({
    req(input$variable)   # wait until a variable is picked
    names(all_vars)[all_vars == input$variable]
  })

  # Choosing either raw count or percentage
  value_type <- reactive({
    req(input$variable)
    percents <- !grepl("\\b(median|mean|average|aggregate|per capita|gini|index)\\b", var_label(), ignore.case = TRUE)
    if (percents) "pct" else "estimate"
  })

  # Dynamically update graph formatting 
  fmt <- reactive({
    if (value_type() == "pct") { list(prefix = "", suffix = "%", digits = 1)} 
    else if (value_type() == "estimate" & grepl("$", var_label(), fixed = TRUE)) { list(prefix = "$", suffix = "", digits = 0)} 
    else {list(prefix = "", suffix = "", digits = 0)}
  })

  # ACS 5 Year Data Download --> map
  ct <- reactive({
    req(input$variable)
    acs5$value <- acs5[[value_type()]]
    acs5 |> filter(variable == input$variable)
  })

  # ACS 1 Year Data Download --> time-series
  time_series_data <- reactive({
    req(input$variable)
    acs1$value <- acs1[[value_type()]]
    acs1 <- acs1 |> 
      filter(variable == input$variable) |>
      mutate(tooltip = paste0(year, ": ", fmt()$prefix, round(value,2), fmt()$suffix))
  })

  # Map
  output$ctmap_dynamic <- renderLeaflet({
    pal <- colorNumeric(
      palette = "magma",
      domain = ct()$value)
    pal_reverse <- colorNumeric(
      palette = "magma",
      domain = ct()$value,
      reverse = TRUE)
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(data = ct(),
            color = ~pal_reverse(value),
            weight = 0.5,
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            label = ~paste0(town, ": ", fmt()$prefix,
                      formatC(value, format = "f", digits = fmt()$digits, big.mark = ","),
                      fmt()$suffix),
            highlightOptions = highlightOptions(
              color = "red",
              weight = 2,
              bringToFront = TRUE)) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ct()$value,
        title = var_label(),
        labFormat = labelFormat(
          prefix = fmt()$prefix,
          suffix = fmt()$suffix,
          digits = fmt()$digits,
          big.mark = ",",
          transform = function(x) sort(x, decreasing = TRUE)))
  })

  # Time-Series Visualization 
  output$time_series <- renderPlotly({ 
    g <- suppressWarnings(ggplot(time_series_data(), aes(x=year, y=value)) + 
      geom_line(linewidth = 0.8) +
      geom_point(aes(text = tooltip)) +
      theme_minimal() +
      labs(title = paste0("Statewide Summary: ", var_label()), x = "Year", y = var_label()) +
      scale_y_continuous(labels = label_comma(), limits = c(0,NA)))

    plotly::ggplotly(g, tooltip = "text")
  })

  # Output table
  output$town_table <- renderTable({
    heading <- if (grepl("\\$", var_label())) "Estimate ($)" else "Estimate"
    ct() |>
      sf::st_drop_geometry() |>
      arrange(desc(estimate)) |>
      mutate(Estimate = scales::comma(estimate)) |>
      select(Town = town, Estimate = Estimate) |>
      setNames(c("Town", heading))
  }, digits = 0)

  # # Download data button
  output$download_table <- downloadHandler(
    filename = function() {paste0(gsub(" ", "_", var_label()), ".csv")},
    content = function(file) {
        clean_data = sf::st_drop_geometry(ct())
      write.csv(clean_data, file)}
  )

# Home Test
home_test_server("home_test", acs5, acs1)  
}

shinyApp(ui = ui, server = server)