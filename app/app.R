library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(scales)
library(plotly)
source(here::here("app", "ingest.R"))
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
    selectInput("beat", "Beat", choices = names(beats)),
    uiOutput("variable_ui")
  ),

  navset_tab(
    nav_panel(
      "Home", 
      "", 
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
server <- function(input, output) {

  # Sidebar
  output$variable_ui <- renderUI({
    selectInput("variable", "Variable", choices = beats[[input$beat]])
  })

  # Dynamic variable label
  var_label <- reactive({
    names(all_vars)[all_vars == input$variable]
  })

  # ACS 5 Year Data Download --> map
  ct <- reactive({
    acs5 |> filter(variable == input$variable)
  })

  # ACS 1 Year Data Download --> time-series
  time_series_data <- reactive({
    acs1 <- acs1 |> filter(variable == input$variable)
  })

  # Map
  output$ctmap_dynamic <- renderLeaflet({
    pal <- colorNumeric(
      palette = "magma",
      domain = ct()$pct)
    pal_reverse <- colorNumeric(
      palette = "magma",
      domain = ct()$pct,
      reverse = TRUE)
    #money <- if (grepl("\\$", var_label())) "$" else ""
    money <- "" # we can keep the above line if we add a user option to see counts
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(data = ct(),
            color = ~pal_reverse(pct),
            weight = 0.5,
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            label = ~paste0(town, ": ",  money, round(pct,1), "%"),
            highlightOptions = highlightOptions(
              color = "red",
              weight = 2,
              bringToFront = TRUE)) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ct()$pct,
        title = var_label(),
        labFormat = labelFormat(
          prefix = money,
          suffix = "%",
          big.mark = ",",
          transform = function(x) sort(x, decreasing = TRUE)))
  })

  # Time-Series Visualization 
  output$time_series <- renderPlotly({ 
    g <- suppressWarnings(ggplot(time_series_data(), aes(x=year, y=pct)) + 
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
  
}

shinyApp(ui = ui, server = server)