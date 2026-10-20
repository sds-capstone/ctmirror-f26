library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(scales)
library(plotly)
source(here::here("app", "ingest.R"))
options(tigris_use_cache = TRUE)

# Beats and variables (WIP: now read from the sheet, one variable per ACS table)
coding <- readxl::read_excel(here::here("acs5_2024_variables.xlsx"),
                             sheet = "Variable Coding", col_types = "text") |>
  filter(!str_detect(table, "PR$")) |> # Puerto Rico
  separate_longer_delim(beat, delim = ",") |> # split column into rows
  mutate(beat = str_to_title(str_squish(beat)),
         label = if_else(str_detect(tolower(paste(label, concept)), "dollars"),
                         paste0(concept, " ($)"), concept))  # "($)" keeps the money formatting working

beats <- split(setNames(coding$name, coding$label), coding$beat)
all_vars <- unlist(unname(beats))
all_vars <- all_vars[!duplicated(all_vars)]

# Define UI for app 
ui <- page_sidebar(
  # App title ----
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
  # Output: 
  
  #,
  #
)

# Define server logic 

server <- function(input, output) {

  output$variable_ui <- renderUI({
    selectInput("variable", "Variable", choices = beats[[input$beat]])
  })

  var_label <- reactive({
    names(all_vars)[all_vars == input$variable]
  })

  # Data Download
  ct <- reactive({
    req(input$variable)
    get_acs(geography = "county subdivision",
            variables = input$variable,
            state = "CT",
            year = 2024,
            geometry = TRUE) |>
      mutate(town_name = sub(" town.*", "", NAME))
  })

  # Time-Series Plot 
  # creating a blank dataframe 

  time_series_data <- reactive({
    
    empty <- data.frame(year=double(), var_name=double(), tooltip = character())

    # looping through years and calling API 
    for (x in 2009:2024) {
      api_df <- tryCatch(
        get_acs(geography = "state", # state-level geography 
                  variables = input$variable,
                  state = "CT", 
                  year = x), 
        error = function(e) {
                  message("Skipping ", x, ": ", conditionMessage(e))
                  NULL
                }) 
      if (is.null(api_df)) next
      empty <- empty |> 
          add_row(year=x, var_name=api_df$estimate, tooltip = paste0(year, ": ", api_df$estimate))
    }
    empty 
  })

  output$ctmap_dynamic <- renderLeaflet({
    pal <- colorNumeric(
      palette = "magma",
      domain = ct()$estimate)
    pal_reverse <- colorNumeric(
      palette = "magma",
      domain = ct()$estimate,
      reverse = TRUE)
    money <- if (grepl("\\$", var_label())) "$" else ""
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(data = ct(),
            color = ~pal_reverse(estimate),
            weight = 0.5,
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            label = ~paste0(town_name, ": ",  money, scales::comma(estimate)),
            highlightOptions = highlightOptions(
              color = "red",
              weight = 2,
              bringToFront = TRUE)) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ct()$estimate,
        title = var_label(),
        labFormat = labelFormat(
          prefix = money,
          big.mark = ",",
          transform = function(x) sort(x, decreasing = TRUE)))
  })

  # Time-Series Visualization 
  output$time_series <- renderPlotly({ 
    g <- suppressWarnings(ggplot(time_series_data(), aes(x=year, y=var_name)) + 
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
      select(Town = town_name, Estimate = Estimate) |>
      setNames(c("Town", heading))
  }, digits = 0)

  # Download data button
  output$download_table <- downloadHandler(
    filename = function() {paste0(gsub(" ", "_", var_label()), ".csv")},
    content = function(file) {
        clean_data = sf::st_drop_geometry(ct())
      write.csv(clean_data, file)}
  )
  
}

shinyApp(ui = ui, server = server)