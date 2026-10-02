library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)
library(leaflet)
options(tigris_use_cache = TRUE)

# Beats and variables
beats <- list(
  "Housing" = c("Median home value ($)" = "B25077_001",
                "Median gross rent ($)" = "B25064_001"),
  "Education" = c("Bachelor's degree (count)" = "B15003_022"),
  "Race" = c("White alone (count)"                   = "B02001_002",
           "Black alone (count)"                   = "B02001_003",
           "American Indian/Alaska Native alone (count)" = "B02001_004",
           "Asian alone (count)"                   = "B02001_005",
           "Native Hawaiian/Pacific Islander alone (count)" = "B02001_006",
           "Some other race alone (count)"         = "B02001_007",
           "Two or more races (count)"             = "B02001_008",
           "Hispanic or Latino, any race (count)"  = "B03003_003"),
  "Language" = c("Speak Spanish at home (count)" = "C16001_003"),
  "Income" = c("Median household income ($)" = "B19013_001",
               "Per capita income ($)" = "B19301_001")
)
all_vars <- unlist(unname(beats))

# Define UI for app 
ui <- page_sidebar(
  # App title ----
  title = "Connecticut Mirror Data Dashboard",
  fillable = FALSE,

  sidebar = sidebar(
    selectInput("beat", "Beat", choices = names(beats)),
    uiOutput("variable_ui")
  ),

  # Output: 
  plotOutput(outputId = "ctplot"),
  plotOutput(outputId = "ctmap_static"),
  leafletOutput(outputId = "ctmap_dynamic"),
  card(max_height = 350, tableOutput(outputId = "town_table")),
  downloadButton(outputId = "download_table", label = "Download Data")
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

  output$ctplot <- renderPlot({
    ct() |>
      slice_max(estimate, n = 10) |>
      ggplot(aes(x = estimate, y = reorder(town_name, estimate))) +
      geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe),
              orientation = "y") +
      geom_point(color = "#761080", size = 3) +
      theme_minimal() +
      labs(title = paste("CT Towns with Highest", var_label()),
          subtitle = "2020-2024 American Community Survey",
          y = "",
          x = "ACS estimate (bars represent margin of error)")
  })

  output$ctmap_static <- renderPlot({
    ggplot(ct(), aes(fill = estimate)) +
      geom_sf() + 
      scale_fill_viridis_c(option = "magma",
                           guide = guide_colourbar(reverse = TRUE)) +
      theme_void() +
      labs(fill = str_wrap(var_label(), 20),
          title = paste(var_label(), "in CT Towns"),
          caption = "2020-2024 ACS, US Census Bureau")
  })

  output$ctmap_dynamic <- renderLeaflet({
    pal <- colorNumeric(
      palette = "magma",
      domain = ct()$estimate)
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(data = ct(),
            color = ~pal(estimate),
            weight = 0.5,
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            label = ~paste0(town_name, " ", estimate),
            highlightOptions = highlightOptions(
              color = "red",
              weight = 2,
              bringToFront = TRUE)) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ct()$estimate,
        title = var_label())
  })

  # Output table
  output$town_table <- renderTable({
    ct() |>
      sf::st_drop_geometry() |>
      arrange(desc(estimate)) |>
      select(Town = town_name, Estimate = estimate)
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