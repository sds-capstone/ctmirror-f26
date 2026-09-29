library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)
library(leaflet)
options(tigris_use_cache = TRUE)

# Define UI for app 
ui <- page_sidebar(
  # App title ----
  title = "Connecticut Mirror Data Dashboard",
  fillable = FALSE,

  # Output: 
  plotOutput(outputId = "ctplot"),
  plotOutput(outputId = "ctmap_static"),
  leafletOutput(outputId = "ctmap_dynamic"),
  card(max_height = 350, tableOutput(outputId = "town_table"))
)

# Data Download

ct <- get_acs(geography = "county subdivision",
              variables = c(medincome = "B19013_001"),
              state = "CT",
              year = 2024,
              geometry = TRUE) |>
  mutate(town_name = sub(" town.*", "", NAME))

# Define server logic 

server <- function(input, output) {

  output$ctplot <- renderPlot({
    ct |>
      slice_max(estimate, n = 10) |>
      ggplot(aes(x = estimate, y = reorder(town_name, estimate))) +
      geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
      geom_point(color = "#761080", size = 3) +
      theme_minimal() +
      labs(title = "CT Towns with Highest Median Household Income",
          subtitle = "2020-2024 American Community Survey",
          y = "",
          x = "ACS estimate (bars represent margin of error)")
  })

  output$ctmap_static <- renderPlot({
    ggplot(ct, aes(fill = estimate)) +
      geom_sf() + 
      scale_fill_viridis_c(option = "magma", direction = -1,
                           guide = guide_colourbar(reverse = TRUE)) +
      theme_void() +
      labs(fill = "Median Household\nIncome ($)",
          title = "Median Household Income in CT Towns",
          caption = "2020-2024 ACS, US Census Bureau")
  })

  output$ctmap_dynamic <- renderLeaflet({
    pal <- colorNumeric(
      palette = "magma",
      domain = ct$estimate,
      reverse = TRUE)
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(data = ct,
            color = ~pal(estimate),
            weight = 0.5,
            smoothFactor = 0.2,
            fillOpacity = 0.75,
            label = ~paste0(town_name, " ", estimate)) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ct$estimate,
        title = "Median Household Income ($)")
  })

  output$town_table <- renderTable({
    ct |>
      sf::st_drop_geometry() |>
      arrange(desc(estimate)) |>
      select(Town = town_name, `Median Household Income ($)` = estimate)
  }, digits = 0)
  
}

shinyApp(ui = ui, server = server)