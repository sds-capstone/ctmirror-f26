library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)

# Define UI for app 
ui <- page_sidebar(
  # App title ----
  title = "Connecticut Mirror Data Dashboard",

  # Output: 
  plotOutput(outputId = "ctplot"),
  plotOutput(outputId = "ctmap_static")
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
      theme_void() +
      labs(fill = "Median household\nincome ($)",
          title = "Median Household Income in CT Towns",
          caption = "2020-2024 ACS, US Census Bureau")
  })

}

shinyApp(ui = ui, server = server)