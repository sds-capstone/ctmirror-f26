library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Connecticut Mirror Data Dashboard",

  # Output: Histogram ----
  plotOutput(outputId = "ctplot")
)

# Data Download

ct <- get_acs(geography = "county subdivision",
              variables = c(medincome = "B19013_001"),
              state = "CT",
              year = 2024)

# Define server logic required to draw a histogram ----

server <- function(input, output) {

  output$ctplot <- renderPlot({

    ct |>
      slice_max(estimate, n = 10) |>
      mutate(NAME = gsub("County, Connecticut", "", NAME)) |>
      ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
      geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
      geom_point(color = "#761080", size = 3) +
      labs(title = "CT Towns with Highest Median Household Income",
          subtitle = "2020-2024 American Community Survey",
          y = "",
          x = "ACS estimate (bars represent margin of error)")
  })
}

shinyApp(ui = ui, server = server)