library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(scales)
library(plotly)
source("ingest.R")
source("table_dimensions.R")
source("home_tab.R")

acs5 <- readRDS("data/acs5.rds")
acs1 <- readRDS("data/acs1.rds")
options(tigris_use_cache = TRUE)

# Define UI for app
ui <- page_sidebar(
  title = "Connecticut Mirror Data Dashboard",
  fillable = FALSE,

  # One sidebar for the whole app
  sidebar = sidebar(home_sidebar_ui("home")),

  navset_tab(
    nav_panel("Home", home_ui("home")),
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
  home_server("home", acs5, acs1)

  # THIS THROWS AN ERROR NOW - DATA TAB NEEDS FIXING !!!
  
  # Output table 
  # output$town_table <- renderTable({
  #   heading <- if (grepl("\\$", var_label())) "Estimate ($)" else "Estimate"
  #   ct() |>
  #     sf::st_drop_geometry() |>
  #     arrange(desc(estimate)) |>
  #     mutate(Estimate = scales::comma(estimate)) |>
  #     select(Town = town, Estimate = Estimate) |>
  #     setNames(c("Town", heading))
  # }, digits = 0)

  # # Download data button
  # output$download_table <- downloadHandler(
  #   filename = function() {paste0(gsub(" ", "_", var_label()), ".csv")},
  #   content = function(file) {
  #       clean_data = sf::st_drop_geometry(ct())
  #     write.csv(clean_data, file)}
  # )
}

shinyApp(ui = ui, server = server)