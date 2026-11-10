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
source("data_tab.R")

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
      data_UI("data")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  dfs <- home_server("home", acs5, acs1)
  data_Server("data", ct = dfs$ct, time_series_data = dfs$time_series_data, dfs$var_label)

}

shinyApp(ui = ui, server = server)