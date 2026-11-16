library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)
library(leaflet)
library(scales)
library(plotly)
library(readxl)
library(DT)
library(shinyjs)
source("ingest.R") # need this for find_variables tab
source("table_dimensions.R")
source("home_tab.R")
source("data_tab.R")
source("find_variables_tab.R")

glossary <- read_excel("acs5_2024_variables.xlsx", sheet = "Variable Coding")
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
      "Find variables", "",
      find_variables_ui("find_variables")
    ),
    nav_panel(
      "Data", "", 
      data_UI("data")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Making our cached dfs into reactive objects
  main_acs5 <- reactiveVal(acs5)
  main_acs1 <- reactiveVal(acs1)

  # Empty reactive that will signal which ad_hoc variable was fetched
  new_table <- reactiveVal(NULL)

  # Save ad_hoc API call from the find variables module
  ad_hoc <- find_variables_server("find_variables", glossary, vars)

  # Merge any user inputted ad hoc row with the cached data frames
  observeEvent(ad_hoc(), {
    i <- ad_hoc()

    req(i$table)

    # Removes duplicates if user selects an already cached variable
    drop_table <- function(df, tbl) df[!startsWith(df$variable, paste0(tbl, "_")), ]

    # Fetch ad hoc row and append it to the cached data, for both acs5 and acs1 dfs
    if (!is.null(i$row_acs5)) {
      main_acs5(dplyr::bind_rows(drop_table(main_acs5(), i$table), i$row_acs5))
    }
      
    if (!is.null(i$row_acs1)) {
      main_acs1(dplyr::bind_rows(drop_table(main_acs1(), i$table), i$row_acs1))
    }

    # Updating reactive value with ad hoc variable
    new_table(list(id = i$table, at = Sys.time()))
  })

  dfs <- home_server("home", acs5 = main_acs5, acs1 = main_acs1, jump_to = new_table) # jump_to signals the sidebar to select the recently fetched ad hoc variable
  data_Server("data", ct = dfs$ct, time_series_data = dfs$time_series_data, dfs$var_label)

}

shinyApp(ui = ui, server = server)