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
library(shinyWidgets)
source("ingest.R") # need this for find_variables tab
source("table_dimensions.R")
source("home_tab.R")
source("data_tab.R")
source("find_variables_tab.R")

glossary <- read_excel("acs5_2024_variables.xlsx", sheet = "Variable Coding")
acs5 <- readRDS("data/acs5.rds")
acs1 <- readRDS("data/acs1.rds")
options(tigris_use_cache = TRUE)

# Brand tokens: estimated from logo and CT Mirror website
ctm_red   <- "#b80000"   
ctm_dark  <- "#1a1a1a"
ctm_gray  <- "#5f6368"
ctm_light <- "#f6f6f6"
ctm_serif <- "Georgia, 'Times New Roman', serif"        # headings
ctm_sans  <- "-apple-system, 'Helvetica Neue', Arial, sans-serif"  # body
 
ctm_theme <- bs_theme(
  version = 5,
  bg = "#ffffff", fg = ctm_dark,
  primary = ctm_red,
  secondary = ctm_gray,
  base_font = ctm_sans,
  heading_font = ctm_serif
) |>
  bs_add_rules("
    /* Header: white with a red rule, like the site's masthead */
    .bslib-page-sidebar > header {
      background: #fff !important;
      border-bottom: 3px solid #b80000;
      padding: 8px 16px;
    }

    /* Tabs: red underline for the active tab */
    .nav-tabs .nav-link { color: #5f6368; font-weight: 600; border: none; }
    .nav-tabs .nav-link:hover { color: #b80000; }
    .nav-tabs .nav-link.active {
      color: #b80000; background: transparent;
      border-bottom: 3px solid #b80000;
    }

    /* Sidebar: light gray panel with uppercase section labels */
    .bslib-sidebar-layout > .sidebar { background: #f6f6f6; }
    .sidebar label.control-label {
      text-transform: uppercase; letter-spacing: .04em;
      font-size: .75rem; font-weight: 700; color: #5f6368;
    }

    /* Buttons and links */
    .btn-primary { background: #b80000; border-color: #b80000; }
    .btn-primary:hover { background: #8f0000; border-color: #8f0000; }
    a { color: #b80000; }
  ")

# Define UI for app
ui <- page_sidebar(
  theme = ctm_theme,
  title = tags$div(
    style = "display: flex; align-items: center; gap: 14px; line-height: 1; margin: 0;",
    tags$a(
      href = "https://ctmirror.org/", # links to CT Mirror's website
      target = "_blank",
      rel = "noopener",
      style = "display: flex; align-items: center;",
      tags$img(
        src = "ctmirror_logo.png",
        alt = "CT Mirror",
        style = "display: block; height: 38px; width: auto;"
      )
    ),
    tags$div(style = "width: 1px; height: 28px; background: #c9c9c9;"), #thin vertical divider
    tags$span(
      "Data Dashboard",
      style = "display: block; margin: 0; font-size: 1.15rem; font-weight: 600; color: #333; letter-spacing: 0.02em; padding-top: 6px"
    )
  ),
  fillable = FALSE,

  # One sidebar for the whole app
  sidebar = sidebar(home_sidebar_ui("home"), width = 320),

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