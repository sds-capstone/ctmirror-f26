library(shiny)
library(bslib)
library(tidycensus)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Hello World!"
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {


  
}

shinyApp(ui = ui, server = server)