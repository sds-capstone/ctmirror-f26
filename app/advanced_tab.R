
# Map and time series (go in the tab body)
advanced_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(h5("Advanced Explorer")),
  )
}

advanced_server <- function(id, acs5, acs1) {

  moduleServer(id, function(input, output, session) {
  })
}