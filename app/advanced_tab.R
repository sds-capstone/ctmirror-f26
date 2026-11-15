advanced_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(max_height = 350)
  )
}

advanced_server <- function(id, acs5_ts) {
  moduleServer(id, function(input, output, session) {

  })

}