# Module - Data tab

data_UI <- function(id) {
  ns <- NS(id)
  tagList(
    card(h5("What is the American Community Survey?"), 
    p("The U.S. Census Bureau authors the American Community Survey (ACS), a publicly available data set started in 2005 to collect information about demographics, education, economics, 
    housing, and more. About 3.5 million residential addresses are sampled yearly across the country."),
    h5("Central ACS Data Products"), p("There are different types of ACS releases, including 1-Year Estimates, which contains 12-month data for areas with 
    populations of 65,000+ individuals, and 5-Year Estimates, which merges 60-month data for areas of all population sizes."),
    h5("Our Data Products"), p("The first table below is filtered by your variable selection and contains town-level CT data from the most recent 5-year Estimate.
    The second table below is filtered by your variable selection and contains statewide CT data from 1-Year Estimates.")),
    card(max_height = 350, tableOutput(outputId = NS(id, "town_table"))),
    downloadButton(outputId = NS(id, "download_map_data"), label = "Download Data"),
    card(max_height = 350, tableOutput(outputId = NS(id, "time_series"))),
    downloadButton(outputId = NS(id, "download_time_series"), label = "Download Data"),

  )
}

data_Server <- function(id, ct, time_series_data, var_label) {
  moduleServer(id, function(input, output, session) {
    
    output$town_table <- renderTable({
      ct() |>
        sf::st_drop_geometry()
    })

    output$download_map_data <- downloadHandler(
      filename = function() {paste0(gsub(" ", "_", var_label()), ".csv")},
       content = function(file) {
           clean_data = sf::st_drop_geometry(ct())
         write.csv(clean_data, file)}
     ) 

    output$time_series <- renderTable({
      time_series_data() |>
        select(-tooltip)
    })

    yr_range <- reactive({
      min <- time_series_data() |> pull(year) |> min()
      max <- time_series_data() |> pull(year) |> max()
      list(min = min, max = max)
    })

    output$download_time_series <- downloadHandler(
      filename = function() {paste0(gsub(" ", "_", var_label()), "_Statewide_", yr_range()$min, "-", yr_range()$max, ".csv")},
       content = function(file) {
           d <- time_series_data() |> select(-tooltip)
         write.csv(d, file)}
     ) 
    
  })
}
