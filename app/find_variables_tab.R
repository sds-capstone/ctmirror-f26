# Module - Find variables tab

find_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      dataTableOutput(outputId = NS(id, "table")),
      card_footer(
        actionButton(ns("call_api"), "Please select a row first", class = "btn-primary")
      )
    ),
  )
}

find_variables_server <- function(id, glossary, vars) {
  moduleServer(id, function(input, output, session) {

    # Dictionary to join raw data output from API calls. Identical to workflow in get_data
    dict <- vars |>
      rename(variable = "name") |>
      select(variable, table, label)

    # Glossary of beat-encoded variables from the xlsx file in the directory
    # Some column names are changed for easiest user readability
    glossary <- glossary |>
      rename(code = "name", variable = "concept") |>
      select(-label) |>
      mutate(
        beat = factor(beat),
        `high priority` = factor(`high priority`)
      )

    # Producing the filterable table using the glossary data frame
    output$table <- renderDT({
      datatable(glossary, filter = "top", selection = "single")
    })

    # Fetch the corresponding table of the row the user clicked on
    selected_table <- reactive({
      index <- input$table_rows_selected # DT automatically detects row index
      if (length(index) != 1) return(NULL)
      as.character(glossary[["table"]][index])
    })

    # Keep button label in sync with table row selection
    # Runs once at the start and every time a selection changes
    observe({
      i <- selected_table()
      if (is.null(i)) {
        updateActionButton(session, "call_api", label = "Make a custom API call: Select a row first")
        shinyjs::disable("call_api") # makes button unclickable if nothing is selected
      } else {
        updateActionButton(session, "call_api", label = paste0("Make custom API call for: ", i))
        shinyjs::enable("call_api")
      }
    })

    # Define empty list for non-priority selections and reactive for storing 
    empty_result <- list(table = NULL, row_acs5 = NULL, row_acs1 = NULL)
    acs_new <- reactiveVal(empty_result)

    # Clear stored results when row selection changes
    observeEvent(input$table_rows_selected, {
      acs_new(empty_result)
    }, ignoreNULL = FALSE)

    # Call API when user clicks button
    observeEvent(input$call_api, {
      i <- selected_table()
      
      # Exit if no row is selected -- probably we can delete but just in case
      if (is.null(i)) {
        acs_new(empty_result)
        return()
      }

      # API calls wrapped in progress bar! For user comfort! 
      withProgress(message = paste("Fetching", i), value = 0, {

        # ACS 5-year call
        incProgress(0.1, detail = "5-year ACS, town-level") # incrementing progress bar by 10%
        result1 <- tryCatch(
          get_acs(
            geography = "county subdivision",
            state = "CT",
            table = i,
            summary_var = paste0(i, "_001"),
            year = 2024,
            output = "tidy",
            geometry = FALSE,
            survey = "acs5"
          ),
          error = function(e) {
            message("Skipping ", i, ": ", conditionMessage(e))
            showNotification(paste0("Couldn't get ", i, ": ", conditionMessage(e)),
                             type = "error")
            NULL
          }
        )

        # Cleaning steps for 5year, same as in get_data file
        if (!is.null(result1)) {
          ct_geo <- tigris::county_subdivisions(state = "CT", year = 2024, cb = TRUE) |>
            dplyr::select(GEOID, NAME, geometry)
          result1 <- result1 |>
            select(-NAME) |>
            left_join(dict, by = "variable") |>
            mutate(pct = (estimate / summary_est) * 100) |>
            left_join(ct_geo, by = "GEOID") |>
            mutate(town = sub(" town.*", "", NAME)) |>
            select(-NAME) |>
            sf::st_as_sf()
        }

        # ACS 1-year call
        years <- setdiff(2005:2024, 2020) # need to eventually be dynamic
        step <- 0.85 / length(years) # step length for progress bar incrementation!

        result2 <- lapply(years, function(x) {
          incProgress(step, detail = paste("1-year ACS, ", x)) # incrementing progress bar every year in the loop

          df <- tryCatch(
            get_acs(
              geography = "state",
              table = i,
              state = "CT",
              summary_var = paste0(i, "_001"),
              survey = "acs1",
              geometry = FALSE,
              year = x
            ),
            error = function(e) {
              message("Skipping ", x, ": ", conditionMessage(e))
              NULL
            }
          )
          if (is.null(df)) return(NULL)
          mutate(df, year = x)
        }) |>
          bind_rows()     

        # Potential error handling(not quite sure we need this) and cleaning (same as in get_data)
        if (nrow(result2) == 0) {
          result2 <- NULL
          showNotification(paste0("No 1-year data found for ", i), type = "warning")
        } else {
          result2 <- result2 |>
            select(-NAME) |>
            left_join(dict, by = "variable") |>
            mutate(pct = (estimate / summary_est) * 100)
        }
      })

      # Updating reactive object with a list of the ad hoc objects
      acs_new(list(table = i, row_acs5 = result1, row_acs1 = result2))
    })

    # Return reactive list
    acs_new
  })
}
