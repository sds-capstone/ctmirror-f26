home_test_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("beat"), "Beat", choices = NULL),
      selectizeInput(ns("table"), "Variable", choices = NULL,
                     options = list(placeholder = "Type to search...")),
      uiOutput(ns("filters_ui")),
      uiOutput(ns("percent_of_ui"))
    ),
    leafletOutput(ns("map")),
    plotlyOutput(ns("time_series"))
  )
}

home_test_server <- function(id, acs5, acs1) {

  towns <- acs5 |>
    sf::st_drop_geometry() |>
    filter(!str_detect(town, "not defined"))
  shapes <- acs5[!duplicated(acs5$GEOID) & !str_detect(acs5$town, "not defined"), c("GEOID", "town")]

  # One row per table per beat, only tables that are in acs5
  tables <- vars |>
    filter(table %in% towns$table) |>
    distinct(table, concept, beats) |>
    separate_longer_delim(beats, delim = ",") |>
    mutate(beat = str_to_title(str_squish(beats))) |>
    filter(!is.na(beat), beat != "Meta")

  # Medians, indexes etc. can't be added up: show one line at a time instead
  not_additive <- "\\b(median|mean|average|per capita|aggregate)\\b"
  # Lines overlap or leave people out, so they can't be added up either. Each line is shown
  # as a percent of the line above it instead ("Rented > Over 30 Percent" = % of renters).
  # B25140: "Over 50 Percent" is also "Over 30 Percent". B26103/B26203 list only some types.
  share_of_parent <- c("B25140", "B26103", "B26203")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    updateSelectInput(session, "beat", choices = c("All beats", sort(unique(tables$beat))))

    observeEvent(input$beat, {
      in_beat <- if (input$beat == "All beats") tables else filter(tables, beat == input$beat)
      in_beat <- distinct(in_beat, table, concept)
      updateSelectizeInput(session, "table", choices = setNames(in_beat$table, in_beat$concept),
                           selected = "", server = TRUE)
    })

    # Everything about the chosen table
    tbl <- reactive({
      req(input$table)
      concept <- tables$concept[tables$table == input$table][1]
      leaves <- filter(table_dimensions, table == input$table)
      list(
        id = input$table,
        concept = concept,
        leaves = leaves,
        dims = dim_names(input$table, concept),
        additive = !str_detect(concept, regex(not_additive, ignore_case = TRUE)) && nrow(leaves) > 1 &&
          !input$table %in% share_of_parent,
        of_parent = input$table %in% share_of_parent,
        dollars = str_detect(concept, regex("dollars", ignore_case = TRUE))
      )
    })

    # Input ids include the table, so picks never carry over to another table
    filter_id <- function(d) paste0(tbl()$id, "_", d)

    output$filters_ui <- renderUI({
      t <- tbl()
      map(names(t$dims), \(d) {
        values <- unique(na.omit(t$leaves[[d]]))
        if (t$additive) {
          selectizeInput(ns(filter_id(d)), t$dims[[d]], choices = values, multiple = TRUE,
                         selected = if (d == names(t$dims)[1]) values[1],
                         options = list(placeholder = "All"))
        } else {
          selectInput(ns(filter_id(d)), t$dims[[d]], choices = c("All", values))
        }
      })
    })

    # "Percent of" spells out the group: with Male + Under 5 years picked, the choices are
    # Everyone, Male (= % of males who are under 5) and Under 5 years (= % of under-5s who are male)
    output$percent_of_ui <- renderUI({
      t <- tbl()
      req(t$additive, length(t$dims) >= 2)
      p <- compact(picks())
      groups <- map_chr(names(p), \(d) paste(map_chr(p[setdiff(names(p), d)], \(v) paste(v, collapse = " or ")), collapse = ", "))
      keep <- groups != ""
      choices <- c("Everyone" = "table", setNames(names(p)[keep], groups[keep]))
      selected <- isolate(input$percent_of)
      selectInput(ns("percent_of"), "Percent of", choices = choices,
                  selected = if (isTRUE(selected %in% choices)) selected else "table")
    })

    picks <- reactive(map(set_names(names(tbl()$dims) %||% character()), \(d) input[[filter_id(d)]]))

    # Codes that match the picks; skip = a dim to ignore (for the denominator).
    # If the Census publishes that exact group as its own line (e.g. "Male"), use it;
    # otherwise add up the matching leaves.
    matching_codes <- function(skip = NULL) {
      p <- compact(picks()[setdiff(names(picks()), skip)])
      if (all(lengths(p) == 1)) {
        lines <- filter(vars, table == tbl()$id)
        exact <- lines$name[label_path(lines$label) == paste(unlist(p), collapse = "!!")]
        if (length(exact) == 1) return(exact)
      }
      leaves <- tbl()$leaves
      keep <- rep(TRUE, nrow(leaves))
      for (d in setdiff(names(picks()), skip)) {
        chosen <- picks()[[d]]
        if (length(chosen) > 0) keep <- keep & leaves[[d]] %in% chosen
      }
      leaves$code[keep]
    }

    # Not additive: the one line whose label is exactly the picks, e.g. "Female!!Bachelor's degree"
    single_code <- reactive({
      t <- tbl()
      if (nrow(t$leaves) <= 1) return(c(t$leaves$code, paste0(t$id, "_001"))[1])
      chosen <- unlist(picks())
      chosen <- chosen[chosen != "All"]
      rows <- filter(vars, table == t$id)
      code <- rows$name[label_path(rows$label) == paste(chosen, collapse = "!!")]
      validate(need(length(code) == 1, "The Census doesn't publish that combination. Try \"All\" for one of the filters."))
      code
    })

    # The line one level up from the picked line ("Rented > Over 30 Percent" -> "Rented")
    parent_code <- reactive({
      rows <- filter(vars, table == tbl()$id)
      path <- label_path(rows$label[rows$name == single_code()])
      rows$name[label_path(rows$label) == str_remove(path, "(^|!!)[^!]*$")]
    })

    codes <- reactive({
      if (!tbl()$additive) return(single_code())
      codes <- matching_codes()
      validate(need(length(codes) > 0, "No lines match these picks. Try clearing a filter."))
      codes
    })

    percent_of <- reactive({
      dims <- names(tbl()$dims)
      if (tbl()$additive && length(dims) >= 2 && isTRUE(input$percent_of %in% dims)) input$percent_of else "table"
    })

    shows_percent <- reactive(tbl()$additive || tbl()$of_parent)

    format_value <- function(x) {
      if (shows_percent()) paste0(round(x, 1), "%")
      else if (tbl()$dollars) dollar(x)
      else comma(x)
    }

    # Adds up towns or acs1 by `group` ("GEOID" or "year"): count, pct and its denominator
    summarise_codes <- function(data, group) {
      codes <- codes()
      d <- data |>
        filter(variable %in% codes) |>
        group_by(across(all_of(group))) |>
        summarise(count = sum(estimate), pct = sum(pct), .groups = "drop") |>
        mutate(denom = count / pct * 100)
      if (percent_of() != "table") {
        denom_codes <- matching_codes(skip = percent_of())
        denoms <- data |>
          filter(variable %in% denom_codes) |>
          group_by(across(all_of(group))) |>
          summarise(denom = sum(estimate), .groups = "drop")
        d <- d |>
          select(-denom) |>
          left_join(denoms, by = group) |>
          mutate(pct = count / denom * 100)
      }
      if (tbl()$of_parent) {
        parent_code <- parent_code()
        d <- data |>
          filter(variable == parent_code) |>
          group_by(across(all_of(group))) |>
          summarise(denom = sum(estimate), .groups = "drop") |>
          right_join(select(d, -denom), by = group) |>
          mutate(pct = count / denom * 100)
      }
      mutate(d, value = if (shows_percent()) pct else count)
    }

    var_label <- reactive({
      chosen <- unlist(picks())
      chosen <- chosen[chosen != "All"]
      if (length(chosen) == 0) tbl()$concept else paste0(tbl()$concept, ": ", paste(chosen, collapse = ", "))
    })

    # Every town stays on the map; towns with nobody in the table (0 of 0) are gray
    ct <- reactive({
      left_join(shapes, summarise_codes(towns, "GEOID"), by = "GEOID") |>
        mutate(value = if_else(is.finite(value), value, NA))
    })

    time_series_data <- reactive({
      summarise_codes(acs1, "year") |>
        filter(is.finite(value)) |>
        mutate(tooltip = paste0(year, ": ", format_value(value)))
    })

    # Map (same as the main Home map; hover also shows the count for percents)
    output$map <- renderLeaflet({
      d <- ct()
      pal <- colorNumeric(palette = "magma", domain = d$value)
      pal_reverse <- colorNumeric(palette = "magma", domain = d$value, reverse = TRUE)
      labels <- paste0(d$town, ": ", format_value(d$value),
                       if (shows_percent()) paste0(" (", comma(d$count), " of ", comma(d$denom), ")"))
      labels[is.na(d$value)] <- paste0(d$town[is.na(d$value)], ": no data")
      leaflet() |>
        addProviderTiles(providers$OpenStreetMap) |>
        addPolygons(data = d,
                    color = ~pal_reverse(value),
                    weight = 0.5,
                    smoothFactor = 0.2,
                    fillOpacity = 0.75,
                    label = labels,
                    highlightOptions = highlightOptions(
                      color = "red",
                      weight = 2,
                      bringToFront = TRUE)) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = d$value,
          title = str_replace_all(str_wrap(var_label(), 30), "\n", "<br>"),
          labFormat = labelFormat(
            prefix = if (!tbl()$additive && tbl()$dollars) "$" else "",
            suffix = if (shows_percent()) "%" else "",
            big.mark = ",",
            transform = function(x) sort(x, decreasing = TRUE)))
    })

    # Time series (same as the main Home one)
    output$time_series <- renderPlotly({
      d <- time_series_data()
      validate(need(nrow(d) > 0, "No 1-year data for this selection."))
      g <- suppressWarnings(ggplot(d, aes(x = year, y = value)) +
        geom_line(linewidth = 0.8) +
        geom_point(aes(text = tooltip)) +
        theme_minimal() +
        labs(title = paste0("Statewide Summary: ", var_label()), x = "Year", y = NULL) +
        scale_y_continuous(labels = \(x) format_value(x), limits = c(0, NA)))
      plotly::ggplotly(g, tooltip = "text")
    })
  })
}
