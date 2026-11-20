# Module - Home tab (map and statewide trend)
# The controls live in the app-wide sidebar (sidebar.R); this module only reads its selection.

# Last year of the ACS 5-year window behind data/acs5.rds (the file has no year column).
# Update this when the data is refreshed; the map caption reads from it.
acs5_end_year <- 2024

# Map and time series (go in the tab body)
home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")),
    uiOutput(ns("map_caption")),
    plotlyOutput(ns("time_series")),
    uiOutput(ns("trend_caption"))
  )
}

# sel: the selection returned by sidebar_server() (tbl, picks, percent_of)
home_server <- function(id, acs5, acs1, sel) {

  towns <- reactive({acs5() |>
    sf::st_drop_geometry() |>
    filter(!str_detect(town, "not defined"))})
  shapes <- reactive({acs5()[!duplicated(acs5()$GEOID) & !str_detect(acs5()$town, "not defined"), c("GEOID", "town")]})

  moduleServer(id, function(input, output, session) {

    # Codes that match the picks; skip = a dim to ignore (for the denominator).
    # If the Census publishes that exact group as its own line (e.g. "Male"), use it;
    # otherwise add up the matching leaves.
    matching_codes <- function(skip = NULL) {
      p <- compact(sel$picks()[setdiff(names(sel$picks()), skip)])
      if (all(lengths(p) == 1)) {
        lines <- filter(vars, table == sel$tbl()$id)
        exact <- lines$name[label_path(lines$label) == paste(unlist(p), collapse = "!!")]
        if (length(exact) == 1) return(exact)
      }
      leaves <- sel$tbl()$leaves
      keep <- rep(TRUE, nrow(leaves))
      for (d in setdiff(names(sel$picks()), skip)) {
        chosen <- sel$picks()[[d]]
        if (length(chosen) > 0) keep <- keep & leaves[[d]] %in% chosen
      }
      leaves$code[keep]
    }

    # Not additive: the one line whose label is exactly the picks, e.g. "Female!!Bachelor's degree"
    single_code <- reactive({
      t <- sel$tbl()
      if (nrow(t$leaves) <= 1) return(c(t$leaves$code, paste0(t$id, "_001"))[1])
      chosen <- unlist(sel$picks())
      chosen <- chosen[chosen != "All"]
      rows <- filter(vars, table == t$id)
      code <- rows$name[label_path(rows$label) == paste(chosen, collapse = "!!")]
      validate(need(length(code) == 1, "The Census doesn't publish that combination. Try \"All\" for one of the filters."))
      code
    })

    # The line one level up from the picked line ("Rented > Over 30 Percent" -> "Rented")
    parent_code <- reactive({
      rows <- filter(vars, table == sel$tbl()$id)
      path <- label_path(rows$label[rows$name == single_code()])
      rows$name[label_path(rows$label) == str_remove(path, "(^|!!)[^!]*$")]
    })

    codes <- reactive({
      if (!sel$tbl()$additive) return(single_code())
      codes <- matching_codes()
      validate(need(length(codes) > 0, "No lines match these picks. Try clearing a filter."))
      codes
    })


    shows_percent <- reactive(sel$tbl()$additive || sel$tbl()$of_parent)

    format_value <- function(x) {
      if (shows_percent()) paste0(round(x, 1), "%")
      else if (sel$tbl()$dollars) dollar(x)
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
      if (sel$percent_of() != "table") {
        denom_codes <- matching_codes(skip = sel$percent_of())
        denoms <- data |>
          filter(variable %in% denom_codes) |>
          group_by(across(all_of(group))) |>
          summarise(denom = sum(estimate), .groups = "drop")
        d <- d |>
          select(-denom) |>
          left_join(denoms, by = group) |>
          mutate(pct = count / denom * 100)
      }
      if (sel$tbl()$of_parent) {
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
      chosen <- unlist(sel$picks())
      chosen <- chosen[chosen != "All"]
      if (length(chosen) == 0) sel$tbl()$concept else paste0(sel$tbl()$concept, ": ", paste(chosen, collapse = ", "))
    })

    # Every town stays on the map; towns with nobody in the table (0 of 0) are gray
    ct <- reactive({
      left_join(shapes(), summarise_codes(towns(), "GEOID"), by = "GEOID") |>
        mutate(value = if_else(is.finite(value), value, NA))
    })

    time_series_data <- reactive({
      summarise_codes(acs1(), "year") |>
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
            prefix = if (!sel$tbl()$additive && sel$tbl()$dollars) "$" else "",
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

    # Captions: where the numbers come from (one under the map, one under the trend)
    output$map_caption <- renderUI({
      tags$p(
        class = "text-muted small mt-2",
        "Map: ", acs5_end_year - 4, "–", acs5_end_year, " American Community Survey (ACS) ",
        "5-year estimates. These estimates pool five years of survey responses, which makes them reliable ",
        "for every town in Connecticut. However, these estimates are less current than 1-year ACS data.",
      
      )
    })

    # Wording follows the selected variable (ad hoc variables may lack 1-year data)
    output$trend_caption <- renderUI({
      table_id <- sel$tbl()$id
      y1 <- sort(unique(as.numeric(acs1()$year[startsWith(acs1()$variable, paste0(table_id, "_"))])))
      y1 <- y1[is.finite(y1)]

      if (length(y1) == 0) {
        text <- "Trend: no ACS 1-year data is available for this variable, so there is no statewide time series."
      } else {
        gaps <- setdiff(seq(min(y1), max(y1)), y1)
        # Collapse runs of missing years: c(2006:2015, 2020) -> "2006–2015", "2020"
        as_ranges <- function(x) {
          runs <- split(x, cumsum(c(1, diff(x) != 1)))
          vapply(runs, \(r) if (length(r) == 1) as.character(r) else paste0(min(r), "–", max(r)), character(1))
        }
        text <- paste0(
          "Trend: statewide ACS 1-year estimates, ", min(y1),
          if (max(y1) > min(y1)) paste0("–", max(y1)),
          if (length(gaps) > 0) paste0(" (no data for ", paste(as_ranges(gaps), collapse = ", "),
                                       if (2020 %in% gaps) "; the Census Bureau did not publish standard 1-year estimates for 2020", ")"),
          ". 1-year estimates are the most current, but they come from a smaller sample and are only ",
          "published for areas with 65,000+ residents, so they are not available for most towns. ",
          "They can differ from the 5-year estimates on the map, so compare the two with care."
        )
      }
      tags$p(class = "text-muted small mt-2", text)
    })

    list(ct = ct, time_series_data = time_series_data, var_label = var_label)
  })
}
