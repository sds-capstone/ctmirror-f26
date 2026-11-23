# Module - Sidebar shared by every tab
# The sidebar belongs to the app, not to one tab: it owns the beat, variable, filter and
# "Percent of" picks, and hands them to every tab that needs them (Home, Data, ...).
# Switching tabs never resets it, and changes made on other tabs (e.g. fetching a variable
# on Find variables) update it without throwing away the user's picks.

# Medians, indexes etc. can't be added up: show one line at a time instead
not_additive <- "\\b(median|mean|average|per capita|aggregate|gini|index)\\b"
# Lines overlap or leave people out, so they can't be added up either. Each line is shown
# as a percent of the line above it instead ("Rented > Over 30 Percent" = % of renters).
# B25140: "Over 50 Percent" is also "Over 30 Percent". B26103/B26203 list only some types.
share_of_parent <- c("B25140", "B26103", "B26203")

beat_defaults <- c(
  "All beats" = "B01003", # Total Population
  "Demographic" = "B01003", # Total Population
  "Economy" = "B23025", # Employment Status
  "Education" = "B15003", # Educational Attainment
  "Health" = "B27001", # Health Insurance Coverage Status
  "Housing" = "B25077", # Median Value (Dollars)
  "Human Services" = "B22003", # Receipt of Food Stamps/SNAP
  "Immigration" = "B05002", # Place of Birth by Nativity and Citizenship Status
  "Income" = "B19013", # Median Household Income
  "Language" = "C16001", # Language Spoken at Home
  "Race" = "B02001"  # Race
)

# What the sidebar is doing on each tab (keys are the nav_panel values in app.R)
tab_notes <- c(
  home = "These selections drive the map and the statewide trend.",
  find_variables = "Variables you fetch here are added to the list below and selected for you.",
  data = "The tables on this tab show the selection below."
)

sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Select & Search Variables"),
    tags$div(                        # DO NOT REMOVE THIS EMPTY DIV - fixes the Shiny client error "Unable to find selectize-plugin-a11y plugin"
      style = "display:none;",       # Forces Selectize dependencies/plugins to load up front rather than dynamically
      selectizeInput("selectize_preload", label = NULL, choices = NULL)
    ),
    uiOutput(ns("status")),
    # Open menu is exactly as wide as the selected box; long option text wraps instead of widening it
    tags$style(HTML("
      .bs-container .dropdown-menu,
      .bootstrap-select .dropdown-menu { min-width: 0 !important; max-width: 100%; }
      .bs-container .dropdown-menu li a span.text,
      .bootstrap-select .dropdown-menu li a span.text { white-space: normal; }
      .bs-container .dropdown-menu .inner,
      .bootstrap-select .dropdown-menu .inner { overflow-x: hidden; }
    ")),
    # Beat and Variable share the same look: dropdown with a search bar inside the open panel.
    # Beat starts with "All beats" already selected so the app never opens on a blank beat.
    shinyWidgets::pickerInput(
      ns("beat"), "Beat",
      choices = "All beats", selected = "All beats",
      width = "100%",
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search beats...",
        liveSearchNormalize = TRUE,
        container = "body",
        size = 10
      )
    ),
    shinyWidgets::pickerInput(
      ns("table"), "Variable",
      choices = NULL,
      width = "100%",
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search variables...",
        liveSearchNormalize = TRUE,
        container = "body",
        size = 10,
        noneResultsText = "No variables match {0}"
      )
    ),
    uiOutput(ns("filters_ui")),
    uiOutput(ns("percent_of_ui"))
  )
}

# acs5: reactive town-level data (cached + ad hoc). active_tab: reactive value of the open tab.
# jump_to: reactive list(id, at) that fires when a variable is fetched on another tab.
# Returns the selection as reactives: tbl (everything about the chosen table), picks, percent_of.
sidebar_server <- function(id, acs5, active_tab = reactive(NULL), jump_to = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Tables that have town-level data, one row per table per beat
    tables <- reactive({
      town_tables <- unique(acs5()$table)
      vars |>
        filter(table %in% town_tables) |>
        distinct(table, concept, beats) |>
        separate_longer_delim(beats, delim = ",") |>
        mutate(beat = str_to_title(str_squish(beats))) |>
        filter(!is.na(beat), beat != "Meta")
    })

    # A variable fetched on another tab that should be selected once the dropdowns catch up
    target <- reactiveVal(NULL)

    observeEvent(jump_to(), {
      id <- jump_to()$id
      req(id)
      if (id %in% tables()$table) {
        target(id)
      } else {
        # No beat or no town-level rows: it can't go in the dropdown, so don't wait for it
        showNotification(paste0(id, " was fetched, but it has no town-level data to map."),
                         type = "warning")
      }
    }, priority = 10)

    # Beat dropdown: refreshed at startup and whenever the data changes
    observeEvent(tables(), {
      current <- isolate(input$beat)
      in_current <- isTruthy(current) &&
        (current == "All beats" || isTRUE(target() %in% tables()$table[tables()$beat == current]))
      shinyWidgets::updatePickerInput(
        session, "beat",
        choices  = c("All beats", sort(unique(tables()$beat))),
        # Move to All beats only when nothing valid is selected yet, or when a fetched
        # variable is waiting that isn't in the user's beat; otherwise keep the user's beat
        selected = if (!isTruthy(current) || (!is.null(target()) && !in_current)) "All beats" else current
      )
    }, priority = 5)

    # Variable dropdown: refreshed when the beat or the data changes
    observeEvent(list(input$beat, tables()), {
      req(input$beat)
      d <- tables()
      if (input$beat != "All beats") d <- d[d$beat == input$beat, ]
      d <- distinct(d, table, concept)
      current <- isolate(input$table)

      selected <- if (!is.null(target()) && target() %in% d$table) {
        target()                                    # a variable was just fetched on another tab
      } else if (isTruthy(current) && current %in% d$table) {
        current                                     # keep the user's variable if it is still listed
      } else if (isTRUE(beat_defaults[input$beat] %in% d$table)) {
        unname(beat_defaults[input$beat])           # otherwise the beat's default
      } else {
        d$table[1]                                  # a beat with no default
      }

      if (identical(selected, target())) target(NULL)

      shinyWidgets::updatePickerInput(session, "table",
                                      choices  = setNames(d$table, d$concept),
                                      selected = selected)
    })

    # Everything about the chosen table. Reads only input$table and the fixed variable list,
    # so data changes from other tabs don't redraw the filters and wipe the user's picks.
    tbl <- reactive({
      req(input$table)
      concept <- vars$concept[vars$table == input$table][1]
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
                         options = list(placeholder = "All", plugins = list("remove_button")))
        } else {
          selectInput(ns(filter_id(d)), t$dims[[d]], choices = c("All", values))
        }
      })
    })

    picks <- reactive(map(set_names(names(tbl()$dims) %||% character()), \(d) input[[filter_id(d)]]))

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

    percent_of <- reactive({
      dims <- names(tbl()$dims)
      if (tbl()$additive && length(dims) >= 2 && isTRUE(input$percent_of %in% dims)) input$percent_of else "table"
    })

    # One line telling the user what the sidebar is doing on the open tab
    output$status <- renderUI({
      note <- tab_notes[active_tab() %||% "home"]
      req(!is.na(note))
      tags$p(class = "text-muted small", note)
    })

    list(tbl = tbl, picks = picks, percent_of = percent_of)
  })
}
