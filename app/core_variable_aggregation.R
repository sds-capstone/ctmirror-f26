# Highest priority variables for the suggested home tab

# One row per highest priority variable, "count" = requires manual aggregation and "median" = already only 1 row
core_vars <- tribble(
  ~beat,            ~core_var,                    ~table,   ~type,    ~universe,
  "Demographics",   "Age",                        "B01001", "count",  "total population",
  "Demographics",   "Sex",                        "B01001", "count",  "total population",
  "Race",           "Race and ethnicity",         "B03002", "count",  "total population",
  "Income",         "Median household income",    "B19013", "median", "households",
  "Income",         "Household income",           "B19001", "count",  "households",
  "Housing",        "Median home value",          "B25077", "median", "owner-occupied homes",
  "Housing",        "Own or rent",                "B25003", "count",  "occupied homes",
  "Education",      "Highest degree (age 25+)",   "B15003", "count",  "people 25 and over",
  "Health",         "Health insurance",           "B27001", "count",  "civilian noninstitutionalized population",
  "Human Services", "Poverty",                    "B17101", "count",  "people in housing units"
)

# One row per checkbox/level. `labels` = the end of the Census label for each line to add up,
# e.g. "Under 5 years" matches both "Total:!!Male:!!Under 5 years" and "Total:!!Female:!!Under 5 years".
# Medians don't need labels: they always use the table's line 001.
core_levels <- tribble(
  ~core_var,                  ~level,                          ~labels,
  "Age",                      "Under 5",                       "Under 5 years",
  "Age",                      "5 to 17",                       c("5 to 9 years", "10 to 14 years", "15 to 17 years"),
  "Age",                      "18 to 24",                      c("18 and 19 years", "20 years", "21 years", "22 to 24 years"),
  "Age",                      "25 to 34",                      c("25 to 29 years", "30 to 34 years"),
  "Age",                      "35 to 44",                      c("35 to 39 years", "40 to 44 years"),
  "Age",                      "45 to 54",                      c("45 to 49 years", "50 to 54 years"),
  "Age",                      "55 to 64",                      c("55 to 59 years", "60 and 61 years", "62 to 64 years"),
  "Age",                      "65 to 74",                      c("65 and 66 years", "67 to 69 years", "70 to 74 years"),
  "Age",                      "75 and over",                   c("75 to 79 years", "80 to 84 years", "85 years and over"),
  "Sex",                      "Male",                          "Total!!Male",
  "Sex",                      "Female",                        "Total!!Female",
 
  # B03002: the race groups are the "Not Hispanic or Latino" ones
  "Race and ethnicity",       "Hispanic or Latino (any race)", "Total!!Hispanic or Latino",
  "Race and ethnicity",       "White",                         "Not Hispanic or Latino!!White alone",
  "Race and ethnicity",       "Black or African American",     "Not Hispanic or Latino!!Black or African American alone",
  "Race and ethnicity",       "American Indian and Alaska Native", "Not Hispanic or Latino!!American Indian and Alaska Native alone",
  "Race and ethnicity",       "Asian",                         "Not Hispanic or Latino!!Asian alone",
  "Race and ethnicity",       "Native Hawaiian and Other Pacific Islander", "Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone",
  "Race and ethnicity",       "Some other race",               "Not Hispanic or Latino!!Some other race alone",
  "Race and ethnicity",       "Two or more races",             "Not Hispanic or Latino!!Two or more races",
 
  "Median household income",  "Median household income",       NA,
  "Median home value",        "Median home value",             NA,
 
  # B19001: 16 brackets merged into 7
  "Household income",         "Under $25,000",                 c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999", "$20,000 to $24,999"),
  "Household income",         "$25,000 to $49,999",            c("$25,000 to $29,999", "$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999"),
  "Household income",         "$50,000 to $74,999",            c("$50,000 to $59,999", "$60,000 to $74,999"),
  "Household income",         "$75,000 to $99,999",            "$75,000 to $99,999",
  "Household income",         "$100,000 to $149,999",          c("$100,000 to $124,999", "$125,000 to $149,999"),
  "Household income",         "$150,000 to $199,999",          "$150,000 to $199,999",
  "Household income",         "$200,000 or more",              "$200,000 or more",
 
  "Own or rent",              "Own",                           "Owner occupied",
  "Own or rent",              "Rent",                          "Renter occupied",
 
  "Highest degree (age 25+)", "No high school diploma",        c("No schooling completed", "Nursery school", "Kindergarten",
                                                                 paste(c("1st", "2nd", "3rd", paste0(4:11, "th")), "grade"),
                                                                 "12th grade, no diploma"),
  "Highest degree (age 25+)", "High school diploma or GED",    c("Regular high school diploma", "GED or alternative credential"),
  "Highest degree (age 25+)", "Some college, no degree",       c("Some college, less than 1 year", "Some college, 1 or more years, no degree"),
  "Highest degree (age 25+)", "Associate's degree",            "Associate's degree",
  "Highest degree (age 25+)", "Bachelor's degree",             "Bachelor's degree",
  "Highest degree (age 25+)", "Graduate or professional degree", c("Master's degree", "Professional school degree", "Doctorate degree"),
 
  # B27001: matches the line under every sex/age group
  "Health insurance",         "Has health insurance",          "With health insurance coverage",
  "Health insurance",         "No health insurance",           "No health insurance coverage",
 
  "Poverty",                  "Below the poverty line",        "Income in the past 12 months below poverty level",
  "Poverty",                  "At or above the poverty line",  "Income in the past 12 months at or above poverty level"
)
 
# Census labels with the parts that change between years removed
clean_label <- function(x) x |> str_remove("^Estimate!!") |> str_remove_all(":") |> str_to_lower()
 
acs_labels <- vars |>
  distinct(code = name, table, label) |>
  mutate(path = clean_label(label))
 
# One row per ACS code, e.g. Age / Under 5 / B01001_003.
count_codes <- core_levels |>
  left_join(select(core_vars, core_var, table, type), by = "core_var") |>
  filter(type == "count") |>
  unnest_longer(labels) |>
  mutate(pattern = clean_label(labels)) |>
  inner_join(acs_labels, by = "table", relationship = "many-to-many") |>
  filter(path == pattern | str_ends(path, fixed(paste0("!!", pattern))))
 
median_codes <- core_vars |>
  filter(type == "median") |>
  transmute(core_var, level = core_var, table, code = paste0(table, "_001"))
 
level_codes <- bind_rows(count_codes, median_codes) |>
  distinct(core_var, level, table, code)
 
# if a label didn't match anything (typo, or the Census reworded it)
unmatched <- core_levels |>
  unnest_longer(labels) |>
  filter(!is.na(labels)) |>
  anti_join(count_codes, by = c("core_var", "level", "labels"))
if (nrow(unmatched) > 0) {
  warning("These labels matched no ACS line: ",
          paste0(unmatched$core_var, " / ", unmatched$labels, collapse = "; "))
}