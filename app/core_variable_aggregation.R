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

# One row per checkbox/level
core_levels <- tribble(
  ~core_var,                  ~level,                          ~lines,
  
  # B01001: lines 3-25 are males by age, 27-49 females by age which is why each level requires two sets of lines
  "Age",                      "Under 5",                       c(3, 27),
  "Age",                      "5 to 17",                       c(4:6, 28:30),
  "Age",                      "18 to 24",                      c(7:10, 31:34),
  "Age",                      "25 to 34",                      c(11:12, 35:36),
  "Age",                      "35 to 44",                      c(13:14, 37:38),
  "Age",                      "45 to 54",                      c(15:16, 39:40),
  "Age",                      "55 to 64",                      c(17:19, 41:43),
  "Age",                      "65 to 74",                      c(20:22, 44:46),
  "Age",                      "75 and over",                   c(23:25, 47:49),
  "Sex",                      "Male",                          2,
  "Sex",                      "Female",                        26,
  
  # B03002: Hispanic (any race) is line 12, the race lines below are all not Hispanic
  "Race and ethnicity",       "Hispanic or Latino (any race)", 12,
  "Race and ethnicity",       "White",                         3,
  "Race and ethnicity",       "Black or African American",     4,
  "Race and ethnicity",       "American Indian and Alaska Native", 5,
  "Race and ethnicity",       "Asian",                         6,
  "Race and ethnicity",       "Native Hawaiian and Other Pacific Islander", 7,
  "Race and ethnicity",       "Some other race",               8,
  "Race and ethnicity",       "Two or more races",             9,
  
  # Medians: line 1 is the median itself!! No aggregation needed :D
  "Median household income",  "Median household income",       1,
  "Median home value",        "Median home value",             1,
  
  # B19001: Merged into 7
  "Household income",         "Under $25,000",                 2:5,
  "Household income",         "$25,000 to $49,999",            6:10,
  "Household income",         "$50,000 to $74,999",            11:12,
  "Household income",         "$75,000 to $99,999",            13,
  "Household income",         "$100,000 to $149,999",          14:15,
  "Household income",         "$150,000 to $199,999",          16,
  "Household income",         "$200,000 or more",              17,
  "Own or rent",              "Own",                           2,
  "Own or rent",              "Rent",                          3,
  
  # B15003: 2-16 no diploma, 17-18 diploma/GED, 19-20 some college, 21-25 degrees
  "Highest degree (age 25+)", "No high school diploma",          2:16,
  "Highest degree (age 25+)", "High school diploma or GED",      17:18,
  "Highest degree (age 25+)", "Some college, no degree",         19:20,
  "Highest degree (age 25+)", "Associate's degree",              21,
  "Highest degree (age 25+)", "Bachelor's degree",               22,
  "Highest degree (age 25+)", "Graduate or professional degree", 23:25,
  
  # B27001: every age group has 3 lines (subtotal, with insurance, without)
  "Health insurance",         "Has health insurance",          c(seq(4, 28, 3), seq(32, 56, 3)),
  "Health insurance",         "No health insurance",           c(seq(5, 29, 3), seq(33, 57, 3)),
  "Poverty",                  "Below the poverty line",        2,
  "Poverty",                  "At or above the poverty line",  3
)

# One row per ACS code, e.g. Age / Under 5 / B01001_003
level_codes <- core_levels |>
  left_join(select(core_vars, core_var, table), by = "core_var") |>
  unnest_longer(lines) |>
  mutate(code = paste0(table, "_", str_pad(lines, 3, pad = "0")))
