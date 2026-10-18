library(tidyverse)
library(readxl)

file <- here::here("acs5_2024_variables.xlsx")

vars <- read_excel(file, sheet = "All Variables") |>
  select(name, label, table) |>
  inner_join(read_excel(file, sheet = "Variable Coding") |>
               select(table, concept, beats = beat, priority = `high priority`),
              by = "table") |>
  filter(!str_detect(table, "PR$")) |>
  mutate(priority  = priority %in% "yes",
         var_label = paste0(concept, ": ",
                            label |>
                              str_remove("^Estimate!!") |>
                              str_remove_all(":| ?--") |>
                              str_replace_all("!!", " > ")))

table_beats <- vars |>
  distinct(table, beat = beats) |>
  separate_longer_delim(beat, delim = ",") |>
  mutate(beat = str_to_lower(str_trim(beat))) |>
  filter(beat != "meta")