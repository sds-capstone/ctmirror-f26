# Splits every ACS table into dimensions for the Home Test filters. "Estimate!!Total:!!Male:!!Under 5 years:!!With a disability"
# ie dim1 = "Male", dim2 = "Under 5 years", dim3 = "With a disability" (up to dim7)

# "Estimate!!Total:!!Male:!!Under 5 years" -> "Male!!Under 5 years" (the total itself -> "")
label_path <- function(label) {
  label |> str_remove("^Estimate!!") |> str_remove_all(":") |> str_remove("^Total!?!?")
}

# Every path that has another path underneath it: "Male!!Under 5 years" -> "Male"
parent_paths <- function(path) {
  parts <- str_split(path, "!!")
  unique(unlist(map(parts[lengths(parts) > 1], \(p) map_chr(seq_len(length(p) - 1), \(k) paste(p[1:k], collapse = "!!")))))
}

# TRUE for a line whose groups all show up again in a longer line of the same table
repeated_total <- function(path) {
  parts <- str_split(path, "!!")
  longest <- max(lengths(parts))
  map_lgl(parts, \(p) length(p) < longest &&
            any(map_lgl(parts[lengths(parts) > length(p)], \(q) all(p %in% q))))
}

# One row per most-detailed line ("leaf") of every table
table_dimensions <- vars |>
  filter(!str_detect(table, "PR$")) |>
  mutate(path = label_path(label)) |>
  group_by(table) |>

  # a row is a leaf if no other row in its table sits underneath it
  filter(path != "", !path %in% parent_paths(path)) |>

  # some tables repeat a breakdown's totals at the top (B21001 starts with Veteran /
  # Nonveteran, then Male > 18 to 34 > Veteran ...); those repeats would count twice
  filter(!repeated_total(path)) |>
  ungroup() |>
  separate_wider_delim(path, "!!", names = paste0("dim", 1:7), cols_remove = FALSE,
                       too_few = "align_start", too_many = "merge") |>
  select(code = name, table, path, dim1:dim7)

# Friendly names for a table's dims, guessed from its concept and matched by content:
# "Health Insurance Coverage Status by Sex by Age" -> dim1 "Sex", dim2 "Age",
# dim3 "Health Insurance Coverage Status". Dims with only one value are left out
dim_names <- function(tbl, concept) {
  leaves <- filter(table_dimensions, table == tbl)
  dims <- keep(paste0("dim", 1:7), \(d) n_distinct(na.omit(leaves[[d]])) > 1)
  if (length(dims) == 0) return(character())
  values <- map(set_names(dims), \(d) unique(na.omit(leaves[[d]])))
  parts <- str_split_1(concept, " by ") |>
    str_remove_all(" \\(.*?\\)") |>
    str_remove(" (in|for|of) the .*$")

  # share of a dim's values that mention the concept part ("Disability Status" -> "disab", "statu")
  score <- function(v, part) {
    if (part == "Sex") return(as.numeric(all(str_detect(v, "^(Male|Female)$"))))
    if (part == "Age") return(as.numeric(mean(str_detect(v, "years")) > 0.5))
    stems <- unique(str_sub(str_extract_all(str_to_lower(part), "[a-z]{4,}")[[1]], 1, 5))
    mean(map_lgl(str_to_lower(v), \(x) any(str_detect(x, fixed(stems)))))
  }
  scores <- outer(dims, parts, Vectorize(\(d, p) score(values[[d]], p)))
  dimnames(scores) <- list(dims, parts)
  
  # on a tie, prefer the concept's own order ("A by B": A is usually dim1)
  scores <- scores - 0.01 * abs(outer(seq_along(dims), seq_along(parts), "-")) * (scores > 0)

  # best matches first, one part per dim
  out <- set_names(rep(NA_character_, length(dims)), dims)
  s <- scores
  while (length(s) > 0 && max(s) > 0) {
    hit <- which(s == max(s), arr.ind = TRUE)[1, ]
    out[rownames(s)[hit[1]]] <- colnames(s)[hit[2]]
    s <- s[-hit[1], -hit[2], drop = FALSE]
  }
  # leftovers: "(detail)" of a part they match, else an unused part, else their own values
  for (d in dims[is.na(out)]) {
    unused <- setdiff(parts, out)
    out[d] <- if (max(scores[d, ]) > 0) paste(parts[which.max(scores[d, ])], "(detail)")
      else if (length(unused) > 0) unused[1]
      else if (length(values[[d]]) <= 3) str_trunc(paste(values[[d]], collapse = " / "), 50)
      else paste(parts[1], "(detail)")
  }
  # the first dim of a part gets the plain name, later ones "(detail)"
  base <- str_remove(out, " \\(detail\\)$")
  if_else(duplicated(base) & base %in% parts, paste(base, "(detail)"), base) |> set_names(dims)
}
