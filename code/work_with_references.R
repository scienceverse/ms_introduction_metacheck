# Script to get all references in a single table
library(dplyr)

# 1) Extract nested data frames
bibs <- lapply(xml, function(x) x$bib)

# 2) Normalize each 'bib' so that 'year' is integer of correct length (even for 0-row dfs)
coerce_year_to_integer <- function(df) {
  # If NULL, replace by an empty data frame
  if (is.null(df)) df <- data.frame()
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  nr <- nrow(df)
  
  # If 'year' is missing, create it with the right length
  if (!"year" %in% names(df)) {
    df$year <- if (nr == 0) integer(0) else rep(NA_integer_, nr)
    return(df)
  }
  
  # Coerce to character first, then to integer
  y_chr <- as.character(df$year)
  y_int <- suppressWarnings(as.integer(y_chr))
  
  # For non-numeric forms, try to extract a 4-digit year
  need_extract <- is.na(y_int) & !is.na(y_chr) & nzchar(y_chr)
  if (any(need_extract)) {
    m   <- regexpr("\\b\\d{4}\\b", y_chr[need_extract])
    got <- ifelse(m > 0,
                  substring(y_chr[need_extract], m, m + attr(m, "match.length") - 1),
                  NA_character_)
    y_int[need_extract] <- suppressWarnings(as.integer(got))
  }
  
  # Assign back; lengths now match nrow(df), including the 0-row case
  df$year <- y_int
  df
}

bibs_fixed <- lapply(bibs, coerce_year_to_integer)

# 3) Bind after coercion — now types align everywhere
all_psych_science_refs <- dplyr::bind_rows(bibs_fixed)

# 4) How many papers have a DOI?
# 1) Flag presence of DOI (per your rule: any non-NA value counts)
all_psych_science_refs <- all_psych_science_refs %>%
  mutate(has_doi = !is.na(doi))

# 2) Overall percentage with DOI
overall <- all_psych_science_refs %>%
  summarize(
    n = n(),
    with_doi = sum(has_doi, na.rm = TRUE),
    pct_with_doi = 100 * with_doi / n
  )

overall

# add year
all_psych_science_refs <- all_psych_science_refs %>%
  rename(year_ref = year)

library(dplyr)

# bring 'year' into all_psych_science_refs by id
all_psych_science_refs <- all_psych_science_refs %>%
  left_join(psych_science %>% select(id, year), by = "id")


df <- all_psych_science_refs

# Compute percentage with DOI per year
by_year <- df %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarize(
    n = n(),
    pct_with_doi = 100 * sum(has_doi) / n(),
    .groups = "drop"
  ) %>%
  arrange(year)

# Simple line plot
ggplot(by_year, aes(x = year, y = pct_with_doi)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Percentage of Rows with DOI per Year",
    x = "Year",
    y = "DOI (%)"
  ) +
  theme_minimal()

