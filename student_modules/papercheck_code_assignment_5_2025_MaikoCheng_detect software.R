# install packages: 
install.packages("remotes")
library(remotes)
# Installing Papercheck will take a few minutes
remotes::install_github("scienceverse/papercheck")
library(papercheck)
install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages("tidyverse")
library(tidyverse)


# Load papers
papers <- readRDS("9_papers_metascience.Rds")

## New Module
## SoftwareFinder

GROQ_API_KEY <- "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" # Personal Key - Maiko Cheng
Sys.setenv(GROQ_API_KEY = "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")
MAX_CALLS <- 30
# getting all methods sections to the LLM
methods_sent <- search_text(
  papers,
  pattern = ".*",
  return  = "sentence",
  ignore.case = TRUE
) |>
  filter(str_detect(section, regex("method", ignore_case = TRUE))) |>
  mutate(row_id = row_number())

if (nrow(methods_sent) == 0) {
  cat("No Methods text found.\n"); quit(save="no")
}

# Finding the cues (a^b)
cue_terms <- c("model\\w*", "engine\\w*", "develop\\w*", "software\\w*", "sdk\\b", "runtime\\b",
               "api\\b", "toolkit\\b", "framework\\b", "platform\\b")
sw_lexicon <- c("unity", "unreal", "openxr", "steamvr", "oculus", "godot",
                "vrtk", "xr toolkit", "openvr", "monado", "pico sdk", "hololens",
                "three\\.js", "webxr", "openxrs", "viveport", "mixed reality toolkit|mrtk", "whirligig")

score_sentence <- function(x) {
  cues <- str_count(str_to_lower(x), str_c(cue_terms, collapse="|"))
  sws  <- str_count(str_to_lower(x), str_c(sw_lexicon, collapse="|"))
  # weight software hits higher than generic cues
  2*sws + cues
}

ranked <- methods_sent |>
  mutate(score = score_sentence(text)) |>
  # keep sentences that have either a software hit or at least one cue
  filter(score > 0) |>
  distinct(text, .keep_all = TRUE) |>
  arrange(desc(score), nchar(text)) |>
  slice_head(n = MAX_CALLS)

if (nrow(ranked) == 0) {
  cat("No likely software mentions found in Methods.\n"); quit(save="no")
}

# Output from LLM
query <- paste(
  "From EACH INPUT SENTENCE, extract SOFTWARE actually used to project the scene on the headset",
  "(e.g., Unity, Unreal Engine, Oculus Runtime, OpenXR, SteamVR, Godot, VRTK/XR Toolkit).",
  "Treat a word as software only if it appears in the SAME SENTENCE as any of: model*, engine*,",
  "develop*, software*, sdk, runtime, api, toolkit, framework, platform (stem match allowed).",
  "Ignore device-only brand names unless called a runtime/SDK/platform.",
  "Return STRICT JSON only:",
  '{ "software": ["<Name 1>", ...], "homonyms": [ {"term":"<not software>", "reason":"<why>"} ] }',
  'If none: { "software": [], "homonyms": [] }',
  sep = "\n"
)

raw_llm <- llm(
  text        = ranked,
  query       = query,
  text_col    = "text",
  temperature = 0,
  maxTokens   = 512
)

# Parsing
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
library(jsonlite)

`%||%` <- function(x, y) if (is.null(x)) y else x

parsed <- raw_llm |>
  dplyr::mutate(
    row_id = ranked$row_id,
    id     = ranked$id   # <-- if your identifier column has another name, change here
  ) |>
  dplyr::mutate(parsed = purrr::map(answer, ~ {
    out <- tryCatch(jsonlite::fromJSON(.x, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(out)) out <- list(software = list(), homonyms = list())
    # normalize software -> character vector
    sw <- out$software %||% list()
    sw <- as.character(unlist(sw, recursive = TRUE, use.names = FALSE))
    # normalize homonyms -> tibble(term, reason)
    hm <- out$homonyms %||% list()
    if (length(hm) == 0) {
      hm_tbl <- tibble::tibble(term = character(), reason = character())
    } else if (is.data.frame(hm)) {
      hm_tbl <- tibble::as_tibble(hm)
    } else {
      hm_tbl <- tibble::as_tibble(dplyr::bind_rows(hm))
    }
    if (!"term" %in% names(hm_tbl))   hm_tbl$term   <- character()
    if (!"reason" %in% names(hm_tbl)) hm_tbl$reason <- character()
    list(software = sw, homonyms = hm_tbl)
  })) |>
  dplyr::mutate(
    software = purrr::map(parsed, ~ .x$software),   # always character()
    homonyms = purrr::map(parsed, ~ .x$homonyms)    # tibble(term, reason)
  )

# frequency table
# Long with IDs
sw_long <- parsed |>
  select(row_id, id, software) |>
  tidyr::unnest_longer(software, values_to = "software") |>
  mutate(software = stringr::str_squish(software)) |>
  filter(!is.na(software), software != "") |>
  mutate(software_norm = dplyr::case_when(
    str_detect(str_to_lower(software), "^unity") ~ "Unity",
    str_detect(str_to_lower(software), "^unreal") ~ "Unreal Engine",
    str_detect(str_to_lower(software), "openxr") ~ "OpenXR",
    str_detect(str_to_lower(software), "steamvr") ~ "SteamVR",
    str_detect(str_to_lower(software), "oculus") ~ "Oculus Runtime",
    str_detect(str_to_lower(software), "godot") ~ "Godot",
    str_detect(str_to_lower(software), "whirligig") ~ "Whirligig",
    str_detect(str_to_lower(software), "vrtk|xr toolkit|xrtoolkit|unity xr") ~ "VRTK / XR Toolkit",
    TRUE ~ stringr::str_to_title(software)
  ))

# Unique presence per study (avoid double-counting same software repeated in one paper)
sw_by_study <- sw_long |>
  distinct(id, software_norm) |>
  arrange(software_norm, id)

# Counts per software + which studies mention it
sw_freq_ids <- sw_by_study |>
  group_by(software_norm) |>
  summarise(
    n = n(),
    studies = paste(sort(unique(id)), collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(desc(n), software_norm)

# One-row wide table of counts (as before)
sw_wide <- sw_freq_ids |>
  mutate(software_col = make.names(software_norm)) |>
  select(software_col, n) |>
  tidyr::pivot_wider(names_from = software_col, values_from = n, values_fill = 0)

# (Optional) Study × Software presence matrix (0/1), handy for auditing
study_software <- sw_by_study |>
  mutate(value = 1L) |>
  tidyr::pivot_wider(
    names_from = software_norm, values_from = value, values_fill = 0
  )
n_unique <- nrow(sw_freq_ids)
# homonyms
software_labels <- unique(sw_long$software_norm)
normalize_sw_name <- function(x) {
  x <- stringr::str_squish(x)
  dplyr::case_when(
    stringr::str_detect(stringr::str_to_lower(x), "^unity") ~ "Unity",
    stringr::str_detect(stringr::str_to_lower(x), "^unreal|\\bue\\s*\\d?\\b") ~ "Unreal Engine",
    stringr::str_detect(stringr::str_to_lower(x), "openxr") ~ "OpenXR",
    stringr::str_detect(stringr::str_to_lower(x), "steamvr") ~ "SteamVR",
    stringr::str_detect(stringr::str_to_lower(x), "oculus") ~ "Oculus Runtime",
    stringr::str_detect(stringr::str_to_lower(x), "godot") ~ "Godot",
    stringr::str_detect(stringr::str_to_lower(x), "vrtk|xr toolkit|xrtoolkit|unity xr") ~ "VRTK / XR Toolkit",
    TRUE ~ stringr::str_to_title(x)
  )
}
homonyms_all <- parsed |>
  dplyr::select(row_id, id, homonyms) |>
  tidyr::unnest(homonyms, keep_empty = TRUE) |>
  dplyr::filter(!is.na(term), term != "") |>
  dplyr::left_join(methods_sent |> dplyr::select(row_id, p, s), by = "row_id") |>
  dplyr::mutate(position = paste0("p", p, "/s", s)) |>
  dplyr::select(term, id, position, reason)

homonyms_sw_only <- homonyms_all |>
  dplyr::mutate(term_norm = normalize_sw_name(term)) |>
  dplyr::filter(term_norm %in% software_labels) |>
  dplyr::arrange(term_norm, id, position)

# paper has software?
per_paper_any <- parsed |>
  dplyr::mutate(has_sw = purrr::map_int(software, length) > 0) |>
  dplyr::group_by(id) |>
  dplyr::summarise(any_software = any(has_sw), .groups = "drop")

papers_no_sw <- per_paper_any |>
  dplyr::filter(!any_software) |>
  dplyr::pull(id)

# Print
if (length(papers_no_sw) > 0) {
  cat("\nPapers without software mentions (based on analyzed sentences):\n")
  for (pid in papers_no_sw) {
    cat(sprintf("%s: NO SOFTWARE SPECIFIED\n", pid))
  }
} else {
  cat("\nAll analyzed papers included at least one software mention.\n")
}

cat(sprintf("These paper mention %d (different) software used to project scene on headset.\n", n_unique))
cat("The frequencies of correctly reporting of the software can be seen in the table below (a^b):\n\n")
print(sw_wide)

cat("\nWhich study uses which software:\n")
print(sw_freq_ids)        # shows counts + comma-separated study IDs
# Optionally:
#print(study_software)   # matrix: rows=study id, columns=software


if (nrow(homonyms_sw_only) == 0) {
  cat("— No homonyms detected.\n")
} else {
  cat("\nFound homonyms of these software in these cases: \n\n")
  print(homonyms_sw_only)
  cat('\n"Think critically whether to change these homonyms if possible."\n')
}


