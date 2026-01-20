# PAPERCHECK MODULE: Self-Report Administration Method Detection Vfinal

library(papercheck)
library(dplyr)
library(tidyverse)
library(openxlsx)
# TUE EMAIL API KEY: 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
# GMAIL EMAIL API KEY: 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
# jpGmail key: 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
Sys.setenv(GROQ_API_KEY = "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")
llm_max_calls(100)
papers <- readRDS("9_papers_metascience.Rds")

USE_LLM <- TRUE

cat("Starting Papercheck analysis...\n")

# --------------------------- DEFINE PATTERNS

patterns <- list(
  ORAL = c(
    "oral(?:ly)?\\s+(?:asked|administered|presented|given|delivered|conducted)",
    "verbal(?:ly)?\\s+(?:asked|administered|presented|delivered|conducted)",
    "verbal\\s+questionnaire",
    "spoken\\s+(?:questions?|questionnaire|responses?)",
    "(?:asked|presented|delivered|read)\\s+(?:orally|verbally)",
    "(?:questions?|items?)\\s+(?:were|was)\\s+(?:read|spoken|asked)\\s+(?:aloud|orally|verbally|out loud)",
    "interviewer\\s+(?:read|asked|presented|delivered)",
    "read\\s+(?:aloud|out\\s+loud)\\s+(?:to|by)",
    "researcher\\s+(?:orally|verbally)\\s+(?:asked|presented|administered)",
    "(?:orally|verbally)\\s+(?:responding|responded|answering|answered)"
  ),
  WRITTEN = c(
    "paper(?:-based)?\\s+(?:questionnaire|survey|form|version)",
    "pen(?:\\s+and\\s+paper|-and-paper|\\s*&\\s*paper)",
    "paper\\s+and\\s+pencil",
    "piece\\s+of\\s+paper",
    "printed\\s+(?:questionnaire|survey|form|version|copy)",
    "(?:removed|took\\s+off).*headset.*(?:to\\s+)?(?:complete|answer|fill)",
    "manually\\s+(?:completed?|fill(?:ed)?)"
  ),
  DIGITAL = c(
    "(?:completed?|administered)\\s+(?:via|through|using|on)\\s+(?:computer|tablet|online|screen|digital)",
    "(?:Qualtrics|SurveyMonkey|Google\\s+Forms|LimeSurvey|REDCap)",
    "(?:typed|entered)\\s+(?:responses?|answers?)",
    "(?:answered?|responded?).*(?:using|via|on)\\s+(?:tablet|computer|digital)",
    "participants.*(?:entered|completed).*(?:URL|online|digitally)",
    "hand.*tablet.*(?:complete|answer|respond)",
    "verbally\\s+respond.*(?:tablet|digital|screen)"
  )
)

action_verbs <- "complet|administ|respond|answer|provid|fill|present|ask|submit|conduct|deliver|gave|given|took|remove|collected|hand"

exclude_patterns <- paste(c(
  "questionnaire\\s+(?:consist|measure|assess|include|contain|evaluat|scale)",
  "questionnaire\\s+(?:was|were|is|has been)\\s+(?:developed|designed|validated|adapted|used to measure)",
  "items?\\s+(?:in|of|on)\\s+(?:the|a)\\s+questionnaire",
  "(?:reliability|validity|factor).*questionnaire",
  "questionnaire.*(?:Cronbach|alpha|psychometric)",
  "(?:this|the)\\s+(?:paper|study|experiment|research)\\s+(?:introduces|presents|describes|compares|investigates)",
  "(?:data|results)\\s+(?:collected|obtained)\\s+(?:in|through|via)\\s+(?:a\\s+)?(?:web|online|survey)",
  "(?:compared to|contrast)",
  "original\\s+(?:set|photos|images|stimuli)",
  "(?:computer|laptop|tablet|screen)\\s+(?:vary|differ|specification|brightness|used for|display)",
  "(?:experiment|study)\\s+(?:presented|uses|employed)",
  "visual\\s+acuity\\s+(?:was|not)\\s+tested"
), collapse = "|")

# --------------------------- SEARCH & FILTER

master_pattern <- paste(c(unlist(patterns), "questionnaire", "survey", "self-report", "self report"), collapse = "|")
search_results <- search_text(papers, pattern = master_pattern, return = "sentence")
initial_count <- nrow(search_results)

search_results <- search_results %>%
  filter(
    str_detect(text, regex(action_verbs, ignore_case = TRUE)) &
    !str_detect(tolower(text), regex(exclude_patterns))
  ) %>%
  bind_rows(
    search_results %>% filter(str_detect(tolower(text), "paper-based questionnaire|pen and paper|removed.*headset.*complete|Qualtrics|SurveyMonkey|verbally respond"))
  ) %>%
  distinct()

# --------------------------- CLASSIFY

classify_method <- function(text) {
  text_lower <- tolower(text)
  for (method in names(patterns)) {
    if (str_detect(text_lower, paste(patterns[[method]], collapse = "|"))) {
      return(method)
    }
  }
  return("AMBIGUOUS")
}

search_results$method_category <- sapply(search_results$text, classify_method)

# --------------------------- LLM RESOLUTION

ambiguous <- search_results %>% filter(method_category == "AMBIGUOUS")

if (USE_LLM && nrow(ambiguous) > 0) {
  cat("Querying LLM for", nrow(ambiguous), "ambiguous sentences...\n")
  
  resolve_ambiguous <- function(df) {
    if (nrow(df) == 0 || nrow(df) > 100) return(NULL)
    
    llm_query <- "You are analyzing scientific papers. Each sentence mentions questionnaires or surveys. Your task is to classify HOW participants physically provided their responses.

Categories (choose EXACTLY ONE):

ORAL - Participants spoke their answers aloud. The researcher/interviewer read questions and participants verbally responded. Keywords: orally, verbally, spoken, interviewer read aloud.
WRITTEN - Participants wrote answers on physical paper. They used pen/pencil on paper forms. Keywords: pen-and-paper, paper-based, printed form, wrote on paper.
DIGITAL - Participants typed/touched responses on electronic devices. This includes computers, tablets, smartphones, touchscreens, online surveys, VR interfaces. Keywords: computer, tablet, Qualtrics, online, digital, typed, touchscreen.
NOT_SPECIFIED - The sentence mentions questionnaires/surveys but does NOT specify how participants physically provided responses. If you cannot clearly determine the method from the sentence, choose this.

IMPORTANT: 
- Focus on HOW responses were recorded (the physical modality)
- Be conservative: when in doubt, choose NOT_SPECIFIED
- Ignore mentions of questionnaire development, validation, or measurement properties
- Look only for explicit descriptions of the administration process

Return ONLY ONE WORD: ORAL, WRITTEN, DIGITAL, or NOT_SPECIFIED"
    
    tryCatch(llm(df, llm_query), error = function(e) NULL)
  }
  
  llm_results <- resolve_ambiguous(ambiguous)
  
  if (!is.null(llm_results)) {
    llm_results$answer_clean <- toupper(str_extract(llm_results$answer, "ORAL|WRITTEN|DIGITAL|NOT_SPECIFIED"))
    search_results <- search_results %>%
      left_join(llm_results %>% select(text, llm_category = answer_clean), by = "text", relationship = "many-to-many") %>%
      mutate(final_category = ifelse(method_category == "AMBIGUOUS", llm_category, method_category))
  } else {
    search_results$final_category <- search_results$method_category
  }
} else {
  if (!USE_LLM) cat("LLM disabled - skipping ambiguous classification\n")
  search_results$final_category <- search_results$method_category
  search_results$llm_category <- NA
}

search_results <- search_results %>%
  mutate(classification_source = case_when(
    method_category != "AMBIGUOUS" ~ "REGEX",
    !is.na(llm_category) ~ "LLM",
    TRUE ~ "UNCLASSIFIED"
  ))

# --------------------------- PAPER SUMMARY (DETAILED VERSION)

paper_summary <- search_results %>%
  group_by(id) %>%
  summarise(
    total_sentences = n(),
    oral_count = sum(final_category == "ORAL"),
    written_count = sum(final_category == "WRITTEN"),
    digital_count = sum(final_category == "DIGITAL"),
    not_specified_count = sum(final_category == "NOT_SPECIFIED", na.rm = TRUE),
    methods_found = paste(unique(final_category[final_category %in% c("ORAL", "WRITTEN", "DIGITAL")]), collapse = ", "),
    has_method_specified = any(final_category %in% c("ORAL", "WRITTEN", "DIGITAL")),
    example_sentence = first(text[final_category %in% c("ORAL", "WRITTEN", "DIGITAL")], default = ""),
    .groups = "drop"
  ) %>%
  mutate(
    methods_found = ifelse(methods_found == "", "NOT_REPORTED", methods_found),
    specified_ratio = (oral_count + written_count + digital_count) / total_sentences,
    reporting_quality = case_when(
      specified_ratio >= 0.75 ~ "EXCELLENT",
      specified_ratio >= 0.50 ~ "GOOD",
      specified_ratio >= 0.25 ~ "AVERAGE",
      specified_ratio > 0 ~ "BAD",
      TRUE ~ "NOT REPORTED"
    )
  )

# --------------------------- GENERATE EMAILS

email_template_missing <- function(study_id, total_sentences) {
  glue::glue("
----------------------------------------
EMAIL FOR: {study_id}
----------------------------------------

Subject: Papercheck Review - Missing Information on method of administering self-report questions

Dear Author,

We are conducting an automated review of your manuscript using Papercheck, a tool that combines keyword search and LLM-assisted analysis to identify common reporting practices.

Papercheck detected that your study involved participants providing self-report data (e.g., through questionnaires, interviews, or digital interfaces). However, it did not detect an explicit description of how these self-reports were administered (e.g., verbally, in writing, or through a computer, tablet, or VR interface).

Reporting this information is essential for transparency, replication, and the general validity and generalizability of your findings, increasing the visibility and likelihood of your study being cited.

We would like to ask you to revise and update the manuscript to ensure that this information is clearly stated in the main text.

Best regards,
The Research Team, 
on behalf of Papercheck

Analysis Details:
  - Questionnaire mentions found: {total_sentences}
  - Administration method detected: NO
  - Action required: Add explicit description of questionnaire administration method

")
}

email_template_detected <- function(study_id, methods, oral, written, digital, example, quality) {
  glue::glue("
----------------------------------------
EMAIL FOR: {study_id}
----------------------------------------

Subject: Papercheck Review - Detected method of administering self-report questions

Dear Author,

We are conducting an automated review of your manuscript using Papercheck, a tool that combines keyword search and LLM-assisted analysis to identify common reporting practices.

Good news! Papercheck successfully detected explicit information about how self-report questions were administered in your study.

DETECTED METHOD(S): {methods}

EXAMPLE SENTENCE:
\"{example}\"

This information enhances the transparency and replicability of your work. No action is required for this section.

Best regards,
The Research Team, on behalf of Papercheck

Analysis Details:
  - Administration method detected: YES ({methods})
  - ORAL mentions: {oral}
  - WRITTEN mentions: {written}
  - DIGITAL mentions: {digital}
  - Reporting quality: {quality}

")
}

feedback_file <- "author_feedback_emails.txt"
sink(feedback_file)

cat("PAPERCHECK AUTOMATED FEEDBACK EMAILS\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

for(i in 1:nrow(paper_summary)) {
  if (!paper_summary$has_method_specified[i]) {
    cat(email_template_missing(paper_summary$id[i], paper_summary$total_sentences[i]))
  } else {
    cat(email_template_detected(
      paper_summary$id[i],
      paper_summary$methods_found[i],
      paper_summary$oral_count[i],
      paper_summary$written_count[i],
      paper_summary$digital_count[i],
      substr(paper_summary$example_sentence[i], 1, 150),
      paper_summary$reporting_quality[i]
    ))
  }
}

cat("\nOVERALL SUMMARY\n")
cat("Total papers analyzed:", nrow(paper_summary), "\n")
cat("Papers with method reported:", sum(paper_summary$has_method_specified), "\n")
cat("Papers requiring feedback:", sum(!paper_summary$has_method_specified), "\n")

sink()

# --------------------------- CREATE EXCEL WORKBOOK

processing_summary <- data.frame(
  Stage = c("Initial Search", "After Filtering", "Regex Classified", "Sent to LLM", "Final Classified"),
  Count = c(initial_count, nrow(search_results), 
            sum(search_results$method_category != "AMBIGUOUS"),
            nrow(ambiguous),
            sum(search_results$final_category %in% c("ORAL", "WRITTEN", "DIGITAL")))
)

sentences_classified <- search_results %>%
  group_by(id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup() %>%
  select(study_id = id, sentence_id, classification_source, 
         initial_category = method_category, final_category, sentence_text = text)

all_sentences <- search_text(papers, pattern = master_pattern, return = "sentence") %>%
  mutate(
    passed_filter = text %in% search_results$text,
    exclusion_reason = case_when(
      passed_filter ~ "KEPT",
      !str_detect(text, regex(action_verbs, ignore_case = TRUE)) ~ "REMOVED - No action verbs",
      str_detect(tolower(text), regex(exclude_patterns)) ~ "REMOVED - Exclusion pattern",
      TRUE ~ "REMOVED - Other"
    )
  ) %>%
  left_join(search_results %>% select(text, method_category, final_category, classification_source), by = "text") %>%
  mutate(across(c(method_category, final_category, classification_source), ~replace_na(.x, "N/A"))) %>%
  select(study_id = id, section, passed_filter, exclusion_reason, 
         classification_source, initial_category = method_category, final_category, sentence_text = text)

wb <- createWorkbook()
header_style <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                           fgFill = "#4F81BD", border = "TopBottom", textDecoration = "bold")

sheets <- list(
  "Processing_Summary" = processing_summary,
  "Paper_Summary" = paper_summary %>% select(study_id = id, methods_found, reporting_quality, 
                                              total_sentences, oral_count, written_count, 
                                              digital_count, not_specified_count, example_sentence),
  "Sentences_Classified" = sentences_classified,
  "All_Sentences_Pipeline" = all_sentences
)

for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
  addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:ncol(sheets[[sheet_name]]), gridExpand = TRUE)
  setColWidths(wb, sheet_name, cols = 1:(ncol(sheets[[sheet_name]])-1), widths = "auto")
  setColWidths(wb, sheet_name, cols = ncol(sheets[[sheet_name]]), widths = 60)
}

saveWorkbook(wb, "papercheck_complete_analysis.xlsx", overwrite = TRUE)

# --------------------------- FINAL SUMMARY

cat("----------------------------------------------------------------------\n")
cat("PAPERCHECK ANALYSIS COMPLETE\n")
cat("----------------------------------------------------------------------\n\n")

cat("FILES GENERATED:\n")
cat("  1. author_feedback_emails.txt - Automated feedback emails\n")
cat("  2. papercheck_complete_analysis.xlsx - Complete analysis workbook\n\n")

cat("SUMMARY:\n")
cat("  • Sentences found:", initial_count, "\n")
cat("  • Sentences analyzed:", nrow(search_results), "\n")
cat("  • Definitively classified:", sum(search_results$final_category %in% c("ORAL", "WRITTEN", "DIGITAL")), "\n")
cat("  • Papers with methods reported:", sum(paper_summary$has_method_specified), "/", nrow(paper_summary), "\n")
cat("  • Emails requiring action:", sum(!paper_summary$has_method_specified), "\n")
cat("----------------------------------------------------------------------\n")
