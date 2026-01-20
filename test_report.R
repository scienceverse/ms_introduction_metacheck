devtools::install_github("scienceverse/metacheck", ref = "dev-reports")
devtools::install_github("scienceverse/metacheck")
library(metacheck)
paper <- psychsci[[103]] 
paper <- psychsci[[203]] # also has github links
paper <- psychsci[[233]]
paper <- psychsci[[221]]
paper <- psychsci[[10]] # No Links
paper <- read(demoxml())
output_file <- paste0(paper$id, "_report_all.html")

paper <- psychsci

bayes <- search_text(paper, "Bayes|BF")
report(
  paper,
  modules = c("code_check", "reference_check", "effect_size", "nonsignificant_pvalue", "marginal", "exact_p", "prereg_check", "causal_claims", "statcheck"),
  output_file = paste0(paper$id, "_report_all.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_all.html"))



report(
  paper,
  modules = c("open_data_code_check"),
  output_file = paste0(paper$id, "_report_all.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_all.html"))


report(
  paper,
  modules = c("causal_claims"),
  output_file = paste0(paper$id, "_report_causal.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_causal.html"))


report(
  paper,
  modules = c("exact_p"),
  output_file = paste0(paper$id, "_report_p.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_p.html"))

report(
  paper,
  modules = c("prereg_check"),
  output_file = paste0(paper$id, "_report_prereg.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_prereg.html"))

paper <- psychsci[[49]]

report(
  paper,
  modules = c("nonsignificant_pvalue"),
  output_file = paste0(paper$id, "_report_nonsignificant_pvalue.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_nonsignificant_pvalue.html"))

report(
  paper,
  modules = c("marginal"),
  output_file = paste0(paper$id, "_report_marginal.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_marginal.html"))


paper <- psychsci[[10]] # No Links
report(
  paper,
  modules = c("statcheck"),
  output_file = paste0(paper$id, "_report_statcheck.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_statcheck.html"))

report(
  paper,
  modules = c("effect_size"),
  output_file = paste0(paper$id, "_report_effectsize.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_effectsize.html"))


paper <- psychsci[[103]]
paper <- psychsci[[69]]

xml <- psychsci
res1 <- aspredicted_links(psychsci)
res2 <- aspredicted_links(psychsci)

# Rows in res1 but not in res2
unique_to_res1 <- anti_join(res1, res2)

# Rows in res2 but not in res1
unique_to_res2 <- anti_join(res2, res1)


paper <- psychsci[["09567976231168777"]]
View(psychsci)

output_file = paste0(paper$id, "_report.html")

report(
  paper,
  modules = c("effect_size"),
  output_file = output_file,
  output_format = "html"
)
browseURL(output_file)

email(email = "d.lakens@tue.nl")

filename <- demoxml()
paper <- read(filename)
output_file = paste0(paper$id, "_report.html")
report(paper, 
       output_file = output_file,
       modules = c("nonsignificant_pvalue"),
       output_format = "html")
browseURL(output_file)

# for testing: paper <- psychsci[[109]]
# for testing: paper <- psychsci[[124]]
# for testing: paper <- psychsci[[129]]
# for testing: paper <- psychsci[[130]]

output_file = paste0(paper$id, "_report.html")

report(
  paper,
  modules = c("reference_check"),
  output_file = paste0(paper$id, "_report_ref.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_ref.html"))



paper <- psychsci[[218]]

paper <- psychsci[["09567976241239935"]]

report(
  paper,
  modules = c("prereg_check"),
  output_file = paste0(paper$id, "_report_ref.html"),
  output_format = "html"
)
browseURL(paste0(paper$id, "_report_ref.html"))


devtools::install_github("libscie/datacheck")
checker()

datacheckDf

install.packages("pii")


# Define the folder path
folder_path <- "C:/Users/DLakens/OneDrive - TU Eindhoven/git_repos/papercheck_paper/data/all_osf_files_psych_science"

# List all .xlsx files recursively
xlsx_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

# The following file contains a lot of sensitive data
# C:/Users/DLakens/OneDrive - TU Eindhoven/git_repos/papercheck_paper/data/all_osf_files_psych_science/4kmf8/Mindfulness and false memory/Raw Data/raw_recognition_data.xlsx

library(readxl)
library(datacheck)
library(detector)
library(pii)

# Initialize a results list
results <- list()


for (file in xlsx_files) {
  message("Processing: ", file)
  
  result <- tryCatch({
    temp_df <- read_excel(file)
    check <- check_PII(temp_df)
    check2 <- datacheckDf(temp_df)
    list(success = TRUE, check = check, check2 = check2)
  }, error = function(e) {
    message("Error in file: ", file, " -> ", e$message)
    list(success = FALSE, check = NULL)
  })
  
  results[[file]] <- result
}



# Create summary for check_PII
summary_checkPII <- do.call(rbind, lapply(names(results), function(f) {
  data.frame(
    file = f,
    success = results[[f]]$success,
    pii_result = paste(capture.output(print(results[[f]]$check)), collapse = "; ")
  )
}))

# Create summary for datacheckDf
summary_datacheck <- do.call(rbind, lapply(names(results), function(f) {
  data.frame(
    file = f,
    success = results[[f]]$success,
    datacheck_result = paste(capture.output(print(results[[f]]$check2)), collapse = "; ")
  )
}))

pdf2grobid(
  "C:\\Users\\DLakens\\OneDrive - TU Eindhoven\\git_repos\\metacheck\\inst\\extdata\\to_err_is_human.pdf"
)

paper <- read("C:\\Users\\DLakens\\OneDrive - TU Eindhoven\\git_repos\\metacheck\\inst\\extdata\\to_err_is_human.xml")
demopdf
demoxml

rb_url <- rbox_links(paper)
research_box_info <- rbox_retrieve(research_box_links)

# statcheck tests are t, F, Z, 


# Single sentence
df1 <- causal_relations("Smoking causes cancer")
print(df1)
# Multiple sentences (batch)
df2 <- causal_relations(c("Insomnia causes depression.", "Rain leads to flooding."))
print(df2)

# Custom parameters and verbose diagnostics
df3 <- causal_relations(
 sentence = "Stress increases blood pressure.",
 rel_mode = "auto",
 rel_threshold = 0.4,
 cause_decision = "cls+span",
 timeout = 10,
 verbose = TRUE
)
print(df3)
