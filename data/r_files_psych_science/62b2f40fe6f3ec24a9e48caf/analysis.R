##################################################################
##               Analysis code for Hilgard et al                ##
##################################################################

xfun::pkg_attach2(
  "here",
  "devtools",
  "rmarkdown")
setwd(here::here("data/vvg-2d4d-master/"))

built_files <- c(
  "aggressed-condition_hist.png", "AggressionTable.txt",
  "clean_data.txt", "Difficulty-EFA_hist.png", "Figure1.png",
  "Figure2a.png", "Figure2b.png", "full_data.RData", "full_data.txt",
  "Gamevars.txt", "Results.docx", "Rplots.pdf", "session_info.txt",
  "SuppFigure1.png", "Supplement.pdf", "violence-condition_hist.png"
)

# delete all the build file
purrr::map(built_files, function(x) file.remove(x))

#source("master.R")

source("0_data_aggregation.R")
source("1_data_cleaning.R")
source("2_analysis.R")
source("3_plotting.R")

# Fix type in Results.Rmd
system("sed -i 's/``` {/```{/g' Results.Rmd")


render("Results.Rmd", "word_document")
render("Supplement.Rmd", "pdf_document")

sink("session_info.txt")
devtools::session_info()
sink(file = NULL)

devtools::install_github("ljcolling/ODPHelper", upgrade = "never")
library(ODPHelper)

# list files that were built


hilgard = list()

hilgard$built_files = map_df(built_files, function(x) tibble(file = x, built = file.exists(x)))


# Write out session and package information
hilgard$session_info <- desc_session()
saveRDS(hilgard, here::here("./made/hilgard.Rdata"))
