##################################################################
##                Analysis code for Yousif et al                ##
##################################################################
xfun::pkg_attach2(
  "here",
  "tidyverse",
  "readxl",
  "remotes")

remotes::install_github("ljcolling/ODPhelper", upgrade = "never")
library(ODPHelper)

data_file <-  here::here("data","Additive Area - Raw Data and Info.xlsx")
sheets <- readxl::excel_sheets(data_file)

experiment_list <- c("1" = "Exp 1 -",
                     "2" = "Exp 2 -",
                     "3" = "Exp 3 -",
                     "4" = "Exp 4 -",
                     "5" = "Exp 5 -")
yousif = list()
map_df(1: 5, function(x) readxl::read_excel(path = data_file, sheet = sheets[grepl(x = sheets, pattern = experiment_list[x])]) %>%
  summarise(id = unique(id) %>% length(), id2 = unique(id2) %>% length()) %>%
    add_column(Experiment = names(experiment_list[x]), .before = 1)) -> yousif$subject_numbers

yousif$session_info <- desc_session()
saveRDS(yousif, here::here("./made/yousif.Rdata"))
