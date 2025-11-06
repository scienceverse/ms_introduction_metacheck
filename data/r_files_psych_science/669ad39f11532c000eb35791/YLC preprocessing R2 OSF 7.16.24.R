# YLC Preprocessing Revision 2 for OSF
# 7.16.2024

require(tidyverse)

# Read raw data
input <- read_csv("YLC initial data R2 OSF 7.16.24.csv")

# Calculate needed variables
working_data <- input %>%
  mutate(year_cent5 = year - 5,
         ac_val = rowMeans(select(., agn_val, com_val), na.rm = TRUE),
         agn_val0 = ifelse(agn_pres==0, 0, agn_val),
         com_val0 = ifelse(com_pres==0, 0, com_val),
         pwb_z = scale(pwb)[,1],
         swls_z = scale(swls)[,1],
         neur_z = scale(neur)[,1],
         extra_z = scale(extra)[,1],
         consc_z = scale(consc)[,1],
         agree_z = scale(agree)[,1],
         open_z = scale(open)[,1],
         bdi_z = scale(bdi)[,1],
         bdi_s = sqrt(bdi+.01),
         bdi_sz = scale(bdi_s)[,1]) %>%
  mutate(wb_comp = rowMeans(select(., pwb_z, swls_z), na.rm = TRUE)) 
  
# Calculate centered versions of variables for MLM analyses
varNames <- c("agn_val", "com_val", "agn_val0", "com_val0", "ac_val", "end_val", "bdi", "pwb", "swls", "wb_comp")
for(v in varNames){
  meanVar <- paste(v, "pmean", sep = "_")
  meanVarUC <- paste(v, "pmean_uc", sep = "_") #UC = uncentered
  pmcVar <- paste(v, "pmc", sep = "_")
  working_data[[meanVarUC]] <- ave(working_data[[v]], working_data[["ID"]],
                       FUN=function(x) mean(x, na.rm=TRUE))
  working_data[[meanVar]] <- scale(working_data[[meanVarUC]], scale = F)[,1]
  working_data[[pmcVar]] <- working_data[[v]] - working_data[[meanVarUC]]
}

# Output final processed data
working_data %>%
  select(
    ID, year, year_cent5,
    WC, end_val:`t1: sex of respondent`,
    `date completed interview (dd-mmm-year)`,
    "highest level of education completed", "race/ethnicity",
    one_of(varNames),
    contains("_pmean"),
    ends_with("_pmc"),
    ends_with("_z"),
    ends_with("_sz"),
    extra, neur, agree, consc, open,
    extra_z, neur_z, agree_z, consc_z, open_z) %>%
  write_csv("YLC processed data R2 OSF 7.16.24.csv")

