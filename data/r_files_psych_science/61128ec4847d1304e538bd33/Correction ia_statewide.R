# Skript zur Korrektur des Datensatzes ia_statewide

dat <- data.table::fread("ia_statewide_2020_04_01 original.csv",stringsAsFactors = F)

county <- vector()
for (i in 1:NROW(dat)) {
  x <- unique(unlist(strsplit(dat$county_name[i],"[|]")))
  county[i] <- x[!x=="NA"]
}

dat$county_name <- county
write.csv(dat, "ia_statewide_2019_02_25_updated.csv")
