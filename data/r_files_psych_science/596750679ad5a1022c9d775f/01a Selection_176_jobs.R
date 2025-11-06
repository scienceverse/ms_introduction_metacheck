##This script will extract the SOEP indicators from Jaap's hard drive. The result will be saved in the Dropbox

library(foreign)

soep2004 <- read.spss("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/spss_en/UP.sav", to.data.frame = T, use.value.labels = F)

berufe <- soep2004$UIS88
berufe <- attr(berufe,"value.labels")
berufe_names <-names(berufe)
berufe_full <- cbind(berufe,berufe_names)
berufe_full <- as.data.frame(berufe_full)

#note: these are the data of the DevPsych publication, changers + stayers
load("/Users/jaapdenissen/Documents/Work/Projects/SOEP/DevPsych/Submission/Analyses/Univariate/data.rda")

berufe_used <- as.numeric(rbind(data$is8805, data$is8809))
berufe_used <- as.data.frame(table(berufe_used))

names(berufe_used) <- c("berufe", "freq")

complete_info <- merge(berufe_full, berufe_used, by = "berufe")

german_isco <- read.csv("/Users/jaapdenissen/Documents/Work/Projects/SOEP/DevPsych/Rev1/Analyses/Bundesagentur/ISCO codes/professions_deutsch.csv", header = F, sep = ",")

names(german_isco) <- c("berufe", "deutsch")

complete_info <- merge(complete_info, german_isco, by = "berufe")

#this results in the 176 professions
used_professions <- complete_info[which(complete_info$freq>=10),]
used_professions$coded05 <- 1
used_professions$coded09 <- 1
used_professions$is8805 <- used_professions$berufe
used_professions$is8809 <- used_professions$berufe

#check how many people we missed as a result
coverage.05 <- merge(data[,c("persnr", "is8805")], used_professions[,c("is8805", "coded05")], by="is8805", all.x=T)
#check how many people we missed as a result
coverage.09 <- merge(data[,c("persnr", "is8809")], used_professions[,c("is8809", "coded09")], by="is8809", all.x=T)

#percentage of fallout
fallout.05 <- 100 * length(coverage.05[is.na(coverage.05$coded05),"coded05"]) / length(coverage.05$coded05)
fallout.09 <- 100 * length(coverage.09[is.na(coverage.09$coded09),"coded09"]) / length(coverage.09$coded09)

#percentage of participants whose jobs we covered
100 - round((fallout.05 + fallout.09) / 2, 1)

#number of changers (why check?)
nrow(data[which(data$is8805!=data$is8809),])

#create data file
ratings <- read.delim("~/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data/ba2_ratings.csv")
isco <- used_professions[,1:3]
names(isco) <- c("ISCO", "profession", "freq_soep20052009")

#shared data set
isco_big5_ratings <- merge(isco, ratings, by = "ISCO")
isco_big5_ratings <- isco_big5_ratings[,-4]
names(isco_big5_ratings)[4:8] <- c("job_ext", "job_agr", "job_con", "job_sta", "job_ope")

write.table(isco_big5_ratings, "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data/share_isco_big5_ratings.csv", sep="\t")