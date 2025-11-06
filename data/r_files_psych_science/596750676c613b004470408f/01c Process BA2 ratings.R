###################################
### PERSON-JOB-FIT AND INCOME #####
###################################

##########################
#### PREPARE BA DATA  ####
##########################


### LOAD PACKAGES AND DATA

#input.1 = "C:/Users/Maike Luhmann/Dropbox/SOEP_fit"
input.1 = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats"


library(foreign)
library(reshape)
library(psych)

setwd(input.1)

raw <- read.spss("/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data/JobRatings_11242015.sav", to.data.frame=T, use.value.labels = F)
job_codes <- read.csv("/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Ratings_questionnaire/Professions_Questionnaire_Order.csv", header = T, sep = ",")

#check the structure of the Q: is there a clear "cut"? Answer: yes. So you can try to aggregate...

raw[,380:385]

# Aggregate data 

molten <- melt(raw)

molten <- molten[which(!is.na(molten$value)),]
molten <- molten[-which(molten$variable=="CASE"),]
molten <- molten[-which(molten$variable=="STARTED"),]
molten$REF <- NULL
molten$MODE <- NULL

# new variables
molten$job_order <- substr(molten$variable, 1,4)
molten$per_item <- paste ("per", substr(molten$variable, 6,7), sep = "" )

# merge with job codes, only keep relevant variables
molten <- merge(molten, job_codes, by="job_order")
molten <- molten[,c("SERIAL", "ISCO", "description_isco", "per_item", "value")]

# aggregate by ISCO 
cast.1 <- cast(molten, SERIAL+ISCO+description_isco~per_item, fun=mean)

# recode personality items
names(cast.1)
bfi_r <- c("per06", "per03", "per08", "per05", "per11")
cast.2 = cast.1
cast.2[bfi_r] <- 8-cast.1[bfi_r]

# drop all ISCO = NA
cast.3 = cast.2[!is.na(cast.2$ISCO),]

#E  B101_02	kommunikativ, gesprÃ¤chig ist. B101_06	zurÃ¼ckhaltend ist. B101_09	aus sich herausgehen kann, gesellig ist.
#A  B101_03	manchmal etwas grob zu anderen ist. B101_07	verzeihen kann.B101_13	rÃ¼cksichtsvoll und freundlich mit anderen umgeht.
#C  B101_01	grÃ¼ndlich arbeitet. B101_08	eher faul ist. B101_12	Aufgaben wirksam und effizient erledigt.
#ES B101_05	sich oft Sorgen macht. B101_11	leicht nervÃ¶s wird. B101_15	entspannt ist, mit Stress gut umgehen kann.
#O  B101_04	originell ist, neue Ideen einbringt. #B101_10	 kÃ¼nstlerische Erfahrungen schÃ¤tzt. #B101_14	eine lebhafte Phantasie, Vorstellungen hat.

#First for rater HZEE
alpha(cast.3[which(cast.3$SERIAL=="HZEE"),c("per02", "per06", "per09")])$total$raw_alpha
alpha(cast.3[which(cast.3$SERIAL=="HZEE"),c("per03", "per07", "per13")])$total$raw_alpha
alpha(cast.3[which(cast.3$SERIAL=="HZEE"),c("per01", "per08", "per12")], check.keys=F)$total$raw_alpha
alpha(cast.3[which(cast.3$SERIAL=="HZEE"),c("per05", "per11", "per15")])$total$raw_alpha
alpha(cast.3[which(cast.3$SERIAL=="HZEE"),c("per04", "per10", "per14")])$total$raw_alpha

# What happened with conscientiousness?
x = cast.3[which(cast.3$SERIAL=="HZEE"),c("per01", "per08", "per12")]
pairs.panels(as.data.frame(x))
corr.test(as.data.frame(x))
# Conclusion: Most professions received a rating of 6 -> almost no variance
alpha(cast.3[which(cast.3$SERIAL=="HZEE"),c("per01", "per08", "per12")], check.keys=F)
# Conclusion: Item 1 did not work well for this rater

#Second for rater 4X4H
alpha(cast.3[which(cast.3$SERIAL=="4X4H"),c("per02", "per06", "per09")])$total$raw_alpha
alpha(cast.3[which(cast.3$SERIAL=="4X4H"),c("per03", "per07", "per13")])$total$raw_alpha
alpha(cast.3[which(cast.3$SERIAL=="4X4H"),c("per01", "per08", "per12")])$total$raw_alpha
alpha(cast.3[which(cast.3$SERIAL=="4X4H"),c("per05", "per11", "per15")])$total$raw_alpha
alpha(cast.3[which(cast.3$SERIAL=="4X4H"),c("per04", "per10", "per14")])$total$raw_alpha

cast.3$ba2_e <- rowMeans(cast.3[,c("per02", "per06", "per09")])
cast.3$ba2_a <- rowMeans(cast.3[,c("per03", "per07", "per13")])
cast.3$ba2_c <- rowMeans(cast.3[,c("per01", "per08", "per12")])
cast.3$ba2_s <- rowMeans(cast.3[,c("per05", "per11", "per15")])
cast.3$ba2_o <- rowMeans(cast.3[,c("per04", "per10", "per14")])


# Reshape 
names(cast.3)
cast2 <- as.data.frame(cast.3[,c(1:3, 19:23)])

molten2 <- melt(cast2, id=c("SERIAL", "ISCO", "description_isco"))
cast2 <- cast(molten2, ISCO+description_isco~variable+SERIAL, fun=mean)


# Interrater correlation

attach(cast2)
cor(ba2_e_4X4H, ba2_e_HZEE, use="complete.obs")
cor(ba2_a_4X4H, ba2_a_HZEE, use="complete.obs")
cor(ba2_c_4X4H, ba2_c_HZEE, use="complete.obs")
cor(ba2_s_4X4H, ba2_s_HZEE, use="complete.obs")
cor(ba2_o_4X4H, ba2_o_HZEE, use="complete.obs")
detach(cast2)

# Internal consistency of raters 

alpha(cast2[,c("ba2_e_4X4H", "ba2_e_HZEE")])$total$raw_alpha
alpha(cast2[,c("ba2_a_4X4H", "ba2_a_HZEE")])$total$raw_alpha
alpha(cast2[,c("ba2_c_4X4H", "ba2_c_HZEE")])$total$raw_alpha
alpha(cast2[,c("ba2_s_4X4H", "ba2_s_HZEE")])$total$raw_alpha
alpha(cast2[,c("ba2_o_4X4H", "ba2_o_HZEE")])$total$raw_alpha

# Intraclass correlation (ICC) as indicator of interrater reliability 
# Interpret ICC2 (2nd line)

ICC(cast2[,c("ba2_e_4X4H", "ba2_e_HZEE")])
ICC(cast2[,c("ba2_a_4X4H", "ba2_a_HZEE")])
ICC(cast2[,c("ba2_c_4X4H", "ba2_c_HZEE")])
ICC(cast2[,c("ba2_s_4X4H", "ba2_s_HZEE")])
ICC(cast2[,c("ba2_o_4X4H", "ba2_o_HZEE")])


# Aggregate across the two ratings 

cast2$ba2_e <- rowMeans(cast2[,c("ba2_e_4X4H", "ba2_e_HZEE")], na.rm = T)
cast2$ba2_a <- rowMeans(cast2[,c("ba2_a_4X4H", "ba2_a_HZEE")], na.rm = T)
cast2$ba2_c <- rowMeans(cast2[,c("ba2_c_4X4H", "ba2_c_HZEE")], na.rm = T)
cast2$ba2_s <- rowMeans(cast2[,c("ba2_s_4X4H", "ba2_s_HZEE")], na.rm = T)
cast2$ba2_o <- rowMeans(cast2[,c("ba2_o_4X4H", "ba2_o_HZEE")], na.rm = T)


# Load ratings obtained for Jaap's Dev Psych paper to validate ratings

setwd(input.1)
ba1_ratings <- read.csv("/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Ratings_questionnaire/Berufsratings_integrated.csv", header = T, sep = "\t")


ba1_ratings[,2:6] <- sapply(ba1_ratings[, 2:6], as.character)
ba1_ratings[,2:6] <- sapply(ba1_ratings[, 2:6], as.numeric)

merged_ba2 <- merge(ba1_ratings, cast2, by="ISCO")


# Correlations between old and new ratings

attach(merged_ba2)
cor(e_imp, ba2_e, use="complete.obs")
cor(a_imp, ba2_a, use="complete.obs")
cor(c_imp, ba2_c, use="complete.obs")
cor(s_imp, ba2_s, use="complete.obs")
cor(o_imp, ba2_o, use="complete.obs")
detach(merged_ba2)


# Select variables for export
names(merged_ba2)
ratings <- merged_ba2[,c(1, 7, 18:22)]

describe(ratings[,3:7])

write.table(ratings, "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data/ba2_ratings.csv", sep="\t") 
