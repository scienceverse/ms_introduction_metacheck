# Children Speciesism - Study 2

library(psych)
library(effsize)
library(dplyr)
library(yarrr)
library(lsr)
library(tidyr)
library(lme4)
library(car)


# Functions

swap_1_and_3 <- function(df_swap) {
  df_swap <- ifelse(df_swap == 1, 4, df_swap)
  df_swap <- ifelse(df_swap == 3, 1, df_swap)
  df_swap <- ifelse(df_swap == 4, 3, df_swap)
  df_swap
}

perc_human <- function(dataf, thing) {
  round(nrow(filter(dataf, thing == 1))/nrow(dataf)*100.0, digits=2)
}

perc_animal <- function(dataf, thing) {
  round(nrow(filter(dataf, thing == 3))/nrow(dataf)*100.0, digits=2)
}

perc_cantdecide <- function(dataf, thing) {
  round(nrow(filter(dataf, thing == 2))/nrow(dataf)*100.0, digits=2)
}




##########
### Adults study
##########

# Load Data

dfa <- read.csv("Study2_adults.csv",header=TRUE, stringsAsFactors=FALSE) 
dfa <- tbl_df(dfa)
n_raw <- nrow(dfa)


# Exclude those who failed the attention check
dfa <- filter(dfa, (!is.na(DH100) | !is.na(DH100.1)) & attn == 3)

dfa$Age <- dfa$age

dfa$DH100 <- coalesce(dfa$DH100, dfa$DH100.1)
dfa$DH10 <- coalesce(dfa$DH10, dfa$DH10.1)
dfa$DH2 <- coalesce(dfa$DH2, dfa$DH2.1)
dfa$HD1 <- coalesce(dfa$HD1, dfa$HD1.1)
dfa$HD2 <- coalesce(dfa$HD2, dfa$HD2.1)
dfa$HD10 <- coalesce(dfa$HD10, dfa$HD10.1)
dfa$HD100 <- coalesce(dfa$HD100, dfa$HD100.1)

dfa$PH100 <- coalesce(dfa$PH100, dfa$PH100.1)
dfa$PH10 <- coalesce(dfa$PH10, dfa$PH10.1)
dfa$PH2 <- coalesce(dfa$PH2, dfa$PH2.1)
dfa$HP1 <- coalesce(dfa$HP1, dfa$HP1.1)
dfa$HP2 <- coalesce(dfa$HP2, dfa$HP2.1)
dfa$HP10 <- coalesce(dfa$HP10, dfa$HP10.1)
dfa$HP100 <- coalesce(dfa$HP100, dfa$HP100.1)

dfa$DP1 <- coalesce(swap_1_and_3(swap_1_and_3(dfa$DP1)), swap_1_and_3(dfa$DP1.1))
dfa$PP10 <- coalesce(dfa$PP10, dfa$PP10.1)
dfa$HA10 <- coalesce(dfa$HA10, dfa$HA10.1)
dfa$HW10 <- coalesce(dfa$HW10, dfa$HW10.1)

dfa$DH2 <- swap_1_and_3(dfa$DH2)
dfa$DH10 <- swap_1_and_3(dfa$DH10)
dfa$DH100 <- swap_1_and_3(dfa$DH100)

dfa$PH2 <- swap_1_and_3(dfa$PH2)
dfa$PH10 <- swap_1_and_3(dfa$PH10)
dfa$PH100 <- swap_1_and_3(dfa$PH100)

#dfa <- filter(dfa, HA10 == 1 & HW10 == 1)
#dfa <- filter(dfa, HA10 == 1)
#dfa <- filter(dfa, HW10 == 1)
dfa <- filter(dfa, HA10 == 1 | HW10 == 1) # exclude those who failed the plates and worms question

sprintf("Final N = %s (Excluded = %s); female = %s; M-age = %s (SD = %s)", 
        nrow(dfa), (n_raw - nrow(dfa)), nrow(filter(dfa, femaleAs1MaleAs0 == 1)), round(mean(dfa$age, na.rm=T), digits=2), round(sd(dfa$age, na.rm=T), digits=2))


# Scoring system

dfa <- dfa %>% mutate(
  score_hd100 = ifelse(HD100 == 1, 7.64, ifelse(HD100 == 2, 3.82, 0)),
  score_hd10 = ifelse(HD10 == 1, 4.32, ifelse(HD10 == 2, 2.16, 0)),
  score_hd2 = ifelse(HD2 == 1, 2, ifelse(HD2 == 2, 1, 0)),
  score_hd1 = ifelse(HD1 == 1, 1, ifelse(HD1 == 2, 0, -1)),
  score_dh2 = ifelse(DH2 == 1, 0, ifelse(DH2 == 2, -1, -2)),
  score_dh10 = ifelse(DH10 == 1, 0, ifelse(DH10 == 2, -2.16, -4.32)),
  score_dh100 = ifelse(DH100 == 1, 0, ifelse(DH100 == 2, -3.82, -7.64)),
  score_hd = score_hd100 + score_hd10 + score_hd2 + score_hd1 + score_dh2 + score_dh10 + score_dh100
)

dfa <- dfa %>% mutate(
  score_hp100 = ifelse(HP100 == 1, 7.64, ifelse(HP100 == 2, 3.82, 0)),
  score_hp10 = ifelse(HP10 == 1, 4.32, ifelse(HP10 == 2, 2.16, 0)),
  score_hp2 = ifelse(HP2 == 1, 2, ifelse(HP2 == 2, 1, 0)),
  score_hp1 = ifelse(HP1 == 1, 1, ifelse(HP1 == 2, 0, -1)),
  score_ph2 = ifelse(PH2 == 1, 0, ifelse(PH2 == 2, -1, -2)),
  score_ph10 = ifelse(PH10 == 1, 0, ifelse(PH10 == 2, -2.16, -4.32)),
  score_ph100 = ifelse(PH100 == 1, 0, ifelse(PH100 == 2, -3.82, -7.64)),
  score_hp = score_hp100 + score_hp10 + score_hp2 + score_hp1 + score_ph2 + score_ph10 + score_ph100
)

# Scores

dfa %>% skimr::skim(score_hd, score_hp)

t.test(dfa$score_hd, mu = 0)
cohensD(dfa$score_hd, mu = 0)
d.ci(cohensD(dfa$score_hd, mu = 0), n=nrow(dfa), n1=nrow(dfa), alpha=.05)

t.test(dfa$score_hp, mu = 0)
cohensD(dfa$score_hp, mu = 0)
d.ci(cohensD(dfa$score_hp, mu = 0), n=nrow(dfa), n1=nrow(dfa), alpha=.05)


# Correlations with age

cor.test(dfa$age, dfa$score_hd)
cor.test(dfa$age, dfa$score_hp)


## Percentages

perc_human(dfa, dfa$HD100)
perc_human(dfa, dfa$HD10)
perc_human(dfa, dfa$HD2)
perc_human(dfa, dfa$HD1)
perc_human(dfa, dfa$DH2)
perc_human(dfa, dfa$DH10)
perc_human(dfa, dfa$DH100)

perc_human(dfa, dfa$HP100)
perc_human(dfa, dfa$HP10)
perc_human(dfa, dfa$HP2)
perc_human(dfa, dfa$HP1)
perc_human(dfa, dfa$PH2)
perc_human(dfa, dfa$PH10)
perc_human(dfa, dfa$PH100)


perc_animal(dfa, dfa$HD100)
perc_animal(dfa, dfa$HD10)
perc_animal(dfa, dfa$HD2)
perc_animal(dfa, dfa$HD1)
perc_animal(dfa, dfa$DH2)
perc_animal(dfa, dfa$DH10)
perc_animal(dfa, dfa$DH100)

perc_animal(dfa, dfa$HP100)
perc_animal(dfa, dfa$HP10)
perc_animal(dfa, dfa$HP2)
perc_animal(dfa, dfa$HP1)
perc_animal(dfa, dfa$PH2)
perc_animal(dfa, dfa$PH10)
perc_animal(dfa, dfa$PH100)

perc_cantdecide(dfa, dfa$HD100)
perc_cantdecide(dfa, dfa$HD10)
perc_cantdecide(dfa, dfa$HD2)
perc_cantdecide(dfa, dfa$HD1)
perc_cantdecide(dfa, dfa$DH2)
perc_cantdecide(dfa, dfa$DH10)
perc_cantdecide(dfa, dfa$DH100)

perc_cantdecide(dfa, dfa$HP100)
perc_cantdecide(dfa, dfa$HP10)
perc_cantdecide(dfa, dfa$HP2)
perc_cantdecide(dfa, dfa$HP1)
perc_cantdecide(dfa, dfa$PH2)
perc_cantdecide(dfa, dfa$PH10)
perc_cantdecide(dfa, dfa$PH100)

# Dog exposure

dfa %>% group_by(dog_exposure) %>% skimr::skim(score_hd, score_hp)
t.test(filter(dfa, dog_exposure ==  1)$score_hd, filter(dfa, dog_exposure ==  2)$score_hd)
cohensD(filter(dfa, dog_exposure ==  1)$score_hd, filter(dfa, dog_exposure ==  2)$score_hd)
d.ci(cohensD(filter(dfa, dog_exposure ==  1)$score_hd, filter(dfa, dog_exposure ==  2)$score_hd), 
     n1=nrow(filter(dfa, dog_exposure ==  1)), n2=nrow(filter(dfa, dog_exposure ==  2)), alpha=.05)

t.test(filter(dfa, dog_exposure ==  1)$score_hp, filter(dfa, dog_exposure ==  2)$score_hp)
cohensD(filter(dfa, dog_exposure ==  1)$score_hp, filter(dfa, dog_exposure ==  2)$score_hp)
d.ci(cohensD(filter(dfa, dog_exposure ==  1)$score_hp, filter(dfa, dog_exposure ==  2)$score_hp), 
     n1=nrow(filter(dfa, dog_exposure ==  1)), n2=nrow(filter(dfa, dog_exposure ==  2)), alpha=.05)

# Linear model

id <- as.numeric(rownames(dfa))+1000
dfa <- cbind(id_num=id, dfa)

df_long <- gather(dfa, species, score, score_hp, score_hd)

model <- lmer(score ~ species + age + femaleAs1MaleAs0 + dog_exposure + (1 | participant_id), data = df_long)
Anova(model, test="F")
summary(model)

#GL
# dichotic = lmer(rtime ~ handed*ear + (1 | subject))
#> Anova(dichotic, test="F") # F tests (from car package)

#demographics adults 
dfa %>% skimr::skim(political)
dfa %>% skimr::skim(religiosity)
nrow(filter(dfa, religion == 1))/nrow(dfa)
nrow(filter(dfa, religion == 2 | religion == 3 | religion == 4 | religion == 5))/nrow(dfa)

dfa$eth_white <- ifelse(grepl("1", dfa$ethnicity, fixed=TRUE), T, F)
dfa$eth_black <- ifelse(grepl("2", dfa$ethnicity, fixed=TRUE), T, F)
dfa$eth_hispanic <- ifelse(grepl("3", dfa$ethnicity, fixed=TRUE), T, F)
dfa$eth_asian <- ifelse(grepl("4", dfa$ethnicity, fixed=TRUE), T, F)
dfa$eth_indian <- ifelse(grepl("5", dfa$ethnicity, fixed=TRUE), T, F)
dfa$eth_other <- ifelse(grepl("6", dfa$ethnicity, fixed=TRUE), T, F)


nrow(filter(dfa, eth_white==T))/nrow(dfa)
nrow(filter(dfa, eth_black==T))/nrow(dfa)
nrow(filter(dfa, eth_hispanic==T))/nrow(dfa)
nrow(filter(dfa, eth_asian==T))/nrow(dfa)
nrow(filter(dfa, eth_indian==T))/nrow(dfa)
nrow(filter(dfa, eth_other==T))/nrow(dfa)




##########
### Children study
##########

# Load data

dfc <- read.csv("Study2_children.csv",header=TRUE, stringsAsFactors=FALSE) 
dfc <- tbl_df(dfc)

dfc$age  <-  (dfc$Age.years*12)+dfc$Age.months
dfc$age <- ifelse(dfc$age == 0, NA, dfc$age)
dfc$age_in_years <- dfc$age/12
dfc$Age <- dfc$age_in_years

nrow(dfc)
dfc <- filter(dfc, !is.na(DH100) | !is.na(DH100.1))
nrow(dfc)
dfc <- filter(dfc, age_in_years >= 7 & age_in_years < 10)
nrow(dfc)


dfc$DH100 <- coalesce(dfc$DH100, dfc$DH100.1)
dfc$DH10 <- coalesce(dfc$DH10, dfc$DH10.1)
dfc$DH2 <- coalesce(dfc$DH2, dfc$DH2.1)
dfc$HD1 <- coalesce(dfc$HD1, dfc$HD1.1)
dfc$HD2 <- coalesce(dfc$HD2, dfc$HD2.1)
dfc$HD10 <- coalesce(dfc$HD10, dfc$HD10.1)
dfc$HD100 <- coalesce(dfc$HD100, dfc$HD100.1)

dfc$PH100 <- coalesce(dfc$PH100, dfc$PH100.1)
dfc$PH10 <- coalesce(dfc$PH10, dfc$PH10.1)
dfc$PH2 <- coalesce(dfc$PH2, dfc$PH2.1)
dfc$HP1 <- coalesce(dfc$HP1, dfc$HP1.1)
dfc$HP2 <- coalesce(dfc$HP2, dfc$HP2.1)
dfc$HP10 <- coalesce(dfc$HP10, dfc$HP10.1)
dfc$HP100 <- coalesce(dfc$HP100, dfc$HP100.1)

dfc$DP1 <- coalesce(swap_1_and_3(swap_1_and_3(dfc$DP1)), swap_1_and_3(dfc$DP1.1))
dfc$PP10 <- coalesce(dfc$PP10, dfc$PP10.1)
dfc$HA10 <- coalesce(dfc$HA10, dfc$HA10.1)
dfc$HW10 <- coalesce(dfc$HW10, dfc$HW10.1)

dfc$DH2 <- swap_1_and_3(dfc$DH2)
dfc$DH10 <- swap_1_and_3(dfc$DH10)
dfc$DH100 <- swap_1_and_3(dfc$DH100)

dfc$PH2 <- swap_1_and_3(dfc$PH2)
dfc$PH10 <- swap_1_and_3(dfc$PH10)
dfc$PH100 <- swap_1_and_3(dfc$PH100)

nrow(dfc)
dfc <- filter(dfc, HA10 == 1 | HW10 == 1)  # exclude those who failed the plates and worms question
nrow(dfc)

sprintf("Final N = %s (Excluded = %s); female = %s; M-age = %s (SD = %s)", 
        nrow(dfc), (n_raw - nrow(dfc)), nrow(filter(dfc, Gender == 1)), round(mean(dfc$Age.years, na.rm=T), digits=2), round(sd(dfc$Age.years, na.rm=T), digits=2))


# Scoring system

dfc <- dfc %>% mutate(
  score_hd100 = ifelse(HD100 == 1, 7.64, ifelse(HD100 == 2, 3.82, 0)),
  score_hd10 = ifelse(HD10 == 1, 4.32, ifelse(HD10 == 2, 2.16, 0)),
  score_hd2 = ifelse(HD2 == 1, 2, ifelse(HD2 == 2, 1, 0)),
  score_hd1 = ifelse(HD1 == 1, 1, ifelse(HD1 == 2, 0, -1)),
  score_dh2 = ifelse(DH2 == 1, 0, ifelse(DH2 == 2, -2, -1)),
  score_dh10 = ifelse(DH10 == 1, 0, ifelse(DH10 == 2, -2.16, -4.32)),
  score_dh100 = ifelse(DH100 == 1, 0, ifelse(DH100 == 2, -3.82, -7.64)),
  score_hd = score_hd100 + score_hd10 + score_hd2 + score_hd1 + score_dh2 + score_dh10 + score_dh100
)

dfc <- dfc %>% mutate(
  score_hp100 = ifelse(HP100 == 1, 7.64, ifelse(HP100 == 2, 3.82, 0)),
  score_hp10 = ifelse(HP10 == 1, 4.32, ifelse(HP10 == 2, 2.16, 0)),
  score_hp2 = ifelse(HP2 == 1, 2, ifelse(HP2 == 2, 1, 0)),
  score_hp1 = ifelse(HP1 == 1, 1, ifelse(HP1 == 2, 0, -1)),
  score_ph2 = ifelse(PH2 == 1, 0, ifelse(PH2 == 2, -2, -1)),
  score_ph10 = ifelse(PH10 == 1, 0, ifelse(PH10 == 2, -2.16, -4.32)),
  score_ph100 = ifelse(PH100 == 1, 0, ifelse(PH100 == 2, -3.82, -7.64)),
  score_hp = score_hp100 + score_hp10 + score_hp2 + score_hp1 + score_ph2 + score_ph10 + score_ph100
)

# Scores 

dfc %>% skimr::skim(score_hd, score_hp)
dfc_7_9 <- filter(dfc, age_in_years >= 7 & age_in_years < 10)
dfc_7_9 %>% skimr::skim(score_hd, score_hp)

t.test(dfc$score_hd, mu=0)
t.test(dfc$score_hp, mu=0)
cohensD(dfc$score_hd, mu=0)
d.ci(cohensD(dfc$score_hd, mu = 0), n=nrow(dfc), n1=nrow(dfc), alpha=.05)

cohensD(dfc$score_hp, mu=0)
d.ci(cohensD(dfc$score_hp, mu = 0), n=nrow(dfc), n1=nrow(dfc), alpha=.05)


# Correlations with age

cor.test(dfc$Age, dfc$score_hd)
cor.test(dfc$Age, dfc$score_hp)

# Percentages

perc_human(dfc, dfc$HD100)
perc_human(dfc, dfc$HD10)
perc_human(dfc, dfc$HD2)
perc_human(dfc, dfc$HD1)
perc_human(dfc, dfc$DH2)
perc_human(dfc, dfc$DH10)
perc_human(dfc, dfc$DH100)

perc_human(dfc, dfc$HP100)
perc_human(dfc, dfc$HP10)
perc_human(dfc, dfc$HP2)
perc_human(dfc, dfc$HP1)
perc_human(dfc, dfc$PH2)
perc_human(dfc, dfc$PH10)
perc_human(dfc, dfc$PH100)

perc_animal(dfc, dfc$HD100)
perc_animal(dfc, dfc$HD10)
perc_animal(dfc, dfc$HD2)
perc_animal(dfc, dfc$HD1)
perc_animal(dfc, dfc$DH2)
perc_animal(dfc, dfc$DH10)
perc_animal(dfc, dfc$DH100)

perc_animal(dfc, dfc$HP100)
perc_animal(dfc, dfc$HP10)
perc_animal(dfc, dfc$HP2)
perc_animal(dfc, dfc$HP1)
perc_animal(dfc, dfc$PH2)
perc_animal(dfc, dfc$PH10)
perc_animal(dfc, dfc$PH100)

perc_cantdecide(dfc, dfc$HD100)
perc_cantdecide(dfc, dfc$HD10)
perc_cantdecide(dfc, dfc$HD2)
perc_cantdecide(dfc, dfc$HD1)
perc_cantdecide(dfc, dfc$DH2)
perc_cantdecide(dfc, dfc$DH10)
perc_cantdecide(dfc, dfc$DH100)

perc_cantdecide(dfc, dfc$HP100)
perc_cantdecide(dfc, dfc$HP10)
perc_cantdecide(dfc, dfc$HP2)
perc_cantdecide(dfc, dfc$HP1)
perc_cantdecide(dfc, dfc$PH2)
perc_cantdecide(dfc, dfc$PH10)
perc_cantdecide(dfc, dfc$PH100)

# Dog exposure

dfc %>% group_by(AnimalExp) %>% skimr::skim(score_hd, score_hp)
t.test(filter(dfc, AnimalExp ==  1)$score_hd, filter(dfc, AnimalExp ==  2)$score_hd)
cohensD(filter(dfc, AnimalExp ==  1)$score_hd, filter(dfc, AnimalExp ==  2)$score_hd)
d.ci(cohensD(filter(dfc, AnimalExp ==  1)$score_hd, filter(dfc, AnimalExp ==  2)$score_hd), 
     n1=nrow(filter(dfc, AnimalExp ==  1)), n2=nrow(filter(dfc, AnimalExp ==  2)), alpha=.05)

t.test(filter(dfc, AnimalExp ==  1)$score_hp, filter(dfc, AnimalExp ==  2)$score_hp)
cohensD(filter(dfc, AnimalExp ==  1)$score_hp, filter(dfc, AnimalExp ==  2)$score_hp)
d.ci(cohensD(filter(dfc, AnimalExp ==  1)$score_hp, filter(dfc, AnimalExp ==  2)$score_hp), 
     n1=nrow(filter(dfc, AnimalExp ==  1)), n2=nrow(filter(dfc, AnimalExp ==  2)), alpha=.05)

# Linear model

dfc_long <- gather(dfc, species, score, score_hp, score_hd)

model <- lmer(score ~ species + Age + Gender + AnimalExp + (1 | PID), data = dfc_long)
Anova(model, test="F")
summary(model)


##########
### Combined analysis for Adult and Children samples
##########

dfc_long_comb <- rename(select(dfc_long, score, species, PID), participant_id = PID)
dfa_long_comb <- rename(select(df_long, score, species, id_num), participant_id = id_num)

dfc_long_comb$group = "children"
dfa_long_comb$group = "adults"

df_comb <- rbind(dfc_long_comb, dfa_long_comb)
df_comb$species = ifelse(df_comb$species == "score_hd", "dogs", "pigs")

model <- lmer(score ~ species * group + (1 | participant_id), data = df_comb)
Anova(model, test="F")
summary(model)

#df_comb$group <- ifelse(df_comb$group == "adults", 1, 0)
#df_comb$species <- ifelse(df_comb$species == "dogs", 1, 0)
df_comb$group <-  as.factor(df_comb$group)
df_comb$species <-  as.factor(df_comb$species)

afex::aov_4(score ~ group + (species | participant_id), data = df_comb, anova_table = "pes") 
ci.pvaf(F.value=72.86, df.1=1, df.2=122, N=nrow(df_comb)/2, conf.level=.95) #group
ci.pvaf(F.value=22.52, df.1=1, df.2=122, N=nrow(df_comb)/2, conf.level=.95) #species
ci.pvaf(F.value=3.88, df.1=1, df.2=122, N=nrow(df_comb)/2, conf.level=.95) #interaction

df_comb %>% group_by(species,  group) %>% skimr::skim(score)

pirateplot(score ~ species + group, df_comb)

