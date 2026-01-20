library(foreign) #imports SPSS
library(dplyr) #structures the data
library(psych)
library(reshape2)

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/ZA5678_v12-0_pairfam/Data/SPSS/English")
#load anchor data 
anchor01 <- read.spss('anchor1.sav', use.value.labels=F, to.data.frame=T)
anchor02 <- read.spss('anchor2.sav', use.value.labels=F, to.data.frame=T)
anchor03 <- read.spss('anchor3.sav', use.value.labels=F, to.data.frame=T)
anchor04 <- read.spss('anchor4.sav', use.value.labels=F, to.data.frame=T)
anchor05 <- read.spss('anchor5.sav', use.value.labels=F, to.data.frame=T)
anchor06 <- read.spss('anchor6.sav', use.value.labels=F, to.data.frame=T)
anchor07 <- read.spss('anchor7.sav', use.value.labels=F, to.data.frame=T)
anchor08 <- read.spss('anchor8.sav', use.value.labels=F, to.data.frame=T)
anchor09 <- read.spss('anchor9.sav', use.value.labels=F, to.data.frame=T)
anchor10 <- read.spss('anchor10.sav', use.value.labels=F, to.data.frame=T)
anchor11 <- read.spss('anchor11.sav', use.value.labels=F, to.data.frame=T)
anchor12.capi <- read.spss('anchor12_capi.sav', use.value.labels=F, to.data.frame=T)
anchor12.cati <- read.spss('anchor12_cati.sav', use.value.labels=F, to.data.frame=T)


anchor.al <- bind_rows(anchor01, anchor02, anchor03, anchor04, anchor05, anchor06, anchor07, anchor08, anchor09, anchor10, anchor11,
                       anchor12.capi, anchor12.cati)

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/ZA5678_v12-0_pairfam/Stepups/Data/SPSS/English")
#load stepup data (available from Wave 4 onwards)
stepup04 <- read.spss('stepup_anchor4.sav', use.value.labels=F, to.data.frame=T)
stepup05 <- read.spss('stepup_anchor5.sav', use.value.labels=F, to.data.frame=T)
stepup06 <- read.spss('stepup_anchor6.sav', use.value.labels=F, to.data.frame=T)
stepup07 <- read.spss('stepup_anchor7.sav', use.value.labels=F, to.data.frame=T)
stepup08 <- read.spss('stepup_anchor8.sav', use.value.labels=F, to.data.frame=T)
stepup09 <- read.spss('stepup_anchor9+transition.sav', use.value.labels=F, to.data.frame=T)
stepup10 <- read.spss('stepup_anchor10+transition.sav', use.value.labels=F, to.data.frame=T)
stepup11 <- read.spss('stepup_anchor11+transition.sav', use.value.labels=F, to.data.frame=T)
stepup12.capi <- read.spss('stepup_anchor12_capi+transition.sav', use.value.labels=F, to.data.frame=T)
stepup12.cati <- read.spss('stepup_anchor12_cati+transition.sav', use.value.labels=F, to.data.frame=T)

stepup.al <- bind_rows(stepup04, stepup05, stepup06, stepup07, stepup08, stepup09, stepup10, stepup11, stepup12.capi, stepup12.cati)

data.a <- bind_rows(anchor.al, stepup.al)

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/ZA5678_v12-0_pairfam/Data/SPSS/English")
#load partner  data 
partner01 <- read.spss('partner1.sav', use.value.labels=F, to.data.frame=T)
partner02 <- read.spss('partner2.sav', use.value.labels=F, to.data.frame=T)
partner03 <- read.spss('partner3.sav', use.value.labels=F, to.data.frame=T)
partner04 <- read.spss('partner4.sav', use.value.labels=F, to.data.frame=T)
partner05 <- read.spss('partner5.sav', use.value.labels=F, to.data.frame=T)
partner06 <- read.spss('partner6.sav', use.value.labels=F, to.data.frame=T)
partner07 <- read.spss('partner7.sav', use.value.labels=F, to.data.frame=T)
partner08 <- read.spss('partner8.sav', use.value.labels=F, to.data.frame=T)
partner09 <- read.spss('partner9.sav', use.value.labels=F, to.data.frame=T)
partner10 <- read.spss('partner10.sav', use.value.labels=F, to.data.frame=T)
partner11 <- read.spss('partner11.sav', use.value.labels=F, to.data.frame=T)
partner12 <- read.spss('partner12.sav', use.value.labels=F, to.data.frame=T)


partner.al <- bind_rows(partner01, partner02, partner03, partner04, partner05, partner06, partner07, partner08, partner09, partner10, partner11, partner12)

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/ZA5678_v12-0_pairfam/Stepups/Data/SPSS/English")
#load stepup partner data (available from Wave 4 onwards)
p.stepup04 <- read.spss('stepup_partner4.sav', use.value.labels=F, to.data.frame=T)
p.stepup05 <- read.spss('stepup_partner5.sav', use.value.labels=F, to.data.frame=T)
p.stepup06 <- read.spss('stepup_partner6.sav', use.value.labels=F, to.data.frame=T)
p.stepup07 <- read.spss('stepup_partner7.sav', use.value.labels=F, to.data.frame=T)
p.stepup08 <- read.spss('stepup_partner8.sav', use.value.labels=F, to.data.frame=T)
p.stepup09 <- read.spss('stepup_partner9.sav', use.value.labels=F, to.data.frame=T)
p.stepup10 <- read.spss('stepup_partner10.sav', use.value.labels=F, to.data.frame=T)
p.stepup11 <- read.spss('stepup_partner11.sav', use.value.labels=F, to.data.frame=T)
p.stepup12 <- read.spss('stepup_partner12.sav', use.value.labels=F, to.data.frame=T)


p.stepup.al <- bind_rows(p.stepup04, p.stepup05, p.stepup06, p.stepup07, p.stepup08, p.stepup09, p.stepup10, p.stepup11, p.stepup12)

data.p <- bind_rows(partner.al, p.stepup.al)

data.p=subset(data.p, select=c("id", "pid", "wave", "ppa18i3", "ppa18i8", "ppa18i13", "ppa19i1", "ppa19i8", "ppa19i5", "ppa19i6", 
             "ppa19i3", "ppa19i7", "ppa22ri1", "ppa22ri3", "ppa22ri4",
             "ppa22ri8", "ppa22ri2", "ppa22ri5", "ppa22ri6", "ppa22ri7", "ppa22pi1", "ppa22pi3", "ppa22pi4", "ppa22pi8", "ppa22pi2", 
             "ppa22pi5", "ppa22pi6", "ppa22pi7",
             "pper1i2", "pper1i7", "pper1i13", "psat6",
             "ppa18i18", "psat3", "ppa17i1", "ppa17i8", "ppa17i2", "ppa17i5",
             "ppa17i7", "ppa17i3", "ppa17i6", "ppa17i4"))
             
data.all <- merge(data.a, data.p, by=c("id", "pid", "wave"), all.x=T)

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")

save(data.all, file="data.all.RData")

load("data.all.RData")

#list relevant variables to select
an.dem_vars <- c("id", "pid", "wave", "sex_gen", "psex_gen", "doby_gen", "pdoby_gen", "age", "page", "lfs", "plfs", "yeduc", "pyeduc", 
                 "incnet", "hhincnet")
an.rel_vars <- c("sd3", "hp", "hps2", "hpsep", "relstat", "homosex", "reldur", "nkidsp")
mod_vars = c("pa20i2", "pa18i3", "pa18i8", "pa18i13", "pa19i1", "pa19i8", "pa19i2", "pa19i4", "pa19i5", "pa19i6", 
             "pa19i3", "pa19i7", "pa22ri1", "pa22ri3", "pa22ri4",
             "pa22ri8", "pa22ri2", "pa22ri5", "pa22ri6", "pa22ri7", "pa22pi1", "pa22pi3", "pa22pi4", "pa22pi8", "pa22pi2", 
             "pa22pi5", "pa22pi6", "pa22pi7")
#sd3 Current relationship status: Partnership existent (only wave 1)
#hp has a partner (all waves, anchor and stepups, except for wave 1)
#hps2 condition 2 for single module (=1 if anchor had no relationship in the last three months, =0 otherwise) 
#pa20i2 Being unfaithful would be serious relationship problem
#doby_gen Generated date of birth anchor: year
#pdoby_gen	Generated date of birth partner: year
#age	Age anchor
#page	Age partner
#relstat married/cohabiting etc.
#homosex anchor gay/lesbian
#hpsep separate from the partner from previous wave

#partner moderators
pmod_vars = c("ppa18i3", "ppa18i8", "ppa18i13", "ppa19i1", "ppa19i8", "ppa19i5", "ppa19i6", 
              "ppa19i3", "ppa19i7", "ppa22ri1", "ppa22ri3", "ppa22ri4",
              "ppa22ri8", "ppa22ri2", "ppa22ri5", "ppa22ri6", "ppa22ri7", "ppa22pi1", "ppa22pi3", "ppa22pi4", "ppa22pi8", "ppa22pi2", 
              "ppa22pi5", "ppa22pi6", "ppa22pi7")

#partner dv
pdv_vars = c("pper1i2", "pper1i7", "pper1i13", "psat6",
             "ppa18i18", "psat3", "ppa17i1", "ppa17i8", "ppa17i2", "ppa17i5",
             "ppa17i7", "ppa17i3", "ppa17i6", "ppa17i4")

an.sel_vars <- c("per1i2", "per1i7", "per1i13") # selfesteem
an.lsa_vars <- c("sat6") #life satisfaction

#anchor's perception of relationship 
an.rsa_vars <- c("pa18i18", "sat3") #note: these are on different metrics!
#relationship quality
an.rin_vars <- c("pa17i1", "pa17i8") #intimacy
an.rad_vars <- c("pa17i2", "pa17i5") #admiration
an.rdo_vars <- c("pa17i7", "pa17i3") #dominance
an.rcf_vars <- c("pa17i6", "pa17i4") #conflict
#cheating
an.rif_vars <- c("pa24") 

sl_vars <- c(an.dem_vars, an.sel_vars, mod_vars, an.lsa_vars, an.rel_vars, an.rsa_vars, an.rin_vars, an.rad_vars, an.rdo_vars, an.rcf_vars, an.rif_vars,
             pmod_vars, pdv_vars)
data.sl <- data.all[sl_vars]

write.csv(data.sl, "pairfam_inf_sl.csv", row.names=F)
library(haven)
write_sav(data.sl, "pairfam_inf_sl.sav")


#create scales
data=read.csv("pairfam_inf_sl.csv")
#anchor outcomes
data$per1i2_r <- 6 - data$per1i2
an.sel_vars <- c("per1i2_r", "per1i7", "per1i13")
psych::alpha(data[an.sel_vars])
data$an.sel <- rowMeans(data[an.sel_vars], na.rm=T)

#rescale to 1-5 format, like other variables 
data$an.lsa <- 1+data$sat6*(4/10)

table(data$pa18i18)
table(data$sat3)
table(1+data$sat3*(4/10))
data$sat3_r <- 1+data$sat3*(4/10)
an.rsa_vars <- c("pa18i18", "sat3_r")
psych::corr.test(data[an.rsa_vars])
data$an.rsa <- rowMeans(data[an.rsa_vars], na.rm=T)

an.rin_vars <- c("pa17i1", "pa17i8")
psych::corr.test(data[an.rin_vars])
data$an.rin <- rowMeans(data[an.rin_vars], na.rm=T)

an.rad_vars <- c("pa17i2", "pa17i5")
psych::corr.test(data[an.rad_vars])
data$an.rad <- rowMeans(data[an.rad_vars], na.rm=T)

an.rdo_vars <- c("pa17i7", "pa17i3")
psych::corr.test(data[an.rdo_vars])
data$an.rdo <- rowMeans(data[an.rdo_vars], na.rm=T)

an.rcf_vars <- c("pa17i6", "pa17i4")
psych::corr.test(data[an.rcf_vars])
data$an.rcf <- rowMeans(data[an.rcf_vars], na.rm=T)

#partner outcomes
data$pper1i2_r <- 6 - data$pper1i2
pr.sel_vars <- c("pper1i2_r", "pper1i7", "pper1i13")
psych::alpha(data[pr.sel_vars])
data$pr.sel <- rowMeans(data[pr.sel_vars], na.rm=T)

#rescale to 1-5 format, like other variables 
data$pr.lsa <- 1+data$psat6*(4/10)

table(data$ppa18i18)
table(data$psat3)
table(1+data$psat3*(4/10))
data$psat3_r <- 1+data$psat3*(4/10)
pr.rsa_vars <- c("ppa18i18", "psat3_r")
psych::corr.test(data[pr.rsa_vars])
data$pr.rsa <- rowMeans(data[pr.rsa_vars], na.rm=T)

pr.rin_vars <- c("ppa17i1", "ppa17i8")
psych::corr.test(data[pr.rin_vars])
data$pr.rin <- rowMeans(data[pr.rin_vars], na.rm=T)

pr.rad_vars <- c("ppa17i2", "ppa17i5")
psych::corr.test(data[pr.rad_vars])
data$pr.rad <- rowMeans(data[pr.rad_vars], na.rm=T)

pr.rdo_vars <- c("ppa17i7", "ppa17i3")
psych::corr.test(data[pr.rdo_vars])
data$pr.rdo <- rowMeans(data[pr.rdo_vars], na.rm=T)

pr.rcf_vars <- c("ppa17i6", "ppa17i4")
psych::corr.test(data[pr.rcf_vars])
data$pr.rcf <- rowMeans(data[pr.rcf_vars], na.rm=T)

#####potential moderators
#anchor
data$cheat.problem=data$pa20i2
table(data$cheat.problem, exclude = T)

hostile <- c("pa18i3", "pa18i8", "pa18i13")
psych::alpha(data[hostile])
data$hostile <- rowMeans(data[hostile], na.rm=T)

future <- c("pa19i1", "pa19i8")
psych::corr.test(data[future])
data$future <- rowMeans(data[future], na.rm=T)

sacrifice <- c("pa19i2", "pa19i4")
psych::corr.test(data[sacrifice])
data$sacrifice <- rowMeans(data[sacrifice], na.rm=T)

data$pa19i5R=6-data$pa19i5
data$pa19i6R=6-data$pa19i6
tolerance <- c("pa19i5R", "pa19i6R")
psych::corr.test(data[tolerance])
data$tolerance <- rowMeans(data[tolerance], na.rm=T)

reciprocity <- c("pa19i3", "pa19i7")
psych::corr.test(data[reciprocity])
data$reciprocity <- rowMeans(data[reciprocity], na.rm=T)

#actor assessment of own behavior
aggression <- c("pa22ri1", "pa22ri3")
psych::corr.test(data[aggression])
data$aggression <- rowMeans(data[aggression], na.rm=T)

constructive <- c("pa22ri4", "pa22ri8")
psych::corr.test(data[constructive])
data$constructive <- rowMeans(data[constructive], na.rm=T)

withdrawal <- c("pa22ri2", "pa22ri5")
psych::corr.test(data[withdrawal])
data$withdrawal <- rowMeans(data[withdrawal], na.rm=T)

manipulation <- c("pa22ri6", "pa22ri7")
psych::corr.test(data[manipulation])
data$manipulation <- rowMeans(data[manipulation], na.rm=T)

#actor assessment of partner behavior
aggression.p <- c("pa22pi1", "pa22pi3")
psych::corr.test(data[aggression.p])
data$aggression.p <- rowMeans(data[aggression.p], na.rm=T)

constructive.p <- c("pa22pi4", "pa22pi8")
psych::corr.test(data[constructive.p])
data$constructive.p <- rowMeans(data[constructive.p], na.rm=T)

withdrawal.p <- c("pa22pi2", "pa22pi5")
psych::corr.test(data[withdrawal.p])
data$withdrawal.p <- rowMeans(data[withdrawal.p], na.rm=T)

manipulation.p <- c("pa22pi6", "pa22pi7")
psych::corr.test(data[manipulation.p])
data$manipulation.p <- rowMeans(data[manipulation.p], na.rm=T)

###potential moderators
#partner
p.hostile <- c("ppa18i3", "ppa18i8", "ppa18i13")
psych::alpha(data[p.hostile])
data$p.hostile <- rowMeans(data[p.hostile], na.rm=T)

p.future <- c("ppa19i1", "ppa19i8")
psych::corr.test(data[p.future])
data$p.future <- rowMeans(data[p.future], na.rm=T)

data$ppa19i5R=6-data$ppa19i5
data$ppa19i6R=6-data$ppa19i6
p.tolerance <- c("ppa19i5R", "ppa19i6R")
psych::corr.test(data[p.tolerance])
data$p.tolerance <- rowMeans(data[p.tolerance], na.rm=T)

p.reciprocity <- c("ppa19i3", "ppa19i7")
psych::corr.test(data[p.reciprocity])
data$p.reciprocity <- rowMeans(data[p.reciprocity], na.rm=T)

#partner assessment of own behavior
p.aggression <- c("ppa22ri1", "ppa22ri3")
psych::corr.test(data[p.aggression])
data$p.aggression <- rowMeans(data[p.aggression], na.rm=T)

p.constructive <- c("ppa22ri4", "ppa22ri8")
psych::corr.test(data[p.constructive])
data$p.constructive <- rowMeans(data[p.constructive], na.rm=T)

p.withdrawal <- c("ppa22ri2", "ppa22ri5")
psych::corr.test(data[p.withdrawal])
data$p.withdrawal <- rowMeans(data[p.withdrawal], na.rm=T)

p.manipulation <- c("ppa22ri6", "ppa22ri7")
psych::corr.test(data[p.manipulation])
data$p.manipulation <- rowMeans(data[p.manipulation], na.rm=T)

#partner assessment of actor behavior
p.aggression.p <- c("ppa22pi1", "ppa22pi3")
psych::corr.test(data[p.aggression.p])
data$p.aggression.p <- rowMeans(data[p.aggression.p], na.rm=T)

p.constructive.p <- c("ppa22pi4", "ppa22pi8")
psych::corr.test(data[p.constructive.p])
data$p.constructive.p <- rowMeans(data[p.constructive.p], na.rm=T)

p.withdrawal.p <- c("ppa22pi2", "ppa22pi5")
psych::corr.test(data[p.withdrawal.p])
data$p.withdrawal.p <- rowMeans(data[p.withdrawal.p], na.rm=T)

p.manipulation.p <- c("ppa22pi6", "ppa22pi7")
psych::corr.test(data[p.manipulation.p])
data$p.manipulation.p <- rowMeans(data[p.manipulation.p], na.rm=T)

agg=c("aggression", "aggression.p", "p.aggression", "p.aggression.p")
psych::corr.test(data[agg])

table(data$hpsep, exclude=T)

####prepare socio-demographics for propensity score matching
data$id.couple=paste(data$id, data$pid)

names(data)
table(data$sex_gen, exclude = T)
data$male=ifelse(data$sex_gen==1, 1, 0)
table(data$male, exclude = T)
table(data$psex_gen, exclude = T)
data$p.male=ifelse(data$psex_gen==1, 1, 0)
table(data$p.male, exclude = T)
table(data$doby_gen, exclude = T)
table(data$pdoby_gen, exclude = T) #has many typos

###relationship status
table(data$relstat, exclude = T)
data$married=ifelse(data$relstat==4 | data$relstat==5, 1, 0)
data$livetogether=ifelse(data$relstat ==3 | data$relstat==4 | data$relstat==8 | data$relstat==11, 1, 0)
table(data$married, exclude = T)
table(data$livetogether, exclude = T)

table(data$homosex, exclude = T)

table(data$reldur, exclude = T)

table(data$nkidsp, exclude = T)

table(data$yeduc, exclude = T)
table(data$pyeduc, exclude = T)

###income has lots of missing
sum(is.na(data$incnet))
sum(is.na(data$hhincnet)) #less missing here

table(data$lfs, exclude = T)
data$empl.cat=ifelse(data$lfs==9 | data$lfs==10 | data$lfs==11 | data$lfs==12, 1, 
                     ifelse(data$lfs==4, 2,
                            ifelse(data$lfs==1 | data$lfs==8, 3, 4)))
table(data$empl.cat, exclude = T)

table(data$plfs, exclude = T)
data$p.empl.cat=ifelse(data$plfs==9 | data$plfs==10 | data$plfs==11 | data$plfs==12, 1, 
                       ifelse(data$plfs==4, 2,
                              ifelse(data$plfs==1 | data$plfs==8, 3, 4)))
table(data$p.empl.cat, exclude = T)

write.csv(data, "pairfam_inf_sl.csv", row.names=F)
library(haven)
write_sav(data, "pairfam_inf_sl.sav")


