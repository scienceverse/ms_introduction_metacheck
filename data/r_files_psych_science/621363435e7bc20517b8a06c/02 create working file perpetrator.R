library(dplyr)
library(reshape2)
library(psych)
library(haven)

library(car)
recode.car <- car::recode

data=read.csv("pairfam_inf_sl.csv", header=T)
names(data)

table(data$hp, exclude = T)
table(data$hps2, exclude = T) 
###use hp, hps2 has more missing values
#hp and hps2 nearly identical (r= -.97)
#v=subset(data, select = c(hp, hps2, sd3))
#library(Hmisc)
#rcorr(as.matrix(v))

#select only people who had a partner during one of the waves (otherwise they cannot cheat)
data$an.rel <- 0
data[which(data$sd3==1 | data$hp==1), "an.rel"] <- 1
table(data$an.rel)

an.rel.l2 <- data %>% group_by(id) %>% summarise(an.rel.l2 = max(an.rel, na.rm=T))
#people without any relationship are excluded
table(an.rel.l2$an.rel.l2)
data <- merge(data, an.rel.l2, by="id", all.x=T)
#data points belonging to people without any relationship are excluded
table(data$an.rel.l2)
data <- data[which(data$an.rel.l2==1), ]

#tabulate number of events by wave 
table(data$pa24, data$wave)
#Computes row-wise summaries (how many events)
#1 Yes, I did
sum(table(data$pa24, data$wave)[1,])
#2 Yes, my partner did
sum(table(data$pa24, data$wave)[2,])
#3 Yes, both my partner and I did
sum(table(data$pa24, data$wave)[3,])

data$an.inf <- NA
data[which(data$pa24==1), "an.inf"] <- 1
data[which(data$pa24==4), "an.inf"] <- 0
#shows when people reported cheating
table(data$an.inf, data$wave)
sum(table(data$an.inf, data$wave)[2,1:12])

data$pr.inf <- NA
data[which(data$pa24==2), "pr.inf"] <- 1
data[which(data$pa24==4), "pr.inf"] <- 0
table(data$pr.inf, data$wave)

data$anpr.inf <- NA
data[which(data$pa24==3), "anpr.inf"] <- 1
data[which(data$pa24==4), "anpr.inf"] <- 0
table(data$anpr.inf, data$wave)

#distribution of cheats
inf.l2 <- data %>% group_by(id) %>% summarise(an.inf.kl2 = sum(an.inf, na.rm=T), 
                                              pr.inf.kl2 = sum(pr.inf, na.rm=T), 
                                              anpr.inf.kl2 = sum(anpr.inf, na.rm=T))
table(inf.l2$an.inf.kl2)
table(inf.l2$pr.inf.kl2)
table(inf.l2$anpr.inf.kl2)
library(Hmisc)
rcorr(as.matrix(inf.l2[,c("an.inf.kl2", "pr.inf.kl2", "anpr.inf.kl2")]))

data=merge(data, inf.l2, by="id", all.x = T)


###cases where there was anchor cheating + partner or anchor & partner cheating in the same relationship
cheats=data %>% group_by(id, pid) %>% summarise(an.inf.sum=sum(an.inf, na.rm=T),
                                                pr.inf.sum=sum(pr.inf, na.rm=T),
                                                anpr.inf.sum=sum(anpr.inf, na.rm=T))
table(cheats$an.inf.sum, cheats$pr.inf.sum)
cheats$mult.cheat1=ifelse(cheats$an.inf.sum > 0 & cheats$pr.inf.sum > 0, 1, 0)
cheats$mult.cheat2=ifelse(cheats$an.inf.sum > 0 & cheats$anpr.inf.sum > 0, 1, 0)
cheats$mult.cheat3=ifelse(cheats$pr.inf.sum > 0 & cheats$anpr.inf.sum > 0, 1, 0)
table(cheats$mult.cheat1) #14 cases with anchor cheating (and partner cheating in the same relationship in different years)
table(cheats$mult.cheat2) #21 cases with anchor cheating (and partner+anchor cheating in the same relationship in different years)
table(cheats$mult.cheat3) #12 cases with anchor cheating (and partner+anchor cheating in the same relationship in different years)
sum(cheats$mult.cheat1 ==1 | cheats$mult.cheat2 ==1 | cheats$mult.cheat3 ==1) # 43 cases

data=merge(data, cheats, by=c("id", "pid"), all.x = T)
###remove cases with multiple cheating
data=subset(data, data$mult.cheat1==0 & data$mult.cheat2==0 & data$mult.cheat3 ==0)

save(data, file="data.RData")

load("data.RData")

###################################################
######anchor cheating and controls
###################################################

###indicator of perpetrators and controls vs. other types of cheaters
data$focus.groups=ifelse(data$pr.inf.kl2>0 | data$anpr.inf.kl2>0, 0, 1)
table(data$focus.groups, exclude = T)


########focus on control sample
data=subset(data, data$focus.groups==1)
#create group (cheaters / no cheaters) indicator
data$event.group = ifelse(data$an.inf.kl2==0 & data$pr.inf.kl2==0 & data$anpr.inf.kl2 ==0, 'control', 'cheater')
table(data$event.group) #4610 (rows) with the event
dp=distinct(data, id, .keep_all = T)
table(dp$event.group) #609 cheaters, 13095 controls

#create a dataset with controls
controls=subset(data, data$event.group=='control')

#mark a random relationship in the control sample
inf.0.pr <- controls[c("id", "pid")]
names(inf.0.pr)[2] <-"inf.0.pr"
inf.0.pr <- inf.0.pr[!is.na(inf.0.pr$inf.0.pr),]
inf.0.pr <- inf.0.pr[!duplicated(inf.0.pr$inf.0.pr),]
set.seed(12345)
inf.0.pr <- inf.0.pr %>% group_by(id) %>% sample_n(1)

controls <- merge(controls, inf.0.pr, by="id", all.x=T)
#use only relationship data of the randomly selected relationship 
controls[which(controls$pid==controls$inf.0.pr), "an.use"] <- 1

save(controls, file="controls.RData")
write_sav(controls, "controls.sav")


########focus on anchor-only cheating in the following
#mark only the first cheat
anchor.cheater=subset(data, data$event.group=='cheater')
anchor.cheater <- anchor.cheater[order(anchor.cheater$id, anchor.cheater$an.inf, anchor.cheater$wave), ]
anchor.cheater <- anchor.cheater %>% group_by(id, an.inf) %>% mutate(nr = row_number())
anchor.cheater$an.inf.ev1 <- 0
anchor.cheater[which(anchor.cheater$an.inf==1 & anchor.cheater$nr==1), "an.inf.ev1"] <- 1
table(anchor.cheater$an.inf.ev1)
table(anchor.cheater$an.inf.ev1, anchor.cheater$wave)
anchor.cheater$nr <- NULL

#mark the partner that was cheated on
inf1.pr <- anchor.cheater[which(anchor.cheater$an.inf.ev1==1), c("id", "pid")]
names(inf1.pr)[2] <-"an.inf1.pr"
anchor.cheater <- merge(anchor.cheater, inf1.pr, by="id", all.x=T)

#use only relationship anchor.cheater of the cheated partner 
anchor.cheater$an.use <- NA
anchor.cheater[which(anchor.cheater$pid==anchor.cheater$an.inf1.pr), "an.use"] <- 1
table(anchor.cheater$an.use)
table(anchor.cheater$an.inf.kl2, anchor.cheater$an.use)

anchor.cheater$an.inf1.preva <- NA
anchor.cheater[which(anchor.cheater$an.inf.kl2==0), "an.inf1.preva"] <- 0
anchor.cheater[which(anchor.cheater$an.inf.kl2>0), "an.inf1.preva"] <- 1
table(anchor.cheater$an.inf1.preva, anchor.cheater$an.inf.ev1)
table(anchor.cheater$an.inf1.preva, anchor.cheater$wave)

save(anchor.cheater, file="anchor.cheater.RData")
write_sav(anchor.cheater, "anchor.cheater.sav")

#################propensity score matching
###merge anchor.cheater and control
names(anchor.cheater)
names(controls)

controls=controls[, -177]

controls$an.inf.ev1=NA
controls$an.inf1.pr=NA
controls$an.inf1.preva=NA

controls=controls[, c(1:176, 178, 179, 177, 180)]

a.cheater.controls=rbind(anchor.cheater, controls)
write_sav(a.cheater.controls, "a.cheater.controls.sav")

#############propensity score matching
##duplicate the dataset for further transformations
a.cheater.controls.matched=as.data.frame(a.cheater.controls)
save(a.cheater.controls.matched, file="a.cheater.controls.matched.RData")


table(a.cheater.controls.matched$event.group) #4610 (rows) with the event
dp=distinct(a.cheater.controls.matched, id, .keep_all = T)
table(dp$event.group) #609 cheaters, 13095 controls

######leave only one row per person
names(a.cheater.controls.matched)
a.cheater.controls.matched <- a.cheater.controls.matched[order(a.cheater.controls.matched$id, a.cheater.controls.matched$wave),] 
individual <- a.cheater.controls.matched %>%
  group_by(id) %>%
  summarise(id=first(id),
            event.group=first(event.group),
            male=first(male),
            doby_gen=first(doby_gen),
            age.first=first(age),
            homosex=first(homosex),
            empl.cat=first(empl.cat[which(!is.na(empl.cat))[1]]),
            yeduc=first(yeduc[which(!is.na(yeduc))[1]]),
            income=first(incnet[which(!is.na(incnet))[1]]),
            income.hh=first(hhincnet[which(!is.na(hhincnet))[1]]),
            assessment.last=last(wave),
            assessment.first=first(wave))

a.cheater.controls.matched <- a.cheater.controls.matched[order(a.cheater.controls.matched$id, a.cheater.controls.matched$wave),] 
couple <- a.cheater.controls.matched %>%
  group_by(id, an.use) %>%
  summarise(id=first(id),
            pid=first(pid),
            id.couple=first(id.couple),
            p.male=first(p.male),
            pdoby_gen=first(pdoby_gen),
            page.first=first(page),
            p.empl.cat=first(p.empl.cat[which(!is.na(p.empl.cat))[1]]),
            p.yeduc=first(pyeduc[which(!is.na(pyeduc))[1]]),
            reldur=first(reldur),
            nkidsp=first(nkidsp),
            married=first(married),
            livetogether=first(livetogether))
            
couple=subset(couple, couple$an.use==1)

names(individual)
names(couple)

data.person.new=merge(individual, couple, by="id")

###matching is done based on actor socio-demographics obtained in wave 1 and relationship- and partner-related variables in the first wave 
###of the respective relationship (for cheaters, the first relationship where cheating occurred, for controls, a randomly selected relationship)

write_sav(data.person.new, "data.person.new.sav")

table(data.person.new$event.group, exclude = T)

data.person.new$n=data.person.new$assessment.last-data.person.new$assessment.first+1
table(data.person.new$n, exclude = T)

library(MatchIt)
###missing values on matching covariates are not allowed. Actor missing values were replaced with sample medians; partner missing values 
###were replaced with their anchor values.  

##replace missing with values with partner values
summary(data.person.new$income, na.rm=T)
summary(data.person.new$income.hh, na.rm=T)
##replace missing income with income median
data.person.new$income=recode.car(data.person.new$income, "NA=1051")
summary(data.person.new$income, na.rm=T)

#male had one missing value, got assigned the value of the partner (assuming heterosexuality)
table(data.person.new$male, exclude = T)
data.person.new$male=ifelse(is.na(data.person.new$male), data.person.new$p.male-1, 
                                  data.person.new$male)
table(data.person.new$male, exclude = T)

table(data.person.new$p.male, exclude = T)
data.person.new$p.male=ifelse(is.na(data.person.new$p.male) & data.person.new$male == 1, 0, 
                                    ifelse(is.na(data.person.new$p.male) & data.person.new$male==0, 1, 
                            data.person.new$p.male))
table(data.person.new$p.male, exclude = T)

table(data.person.new$age.first, exclude = T)
table(data.person.new$page.first, exclude = T)
#some values were below 15, coded as NA
data.person.new$age.first=ifelse(data.person.new$age.first<15, NA, data.person.new$age.first)
data.person.new$page.first=ifelse(data.person.new$page.first<15 | data.person.new$page.first>59,
                                  NA, data.person.new$page.first) #59 because maximum age for actor was 49
summary(data.person.new$age.first)
data.person.new$age.first=ifelse(is.na(data.person.new$age.first), 27, data.person.new$age.first)
data.person.new$page.first=ifelse(is.na(data.person.new$page.first), data.person.new$age.first, 
                                   data.person.new$page.first)
summary(data.person.new$page.first)

summary(data.person.new$reldur, exclude = T)
data.person.new$reldur=ifelse(is.na(data.person.new$reldur), 34, data.person.new$reldur)

summary(data.person.new$nkidsp)
data.person.new$nkidsp=ifelse(is.na(data.person.new$nkidsp), 0, data.person.new$nkidsp)

summary(data.person.new$married)
data.person.new$married=ifelse(is.na(data.person.new$married), 0, data.person.new$married)

summary(data.person.new$livetogether)
data.person.new$livetogether=ifelse(is.na(data.person.new$livetogether), 1, data.person.new$livetogether)

summary(data.person.new$yeduc, exclude = T)
data.person.new$yeduc=ifelse(is.na(data.person.new$yeduc), 11.5, data.person.new$yeduc)
data.person.new$p.yeduc=ifelse(is.na(data.person.new$p.yeduc), data.person.new$yeduc, 
                                  data.person.new$p.yeduc)
summary(data.person.new$p.yeduc)

summary(data.person.new$empl.cat, exclude = T)
data.person.new$empl.cat=ifelse(is.na(data.person.new$empl.cat), 1, data.person.new$empl.cat)
data.person.new$p.empl.cat=ifelse(is.na(data.person.new$p.empl.cat), data.person.new$empl.cat, 
                               data.person.new$p.empl.cat)
summary(data.person.new$p.empl.cat)

##matching
data.person.new$event.group=as.numeric(ifelse(data.person.new$event.group=='control', 0, 1))
set.seed(1234)
match.it <- matchit(event.group~ male + p.male+ age.first+page.first+reldur+married+livetogether+nkidsp+n+
                      yeduc+p.yeduc+as.factor(empl.cat)+as.factor(p.empl.cat)+income,
                    data = data.person.new, method="nearest", replace=T, ratio=2)
a <- summary(match.it)
a

##get descriptives
table.full=write.csv(a$sum.all, "table.full.csv")
table.matched=write.csv(a$sum.matched, "table.matched.csv")


person.match <- get_matches(match.it, id = "id.match")
names(person.match)

table(person.match$subclass)

####create the dataset with only matched cases (with no event) and cases with event
person.match=subset(person.match, select=c(id, subclass))
table(person.match$event.group, exclude = T)

names(a.cheater.controls.matched)
names(person.match)

a.data.merge.matched=merge(a.cheater.controls, person.match, by="id")
names(a.data.merge.matched)
table(a.data.merge.matched$event.group, exclude = T)

save(a.data.merge.matched, file="a.data.merge.matched.RData")

######################create time indicators for anchor cheaters
anchor.cheater=subset(a.data.merge.matched, a.data.merge.matched$event.group=='cheater')
anchor.cheater <- anchor.cheater[order(anchor.cheater$id, anchor.cheater$wave), ]
#generates the wave nr when (if) cheating occurred (note: data was collected around the end of each year, with data collection proceeding until spring of the next year)
anchor.cheater$an.inf1ev.wav <- anchor.cheater$wave*anchor.cheater$an.inf.ev1
anchor.cheater$an.inf1ev.wav <- ave(anchor.cheater$an.inf1ev.wav, anchor.cheater$id, FUN=max)
#subtract 1 (midpoint between current wave and 2 years ago) from event wave, after wave 3 (until then, the question was "about the last year")
table(anchor.cheater$an.inf1.preva, anchor.cheater$an.inf1ev.wav)
anchor.cheater[which(anchor.cheater$an.inf1.preva==1 & anchor.cheater$an.inf1ev.wav>3), "an.inf1ev.wav"] <- anchor.cheater[which(anchor.cheater$an.inf1.preva==1 & anchor.cheater$an.inf1ev.wav>3), 
                                                                                   "an.inf1ev.wav"] - 1
table(anchor.cheater$an.inf1.preva, anchor.cheater$an.inf1ev.wav)
anchor.cheater$an.inf1.te <- ifelse(anchor.cheater$an.inf1.preva==1, (anchor.cheater$wave - anchor.cheater$an.inf1ev.wav), 0)
anchor.cheater$an.inf1.preD <- ifelse(anchor.cheater$an.inf1.te<0, -1, 0)
anchor.cheater$an.inf1.postD <- ifelse((anchor.cheater$an.inf1.te>0), 1, 0)
anchor.cheater$an.inf1.preLin <- ifelse(anchor.cheater$an.inf1.te<0, (anchor.cheater$an.inf1.te), 0)
anchor.cheater$an.inf1.postLin <- ifelse(anchor.cheater$an.inf1.te>0, (anchor.cheater$an.inf1.te), 0)

names(anchor.cheater)

myvar=subset(anchor.cheater, select=c(age, an.inf1.postLin, an.inf1.preLin))
corr.test(myvar)

##merge with controls
controls=subset(a.data.merge.matched, a.data.merge.matched$event.group=='control')
controls$an.inf1.preva=0
controls$an.inf1ev.wav=0
controls$an.inf1.te=0
controls$an.inf1.preD=0
controls$an.inf1.postD=0
controls$an.inf1.preLin=0
controls$an.inf1.postLin=0

names(anchor.cheater)
names(controls)


anchor.final=rbind(anchor.cheater, controls)


table(anchor.final$an.use, exclude = T)

###age at entry
anchor.final = anchor.final[order(anchor.final$id, anchor.final$wave), ]
anchor.final = anchor.final %>% group_by(subclass, id) %>% mutate(age.first = first(age))
table(anchor.final$age.first, exclude = T)

anchor.final$c.age=scale(anchor.final$age, scale=F)
anchor.final$sq.age=anchor.final$c.age^2

anchor.final$c.age.first=scale(anchor.final$age.first, scale=F)
anchor.final$sq.age.first=anchor.final$c.age.first^2

myvar=subset(anchor.final, select=c(age, an.inf1.postLin, an.inf1.preLin))
corr.test(myvar)


#use only outcomes for flagged relationships
names(anchor.final)
obs_use <- anchor.final[which(anchor.final$an.use ==1), 
                c("subclass", "id", "wave", "an.lsa", "an.sel", "an.rsa", "an.rin", "an.rad", "an.rdo", "an.rcf", "married", "livetogether",
                  "pr.lsa", "pr.sel", "pr.rsa", "pr.rin", "pr.rad", "pr.rdo", "pr.rcf")]
names(obs_use)[4:19] <- paste0(names(obs_use)[4:19], ".use")
anchor.final <- merge(anchor.final, obs_use, by=c("subclass", "id", "wave"), all.x=T)


###moderator values at the beginning of the flagged relationship 
anchor.final=anchor.final[order(anchor.final$subclass, anchor.final$id, anchor.final$wave),]
anchor.final = anchor.final %>% group_by(id, an.use) %>% mutate(hostile.first.use=first(hostile[which(!is.na(hostile))[1]]),
                                                                future.first.use=first(future[which(!is.na(future))[1]]),
                                                                sacrifice.first.use=first(sacrifice[which(!is.na(sacrifice))[1]]),
                                                                tolerance.first.use=first(tolerance[which(!is.na(tolerance))[1]]),
                                                                reciprocity.first.use=first(reciprocity[which(!is.na(reciprocity))[1]]),
                                                                aggression.first.use=first(aggression[which(!is.na(aggression))[1]]),
                                                                constructive.first.use=first(constructive[which(!is.na(constructive))[1]]),
                                                                withdrawal.first.use=first(withdrawal[which(!is.na(withdrawal))[1]]),
                                                                manipulation.first.use=first(manipulation[which(!is.na(manipulation))[1]]),
                                                                cheat.problem.first.use=first(cheat.problem[which(!is.na(cheat.problem))[1]]),
                                                                p.future.first.use=first(p.future[which(!is.na(p.future))[1]]),
                                                                p.tolerance.first.use=first(p.tolerance[which(!is.na(p.tolerance))[1]]))
anchor.final$hostile.first.use=ifelse(anchor.final$an.use==1, anchor.final$hostile.first.use, NA)
anchor.final$future.first.use=ifelse(anchor.final$an.use==1, anchor.final$future.first.use, NA)
anchor.final$sacrifice.first.use=ifelse(anchor.final$an.use==1, anchor.final$sacrifice.first.use, NA)
anchor.final$tolerance.first.use=ifelse(anchor.final$an.use==1, anchor.final$tolerance.first.use, NA)
anchor.final$reciprocity.first.use=ifelse(anchor.final$an.use==1, anchor.final$reciprocity.first.use, NA)
anchor.final$aggression.first.use=ifelse(anchor.final$an.use==1, anchor.final$aggression.first.use, NA)
anchor.final$constructive.first.use=ifelse(anchor.final$an.use==1, anchor.final$constructive.first.use, NA)
anchor.final$withdrawal.first.use=ifelse(anchor.final$an.use==1, anchor.final$withdrawal.first.use, NA)
anchor.final$manipulation.first.use=ifelse(anchor.final$an.use==1, anchor.final$manipulation.first.use, NA)
anchor.final$cheat.problem.first.use=ifelse(anchor.final$an.use==1, anchor.final$cheat.problem.first.use, NA)
anchor.final$p.future.first.use=ifelse(anchor.final$an.use==1, anchor.final$p.future.first.use, NA)
anchor.final$p.tolerance.first.use=ifelse(anchor.final$an.use==1, anchor.final$p.tolerance.first.use, NA)



###assigns the values at the beginning of the flagged relationship to all waves of the same participant (could be useful for personal well-being analyses)
anchor.final=anchor.final[order(anchor.final$id, anchor.final$wave),]
anchor.final = anchor.final %>% group_by(id) %>% mutate(hostile.first=mean(hostile.first.use, na.rm=T),
                                                        future.first=mean(future.first.use, na.rm=T),
                                                        sacrifice.first=mean(sacrifice.first.use, na.rm=T),
                                                        tolerance.first=mean(tolerance.first.use, na.rm=T),
                                                        reciprocity.first=mean(reciprocity.first.use, na.rm=T),
                                                        aggression.first=mean(aggression.first.use, na.rm=T),
                                                        constructive.first=mean(constructive.first.use, na.rm=T),
                                                        withdrawal.first=mean(withdrawal.first.use, na.rm=T),
                                                        manipulation.first=mean(manipulation.first.use, na.rm=T),
                                                        cheat.problem.first=mean(cheat.problem.first.use, na.rm=T),
                                                        p.future.first=mean(p.future.first.use, na.rm=T),
                                                        p.tolerance.first=mean(p.tolerance.first.use, na.rm=T))
##to check
var=subset(anchor.final, select=c(subclass, id, pid, wave, event.group, an.use, an.rel, an.inf, hostile, hostile.first.use, hostile.first))
var=var[order(var$subclass, var$id, var$wave),]

####testing effects
anchor.final = anchor.final %>% group_by(subclass, id) %>% mutate(testing=n())
table(anchor.final$testing, exclude=T)

###standardize variables
vars_standardize <- anchor.final[,c("an.rsa", "an.lsa", "an.sel", "an.rin", "an.rad", "an.rdo", "an.rcf", 
                            "an.rsa.use", "an.lsa.use", "an.sel.use", "an.rin.use", "an.rad.use", "an.rdo.use", "an.rcf.use",
                            "pr.rsa", "pr.lsa", "pr.sel", "pr.rin", "pr.rad", "pr.rdo", "pr.rcf", 
                            "pr.rsa.use", "pr.lsa.use", "pr.sel.use", "pr.rin.use", "pr.rad.use", "pr.rdo.use", "pr.rcf.use")]
names(vars_standardize) <- paste0( "z.", names(vars_standardize))
vars_standardize[,1:28] <- scale(vars_standardize[,1:28])

anchor.final=cbind(anchor.final, vars_standardize)

save(anchor.final, file="anchor.final.RData")

write_sav(anchor.final, "anchor.final.sav")


###compute number of cheating waves per participant for the target relationship 
anchor.final=anchor.final %>% group_by(id, pid) %>% mutate(an.inf.ev1.mean=sum(an.inf.ev1, na.rm=T))
anchor.final=anchor.final %>% group_by(id, pid, an.inf.ev1.mean) %>% mutate(cheat.sum=sum(an.inf, na.rm=T))
anchor.final=anchor.final %>% group_by(id) %>% mutate(cheat.sum.max=max(cheat.sum, na.rm=T))
table(anchor.final$event.group, anchor.final$cheat.sum.max)
dd=distinct(anchor.final, id, .keep_all=T)
table(dd$event.group, dd$cheat.sum.max)

var=subset(anchor.final, select=c(cheat.sum.max, an.inf1.postLin, an.inf1.preLin, wave))
corr.test(var)

##to check
var=subset(anchor.final, select=c(subclass, id, pid, wave, event.group, an.inf.ev1, an.inf.ev1.mean, an.rel, an.inf, cheat.sum, cheat.sum.max))
var=var[order(var$subclass, var$id, var$wave),]

###separation
####compute an indicator of whether they separated (hpsep not measured in wave 1 and 2)
table(anchor.final$hpsep, exclude=T)
anchor.final=anchor.final[order(anchor.final$subclass, anchor.final$id, anchor.final$wave),]
anchor.final=anchor.final %>% group_by(subclass, id) %>% mutate(hpsep.lead=lead(hpsep))
anchor.final$sep=ifelse(anchor.final$an.use==1, anchor.final$hpsep.lead, NA)
table(anchor.final$sep, exclude=T)
###mark last wave for each person
anchor.final=anchor.final[order(anchor.final$subclass, anchor.final$id, anchor.final$wave),]
anchor.final=anchor.final %>% group_by(subclass, id) %>% mutate(row.n=row_number(), n=n())
anchor.final$last.wave=ifelse(anchor.final$row.n==anchor.final$n, 1, 0)
###replace NA with 0 for last waves
anchor.final$sep.new=ifelse(anchor.final$last.wave==1 & anchor.final$an.use==1 & is.na(anchor.final$sep), 0, anchor.final$sep)

anchor.final=anchor.final %>% group_by(subclass, id) %>% mutate(sep.max=max(sep.new, na.rm=T))
table(anchor.final$sep.max, exclude=T)
anchor.final$sep.max=ifelse(anchor.final$sep.max==-Inf, NA, anchor.final$sep.max)
dd=distinct(anchor.final, id, .keep_all = T)
table(dd$event.group, dd$sep.max, exclude = T)

table(anchor.final$wave, anchor.final$hpsep, exclude = T)
table(anchor.final$wave, anchor.final$hpsep.lead, exclude = T)

#generates the wave nr where separation occurred 
anchor.final$sep.wave<- ifelse(anchor.final$sep==0, 0, anchor.final$wave*anchor.final$sep.new+1)
anchor.final=anchor.final %>% group_by(subclass, id)  %>% mutate(sep.wave.max=max(sep.wave, na.rm=T))
anchor.final$sep.after=ifelse(anchor.final$wave >= anchor.final$sep.wave.max & anchor.final$sep.max==1, 1, 0)

anchor.final$sep.cat=ifelse(anchor.final$an.rel==0 & anchor.final$sep.after==1, '1',
                            ifelse(anchor.final$an.rel==1 & anchor.final$sep.after==1, '2', '0'))
#0=before, 1=separated and single, 2=new relationship

##to check
#var=subset(anchor.final, select=c(subclass, id, pid, wave, an.rel, an.use, hpsep, hpsep.lead, hpsep.lead.last))
var=subset(anchor.final, select=c(subclass, id, pid, wave, an.rel, an.use, hpsep, hpsep.lead, sep, sep.new, sep.wave,
                                  sep.wave.max, sep.after, sep.max, row.n, n, last.wave, event.group))
var=var[order(var$subclass, var$id, var$wave),]

table(anchor.final$sep.max, exclude = T)
table(anchor.final$sep.after, exclude = T)

#% of persons who separated and stayed single vs. repartnered
d=anchor.final %>% group_by(subclass, id) %>% summarise(sep.cat.max=max(sep.cat, na.rm=T),
                                                        event.group=first(event.group))
table(d$sep.cat.max, exclude = T)
table(d$event.group, d$sep.cat.max, exclude = T)

anchor.final=anchor.final %>% group_by(subclass, id) %>% mutate(sep.cat.max=max(sep.cat, na.rm=T))
table(anchor.final$sep.cat.max, exclude = T)

anchor.final$nosep=ifelse(anchor.final$sep.cat.max==0, 1, 0)
anchor.final$single=ifelse(anchor.final$sep.cat.max==1, 1, 0)
anchor.final$repartnered=ifelse(anchor.final$sep.cat.max==2, 1, 0)


##some checking (some skipped wave 2 and have hpsep.lead of wave 3)
anchor.final$d=ifelse(anchor.final$wave==1 & anchor.final$hpsep.lead==1, 1, 0)
var=subset(anchor.final, select=c(id, pid, wave, hpsep, hpsep.lead, sep, sep.max, an.use, event.group, d))
table(anchor.final$testing, anchor.final$sep.max, exclude = T)
##to check
var=subset(anchor.final, select=c(subclass, id, pid, wave, event.group, an.rel, an.use, an.inf,
                                  hpsep, hpsep.lead, sep, sep.max))
var=var[order(var$subclass, var$id, var$wave),]

names(anchor.final)

####commitment
myvar=subset(anchor.final, select=c(future.first.use, tolerance.first.use))
corr.test(myvar)

anchor.final$commitment.first.use=rowMeans(anchor.final[, c("future.first.use", "tolerance.first.use")], na.rm=T)

table(anchor.final$commitment.first.use, exclude = T)

####commitment partner
myvar=subset(anchor.final, select=c(p.future.first.use, p.tolerance.first.use))
corr.test(myvar)

anchor.final$p.commitment.first.use=rowMeans(anchor.final[, c("p.future.first.use", "p.tolerance.first.use")], na.rm=T)

table(anchor.final$p.commitment.first.use, exclude = T)

anchor.final$male=as.factor(anchor.final$male)
anchor.final$an.inf1.postD=as.factor(anchor.final$an.inf1.postD)
anchor.final$homosex=as.factor(anchor.final$homosex)
anchor.final$married=as.factor(anchor.final$married)
anchor.final$livetogether=as.factor(anchor.final$livetogether)
anchor.final$an.rel=as.factor(anchor.final$an.rel)
anchor.final$sep.after=as.factor(anchor.final$sep.after)

summary(anchor.final$row.n)
anchor.final$row.n=anchor.final$row.n-1

save(anchor.final, file="anchor.final.RData")

write_sav(anchor.final, "anchor.final.sav")



