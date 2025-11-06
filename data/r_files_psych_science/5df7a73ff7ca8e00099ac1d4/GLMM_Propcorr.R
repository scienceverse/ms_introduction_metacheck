
###########

# In order to run GLMM on proportion of correct responses check
# Moscatelli 2012 paper. Merpackage is needed to be installed

###########

# start from reading the logfiles
setwd("") # set the wd to where the logfiles are
data <- NA

dataNames = "*.txt"
temp = list.files(pattern=dataNames)
numSUBJ <- length(temp)     

SUBJ = list()
for (i in 1:numSUBJ) {
  x  = (read.csv(temp[i], skip = 1, sep = "", header=FALSE))
  x$id <- i
  SUBJ[i] = list(x)
}

# bind txt files using rbind comment
data = do.call(rbind, SUBJ)

# assign names to the columns
names(data) <- c("stim", "pos", "loopnum", 
                 "trial", "arm", "plane", 
                 "speaker", "response", "side", 
                 "corrside", "correct", "RT", "subj")

# adding the labels of groups: EB/SC
# be careful the order you read the all logfiles, otherwise you might mess up
#print "temp" for checking data order
data$group <- ifelse(data$subj<=17, c('CB'), c('SC'))

# get rid of stim 2 (pink noise) or stim 1 (voice)
data <- subset(data, stim==2)


# divide horizontal and vertical 
data.horizontal <- subset(data, data$plane==1)
data.vertical <- subset(data, data$plane==2)

# filter out plane and stim column  & loopnum
data.horizontal[,1] <- NULL
data.vertical[,1] <- NULL
# loopnum
data[,3] <- NULL
data[,3] <- NULL
data.horizontal[,2] <- NULL
data.vertical[,2] <- NULL


#HORIZONTAL

# filter out front/back response
data.hor.front <- subset(data.horizontal, pos==1)
data.hor.back <- subset(data.horizontal, pos==2)

# response buttons 5 == left, 6 == right
# change response => horizontal:6=1; 5=0; FOR FRONT 
data.hor.front <- within(data.hor.front, response[response==6] <- 1)
data.hor.front <- within(data.hor.front, response[response==5] <- 0)

# change response => horizontal:6=1; 5=0; FOR BACK
data.hor.back <- within(data.hor.back, response[response==5] <- 0)
data.hor.back <- within(data.hor.back, response[response==6] <- 1)
# reverse correctness
data.hor.back <- within(data.hor.back, correct[correct==1] <- -1) #just to assign into a temp
data.hor.back <- within(data.hor.back, correct[correct==0] <- 1)
data.hor.back <- within(data.hor.back, correct[correct==-1] <- 0)

# let's also correct the corrside in the BACK
data.hor.back <- within(data.hor.back, corrside[corrside==5] <- -5) #just to assign into a temp
data.hor.back <- within(data.hor.back, corrside[corrside==6] <- 5)
data.hor.back <- within(data.hor.back, corrside[corrside==-5] <- 6)


# reversing "right" arm for error in coding
data.hor.front<- within(data.hor.front,side[arm=="right"]<- side[arm=="right"]*-1)

# correction of the script mistake
data.hor.back <- within(data.hor.back,side[arm=="left"]<- side[arm=="left"]*-1)
data.hor.back <- within(data.hor.back,side[arm=="ctrh"]<- side[arm=="ctrh"]*-1)


# in the horizontal back condition left arm is right area // right arm is left area of the subject
data.hor.back$arm <-as.character(data.hor.back$arm)
data.hor.back <- within(data.hor.back, arm[arm=="left"] <-("Alien"))#just to assign into a temp
data.hor.back <- within(data.hor.back, arm[arm=='right'] <- 'left')
data.hor.back <- within(data.hor.back, arm[arm=='Alien'] <- 'right')
#####

# recombine horizontal data
data.horizontal <- rbind(data.hor.front, data.hor.back)
# assign the number for the arms - stupid ifelse loop
data.horizontal$numcodedarm <-ifelse(data.horizontal$arm=='left', 1, ifelse(data.horizontal$arm=='ctrh',2, ifelse(data.horizontal$arm=='right',3,9)))


# filter out NaN response
data.horizontal <- subset(data.horizontal, response==1|response==0)

# VERTICAL
#response buttons 8 == down, 5 == up
# change response => vertical:8=1; 5=0
data.vertical<- within(data.vertical, response[response==8] <- 1)
data.vertical <- within(data.vertical, response[response==5] <- 0)

# filter out NaN response
data.vertical <- subset(data.vertical, response==1|response==0)

# filter out front/back response
data.ver.front <- subset(data.vertical, pos==1)
data.ver.back <- subset(data.vertical, pos==2)

#reversing "up" and "ctrv" part (for error in the code), front and back, stim 1 e 2
data.ver.front <- within(data.ver.front,side[arm=="up"]<- side[arm=="up"]*-1)
data.ver.back <- within(data.ver.back,side[arm=="up"]<- side[arm=="up"]*-1)

data.ver.front <- within(data.ver.front,side[arm=="ctrv"]<- side[arm=="ctrv"]*-1)
data.ver.back <- within(data.ver.back,side[arm=="ctrv"]<- side[arm=="ctrv"]*-1)


# COMBINE DATA
data.vertical <- rbind(data.ver.front, data.ver.back)
data.vertical$numcodedarm <-ifelse(data.vertical$arm=='up', 4, ifelse(data.vertical$arm=='ctrv',5, ifelse(data.vertical$arm=='down',6,9)))

# recombine for plane data
data.planes <- rbind(data.horizontal, data.vertical)
data.planes$axis <- ifelse(data.planes$numcodedarm<=3, c('hor'), c('ver'))


################################
# PROPORTION OF RIGHT RESPONSES 
# calculate the proportion of responses by aggregating the 3 main data across the subjects, groups, ...
#vertical

# fit4_lmm.vertical <-aggregate(data.vertical$response, list(data.vertical$pos, data.vertical$numcodedarm, data.vertical$side, data.vertical$subj, data.vertical$group), mean)
# 
# # either needs to use dplyr function or add another aggregate to calculate tot number of trials per condition
# trialnum <-aggregate(data.vertical$response, list(data.vertical$pos, data.vertical$numcodedarm, data.vertical$side, data.vertical$subj, data.vertical$group), length)
# names(trialnum) <- c("headpos", "arm", "speaker", "subj","group", "trials")
# 
# rightnum <-aggregate(data.vertical$response, list(data.vertical$pos, data.vertical$numcodedarm, data.vertical$side, data.vertical$subj, data.vertical$group), sum)
# names(rightnum) <- c("headpos", "arm", "speaker", "subj","group", "rightres")
# 
# fit4_lmm.vertical <- cbind(fit4_lmm.vertical,trialnum$trials, rightnum$rightres)
# names(fit4_lmm.vertical) <- c("headpos", "arm", "speaker", "subj","group", "propright", "trials","rightrespond")

# horizontal
# fit4_lmm.horizontal <-aggregate(data.horizontal$response, list(data.horizontal$pos, data.horizontal$numcodedarm, data.horizontal$side, data.horizontal$subj, data.horizontal$group), mean)
# # either needs to use dplyr function or add another aggregate to calculate tot number of trials per condition
# trialnum <-aggregate(data.horizontal$response, list(data.horizontal$pos, data.horizontal$numcodedarm, data.horizontal$side, data.horizontal$subj, data.horizontal$group), length)
# names(trialnum) <- c("headpos", "arm", "speaker", "subj","group", "trials")
# 
# rightnum <-aggregate(data.horizontal$response, list(data.horizontal$pos, data.horizontal$numcodedarm, data.horizontal$side, data.horizontal$subj, data.horizontal$group), sum)
# names(rightnum) <- c("headpos", "arm", "speaker", "subj","group", "rightres")
# 
# fit4_lmm.horizontal <- cbind(fit4_lmm.horizontal,trialnum$trials, rightnum$rightres)
# names(fit4_lmm.horizontal) <- c("headpos", "arm", "speaker", "subj","group", "propright", "trials","rightrespond")

#plane
# fit4_lmm.planes <-aggregate(data.planes$response, list(data.planes$pos, data.planes$axis, data.planes$side, data.planes$subj, data.planes$group), mean)
# trialnum <-aggregate(data.planes$response, list(data.planes$pos, data.planes$axis, data.planes$side, data.planes$subj, data.planes$group), length)
# names(trialnum) <- c("headpos", "axis", "speaker", "subj","group", "trials")
# 
# rightnum <-aggregate(data.planes$response, list(data.planes$pos, data.planes$axis, data.planes$side, data.planes$subj, data.planes$group), sum)
# names(rightnum) <- c("headpos", "axis", "speaker", "subj","group", "rightres")
# 
# fit4_lmm.planes <- cbind(fit4_lmm.planes,trialnum$trials, rightnum$rightres)
# names(fit4_lmm.planes) <- c("headpos", "axis", "speaker", "subj","group", "propright", "trials","rightrespond")



################################
# PROPORTION OF CORRECT RESPONSES 
fit4_lmm.vertical <-aggregate(data.vertical$correct, list(data.vertical$pos, data.vertical$numcodedarm, data.vertical$side, data.vertical$subj, data.vertical$group), mean)

# either needs to use dplyr function or add another aggregate to calculate tot number of trials per condition
trialnum <-aggregate(data.vertical$correct, list(data.vertical$pos, data.vertical$numcodedarm, data.vertical$side, data.vertical$subj, data.vertical$group), length)
names(trialnum) <- c("head", "arm", "speaker", "subj","group", "trials")

correctnum <-aggregate(data.vertical$correct, list(data.vertical$pos, data.vertical$numcodedarm, data.vertical$side, data.vertical$subj, data.vertical$group), sum)
names(correctnum) <- c("head", "arm", "speaker", "subj","group", "correctres")

fit4_lmm.vertical <- cbind(fit4_lmm.vertical,trialnum$trials, correctnum$correctres)
names(fit4_lmm.vertical) <- c("head", "arm", "speaker", "subj","group", "prop.corr", "trials","corr.respond")



# convert conditions to factors
str(fit4_lmm.vertical)
fit4_lmm.vertical$arm <- factor(fit4_lmm.vertical$arm)
fit4_lmm.vertical$subj <- factor(fit4_lmm.vertical$subj)
fit4_lmm.vertical$head <- factor(fit4_lmm.vertical$head)
fit4_lmm.vertical$group <- factor(fit4_lmm.vertical$group)
#names(fit4_lmm.vertical)[1]<-c('head')


# horizontal
fit4_lmm.horizontal <-aggregate(data.horizontal$correct, list(data.horizontal$pos, data.horizontal$numcodedarm, data.horizontal$side, data.horizontal$subj, data.horizontal$group), mean)

trialnum <-aggregate(data.horizontal$correct, list(data.horizontal$pos, data.horizontal$numcodedarm, data.horizontal$side, data.horizontal$subj, data.horizontal$group), length)
names(trialnum) <- c("head", "arm", "speaker", "subj","group", "trials")

correctnum <-aggregate(data.horizontal$correct, list(data.horizontal$pos, data.horizontal$numcodedarm, data.horizontal$side, data.horizontal$subj, data.horizontal$group), sum)
names(correctnum) <- c("head", "arm", "speaker", "subj","group", "correctres")

fit4_lmm.horizontal <- cbind(fit4_lmm.horizontal,trialnum$trials, correctnum$correctres)
names(fit4_lmm.horizontal) <- c("head", "arm", "speaker", "subj","group", "prop.corr", "trials","corr.respond")


# convert conditions to factors
str(fit4_lmm.horizontal)
fit4_lmm.horizontal$arm <- factor(fit4_lmm.horizontal$arm)
fit4_lmm.horizontal$speaker <- factor(fit4_lmm.horizontal$speaker)
fit4_lmm.horizontal$subj <- factor(fit4_lmm.horizontal$subj)
fit4_lmm.horizontal$head <- factor(fit4_lmm.horizontal$head)
fit4_lmm.horizontal$group <- factor(fit4_lmm.horizontal$group)
#names(fit4_lmm.horizontal)[1]<-c('head')

#plane
fit4_lmm.planes <-aggregate(data.planes$correct, list(data.planes$pos, data.planes$axis, data.planes$side, data.planes$subj, data.planes$group), mean)
trialnum <-aggregate(data.planes$correct, list(data.planes$pos, data.planes$axis, data.planes$side, data.planes$subj, data.planes$group), length)
names(trialnum) <- c("head", "planes", "speaker", "subj","group", "trials")

correctnum <-aggregate(data.planes$correct, list(data.planes$pos, data.planes$axis, data.planes$side, data.planes$subj, data.planes$group), sum)
names(correctnum) <- c("head", "planes", "speaker", "subj","group", "correctres")

fit4_lmm.planes <- cbind(fit4_lmm.planes,trialnum$trials, correctnum$correctres)
names(fit4_lmm.planes) <- c("head", "planes", "speaker", "subj","group", "prop.corr", "trials","corr.respond")


# convert conditions to factors
str(fit4_lmm.planes)
fit4_lmm.planes$planes <- factor(fit4_lmm.planes$planes)
fit4_lmm.planes$speaker <- factor(fit4_lmm.planes$speaker)
fit4_lmm.planes$subj <- factor(fit4_lmm.planes$subj)
fit4_lmm.planes$head <- factor(fit4_lmm.planes$head)
fit4_lmm.planes$group <- factor(fit4_lmm.planes$group)
#names(fit4_lmm.planes)[1]<-c('head')
#names(fit4_lmm.planes)[2]<-c('planes')


##### 
# # save these 3 datasets - right responses
# write.table(fit4_lmm.vertical, "verticalproportion_Data.txt", sep=",", row.names=F, col.names=T)
# write.table(fit4_lmm.horizontal, "horizontalproportion_Data.txt", sep=",", row.names=F, col.names=T)
# write.table(fit4_lmm.planes, "planeproportion_Data.txt", sep=",", row.names=F, col.names=T)

# save these 3 datasets - correct responses
write.table(fit4_lmm.vertical, "verticalCORRproportion_Data.txt", sep=",", row.names=F, col.names=T)
write.table(fit4_lmm.horizontal, "horizontalCORRproportion_Data.txt", sep=",", row.names=F, col.names=T)
write.table(fit4_lmm.planes, "planeCORRproportion_Data.txt", sep=",", row.names=F, col.names=T)
#########################################################
## LMM FITS 
# library(MixedPsy)

# Let's take 1 dataset, e.g. fit4_lmm.vertical to try with GLMM
setwd("")
fit4_lmm.vertical <- read.table("verticalCORRproportion_Data.txt", sep=",", header=T)
fit4_lmm.horizontal <- read.table("horizontalCORRproportion_Data.txt", sep=",", header=T)
fit4_lmm.planes <- read.table("planeCORRproportion_Data.txt", sep=",", header=T)

setwd('/Users/Documents/Rpackages/Merpsychophysics.1')
source("MERpsychophysics.r")

setwd("/Users/Downloads/Federica/Proportion_Data/")

library(lme4)
library(Matrix)
library(lattice)
library(mnormt)
library(MASS)

str(fit4_lmm.vertical)
fit4_lmm.vertical$arm <- factor(fit4_lmm.vertical$arm)
fit4_lmm.vertical$speaker <- factor(fit4_lmm.vertical$speaker)
fit4_lmm.vertical$subj <- factor(fit4_lmm.vertical$subj)
fit4_lmm.vertical$head <- factor(fit4_lmm.vertical$head)
fit4_lmm.vertical$group <- factor(fit4_lmm.vertical$group)
#names(fit4_lmm.vertical)[1]<-c('head')

str(fit4_lmm.horizontal)
fit4_lmm.horizontal$arm <- factor(fit4_lmm.horizontal$arm)
fit4_lmm.horizontal$speaker <- factor(fit4_lmm.horizontal$speaker)
fit4_lmm.horizontal$subj <- factor(fit4_lmm.horizontal$subj)
fit4_lmm.horizontal$head <- factor(fit4_lmm.horizontal$head)
fit4_lmm.horizontal$group <- factor(fit4_lmm.horizontal$group)
#names(fit4_lmm.horizontal)[1]<-c('head')

str(fit4_lmm.planes)
fit4_lmm.planes$planes <- factor(fit4_lmm.planes$planes)
fit4_lmm.planes$speaker <- factor(fit4_lmm.planes$speaker)
fit4_lmm.planes$subj <- factor(fit4_lmm.planes$subj)
fit4_lmm.planes$head <- factor(fit4_lmm.planes$head)
fit4_lmm.planes$group <- factor(fit4_lmm.planes$group)
#names(fit4_lmm.planes)[1]<-c('head')
#names(fit4_lmm.planes)[2]<-c('planes')

#correctnum$correctres
# glmm on horizontal responses

mod0hor <- glmer(formula = cbind(corr.respond, trials - corr.respond) ~ speaker + arm * group * head + (1 | subj), family = binomial(link = "probit"), data = fit4_lmm.horizontal)
#second time
#mod0hor <- glmer(formula = cbind(corr.respond, trials - corr.respond) ~ speaker + arm * group * head + (1 | subj), family = binomial(link = "probit"), data = fit4_lmm.horizontal, start = list(theta = getME(mod0hor,"theta"), fixef = getME(mod0hor,"beta")))
##third time could be charm
#mod0hor <- glmer(formula = cbind(corr.respond, trials - corr.respond) ~ speaker + arm * group * head + (1 | subj), family = binomial(link = "probit"), data = fit4_lmm.horizontal, start = list(theta = getME(mod0hor,"theta"), fixef = getME(mod0hor,"beta")))

# likelihood ratio test
anova(mod0hor, mod0hor1)

# glmm on vertical responses
mod0ver <- glmer(formula = cbind(corr.respond, trials - corr.respond) ~ speaker + arm * group * head + (1 | subj), family = binomial(link = "probit"), data = fit4_lmm.vertical)
mod0ver <- glmer(formula = cbind(corr.respond, trials - corr.respond) ~ speaker + arm * group * head + (1 | subj), family = binomial(link = "probit"), data = fit4_lmm.vertical, start = list(theta = getME(mod0ver,"theta"), fixef = getME(mod0ver,"beta")))

summary(mod0ver)


# glmm on planes - correct respnses
mod0plane <-  glmer(formula = cbind(corr.respond, trials - corr.respond) ~ speaker + planes * group * head + (1 | subj), family = binomial(link = "probit"), data = fit4_lmm.planes)
#mod0plane <- glmer(formula = cbind(corr.respond, trials - corr.respond) ~ speaker + planes * group * head + (1 | subj), family = binomial(link = "probit"), data = fit4_lmm.planes, start = list(theta = getME(mod0plane,"theta"), fixef = getME(mod0plane,"beta")))


  
  
library(car)
hor.anova<- Anova(mod0hor)
ver.anova<-Anova(mod0ver)
plane.anova<-Anova(mod0plane)

plot(mod0hor) # seems to be plotting across 5 speakers

qqnorm(ranef(mod0hor)$sub[,1])
qqline(ranef(mod0hor)$sub[,1])

qqnorm(ranef(mod0ver)$sub[,1])
qqline(ranef(mod0ver)$sub[,1])

qqnorm(ranef(mod0plane)$sub[,1])
qqline(ranef(mod0plane)$sub[,1])

#save the GLMM statistical inference output
write.table(hor.anova, file="Horizontal_GLMM_response.txt", sep=",", row.names=T)
write.table(ver.anova, file="Vertical_GLMM_response.txt", sep=",", row.names=T)
write.table(plane.anova, file="Plane_GLMM_response.txt", sep=",", row.names=T)



####################################
# if you just join the same please first read the anova tables
hor.anova <- read.table("Horizontal_GLMM_response.txt", sep=",", header=T)
ver.anova <- read.table("Vertical_GLMM_response.txt", sep=",", header=T)
plane.anova <- read.table("Plane_GLMM_response.txt", sep=",", header=T)

# multiple comparisons
library(emmeans)
########################
# HORIZONTAL
########################

# 1. arm
emm.hor <- emmeans(mod0hor, specs = pairwise ~ arm,  adjust = "none")
emm.hor <- emmeans(mod0hor, list(pairwise ~ arm),  adjust = "none")

pair.hor.table <-as.data.frame(emm.hor$`pairwise differences of arm`)
pair.hor.table

#extract p values to adjust for the contrasts of interests
pvalues <- round(pair.hor.table$p.value,digits = 4)
p.bonfer<- p.adjust(pvalues,method="bonferroni")
p.FDR<- p.adjust(pvalues,method="fdr")

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')

pair.hor.table <- cbind(pair.hor.table,p.bonfer,p.FDR)


#2. arm x group
emm.hor <- emmeans(mod0hor, list(pairwise ~ arm * group), adjust="none")
emmip(mod0hor, group ~ arm)

pair.hor.table <-as.data.frame(emm.hor$`pairwise differences of arm`)
pair.hor.table

pair.hor.table$p.value<-round(pair.hor.table$p.value, digits = 4)
#choose only contrast of interests which are 
COI <- c(1, 2, 3, 6, 8, 12, 13, 14, 15)
miss_val<-length(1:nrow(pair.hor.table)) - length(COI)
pair.hor.table$con_of_Int <- ifelse(seq_len(nrow(pair.hor.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- pair.hor.table$p.value[COI]
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)

#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)

temp<-temp[order(temp$idx),]
#temp$con<-pair.ver.table$con_of_Int #this is a check point
temp$idx <-NULL
temp

pair.hor.table <- cbind(pair.hor.table,temp)


# #3. arm x group x head
# # trending...
# emmeans(mod0hor, list(pairwise ~ arm * group * head), adjust="none")
# emmip(mod0hor, group ~ arm | head)
# emmip(mod0hor, group ~ head | arm)
# emmip(mod0hor, arm ~ head | group)

# 4. arm x head is trending
emmip(mod0hor,arm ~ head)
emmeans(mod0hor, list(pairwise ~ arm * head), adjust="none")

########################
# VERTICAL
########################
# 1. ARM
emm.ver <- emmeans(mod0ver, list(pairwise ~ arm),  adjust = "none")

pair.ver.table <-as.data.frame(emm.ver$`pairwise differences of arm`)
pair.ver.table

#extract p values to adjust for the contrasts of interests
pvalues <- round(pair.ver.table$p.value,digits = 4)
p.bonfer<- p.adjust(pvalues,method="bonferroni")
p.FDR<- p.adjust(pvalues,method="fdr")

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')

pair.ver.table <- cbind(pair.ver.table,p.bonfer,p.FDR)


# 2. ARM x GROUP
emm.ver <-emmeans(mod0ver, list(pairwise ~ arm * group), adjust="none")
pair.ver.table <-as.data.frame(emm.ver$`pairwise differences of arm`)
pair.ver.table

pair.ver.table$p.value<-round(pair.ver.table$p.value, digits = 4)
#choose only contrast of interests which are 
COI <- c(1, 2, 3, 6, 8, 12, 13, 14, 15)
miss_val<-length(1:nrow(pair.ver.table)) - length(COI)
pair.ver.table$con_of_Int <- ifelse(seq_len(nrow(pair.ver.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- pair.ver.table$p.value[COI]
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)

#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)

temp<-temp[order(temp$idx),]
#temp$con<-pair.ver.table$con_of_Int #this is a check point
temp$idx <-NULL
temp

pair.ver.table <- cbind(pair.ver.table,temp)
pair.ver.table




# 3. ARM x HEAD
emm.ver<- emmeans(mod0ver, list(pairwise ~ arm * head), adjust="none")
pair.ver.table <-as.data.frame(emm.ver$`pairwise differences of arm, head`)
pair.ver.table
pair.ver.table$p.value<-round(pair.ver.table$p.value, digits = 4)
#choose only contrast of interests which are 
COI <- c(1, 2, 3, 6, 8, 12, 13, 14, 15)
miss_val<-length(1:nrow(pair.ver.table)) - length(COI)
pair.ver.table$con_of_Int <- ifelse(seq_len(nrow(pair.ver.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- pair.ver.table$p.value[COI]
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)

#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)

temp<-temp[order(temp$idx),]
#temp$con<-pair.ver.table$con_of_Int #this is a check point
temp$idx <-NULL
temp

pair.ver.table <- cbind(pair.ver.table,temp)
pair.ver.table

# 4. GROUP x HEAD ############
emm.ver<- emmeans(mod0ver, list(pairwise ~ group * head), adjust="none")
pair.ver.table <-as.data.frame(emm.ver$`pairwise differences of group, head`)
pair.ver.table

pair.ver.table$p.value<-round(pair.ver.table$p.value, digits = 4)
#choose only contrast of interests which are 
COI <- c(1, 2, 5, 6)
miss_val<-length(1:nrow(pair.ver.table)) - length(COI)
pair.ver.table$con_of_Int <- ifelse(seq_len(nrow(pair.ver.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- pair.ver.table$p.value[COI]
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)

#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)

temp<-temp[order(temp$idx),]
#temp$con<-pair.ver.table$con_of_Int #this is a check point
temp$idx <-NULL
temp

pair.ver.table <- cbind(pair.ver.table,temp)
pair.ver.table

########################
#PLANES 
########################
#1. planes x group
# to extract uncorrected p values
emm.plane <- emmeans(mod0plane, specs = pairwise ~ planes * group,  adjust = "none")
pair.plane.table <-as.data.frame(emm.plane$contrasts)
pair.plane.table

pair.plane.table$p.value<-round(pair.plane.table$p.value, digits = 4)
#choose only contrast of interests which are 
# 1 2 5 and 6
COI <- c(1, 2, 5, 6)
pair.plane.table$con_of_Int <- ifelse(seq_len(nrow(pair.plane.table)) %in% COI, 1, 0)

pvalues_interest <- round(pair.plane.table$p.value[COI],digits = 4)
# add 99s to have the same length - 99s are for marking non-of-interest pvalues
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),99,99)
p.FDR<- round(c(p.adjust(pvalues_interest,method="fdr"),99,99),digits = 4)

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)

#organise into right order
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp<-temp[c(COI,arr2),]

# bind them to make 1 chart
pair.plane.table <- cbind(pair.plane.table,temp)


#2. head x group
emm.plane<-emmeans(mod0plane, list(pairwise ~ head * group), adjust="none")
pair.plane.table <-as.data.frame(emm.plane$`pairwise differences of head, group`)
pair.plane.table

pvalues<-round(pair.plane.table$p.value, digits = 4)
#choose only contrast of interests which are 
COI <- c(1, 2, 5, 6)
pair.plane.table$con_of_Int <- ifelse(seq_len(nrow(pair.plane.table)) %in% COI, 1, 0)

pvalues_interest <- round(pvalues[COI],digits = 4)
# add 99s to have the same length - 99s are for marking non-of-interest pvalues
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),99,99)
p.FDR<- round(c(p.adjust(pvalues_interest,method="fdr"),99,99),digits = 4)

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)

#organise into right order
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp<-temp[c(COI,arr2),]

# bind them to make 1 chart
pair.plane.table <- cbind(pair.plane.table,temp)


#3. plane x group x head is trending ... 
emmeans(mod0plane, list(pairwise ~ planes * head * group), adjust="none")

