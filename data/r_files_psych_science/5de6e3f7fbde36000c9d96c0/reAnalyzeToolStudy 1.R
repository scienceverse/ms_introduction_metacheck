
setwd("C:/Users/jkwitt/Dropbox/Naming Tools Reanalysis")
require(reshape2)


##############################################.
####     Experiment 1                     ####
##############################################.


df <- read.csv("Data 4-8.csv")
length(unique(df$Subject))
df <- df[which(df$Subject > 1),]
df$isSame <- ifelse(df$HandCondition == 1 & df$Orientation == -1, 1,
                    ifelse(df$HandCondition == 1 & df$Orientation == 1, 0,
                           ifelse(df$HandCondition == 2 & df$Orientation == -1, 0,1)))

####  E1: OLD ANALYSIS ####
#Get means
dd <- aggregate(StimPic.RT ~ Subject + Stim + HandCondition + Orientation, 
                df[which(df$StimPic.RT < 4000 & df$acc == 1),], mean)
dd$isSame <- ifelse(dd$HandCondition == 1 & dd$Orientation == -1, 1,
                    ifelse(dd$HandCondition == 1 & dd$Orientation == 1, 0,
                           ifelse(dd$HandCondition == 2 & dd$Orientation == -1, 0,1)))
aggregate(StimPic.RT ~ Stim + isSame, dd, mean)

# Analyze it the old way - check!
options(contrasts=c("contr.sum","contr.poly"))
mns <- aggregate(StimPic.RT ~ Stim + isSame + Subject, dd, mean)
mns$Subject <- as.factor(mns$Subject)
mns$isSame <- as.factor(mns$isSame)
p1 <- aov(StimPic.RT ~ (Stim * isSame) + 
            Error(Subject / (Stim * isSame)), 
          data = mns)
summary(p1)
interaction.plot(mns$Stim, mns$isSame, mns$StimPic.RT, type="b", col=c("blue","red"))
aggregate(StimPic.RT ~ Stim + isSame, mns, mean)


####  E1: OLD ANALYSIS Diff RT Cut-off ####

#Remove outliers based on individual Ss and stim boxplots

df$RTout <- NA
for (i in unique(df$Subject)) {
  for (j in unique(df$Stim)) {
    a <- which(df$Subject == i & df$Stim == j)
    b <- boxplot(df$StimPic.RT[a], plot=F)
    if(length(which(b$out > median(df$StimPic.RT[a]))) > 0) {
      minUp <- min(b$out[which(b$out > median(df$StimPic.RT[a]))])
      df$RTout[which(df$Subject == i & df$Stim == j & df$StimPic.RT >= minUp)] <- 1
    }
    if(length(which(b$out < median(df$StimPic.RT[a]))) > 0) {
      minUp <- max(b$out[which(b$out < median(df$StimPic.RT[a]))])
      df$RTout[which(df$Subject == i & df$Stim == j & df$StimPic.RT <= minUp)] <- 1
    }
    
  }
}

if(1>2) { #Check outliers
boxplot(df$StimPic.RT ~ df$Subject + df$Stim)
a <- which(df$Stim == "Anima")
points(df$Subject[a], df$StimPic.RT[a], col=df$RTout[a]+1)
a <- which(df$Stim == "Tools")
points(df$Subject[a]+63, df$StimPic.RT[a], col=df$RTout[a]+1)
boxplot(df$StimPic.RT ~ df$Subject + df$Stim, ylim=c(0,4000))
a <- which(df$Stim == "Anima")
points(df$Subject[a]-1, df$StimPic.RT[a], col=df$RTout[a]+1, cex=.5)
a <- which(df$Stim == "Tools")
points(df$Subject[a]+62, df$StimPic.RT[a], col=df$RTout[a]+1,cex=.5)
} #end check outliers


dd <- aggregate(StimPic.RT ~ Subject + Stim + HandCondition + Orientation, 
                df[which(is.na(df$RTout) & df$acc == 1),], mean)
dd$isSame <- ifelse(dd$HandCondition == 1 & dd$Orientation == -1, 1,
                    ifelse(dd$HandCondition == 1 & dd$Orientation == 1, 0,
                           ifelse(dd$HandCondition == 2 & dd$Orientation == -1, 0,1)))
aggregate(StimPic.RT ~ Stim + isSame, dd, mean)

options(contrasts=c("contr.sum","contr.poly"))
mns <- aggregate(StimPic.RT ~ Stim + isSame + Subject, dd, mean)
mns$Subject <- as.factor(mns$Subject)
mns$isSame <- as.factor(mns$isSame)
p1 <- aov(StimPic.RT ~ (Stim * isSame) + 
            Error(Subject / (Stim * isSame)), 
          data = mns)
summary(p1)
interaction.plot(mns$Stim, mns$isSame, mns$StimPic.RT, type="b", col=c("blue","red"))
aggregate(StimPic.RT ~ Stim + isSame, mns, mean)


####  E1: LMER w Raw ####
dd <- df[which(is.na(df$RTout) & df$acc == 1),]
a <- which(dd$Stim == "Tools")
interaction.plot(dd$isSame[a],dd$Subject[a],  dd$StimPic.RT[a])
a <- which(dd$Stim == "Anima")
interaction.plot(dd$isSame[a],dd$Subject[a],  dd$StimPic.RT[a])
a <- which(dd$isSame == 0)
interaction.plot(dd$Stim[a],dd$Subject[a],  dd$StimPic.RT[a])
a <- which(dd$isSame == 1)
interaction.plot(dd$Stim[a],dd$Subject[a],  dd$StimPic.RT[a])
dd$cond <- paste0(dd$Stim,"_",dd$isSame)
interaction.plot(dd$cond, dd$Subject, dd$StimPic.RT)
interaction.plot(dd$Stim, dd$Subject, dd$StimPic.RT)
interaction.plot(dd$isSame, dd$Subject, dd$StimPic.RT)  #everyone seems pretty similar here so maybe exclude from random effects

dd <- df[which(is.na(df$RTout) & df$acc == 1),]
p1 <- lmer(StimPic.RT ~ Stim * isSame + 
             (1 + Stim * isSame | Subject) + 
             (1 + isSame | Pic.Trial.) +
             (1 | Subject:Pic.Trial.),
           dd, control = lmerControl(optimizer = "bobyqa"))
#Singular but look anyway
summary(p1)
coefs <- coef(p1)[[3]]
boxplot(coefs)
coefs$subj <- as.integer(rownames(coefs))

p1 <- lmer(StimPic.RT ~ Stim * isSame + 
             (1 + Stim | Subject) + 
             (1 | Pic.Trial.) +
             (1 | Subject:Pic.Trial.),
           dd, control = lmerControl(optimizer = "bobyqa"))
summary(p1)

p1 <- lmer(log(StimPic.RT) ~ Stim * isSame + 
             (1 + Stim | Subject) + 
             (1 | Pic.Trial.) +
             (1 | Subject:Pic.Trial.),
           dd, control = lmerControl(optimizer = "bobyqa"))
summary(p1)
interaction.plot(dd$Stim, dd$isSame, dd$StimPic.RT)

p1 <- lmer(log(StimPic.RT) ~ isSame + 
             (1 | Subject) + 
             (1 | Pic.Trial.) +
             (1 | Subject:Pic.Trial.),
           dd[which(dd$Stim == "Tools"),], 
           control = lmerControl(optimizer = "bobyqa"))
summary(p1)

a <- aggregate(StimPic.RT ~ Subject + Stim + isSame, dd, mean)
a <- dcast(a, Subject ~ Stim + isSame, value.var = "StimPic.RT")

require(BayesFactor)
ttestBF(a$Tools_0, a$Tools_1, paired = T, rscale = .3)


#All RTs (remove only inaccurate trials)
p1 <- lmer(log(StimPic.RT) ~ Stim * isSame + 
             (1 + Stim | Subject) + 
             (1 | Pic.Trial.) +
             (1 | Subject:Pic.Trial.),
           df[which(df$acc == 1),], control = lmerControl(optimizer = "bobyqa"))
summary(p1)
interaction.plot(dd$Stim, dd$isSame, dd$StimPic.RT)


#### E1: IES ####
mns <- aggregate(StimPic.RT ~ Stim + isSame + Subject, df[which(df$acc == 1),], mean)
mns2 <- aggregate(acc ~ Stim + isSame + Subject, df, mean)
mns <- merge(mns, mns2, by=c("Subject", "Stim","isSame"))
mns$ies <- mns$StimPic.RT / mns$acc

p1 <- lmer(ies ~ Stim * isSame + (1 + Stim | Subject), mns)
summary(p1)
interaction.plot(mns$Stim, mns$isSame, mns$ies)
interaction.plot(df$Stim, df$isSame, df$StimPic.RT)
a <- boxplot(mns$ies)
subjIESouts <- mns$Subject[which(mns$ies >= min(a$out))]
mns$out <- ifelse(mns$ies < min(a$out), 1, 0)
p1 <- lmer(ies ~ Stim * isSame + (1 + Stim | Subject), 
           mns[which(mns$out == 1),])
summary(p1)

#### E1: RTs with Ps who scored 80% or better ####
subjACCouts <- mns$Subject[which(mns$acc < .82)]

dd <- df[which(df$acc == 1),]
for (i in subjACCouts) {
  dd <- dd[which(dd$Subject != i),]
}

p1 <- lmer(StimPic.RT ~ Stim * isSame + 
             (1 + Stim | Subject) + 
             (1 | Pic.Trial.) +
             (1 | Subject:Pic.Trial.),
           dd[which(dd$StimPic.RT < 4000),], control = lmerControl(optimizer = "bobyqa"))
summary(p1)




####  E1: RT Bins ####
dd <- df[which(df$acc == 1),]
dd <- df[which(df$acc == 1 & is.na(df$RTout)),]
dd$RTbin <- NA
for (i in unique(dd$Stim)) {
  for (j in unique(dd$isSame)) {
    a <- which(dd$Stim == i & dd$isSame == j)
    rtBins <- quantile(dd$StimPic.RT[a], c(.2, .4, .6, .8))
    for (k in 4:1) {
      aa <- which(dd$Stim == i & dd$isSame == j & dd$StimPic.RT < rtBins[k])
      dd$RTbin[aa] <- k
    }  
    aa <- which(dd$Stim == i & dd$isSame == j & dd$StimPic.RT >= rtBins[4])
    dd$RTbin[aa] <- 5
  }
}

dd$IX <- ifelse(dd$Stim == "Tools", dd$isSame, dd$isSame+4)
interaction.plot(dd$RTbin, dd$IX, dd$StimPic.RT)

p1 <- lmer(StimPic.RT ~ Stim * isSame * RTbin + 
             (Stim | Subject) + 
             (isSame | Pic.Trial.) +
             (1 | Subject:Pic.Trial.),
           dd, 
           control = lmerControl(optimizer = "bobyqa"))
summary(p1)


p1 <- lmer(StimPic.RT ~ Stim * isSame + 
             (1 | Subject) + 
             (1 | Pic.Trial.),
           dd[which(dd$RTbin == 5),], 
           control = lmerControl(optimizer = "bobyqa"))
summary(p1)

p1 <- lmer(StimPic.RT ~ isSame + 
             (1 | Subject) + 
             (1 | Pic.Trial.),
           dd[which(dd$RTbin == 5 & dd$Stim == "Tools"),], 
           control = lmerControl(optimizer = "bobyqa"))
summary(p1)

p1 <- lmer(StimPic.RT ~ isSame * RTbin + 
             (1 + RTbin | Subject) + 
             (1 | Pic.Trial.),
           dd[which(dd$Stim == "Tools"),], 
           control = lmerControl(optimizer = "bobyqa"))
p1 <- lmer(StimPic.RT ~ isSame * RTbin + 
             (1 | Subject) + 
             (1 | Pic.Trial.),
           dd[which(dd$Stim == "Tools"),], 
           control = lmerControl(optimizer = "bobyqa"))
summary(p1)

p1 <- lmer(StimPic.RT ~ isSame * as.factor(RTbin) + 
             (1 + as.factor(RTbin) | Subject) + 
             (1 | Pic.Trial.),
           dd[which(dd$Stim == "Tools"),], 
           control = lmerControl(optimizer = "bobyqa"))
summary(p1)
p1 <- lmer(StimPic.RT ~ isSame * as.factor(RTbin) + 
             (1 | Subject) + 
             (1 | Pic.Trial.),
           dd[which(dd$Stim == "Tools"),], 
           control = lmerControl(optimizer = "bobyqa"))
summary(p1)



##############################################.
####     Experiment 2                     ####
##############################################.

## Means from SPSS file
pp <- read.csv("exp2meansFromSPSS.csv", header=T)
pf <- melt(pp, id.vars = "subj")
pf$isTool <- ifelse(as.integer(pf$variable) < 9, 1, 0)
pf$orient <- ifelse(as.integer(pf$variable) < 5, 1, 
             ifelse(as.integer(pf$variable) < 9, 2, 
             ifelse(as.integer(pf$variable) < 13, 1, 2)))
durs <- c(167,17,33,50)
pf$dur <- durs[as.integer(pf$variable) %% 4 + 1]


# Analyze it the old way - check!
options(contrasts=c("contr.sum","contr.poly"))
pf$subj <- as.factor(pf$subj)
pf$durF <- as.factor(pf$dur)
pf$orientF <- as.factor(pf$orient)
p1 <- aov(value ~ (durF * orientF) + 
            Error(subj / (durF * orientF)), 
          data = pf[which(pf$isTool == 1),])
summary(p1)
interaction.plot(pf$dur, pf$orient, pf$value, type="b", col=c("blue","red"))


p1 <- lmer(value ~ orient * dur + (1 + orient * dur | subj),
           pf[which(pf$isTool == 1),],
           control = lmerControl(optimizer = "bobyqa"))
summary(p1)

pt <- aggregate(value ~ orient + subj, pf[which(pf$isTool == 1),], mean)
pt <- dcast(pt, subj ~ orient, value.var = "value")
t.test(pt$`1`, pt$`2`,paired = T)
BayesFactor::ttestBF(pt$`1`, pt$`2`,paired = T, rscale = .3)
