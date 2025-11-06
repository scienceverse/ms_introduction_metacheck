# Amrita Lamba's R code to reproduce figures from Psych. Science Article: 
# Anxiety Impedes Adaptive Social Learning Under Uncertainty (Lamba, Frank, FeldmanHall, 2019)
# Last Update: Dec. 23, 2019

# data summary functions (help_functions.R) courtesy of 
# Chang, W. (2018). R graphics cookbook: practical recipes for visualizing data. O'Reilly Media.

# clear everything
rm(list=ls())

# load packages 
libraryBooks <- c("dplyr", "tidyr", "stringr")
invisible(lapply(libraryBooks, require, character.only = TRUE)); rm(libraryBooks)
pd <- position_dodge(0.2)
if(!require(ggplot2)){install.packages("ggplot2")}
library(lme4)

# open ALL_psysciDat_osf data set 
dat <- read.csv(file.choose(),header = T)

# make sure the data was imported correctly
head(dat)

# exclude subjets based on AIC exclusion criteria detailed in Supplementary Materials 
# page 2, Excluded Subjects
dat.ex <- subset(dat, exclusion_code == "0")
pd <- position_dodge(0.4)

# factor variables + rename conditions 
dat.ex$oppnum <- factor(dat.ex$oppnum)
dat.ex$trial<- factor(dat.ex$trial)
dat.ex$condition <- factor(dat.ex$condition)
dat.ex$gad_clinical <- factor(dat.ex$gad_clinical)
dat.ex$block_number <- factor(dat.ex$block_number)

#code for gain vs. loss trials 
#no valence (mean proportion of return = 0.25) = 0 
#positive valence (mean proportion of return > 0.25) = 1
#negative valence (mean proportion of return < 0.25) = 2

attach(dat.ex)
dat.ex$valence[oppnum == 3 & block_number == 1] <- 1
dat.ex$valence[oppnum == 2 & block_number == 1] <- 0
dat.ex$valence[oppnum == 1 & block_number == 1] <- 2

dat.ex$valence[oppnum == 3 & block_number == 2] <- 1
dat.ex$valence[oppnum == 2 & block_number == 2] <- 2
dat.ex$valence[oppnum == 1 & block_number == 2] <- 2

dat.ex$valence[oppnum == 3 & block_number == 3] <- 0 
dat.ex$valence[oppnum == 2 & block_number == 3] <- 2
dat.ex$valence[oppnum == 1 & block_number == 3] <- 0

dat.ex$valence[oppnum == 3 & block_number == 4] <- 2
dat.ex$valence[oppnum == 2 & block_number == 4] <- 2 
dat.ex$valence[oppnum == 1 & block_number == 4] <- 1

dat.ex$valence[oppnum == 3 & block_number == 5] <- 2
dat.ex$valence[oppnum == 2 & block_number == 5] <- 0
dat.ex$valence[oppnum == 1 & block_number == 5] <- 1

dat.ex$valence[oppnum == 3 & block_number == 6] <- 2 
dat.ex$valence[oppnum == 2 & block_number == 6] <- 1
dat.ex$valence[oppnum == 1 & block_number == 6] <- 1

dat.ex$valence[oppnum == 3 & block_number == 7] <- 0
dat.ex$valence[oppnum == 2 & block_number == 7] <- 1 
dat.ex$valence[oppnum == 1 & block_number == 7] <- 0 
detach(dat.ex)

# factor 
dat.ex$valence <- factor(dat.ex$valence)

# rename levels of gad_clinical 
levels(dat.ex$gad_clinical)[levels(dat.ex$gad_clinical)=="0"] <- " Healthy "
levels(dat.ex$gad_clinical)[levels(dat.ex$gad_clinical)=="1"] <- " Anxious "

# rename condition
levels(dat.ex$condition)[levels(dat.ex$condition)=="1"] <- " Trust Game "
levels(dat.ex$condition)[levels(dat.ex$condition)=="2"] <- " Slot Machine Game "

# rename oppnums
levels(dat.ex$oppnum)[levels(dat.ex$oppnum)=="1"] <- " Untrustworthy Start "
levels(dat.ex$oppnum)[levels(dat.ex$oppnum)=="2"] <- " Neutral Start "
levels(dat.ex$oppnum)[levels(dat.ex$oppnum)=="3"] <- " Trustworthy Start "

# rename valence 
levels(dat.ex$valence)[levels(dat.ex$valence)=="1"] <- " Gain "
levels(dat.ex$valence)[levels(dat.ex$valence)=="2"] <- " Loss "

##################### MS Figure 1b Task Parameters ############################
ggplot(data=dat.ex, aes(x=trial, y=propReturned, group=oppnum, colour=oppnum)) +
  geom_line(aes(linetype=oppnum), size=1,position = pd) + 
  geom_point(position = pd) + 
  ylab("Mean Proportion of Return") + 
  xlab("Round Number") +
  ggtitle("Trust & Slot Machine Game Mean Return Parameters") +
  scale_color_manual(values=c("cornflowerblue", "mistyrose3","royalblue4")) + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white"))

################### MS Figure 2a Behavioral Effects ############################
# summarize data 
healthy.dat <- subset(dat.ex, gad_clinical == " Healthy ")
anxious.dat <- subset(dat.ex, gad_clinical == " Anxious ")

# run help_functions.R script to estimate descriptive stats 

healthy.sum <- summarySEwithin(healthy.dat, measurevar="investment", withinvars=c("oppnum","condition"), 
                               idvar="subject")

anxious.sum <- summarySEwithin(anxious.dat, measurevar="investment", withinvars=c("oppnum","condition"), 
                               idvar="subject")

# healthy Plot
healthy.plot <- ggplot(data=healthy.sum, aes(x=oppnum, y=investment, group=condition, colour=condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=investment-se, ymax=investment+se), width=.2,
                position=position_dodge(0.01)) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values=c("dodgerblue4", "lightskyblue")) + 
  xlab("Player Type") +
  ylab("Mean Subject Investment") +
  ylim(0.43,0.63) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white"))

# anxious Plot
anxious.plot <- ggplot(data=anxious.sum, aes(x=oppnum, y=investment, group=condition, colour=condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=investment-se, ymax=investment+se), width=.2,
                position=position_dodge(0.01)) +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values=c("purple4", "plum3")) + 
  xlab("Player Type") +
  ylab("Mean Subject Investment") +
  ylim(0.43,0.63) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white"))

# combine
multiplot(healthy.plot, anxious.plot, cols=2)

##################### MS Figure 2b Behavioral Effects ##########################
healthy.sum <- summarySE(healthy.dat, measurevar="investment", groupvars=c("oppnum","condition","subject"))
ts.healthy <- subset(healthy.sum, oppnum == " Trustworthy Start ")
ns.healthy <- subset(healthy.sum, oppnum == " Neutral Start ")

healthy.delta <- ts.healthy$investment - ns.healthy$investment
healthy.delta.df <- cbind(ts.healthy$subject, healthy.delta, ts.healthy$condition)
healthy.delta.df <- as.data.frame(healthy.delta.df)
names(healthy.delta.df)[1] <- "subject"
names(healthy.delta.df)[2] <- "delta"
names(healthy.delta.df)[3] <- "condition"

healthy.delta.sum <- summarySEwithin(healthy.delta.df, measurevar="delta", withinvars="condition", 
                                     idvar="subject")

levels(healthy.delta.sum$condition)[levels(healthy.delta.sum$condition)=="1"] <- " Trust Game "
levels(healthy.delta.sum$condition)[levels(healthy.delta.sum$condition)=="2"] <- " Slot Machine Game "

# healthy difference plot 
hd <- ggplot(data=healthy.delta.sum, aes(x=condition, y=delta, fill=condition)) +
  geom_bar(colour="black", stat="identity") +
  geom_errorbar(aes(ymin=delta-se, ymax=delta+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("dodgerblue4", "lightskyblue")) +
  theme(legend.title=element_blank()) +
  guides(fill=FALSE) +
  ylim(0, 0.06) + 
  xlab("Condition") +
  ylab("Mean Subject Investment ($)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white"))

# repeat for anxious
anxious.sum <- summarySE(anxious.dat, measurevar="investment", groupvars=c("oppnum","condition","subject"))
ts.anxious <- subset(anxious.sum, oppnum == " Trustworthy Start ")
ns.anxious <- subset(anxious.sum, oppnum == " Neutral Start ")

anxious.delta <- ts.anxious$investment - ns.anxious$investment
anxious.delta.df <- cbind(ts.anxious$subject, anxious.delta, ts.anxious$condition)
anxious.delta.df <- as.data.frame(anxious.delta.df)
names(anxious.delta.df)[1] <- "subject"
names(anxious.delta.df)[2] <- "delta"
names(anxious.delta.df)[3] <- "condition"

anxious.delta.sum <- summarySEwithin(anxious.delta.df, measurevar="delta", withinvars="condition", 
                                     idvar="subject")

levels(anxious.delta.sum$condition)[levels(anxious.delta.sum$condition)=="1"] <- " Trust Game "
levels(anxious.delta.sum$condition)[levels(anxious.delta.sum$condition)=="2"] <- " Slot Machine Game "

ad <- ggplot(data=anxious.delta.sum, aes(x=condition, y=delta, fill=condition)) +
  geom_bar(colour="black", stat="identity") +
  geom_errorbar(aes(ymin=delta-se, ymax=delta+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_fill_manual(values=c("purple4", "plum3")) +
  theme(legend.title=element_blank()) +
  guides(fill=FALSE) +
  ylim(0, 0.06) + 
  xlab("Condition") +
  ylab("Mean Subject Investment ($)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white"))

# combine 
multiplot(hd,ad, cols=2)

####################### MS Figure 3a Anxiety/Valence Effects ###########################

gl.dat <- subset(dat.ex, valence != 0)

# run help_functions.R script 

# summarize data 
sum.dat <- summarySEwithin(gl.dat, measurevar="investment", withinvars=c("valence","condition"), 
                           betweenvars="gad_clinical", idvar="subject")

ggplot(data=sum.dat, aes(x=valence, y=investment, fill = gad_clinical)) +
  geom_bar(colour="black", stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("skyblue3", "plum3")) + 
  geom_errorbar(aes(ymin=investment-se, ymax=investment+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme(legend.title=element_blank()) +
  xlab("Valence") +
  ylab("Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white")) +
  facet_grid( ~ condition)

################## MS Figure 3b Anxiety/Learning Curve Effects ########################
anx.dat <- summarySEwithin(dat.ex, measurevar="investment", withinvars=c("valence","condition","oppnum","block_number"), 
                           betweenvars="gad_clinical", idvar="subject")

ggplot(data=anx.dat, aes(x=block_number, y=investment, group=gad_clinical, colour=gad_clinical)) +
  geom_line(aes(linetype=gad_clinical, color=gad_clinical)) +
  geom_point(position = pd, shape=21) + 
  geom_errorbar(aes(ymin=investment-se, ymax=investment+se), colour="black", width=.1, position=pd) +
  xlab("Block Number") + ylab("Mean Participant Endowments") + 
  geom_point(shape=1) + 
  ggtitle("Mean Block Investments for 
Healthy and Anxious Subjects") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(shape=1) + 
  theme(legend.title=element_blank()) +
  scale_linetype_manual(values=c("solid", "solid","dashed")) +
  scale_color_manual(values=c("skyblue3", "plum3","grey")) +
  theme(text=element_text(family="Arial", size = 7)) +
  theme(legend.position="bottom") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white")) +
  facet_grid(oppnum ~ condition)

####################### MS Figure 4a Model Comparison Plot ################################
dat.fits <- dat.ex %>% select(subject,condition, dbrl_AIC, brl_AIC, rl_AIC,gad_clinical)
dat.fits <- unique(dat.fits)

# convert to long format 
AIC.long <- dat.fits %>% gather(modelNo,AIC,dbrl_AIC, brl_AIC, rl_AIC)
aLL <- summarySE(AIC.long, measurevar="AIC", groupvars=c("modelNo","gad_clinical"))
aLL$gad_clinical <- factor(aLL$gad_clinical)
aLL$modelNo <- factor(aLL$modelNo)

# reorder
aLL$modelNo <- factor(aLL$modelNo, levels = c("dbrl_AIC", "brl_AIC", "rl_AIC"))

levels(aLL$modelNo)[levels(aLL$modelNo)=="dbrl_AIC"] <- " D-BRL "
levels(aLL$modelNo)[levels(aLL$modelNo)=="brl_AIC"] <- " B-RL "
levels(aLL$modelNo)[levels(aLL$modelNo)=="rl_AIC"] <- " RL "

# subset by healthy and anxious
hdat <- subset(aLL, gad_clinical == " Healthy ")
adat <- subset(aLL, gad_clinical == " Anxious ")

hplot <- ggplot(data=hdat, aes(x=modelNo, y=AIC, fill = modelNo)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("steelblue4", "steelblue3","steelblue2")) +
  geom_errorbar(aes(ymin=AIC-se, ymax=AIC+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) + 
  theme(legend.title=element_blank()) +
  xlab("Model") +
  ylab("Model AIC") +
  ylim(-100, 0) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white"))

aplot <- ggplot(data=adat, aes(x=modelNo, y=AIC, fill = modelNo)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("mediumpurple4", "mediumpurple3","mediumpurple2")) +
  geom_errorbar(aes(ymin=AIC-se, ymax=AIC+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) + 
  theme(legend.title=element_blank()) +
  xlab("Model") +
  ylab("Model AIC") +
  ylim(-100, 0) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white"))

multiplot(hplot,aplot, cols=2)

#################### MS Figure 4b Model Parameter Comparison ###########################
diff.dat <- summarySEwithin(dat.ex, measurevar="dbrl_gamDiff", withinvars="condition", 
                            betweenvars="gad_clinical", idvar="subject")

ggplot(data=diff.dat, aes(x=condition, y=dbrl_gamDiff, fill=gad_clinical)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("skyblue3", "plum3")) + 
  geom_errorbar(aes(ymin=dbrl_gamDiff-se, ymax=dbrl_gamDiff+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) + 
  theme(legend.title=element_blank()) +
  xlab("Condition") +
  ylab("Decay Rate Difference") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom",legend.title=element_blank()) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + 
  theme(plot.background = element_rect(fill = "white"))





