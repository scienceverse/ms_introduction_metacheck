library(brms)
library(tidyverse)
library(cowplot)
library(knitr)
library(lme4)
library(doBy)
## for bootstrapping 95% confidence intervals -- from Mike Frank https://github.com/langcog/KTE/blob/master/mcf.useful.R
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #- mean(x,na.rm=na.rm)}

convert_stan_to_dataframe <- function(stan_object){
  sum.df <- data.frame(summary(stan_object)$fixed)
  sum.df$Diff_from_zero <- ifelse((sum.df$l.95..CI * sum.df$u.95..CI) > 0, "*","-")
  return(sum.df)
}

#load the data and apply outlier exclusion criteria
load("BRMS_Dist_Dec28.RDATA")
ads <- read.csv("adults.csv")
fives <- read.csv("fives.csv")
threes <- read.csv("threes.csv")

ads$Age <- "Adult"
fives$Age <- "Five"
threes$Age <- "Three"

ads$Age1 <- "Adult"
fives$Age1 <- "Child"
threes$Age1 <- "Child"

##Legend
# Subject: subject numerical identifier
# Level: block (level of the maze game)
# TrialNo: progressive trial number, within a block
# Pred: Maze Type (Pred = predictable one-animal maze; Unpred = unpredictable two-animal maze)
# Early.Late: Information Structure (early = character name is mentioned early "Is Po hiding the tiger?" vs, late = character name is mentioned late "Is the tiger behind Po?") 
# Right.Wrong: Answer Type (right = required answer is yes; wrong = required answer is no) 
# Length: factor, is the character name a short or a long word
# SoundFile: name of the sound file played on the trial
# QuLength: Length of the question in seconds.
# CharacterLength: Length of the final word of the question in seconds (i.e., the character name in the late condition, the animal name in the early condition)
# QuOnset: onset of question from beginning of recording (in seconds)
# QuOffset: offset of question from beginning of recording (in seconds)
# Response.Onset: onset of participant's response from beginning of recording (in seconds)
# RT.s: response time (in seconds)
# QuLength.ms: Length of the question in milliseconds
# CharacterLength.ms: Length of the final word of the question in milliseconds (i.e., the character name in the late condition, the animal name in the early condition)
# QuOnset.ms: onset of question from beginning of recording (in milliseconds)        
# QuOffset.ms: offset of question from beginning of recording (in milliseconds)     
# ResponseOnset.ms: onset of participant's response from beginning of recording (in milliseconds)
# RT.ms: response time (in milliseconds)           
# Response: What did the participant respond?
# Accuracy: did the participant respond accurately? (yes to a right answer and no to a wrong answer)
# Comments: additional notes (e.g., background noise, difluent responses)


tt.expt <- rbind(ads,threes,fives)
tt.expt$Subject <- paste(tt.expt$Age,tt.expt$Subject, sep = "")
tt.expt$Item = str_split_fixed(gsub(" ","_",tt.expt$SoundFile),"_",2)[,1]
tt.expt$Age <- as.factor(tt.expt$Age)
tt.expt$scale_CharacterLength.ms = scale(log(tt.expt$CharacterLength.ms))

tt.expt$AgeC = 0.5
tt.expt[tt.expt$Age == "Three",]$AgeC = -0.5

tt.expt$Age1C = 0.5
tt.expt[tt.expt$Age1 == "Child",]$Age1C = -0.5

contrasts(tt.expt$Early.Late)[1] = -0.5
contrasts(tt.expt$Early.Late)[2] = 0.5
contrasts(tt.expt$Pred)[1] = -0.5
contrasts(tt.expt$Pred)[2] = 0.5
contrasts(tt.expt$Right.Wrong)[1] = -0.5
contrasts(tt.expt$Right.Wrong)[2] = 0.5


tt.expt$rt_excl <- NA
for (i in levels(tt.expt$Age)){
  tt.expt[tt.expt$Age == i,]$rt_excl <- (tt.expt[tt.expt$Age == i,]$RT.ms - mean(tt.expt[tt.expt$Age == i,]$RT.ms))/(sd(tt.expt[tt.expt$Age == i,]$RT.ms))
}
tt.expt<- subset(tt.expt, rt_excl <= 5000 & rt_excl >= -1.5)


summaryBy(RT.ms ~ Subject, data = tt.expt, FUN = length) -> n.trials
tt.expt <- subset(tt.expt, Subject %in% n.trials[n.trials$RT.ms.length >= 20,]$Subject & Accuracy == 1)

tt.expt$rt <- (tt.expt$RT.ms - mean(tt.expt$RT.ms))/(2.5*sd(tt.expt$RT.ms))
tt.expt$rt <- tt.expt$rt + abs(min(tt.expt$rt))+0.1

#by-participant means
tt.expt.subj = subset(tt.expt, RT.ms < 1500) %>%
  dplyr::select(Age, Early.Late,Pred,Subject,rt) %>%
  dplyr::mutate(rt = log(rt)) %>%
  dplyr::group_by(Age,Early.Late,Pred,Subject) %>%
  dplyr::summarize(rt.m = exp(mean(rt))*1000)

#means over participants
tt.expt.means = tt.expt.subj %>%
  dplyr::group_by(Age,Early.Late,Pred) %>%
  dplyr::summarize(rt.high = ci.high(rt.m),
                   rt.low = ci.low(rt.m),
                   rt.m = mean(rt.m))

# plot overall means over individual participants' means (not included in paper)
ggplot(tt.expt.subj,aes(y = rt.m, x = Early.Late, fill = Pred,color= Pred, group = Subject))+
  facet_wrap(~Age)+
  geom_line(aes(alpha = 0.3),show.legend = F)+
  geom_point(data = tt.expt.means,stat="identity",position=position_dodge(width=0.3),aes(y = rt.m, x = Early.Late, fill = Pred,color= Pred, group = 1, alpha = 1, size = 2))

tt.expt.means$Predictability = factor(tt.expt.means$Pred, labels = c("One Animal","Two Animals"))
tt.expt.means$AgeLong = factor(tt.expt.means$Age, labels = c("Adults","Older children","Younger children"))

#meanplot
meanplot = ggplot(tt.expt.means,aes(y = rt.m, x = Predictability, color= Early.Late, shape=Early.Late))+
  geom_point(aes(),size = 3, stat="identity",position= position_dodge(width=1))+
  geom_errorbar(width = 0,aes(ymin = rt.low,ymax = rt.high),position=position_dodge(width=1))+
  facet_wrap(~AgeLong)+
  ylab("Response Time (ms)")+
  xlab("Maze Type")+
  scale_color_manual(values=c("#C51B8A","#99D8C9"),name="Information Structure", labels=c("Early","Late"))+
  scale_shape_manual(values=c(17,18),name="Information Structure", labels=c("Early","Late"))+
  theme(legend.position="bottom",axis.text=element_text(size=15), axis.title=element_text(size=20))
plot(meanplot)

### log mean plot
tt.expt.subj$rt.m.log= log(tt.expt.subj$rt.m)
tt.expt.means.log = tt.expt.subj %>%
  dplyr::group_by(Age,Early.Late,Pred) %>%
  dplyr::summarize(rt.high = ci.high(rt.m.log),
                   rt.low = ci.low(rt.m.log),
                   rt.m.log = mean(rt.m.log))
tt.expt.means.log$Predictability = factor(tt.expt.means.log$Pred, labels = c("One Animal","Two Animals"))
tt.expt.means.log$AgeLong = factor(tt.expt.means.log$Age, labels = c("Adults","Five-year-olds","Three-year-olds"))


meanplot.log = ggplot(tt.expt.means.log,aes(y = rt.m.log, x = Predictability, color= Early.Late, shape=Early.Late))+
  geom_point(aes(),size = 3, stat="identity",position= position_dodge(width=1))+
  geom_errorbar(width = 0,aes(ymin = rt.low,ymax = rt.high),position=position_dodge(width=1))+
  facet_wrap(~AgeLong)+
  ylab("Log Response Time")+
  xlab("Maze Type")+
  scale_color_manual(values=c("#C51B8A","#99D8C9"),name="Information Structure", labels=c("Early","Late"))+
  scale_shape_manual(values=c(17,18),name="Information Structure", labels=c("Early","Late"))+
  theme(legend.position="bottom",axis.text=element_text(size=15), axis.title=element_text(size=20))
plot(meanplot.log)

# Modify the following code to save an high-resolution copy.
#ggsave("Figure2B_log.png", plot=meanplot.log, width=30, height=30, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/turn-taking/Laura/BRMS_TurnTaking-master/graphs")


tt.expt$Predictability = factor(tt.expt$Pred, labels = c("One Animal","Two Animals"))
tt.expt$AgeLong = factor(tt.expt$Age, labels = c("Adults","Older children","Younger children"))

#distribution plot
distplot = ggplot(tt.expt, aes(x = RT.ms, fill = Early.Late, linetype= Early.Late))+
  geom_density(alpha = 0.5)+
  facet_wrap(AgeLong ~ Predictability, nrow = 3)+
  xlab("Response Time (ms)")+
  xlim(c(-400,3000))+
  theme_cowplot()+theme(axis.text=element_text(size=15), axis.title=element_text(size=20))+
  scale_y_continuous(name="probability density", breaks=c(0, 0.002))+
  scale_fill_manual(name="Information Structure",values=c("#C51B8A","#99D8C9"))+
  scale_linetype_manual(name="Information Structure", values=c(1,2))

distplot

#combine mean and distribution plots
final<-plot_grid(distplot,meanplot, nrow = 2, rel_heights = c(2,1),labels = "AUTO")
# Modify the following code to save an high-resolution copy.
#ggsave("Figure2_colourblind_tris.png", plot=final, width=30, height=30, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/turn-taking/Laura/BRMS_TurnTaking-master/graphs")

# colourblind palette (low contrast in B&W)
#c("#E69F00","#CC79A7")

# Plots collapsing across 3 and 5 year olds
tt.expt.subj = subset(tt.expt, RT.ms < 1500) %>%
  dplyr::select(Age1, Early.Late,Pred,Subject,rt) %>%
  dplyr::mutate(rt = log(rt)) %>%
  dplyr::group_by(Age1,Early.Late,Pred,Subject) %>%
  dplyr::summarize(rt.m = exp(mean(rt))*1000)

tt.expt.means = tt.expt.subj %>%
  dplyr::group_by(Age1,Early.Late,Pred) %>%
  dplyr::summarize(rt.high = ci.high(rt.m),
                   rt.low = ci.low(rt.m),
                   rt.m = mean(rt.m))
tt.expt.means$Predictability = factor(tt.expt.means$Pred, labels = c("One Animal","Two Animals"))
tt.expt.means$AgeLong = factor(tt.expt.means$Age1, labels = c("Adults","Children"))

#meanplot
meanplot = ggplot(tt.expt.means,aes(y = rt.m, x = Predictability, color= Early.Late, shape=Early.Late))+
  geom_point(aes(),size = 3, stat="identity",position= position_dodge(width=1))+
  geom_errorbar(width = 0,aes(ymin = rt.low,ymax = rt.high),position=position_dodge(width=1))+
  facet_wrap(~AgeLong)+
  ylab("Response Time (ms)")+
  xlab("Maze Type")+
  scale_color_manual(values=c("#C51B8A","#99D8C9"),name="Information Structure", labels=c("Early","Late"))+
  scale_shape_manual(values=c(17,18),name="Information Structure", labels=c("Early","Late"))+
  theme(legend.position="bottom",axis.text=element_text(size=15), axis.title=element_text(size=20))
plot(meanplot)

tt.expt$Predictability = factor(tt.expt$Pred, labels = c("One Animal","Two Animals"))
tt.expt$AgeLong = factor(tt.expt$Age1, labels = c("Adults","Children"))

distplot = ggplot(tt.expt, aes(x = RT.ms, fill = Early.Late, linetype= Early.Late))+
  geom_density(alpha = 0.5)+
  facet_wrap(AgeLong ~ Predictability, nrow = 3)+
  xlab("Response Time (ms)")+
  xlim(c(-400,3000))+
  theme_cowplot()+theme(axis.text=element_text(size=15), axis.title=element_text(size=20))+
  scale_y_continuous(name="probability density", breaks=c(0, 0.002))+
  scale_fill_manual(name="Information Structure",values=c("#C51B8A","#99D8C9"))+
  scale_linetype_manual(name="Information Structure", values=c(1,2))

distplot

final.2groups<-plot_grid(distplot,meanplot, nrow = 2, rel_heights = c(2,1),labels = "AUTO")
# Modify the following code to save an high-resolution copy.
#ggsave("Figure2_colourblind_2groups.png", plot=final.2groups, width=30, height=30, unit="cm", dpi=300, path="/Volumes/Macintosh HD/ED_20142017/turn-taking/Laura/BRMS_TurnTaking-master/graphs")

