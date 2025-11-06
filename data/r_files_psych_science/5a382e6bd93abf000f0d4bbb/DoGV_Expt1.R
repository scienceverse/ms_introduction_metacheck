#Code: DoG Experiment 1 (Doebel & Munakata, in press, Psychological Science)

install.packages("survival")
library(survival)
install.packages("psych")
library(psych)
install.packages("pastecs")
library(pastecs)
install.packages("reshape2")
library(reshape2)
install.packages("lme4")
library(lme4)


dogdf<-read.table(file="dogdata",sep=",", header=T)
View(dogdf)

#means and sds by condition
condMeans<-tapply(d$delayMin, d$condition, mean, na.rm=TRUE);condMeans
condSDs<-tapply(d$delayMin, d$condition, sd, na.rm=TRUE);condSDs
condMedians<-tapply(d$delayMin, d$condition, median, na.rm=TRUE);condMedians

#age descriptives
ageMeans<-tapply(d$ageMonths, d$condition, mean, na.rm=TRUE);ageMeans
ageSDs<-tapply(d$delayMin, d$condition, sd, na.rm=TRUE); ageSDs
summary(d$gender)


#####SURVIVAL ANALYSIS###### 

d<-dogdf
d<-subset(d, condition !="Control")  #bc comparing delay and nodelay only in the first step

#hazard ratio indicates risk of failure (eating one marshmallow) so recode cond var so moving from delay to no delay (and
#increase in risk)
d$nodelay.delay<- -1/2*(d$condition=="Delay")+1/2*(d$condition=="NoDelay")

#to get medians in presence of censoring, need to estimate KM survival curves and obtain est survival rate at 50th percentile
kmmod<-survfit(Surv(delayMin, Status) ~ condition, type="kaplan-meier", data=d)
summary(kmmod)
survdiff(Surv(delayMin, Status) ~ condition, data=d)#tests diff btw the two km survival fxs
quantile(kmmod, quantiles=c(.25, .5, .75)) #can't obtain median for delay bc at no point did surv fall below .50 (d/t censoring)

#To use cox regression instead of KM (preferable; nonparametric) need to check if PH assumption is met (first estimate model then check):
coxmod<-coxph(Surv(d$delayMin, d$Status) ~ d$nodelay.delay)
summary(coxmod) #CONFIRMATORY TEST REPORTED IN ANALYSIS

#test for violation of PH assumption:
cox.zph(coxmod) #no violation 

#exploratory analysis requested by reviewer
coxmod<-coxph(Surv(d$delayMin, d$Status) ~ d$nodelay.delay*d$groupIdent1)
summary(coxmod)

#survival plots 
#http://www.r-statistics.com/2013/07/creating-good-looking-survival-curves-the-ggsurv-function/

ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'darkgrey', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = T, xlab = 'Time spent waiting',
                   ylab = 'Something or another', main = ''){
  
  library(ggplot2)
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = ''){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)
    
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(col = col, lty = lty.est) + legend.position = "topright" 
    
    pl <- if(CI == T | CI == 'def') {
      pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci)
    } else (pl)
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = '') {
    n <- s$strata
    
    groups <- factor(unlist(strsplit(names
                                     (s$strata), '='))[seq(2, 2*strata, by = 2)])
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]),
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1))
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(aes(col = group, lty = group))
    #start
    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}
    #end
    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low,lty = group), col = surv.col)}
    } else {pl}
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col) 
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw() 
    } else (pl)
    pl 
  }
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main)
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  pl
}


kmmod<-survfit(Surv(delayMin, Status) ~ condition, type="kaplan-meier", data=d)
summary(kmmod)

survp2 <- ggsurv(kmmod) 
survp2 <- survp2 + theme(legend.position=c(.74,.9),panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                         panel.border = element_blank(),panel.background = element_blank(),
                         axis.line.x = element_line(color="black"),
                         axis.line.y = element_line(color="black"),
                         axis.text.x = element_text(size = 14), 
                         axis.text.y = element_text(size = 12),
                         legend.text = element_text(size = 12),
                         legend.title = element_text(size = 14),
                         legend.key = element_blank(),
                         axis.title.x = element_text(size = 14, face="bold"),
                         axis.title.y = element_text(size = 14, face="bold"))+
  guides(linetype = F)+
  xlab('Time spent waiting (min)') + ylab('Proportion of children resisting the marshmallow') +
  scale_colour_manual(values=c("blue", "red", "purple"), 
                      breaks=c("Waited", "Didn'tWait"),
                      labels=c("In-group waited for two marshmallows", "In-group didn't wait for two marshmallows"), 
                      name="") 
survp2


#Followup comparisons

#control and delay conditions
d<-dogdf
d<-subset(d, condition !="NoDelay") 
d$delay.control<--1/2*(d$condition=="Delay")+1/2*(d$condition=="Control")
coxmod<-coxph(Surv(d$delayMin, d$Status) ~ d$delay.control)
summary(coxmod)

#control and no delay conditions
d<-dogdf
d<-subset(d, condition !="Delay") 
d$nodelay.control<--1/2*(d$condition=="Control")+1/2*(d$condition=="NoDelay")
coxmod<-coxph(Surv(d$delayMin, d$Status) ~ d$nodelay.control)
summary(coxmod)

#exploratory analysis requested by reviewer, including age and sex in model
coxmod<-coxph(Surv(d$delayMin, d$Status) ~ d$nodelay.control + d$ageMonths + d$gender)
summary(coxmod)

coxmod<-coxph(Surv(d$delayMin, d$Status) ~ d$nodelay.control*d$groupIdent1)
summary(coxmod)

#chisq test of diffs betw groups on why q
d<-subset(dogdf, condition !="Control")
chisq.test(d$whyGroup, d$condition)

d<-subset(dogdf, condition !="NoDelay")
chisq.test(d$whyGroup, d$condition)

d<-subset(dogdf, condition !="Delay")
chisq.test(d$whyGroup, d$condition)

#frequencies
d<-dogdf
tapply(d$whyGroup==1, d$condition, sum, na.rm=TRUE)
tapply(d$whyGroup==0, d$condition, sum, na.rm=TRUE)

#descriptives
d<-dogdf
mean(d$whyGroup, na.rm=T)
sum(d$whyGroup, na.rm=T)
tapply(d$whyGroup, d$condition, mean, na.rm=T)


#analysis of self-talk during delay period

#percentages, collapsing across condition (total n is 88)

d<-dogdf
mean(d$greenGroup, na.rm=T)
mean(d$selfTalk, na.rm=T)
sum(d$greenGroup, na.rm=T)
sum(d$SelfTalk, na.rm = T)

#by condition

tapply(d$greenGroup, d$condition, mean, na.rm=T)
tapply(d$greenGroup, d$condition, sum, na.rm=T)
tapply(d$selfTalk, d$condition, sum, na.rm=T)

#chisq tests

d<-subset(dogdf, condition !="Control")
chisq.test(d$greenGroup, d$condition, correct=FALSE)

d<-subset(dogdf, condition !="NoDelay")
chisq.test(d$greenGroup, d$condition, correct=FALSE)

d<-subset(dogdf, condition !="Delay")
chisq.test(d$greenGroup, d$condition, correct=FALSE)


#analysis of identity questions

#means pre and post (control group higher for some reason in preferring green at pretest -- no good reason for this except chance)
d<-dogdf
mean(d$groupIdent1, na.rm=T)
mean(d$groupIdent2, na.rm=T)
tapply(d$groupIdent1, d$condition, mean, na.rm=TRUE)
tapply(d$groupIdent2, d$condition, mean, na.rm=TRUE)
tapply(d$groupIdent1, d$condition, sd, na.rm=TRUE)
tapply(d$groupIdent2, d$condition, sd, na.rm=TRUE)

d<-dogdf
d$chance<-.5-d$groupIdent1
modelc<-lm(chance ~0, d)
modela<-lm(chance ~1, d)
anova(modela, modelc)
summary(modela)

d$chance<-.5-d$groupIdent2
modelc<-lm(chance ~0, d)
modela<-lm(chance ~1, d)
anova(modela, modelc)
summary(modela)

#logistic regression testing group identity (pre and post), condition (all three conditions) and interaction
#predicting probability that child identifies with ingroup (value var)

#contrast codes for conditions
d$lincond<- - 1/2*(d$condition=="NoDelay") + 0*(d$condition=="Control") + 1/2*(d$condition=="Delay")
d$quadcond<- - 1/3*(d$condition=="NoDelay") + 2/3*(d$condition=="Control") - 1/3*(d$condition=="Delay")
d<-subset(d, select=c("ID", "lincond","quadcond", "groupIdent1", "groupIdent2")) # melt the data
d<-melt(d, id=c("ID","lincond", "quadcond"))
d<-na.omit(d)
d$variable<- -1/2*(d$variable=="groupIdent1") + 1/2*(d$variable=="groupIdent2")
#group means collapsing across time 
tapply(d$value, d$lincond, mean, na.rm=TRUE)

#test of cond, time, and interaction 
modela<-glmer(d$value ~ d$lincond +d$variable + d$lincond:d$variable + d$quadcond + d$quadcond:d$variable +
                (1|d$ID), family=binomial(logit))
summary(modela)

#simple test of whether condition predicts identity
modela<-glmer(d$value ~ d$lincond + (1|d$ID), family=binomial(logit))
summary(modela) 

#test if post-test diff is sig between delay and nodelay
d<-subset(dogdf, condition != "Control")
d$nodelay.delay<- -1/2*(d$condition=="Delay")+1/2*(d$condition=="NoDelay")
modela<-glm(d$groupIdent2 ~ d$nodelay.delay)
summary(modela)

summary(lm(groupIdent2 ~ nodelay.delay, d))
summary(lm(groupIdent2 ~ nodelay.delay + groupIdent1, d))


#tests diff betw pre and posttest group identification by group
d<-subset(dogdf, select=c("ID", "condition", "groupIdent1", "groupIdent2"))
d<-melt(d, id=c("ID","condition"))
d<-na.omit(d)
d$variable<- -1/2*(d$variable=="groupIdent1") + 1/2*(d$variable=="groupIdent2")

d1<-subset(d, condition=="Delay")
modela<-glmer(d1$value ~ d1$variable + (1|d1$ID), family=binomial)
modelc<-glmer(d1$value ~ (1|d1$ID), family=binomial)
summary(modela)
anova(modela, modelc)

d1<-subset(d, condition=="NoDelay")
modela<-glmer(d1$value ~ d1$variable + (1|d1$ID), family=binomial)
modelc<-glmer(d1$value ~ (1|d1$ID), family=binomial)
summary(modela)
anova(modela, modelc)

d1<-subset(d, condition=="Control")
modela<-glmer(d1$value ~ d1$variable + (1|d1$ID), family=binomial)
modelc<-glmer(d1$value ~ d1$variable + (1|d1$ID), family=binomial)
summary(modela)
anova(modela, modelc)

#using repeated measures anova to test if diff betw pre and post identity scores differed by condition
d<-dogdf
d$lincond<- - 1/2*(d$condition=="NoDelay") + 0*(d$condition=="Control") + 1/2*(d$condition=="Delay")
d$identdiff<-d$groupIdent1-d$groupIdent2
summary(lm(identdiff ~ lincond, d))  

#simple test of pre-post diffs for each condition
d2<-subset(dogdf, d$condition=="Control")
d2$identdiff<-d2$groupIdent1-d2$groupIdent2
summary(lm(identdiff ~ 1, d2))

d2<-subset(dogdf, d$condition=="Delay")
d2$identdiff<-d2$groupIdent1-d2$groupIdent2
summary(lm(identdiff ~ 1, d2))

d2<-subset(dogdf, d$condition=="NoDelay")
d2$identdiff<-d2$groupIdent1-d2$groupIdent2
summary(lm(identdiff ~ 1, d2))

#analysis of memory for group behavior questions

#comparing performance on memory questions to chance 

tapply(d$memBehGroup1, d$condition, summary, na.rm=TRUE)
tapply(d$memBehGroup2, d$condition, summary, na.rm=TRUE)

t.test(d$memBehGroup1, m=.5)
t.test(d$memBehGroup2, m=.5)
sd(d$memBehGroup1, na.rm=T)
sd(d$memBehGroup2, na.rm=T)
