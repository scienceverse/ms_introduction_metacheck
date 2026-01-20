library('reshape')
library('lme4')
library('ggplot2')
library('plotrix')
library('dplyr')
require(optimx)

## Analysis of Expt 4, 5, 6

setwd('~/Documents/research/2015 noisy channel caitlin/8 data to be released')

alldata = read.csv("Gibson_et_al_2016_E4_E5_E6_anon.csv", header=TRUE)

allimplaus = alldata %>% filter(Condition == "actimplaus" | Condition == "passimplaus" |
                                  Condition == "DOimplaus" | Condition == "POimplaus" |
                                  Condition == "transimplaus" | Condition == "prepimplaus")
allimplaus %>% group_by(Condition) %>% summarize(mean(Correct))

allimplaus$Accent1 <- NA
allimplaus[allimplaus$Accent == "Accent",]$Accent1 <- .5
allimplaus[allimplaus$Accent == "None",]$Accent1 <- -.5

allimplaus$Speaker1 <- NA
allimplaus[allimplaus$Speaker == "Idan",]$Speaker1 <- .5
allimplaus[allimplaus$Speaker == "Nezar",]$Speaker1 <- -.5

## listen_data is the data from 480 participants who listened to the target implausible materials and simply
## transcribed what Idan or Nezar was saying.  Ted went through all these data an marked each trial when there
## was an error in a content word (e.g., "worsened" got heard as "was in" a lot for Nezar) or an error in function
## word omission / inclusion which directly related to the noisy channel deletion / insertion hypothesis
## The column "content_word" indicates a content word error when 1 (0 otherwise)
## The column "fn_word" indicates a function word error when 1 (0 otherwise)

listen_data = read.csv("transcribe_data_all_2016_anon.csv", header=TRUE)

listen_data %>% group_by(Speaker, Condition, Accent) %>% summarise(mean(content_word), mean(fn_word))
listen_data %>% group_by(Speaker, Condition, Accent) %>% summarise(mean(content_word | fn_word))

fn_errors = listen_data %>% group_by(Speaker, Condition, Item, Accent) %>% summarise(fn_errors = mean(fn_word))
content_errors = listen_data %>% group_by(Speaker, Condition, Item, Accent) %>% summarise(content_errors = mean(content_word))

errors = merge(content_errors, fn_errors, c("Condition", "Item", "Accent", "Speaker"))

# nrow(errors)
# [1] 479  ## 480 total item / condition / accent / speaker combinations = 20 * 6 * 2 * 2 (no plaus here)

allimplaus = merge(allimplaus, errors, c("Condition", "Item", "Accent", "Speaker"))


## leave in or exclude items which participants had a really hard time understanding
## if we exclude too much, then the glmer's don't converge
allimplaus = allimplaus %>% filter(content_errors < .05 & fn_errors < .05)

options(dplyr.print_max = 1e9)
allimplaus %>% group_by(Condition, Accent) %>% summarize(mean(content_errors), mean(fn_errors))

# how many items have omitted?
left_over_items = errors %>% filter(content_errors < .05)
# nrow(left_over_items)
# [1] 474

left_over_items = left_over_items %>% filter(fn_errors < .05)
# nrow(left_over_items)
# [1] 468

Subj_acc= allimplaus %>% group_by(Participant) %>% summarise(Acc = mean(Correct))
#options(dplyr.print_max = 1e9)
allimplaus = merge(allimplaus, Subj_acc, by=c("Participant"))

dopodata = allimplaus[allimplaus$Condition == "DOimplaus" | allimplaus$Condition == "POimplaus",]

dopodata$do <- NA
dopodata[dopodata$Condition == "DOimplaus",]$do <- .5
dopodata[dopodata$Condition == "POimplaus",]$do <- -.5

transintransdata = allimplaus[allimplaus$Condition == "transimplaus" | allimplaus$Condition == "prepimplaus",]

transintransdata$trans <- NA
transintransdata[transintransdata$Condition == "transimplaus",]$trans <- .5
transintransdata[transintransdata$Condition == "prepimplaus",]$trans <- -.5

actpassdata = allimplaus[allimplaus$Condition == "actimplaus" | allimplaus$Condition == "passimplaus",]

actpassdata$active <- NA
actpassdata[actpassdata$Condition == "actimplaus",]$active <- .5
actpassdata[actpassdata$Condition == "passimplaus",]$active <- -.5


allimplaus %>% group_by(Condition, Accent) %>% summarise(mean(Correct))
allimplaus %>% group_by(Speaker, Condition, Accent) %>% summarise(mean(Correct))

allimplaus %>% group_by(Condition, Accent) %>% summarise(mean(fn_errors))
allimplaus %>% group_by(Condition, Accent) %>% summarise(mean(content_errors))

allimplaus %>%
  group_by(Condition, Accent) %>%
  summarise(m=mean(Correct),
            s=sqrt(m*(1-m)/length(Correct))) %>%
  mutate(m_lower=m-s*1.96, m_upper=m+s*1.96) %>%
  ggplot(aes(x=Accent, y=m, ymin=m_lower, ymax=m_upper, fill=Accent)) +
  geom_bar(stat="identity") +
  geom_errorbar() +
  facet_wrap(~Condition)

all_results = allimplaus %>% group_by(Condition, Accent, Speaker) %>%
  summarise(m=mean(Correct), s=sqrt(m*(1-m)/length(Correct))) %>%
  mutate(m_lower=m-s*1.96, m_upper=m+s*1.96)

write.csv(all_results, "all_results.csv")


## sum-coded version
do2.fit <- glmer(Correct ~ Accent1 * do *  Speaker1 
                 + (1| Participant)
                 + (0  + Accent1| Participant)
                 + (0  + do| Participant)
                 + (1| Item)
                 + (0  + Accent1| Item)
                 + (0  + do | Item)
                 , data=dopodata, family="binomial"
                 , control = glmerControl(optimizer= c("optimx", "bobyqa"), 
                                          optCtrl  = list(method="nlminb")))

summary(do2.fit)

## sum-coded version for trans/intrans
trans.fit <- glmer(Correct ~ Accent1 * trans * Speaker1 
                   + (1| Participant)
                   + (0  + Accent1| Participant)
                   + (0  + trans| Participant)
                   + (1| Item)
                   + (0  + Accent1 | Item)
                   + (0  + trans | Item), data=transintransdata, family="binomial"
                   , control = glmerControl(optimizer= c("bobyqa")))

summary(trans.fit)

## sum-coded version for active/passive
active.fit <- glmer(Correct ~ Accent1 * active * Speaker1 
                    + (1| Participant)
                    + (0  + Accent1| Participant)
                    + (0  + active| Participant)
                    + (1| Item)
                    + (0  + Accent1 | Item)
                    # + (0  + active | Item)
                    , data=actpassdata, family="binomial"
                    , control = glmerControl(optimizer= c("bobyqa")))

summary(active.fit)

