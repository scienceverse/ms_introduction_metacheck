library(lme4)
library(memisc)
library(Hmisc)
library(MASS)
library(psych)
library(dplyr)
library(lme4)
library(reshape2)
library(boot)
library(lmeresampler)
library(tidyr)
library(lmerTest)
library(nlme)
library(paran)
library(boot)
library(tidyverse)
library(jtools)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(viridis)
library(colorspace)

#read data
data <- read_csv("//132.206.108.110/Lab/Studies/137_BigICCs/all data/data/psa001_ind_all.csv")
#create additional variables before subsetting
data$t_race <- str_sub(data$stim_id, start=1, end=1)
table(data$t_race)
data$t_sex <- str_sub(data$stim_id, start=2, end=2)
table(data$t_sex)

length(unique(data$user_id))
length(unique(data$lab))
length(unique(data$country))
length(unique(data$region))


#subset by trait
aggressive <- subset(data, trait=="aggressive")
attractive <- subset(data, trait=="attractive")
caring <- subset(data, trait=="caring")
confident <- subset(data, trait=="confident")
dominant <- subset(data, trait=="dominant")
emostable <- subset(data, trait=="emostable")
intelligent <- subset(data, trait=="intelligent")
mean <- subset(data, trait=="mean")
responsible <- subset(data, trait=="responsible")
sociable <- subset(data, trait=="sociable")
trustworthy <- subset(data, trait=="trustworthy")
unhappy <- subset(data, trait=="unhappy")
weird <- subset(data, trait=="weird")

aggressive_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=aggressive)
attractive_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=attractive)
caring_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=caring)
confident_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=confident)
dominant_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=dominant)
emostable_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=emostable)
intelligent_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=intelligent)
mean_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=mean)
responsible_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=responsible)
sociable_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=sociable)
trustworthy_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=trustworthy)
unhappy_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=unhappy)
weird_big <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=weird)

###checking caring and intelligent convergence issues using brms

library(brms)
lmer <- function(...){brm(...,cores=4,iter=2000)}

caring_big_brms <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                   + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=caring)
intelligent_big_brms <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=intelligent)

summary(caring_big_brms)
summary(intelligent_big_brms)

###summaries of main models

summary(aggressive_big)
summary(attractive_big)
summary(caring_big)
summary(confident_big)
summary(dominant_big)
summary(emostable_big)
summary(intelligent_big)
summary(mean_big)
summary(responsible_big)
summary(sociable_big)
summary(trustworthy_big)
summary(unhappy_big)
summary(weird_big)

###tables

dir.create('tables')

tab_model(aggressive_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/agg.html")

tab_model(attractive_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/att.html")

tab_model(caring_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/car.html")

tab_model(confident_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/con.html")

tab_model(dominant_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/dom.html")

tab_model(emostable_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/emo.html")

tab_model(intelligent_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/int.html")

tab_model(mean_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/mea.html")

tab_model(responsible_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/res.html")

tab_model(sociable_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/soc.html")

tab_model(trustworthy_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/tru.html")

tab_model(unhappy_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/unh.html")

tab_model(weird_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", p.style="a",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/wei.html")


###graph results

library(readr)
library(readxl)
library(stats)
library(forcats)
library(patchwork)

  iccs <- read_xlsx("ICCs.xlsx", sheet="final")
  iccs_l <- pivot_longer(iccs, cols=c(ends_with("_icc"), ends_with("_l"), ends_with("_u")))
#write_csv(ICCs_l, "ICCs_l.csv") 
#has to be edited some manually in Excel to rearrange
  
iccs_l <- read_csv("ICCs_l.csv")

iccs_l$icc_l_orig <- iccs_l$icc_l

iccs_l$icc_l <- ifelse(iccs_l$icc_l < 0, 0, iccs_l$icc_l_orig)
summary(iccs_l$icc_l)

iccs_l$icc_type <- fct_recode(as.factor(iccs_l$icc_type), Perceiver = "p_icc",
                             Target = "t_icc", `Perceiver by Target` = "pt_icc",
                             Country = "c_icc", `Country by Target` = "ct_icc",
                             Region = "r_icc", `Region by Target` = "rt_icc")
iccs_l$icc_type <- fct_inorder(iccs_l$icc_type, ordered=TRUE)




dir.create("figures")

###FIGURE 2

dpi=300   #pixels per square inch
tiff("figures/iccs_viridis.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = iccs_l, mapping = aes(x = icc_type, y = icc, fill=trait)) +
  geom_col() +
  geom_errorbar(aes(ymin=icc_l, ymax=icc_u),width=0,cex=1.5) + 
  theme_minimal() +
  facet_wrap(~trait, ncol=2) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE)

dev.off()

###FIGURE 2

dpi=300   #pixels per square inch
tiff("figures/iccs_lighter.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = iccs_l, mapping = aes(x = icc_type, y = icc, fill=trait)) +
  geom_col() +
  geom_errorbar(aes(ymin=icc_l, ymax=icc_u),width=0,cex=1.5) + 
  theme_minimal() +
  facet_wrap(~trait, ncol=2) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_discrete_qualitative(palette= "Set 2")

dev.off()

#map spreadsheet for Figure 1

data_shorter <- data[-c(3:8)]
data_shortest <- data_shorter[-c(8)]
map <- data_shortest[!duplicated(data_shortest$user_id),]

map_count <- as.data.frame(table(map$country))
write_csv(map_count, "map_count.csv")

#summary statistics

demos <- data[!duplicated(data$user_id),]
table(demos$sex)
summary(demos$age)


#####BY RACE AND GENDER COMBOS

#AF

aggressive_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_race=="A" & t_sex=="F"))
attractive_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_race=="A" & t_sex=="F"))
caring_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                   + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_race=="A" & t_sex=="F"))
confident_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_race=="A" & t_sex=="F"))
dominant_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_race=="A" & t_sex=="F"))
emostable_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_race=="A" & t_sex=="F"))
intelligent_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_race=="A" & t_sex=="F"))
mean_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                 + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_race=="A" & t_sex=="F"))
responsible_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_race=="A" & t_sex=="F"))
sociable_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_race=="A" & t_sex=="F"))
trustworthy_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_race=="A" & t_sex=="F"))
unhappy_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_race=="A" & t_sex=="F"))
weird_big_af <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                  + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_race=="A" & t_sex=="F"))
options(scipen=99)

summary(aggressive_big_af)
summary(attractive_big_af)
summary(caring_big_af)
summary(confident_big_af)
summary(dominant_big_af)
summary(emostable_big_af)
summary(intelligent_big_af)
summary(mean_big_af)
summary(responsible_big_af)
summary(sociable_big_af)
summary(trustworthy_big_af)
summary(unhappy_big_af)
summary(weird_big_af)

#AM

aggressive_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_race=="A" & t_sex=="M"))
attractive_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_race=="A" & t_sex=="M"))
caring_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                   + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_race=="A" & t_sex=="M"))
confident_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_race=="A" & t_sex=="M"))
dominant_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_race=="A" & t_sex=="M"))
emostable_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_race=="A" & t_sex=="M"))
intelligent_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_race=="A" & t_sex=="M"))
mean_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                 + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_race=="A" & t_sex=="M"))
responsible_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_race=="A" & t_sex=="M"))
sociable_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_race=="A" & t_sex=="M"))
trustworthy_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_race=="A" & t_sex=="M"))
unhappy_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_race=="A" & t_sex=="M"))
weird_big_am <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                  + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_race=="A" & t_sex=="M"))

summary(aggressive_big_am)
summary(attractive_big_am)
summary(caring_big_am)
summary(confident_big_am)
summary(dominant_big_am)
summary(emostable_big_am)
summary(intelligent_big_am)
summary(mean_big_am)
summary(responsible_big_am)
summary(sociable_big_am)
summary(trustworthy_big_am)
summary(unhappy_big_am)
summary(weird_big_am)

#BF

aggressive_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_race=="B" & t_sex=="F"))
attractive_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_race=="B" & t_sex=="F"))
caring_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_race=="B" & t_sex=="F"))
confident_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_race=="B" & t_sex=="F"))
dominant_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_race=="B" & t_sex=="F"))
emostable_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_race=="B" & t_sex=="F"))
intelligent_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_race=="B" & t_sex=="F"))
mean_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_race=="B" & t_sex=="F"))
responsible_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_race=="B" & t_sex=="F"))
sociable_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_race=="B" & t_sex=="F"))
trustworthy_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_race=="B" & t_sex=="F"))
unhappy_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_race=="B" & t_sex=="F"))
weird_big_bf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_race=="B" & t_sex=="F"))

summary(aggressive_big_bf)
summary(attractive_big_bf)
summary(caring_big_bf)
summary(confident_big_bf)
summary(dominant_big_bf)
summary(emostable_big_bf)
summary(intelligent_big_bf)
summary(mean_big_bf)
summary(responsible_big_bf)
summary(sociable_big_bf)
summary(trustworthy_big_bf)
summary(unhappy_big_bf)
summary(weird_big_bf)

#BM

aggressive_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_race=="B" & t_sex=="M"))
attractive_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_race=="B" & t_sex=="M"))
caring_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_race=="B" & t_sex=="M"))
confident_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_race=="B" & t_sex=="M"))
dominant_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_race=="B" & t_sex=="M"))
emostable_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_race=="B" & t_sex=="M"))
intelligent_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_race=="B" & t_sex=="M"))
mean_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_race=="B" & t_sex=="M"))
responsible_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_race=="B" & t_sex=="M"))
sociable_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_race=="B" & t_sex=="M"))
trustworthy_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_race=="B" & t_sex=="M"))
unhappy_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_race=="B" & t_sex=="M"))
weird_big_bm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_race=="B" & t_sex=="M"))

summary(aggressive_big_bm)
summary(attractive_big_bm)
summary(caring_big_bm)
summary(confident_big_bm)
summary(dominant_big_bm)
summary(emostable_big_bm)
summary(intelligent_big_bm)
summary(mean_big_bm)
summary(responsible_big_bm)
summary(sociable_big_bm)
summary(trustworthy_big_bm)
summary(unhappy_big_bm)
summary(weird_big_bm)

#LF

aggressive_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_race=="L" & t_sex=="F"))
attractive_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_race=="L" & t_sex=="F"))
caring_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_race=="L" & t_sex=="F"))
confident_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_race=="L" & t_sex=="F"))
dominant_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_race=="L" & t_sex=="F"))
emostable_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_race=="L" & t_sex=="F"))
intelligent_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_race=="L" & t_sex=="F"))
mean_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_race=="L" & t_sex=="F"))
responsible_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_race=="L" & t_sex=="F"))
sociable_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_race=="L" & t_sex=="F"))
trustworthy_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_race=="L" & t_sex=="F"))
unhappy_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_race=="L" & t_sex=="F"))
weird_big_lf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_race=="L" & t_sex=="F"))

summary(aggressive_big_lf)
summary(attractive_big_lf)
summary(caring_big_lf)
summary(confident_big_lf)
summary(dominant_big_lf)
summary(emostable_big_lf)
summary(intelligent_big_lf)
summary(mean_big_lf)
summary(responsible_big_lf)
summary(sociable_big_lf)
summary(trustworthy_big_lf)
summary(unhappy_big_lf)
summary(weird_big_lf)

#LM

aggressive_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_race=="L" & t_sex=="M"))
attractive_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_race=="L" & t_sex=="M"))
caring_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_race=="L" & t_sex=="M"))
confident_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_race=="L" & t_sex=="M"))
dominant_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_race=="L" & t_sex=="M"))
emostable_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_race=="L" & t_sex=="M"))
intelligent_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_race=="L" & t_sex=="M"))
mean_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_race=="L" & t_sex=="M"))
responsible_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_race=="L" & t_sex=="M"))
sociable_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_race=="L" & t_sex=="M"))
trustworthy_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_race=="L" & t_sex=="M"))
unhappy_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_race=="L" & t_sex=="M"))
weird_big_lm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_race=="L" & t_sex=="M"))

summary(aggressive_big_lm)
summary(attractive_big_lm)
summary(caring_big_lm)
summary(confident_big_lm)
summary(dominant_big_lm)
summary(emostable_big_lm)
summary(intelligent_big_lm)
summary(mean_big_lm)
summary(responsible_big_lm)
summary(sociable_big_lm)
summary(trustworthy_big_lm)
summary(unhappy_big_lm)
summary(weird_big_lm)

#WF

aggressive_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_race=="W" & t_sex=="F"))
attractive_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_race=="W" & t_sex=="F"))
caring_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_race=="W" & t_sex=="F"))
confident_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_race=="W" & t_sex=="F"))
dominant_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_race=="W" & t_sex=="F"))
emostable_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_race=="W" & t_sex=="F"))
intelligent_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_race=="W" & t_sex=="F"))
mean_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_race=="W" & t_sex=="F"))
responsible_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_race=="W" & t_sex=="F"))
sociable_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_race=="W" & t_sex=="F"))
trustworthy_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_race=="W" & t_sex=="F"))
unhappy_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_race=="W" & t_sex=="F"))
weird_big_wf <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_race=="W" & t_sex=="F"))

summary(aggressive_big_wf)
summary(attractive_big_wf)
summary(caring_big_wf)
summary(confident_big_wf)
summary(dominant_big_wf)
summary(emostable_big_wf)
summary(intelligent_big_wf)
summary(mean_big_wf)
summary(responsible_big_wf)
summary(sociable_big_wf)
summary(trustworthy_big_wf)
summary(unhappy_big_wf)
summary(weird_big_wf)

#WM

aggressive_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_race=="W" & t_sex=="M"))
attractive_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_race=="W" & t_sex=="M"))
caring_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_race=="W" & t_sex=="M"))
confident_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_race=="W" & t_sex=="M"))
dominant_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_race=="W" & t_sex=="M"))
emostable_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_race=="W" & t_sex=="M"))
intelligent_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_race=="W" & t_sex=="M"))
mean_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_race=="W" & t_sex=="M"))
responsible_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_race=="W" & t_sex=="M"))
sociable_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_race=="W" & t_sex=="M"))
trustworthy_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_race=="W" & t_sex=="M"))
unhappy_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_race=="W" & t_sex=="M"))
weird_big_wm <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_race=="W" & t_sex=="M"))

summary(aggressive_big_wm)
summary(attractive_big_wm)
summary(caring_big_wm)
summary(confident_big_wm)
summary(dominant_big_wm)
summary(emostable_big_wm)
summary(intelligent_big_wm)
summary(mean_big_wm)
summary(responsible_big_wm)
summary(sociable_big_wm)
summary(trustworthy_big_wm)
summary(unhappy_big_wm)
summary(weird_big_wm)

#IF (inclusive)

aggressive_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_sex=="F"))
attractive_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_sex=="F"))
caring_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_sex=="F"))
confident_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_sex=="F"))
dominant_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_sex=="F"))
emostable_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_sex=="F"))
intelligent_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_sex=="F"))
mean_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_sex=="F"))
responsible_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_sex=="F"))
sociable_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_sex=="F"))
trustworthy_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_sex=="F"))
unhappy_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_sex=="F"))
weird_big_if <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_sex=="F"))

summary(aggressive_big_if)
summary(attractive_big_if)
summary(caring_big_if)
summary(confident_big_if)
summary(dominant_big_if)
summary(emostable_big_if)
summary(intelligent_big_if)
summary(mean_big_if)
summary(responsible_big_if)
summary(sociable_big_if)
summary(trustworthy_big_if)
summary(unhappy_big_if)
summary(weird_big_if)

#IM (inclusive)

aggressive_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(aggressive, t_sex=="M"))
attractive_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                          + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(attractive, t_sex=="M"))
caring_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                      + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(caring, t_sex=="M"))
confident_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(confident, t_sex=="M"))
dominant_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(dominant, t_sex=="M"))
emostable_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                         + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(emostable, t_sex=="M"))
intelligent_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(intelligent, t_sex=="M"))
mean_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                    + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(mean, t_sex=="M"))
responsible_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(responsible, t_sex=="M"))
sociable_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                        + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(sociable, t_sex=="M"))
trustworthy_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                           + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(trustworthy, t_sex=="M"))
unhappy_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                       + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(unhappy, t_sex=="M"))
weird_big_im <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + (1|country) 
                     + (1|stim_id:country) + (1|region) + (1|stim_id:region), data=subset(weird, t_sex=="M"))

summary(aggressive_big_im)
summary(attractive_big_im)
summary(caring_big_im)
summary(confident_big_im)
summary(dominant_big_im)
summary(emostable_big_im)
summary(intelligent_big_im)
summary(mean_big_im)
summary(responsible_big_im)
summary(sociable_big_im)
summary(trustworthy_big_im)
summary(unhappy_big_im)
summary(weird_big_im)

#graphing

library(readxl)

ALL <- read_xlsx("ICCs_no_cis.xlsx", sheet="ALL")
AF <- read_xlsx("ICCs_no_cis.xlsx", sheet="AF")
AM <- read_xlsx("ICCs_no_cis.xlsx", sheet="AM")
BF <- read_xlsx("ICCs_no_cis.xlsx", sheet="BF")
BM <- read_xlsx("ICCs_no_cis.xlsx", sheet="BM")
LF <- read_xlsx("ICCs_no_cis.xlsx", sheet="LF")
LM <- read_xlsx("ICCs_no_cis.xlsx", sheet="LM")
WF <- read_xlsx("ICCs_no_cis.xlsx", sheet="WF")
WM <- read_xlsx("ICCs_no_cis.xlsx", sheet="WM")
IF <- read_xlsx("ICCs_no_cis.xlsx", sheet="IF")
IM <- read_xlsx("ICCs_no_cis.xlsx", sheet="IM")

ICCS_subsets <- rbind(AF, AM, BF, BM, LF, LM, WF, WM, IF, IM, ALL)

ICCS_subsets_l <- pivot_longer(ICCS_subsets, cols=ends_with("_icc"), names_to="icc_type", values_to="icc")

ICCS_subsets_l$t_race <- as.factor(ICCS_subsets_l$t_race)
ICCS_subsets_l$t_race <- fct_inorder(ICCS_subsets_l$t_race, ordered=TRUE)

ICCS_subsets_l$t_sex <- as.factor(ICCS_subsets_l$t_sex)
ICCS_subsets_l$t_sex <- fct_inorder(ICCS_subsets_l$t_sex, ordered=TRUE)

ICCS_subsets_l$t_sexrace <- as.factor(ICCS_subsets_l$t_sexrace)
ICCS_subsets_l$t_sexrace <- fct_inorder(ICCS_subsets_l$t_sexrace, ordered=TRUE)

ICCS_subsets_l$icc_type <- fct_recode(as.factor(ICCS_subsets_l$icc_type), Perceiver = "p_icc",
                              Target = "t_icc", `Perceiver by Target` = "pt_icc",
                              Country = "c_icc", `Country by Target` = "ct_icc",
                              Region = "r_icc", `Region by Target` = "rt_icc")
ICCS_subsets_l$icc_type <- fct_inorder(ICCS_subsets_l$icc_type, ordered=TRUE)

###SM FIGURES

dpi=300   #pixels per square inch
tiff("figures/iccs_agg.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Aggressive"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Aggressive")

dev.off()

tiff("figures/iccs_agg.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Aggressive"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Aggressive")

dev.off()

tiff("figures/iccs_att.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Attractive"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Attractive")

dev.off()

tiff("figures/iccs_car.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Caring"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Caring")

dev.off()

tiff("figures/iccs_con.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Confident"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Confident")

dev.off()

tiff("figures/iccs_dom.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Dominant"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Dominant")

dev.off()

tiff("figures/iccs_emo.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Emotionally Stable"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Emotionally Stable")

dev.off()

tiff("figures/iccs_int.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Intelligent"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Intelligent")

dev.off()

tiff("figures/iccs_mea.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Mean"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Mean")

dev.off()

tiff("figures/iccs_res.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Responsible"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Responsible")

dev.off()

tiff("figures/iccs_soc.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Sociable"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Sociable")

dev.off()

tiff("figures/iccs_tru.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Trustworthy"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Trustworthy")

dev.off()

tiff("figures/iccs_unh.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Unhappy"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Unhappy")

dev.off()

tiff("figures/iccs_wei.tif", width=9*dpi, height=11*dpi, res=dpi)

ggplot(data = subset(ICCS_subsets_l, trait=="Weird"), mapping = aes(x = icc_type, y = icc, fill=t_sexrace)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~t_race*t_sex, ncol=2, nrow=6) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        plot.title = element_text(size=21, hjust=.5),
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE) +
  ggtitle("Weird")

dev.off()



#SUPPLEMENTAL LANGUAGE ANALYSES; these show that using language as a proxy for culture yields similarly small ICCs
#for both language and language-by-target clusters

aggressive_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                          (1|language) + (1|language:stim_id), data=aggressive)
attractive_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                          (1|language) + (1|language:stim_id), data=attractive)
caring_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                      (1|language) + (1|language:stim_id), data=caring)
confident_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                         (1|language) + (1|language:stim_id), data=confident)
dominant_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                        (1|language) + (1|language:stim_id), data=dominant)
emostable_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                         (1|language) + (1|language:stim_id), data=emostable)
intelligent_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                           (1|language) + (1|language:stim_id), data=intelligent)
mean_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                    (1|language) + (1|language:stim_id), data=mean)
responsible_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                           (1|language) + (1|language:stim_id), data=responsible)
sociable_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                        (1|language) + (1|language:stim_id), data=sociable)
trustworthy_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                           (1|language) + (1|language:stim_id), data=trustworthy)
unhappy_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                       (1|language) + (1|language:stim_id), data=unhappy)
weird_lang <- lmer(rating ~ 1 + (1|stim_id) + (1|user_id)  + (1|stim_id:user_id) + 
                     (1|language) + (1|language:stim_id), data=weird)
