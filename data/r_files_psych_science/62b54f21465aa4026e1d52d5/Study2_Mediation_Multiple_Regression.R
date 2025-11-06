# last update: 10/3/2021
# DongWon Oh (Columbia University)
#
# Mediation analysis in Study 2
# "Personality in the social environment predicts personality trait judgments from faces"
# by DongWon Oh, Jared D. Martin, and Jonathan B. Freeman
# in press in Psychological Science

# regressions including mediation analysis

rm(list=ls())
library(tidyverse)
library(psych)
library(mediation)

source("r_output.R")

rootdir <- './'
datadir <- file.path(rootdir,'')

##########################################################################################
# load data

allcor_dist <- read_csv(file.path(datadir,"Study2_CorrelationsOfDistances.csv"))
allcor_dist$face1rank <- allcor_dist$face1 %>% rank(na.last='keep')
allcor_dist$face2rank <- allcor_dist$face2 %>% rank(na.last='keep')
allcor_dist$concrank <- allcor_dist$conc %>% rank(na.last='keep')
allcor_dist$corNEOPIrank <- allcor_dist$corNEOPI_US %>% rank(na.last='keep')


##########################################################################################
# Correlation analyses

# involving face ratings acquired using trait words
cor.fit <- cor.test(allcor_dist$corNEOPIrank,allcor_dist$face1rank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))
cor.fit <- cor.test(allcor_dist$corNEOPIrank,allcor_dist$concrank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))
cor.fit <- cor.test(allcor_dist$concrank,allcor_dist$face1rank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))

# involving face ratings acquired using trait phrase
cor.fit <- cor.test(allcor_dist$corNEOPIrank,allcor_dist$face2rank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))
cor.fit <- cor.test(allcor_dist$corNEOPIrank,allcor_dist$concrank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))
cor.fit <- cor.test(allcor_dist$concrank,allcor_dist$face2rank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))

# covariates
cor.fit <- cor.test(allcor_dist$rankSimASJP,allcor_dist$face1rank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))
cor.fit <- cor.test(allcor_dist$rankSimASJP,allcor_dist$concrank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))

cor.fit <- cor.test(allcor_dist$rankSimTFL,allcor_dist$face1rank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))
cor.fit <- cor.test(allcor_dist$rankSimTFL,allcor_dist$concrank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))


cor.fit <- cor.test(allcor_dist$rankSimHHI,allcor_dist$face1rank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))
cor.fit <- cor.test(allcor_dist$rankSimHHI,allcor_dist$concrank,method="spearman",exact=F)
print_cor_result(cor.fit,n=nrow(allcor_dist))

##########################################################################################
##########################################################################################
# Mediation analysis via single regression models

##################
### DV1 (face impressions obtained using trait words)

fit.iv_dv <- lm(face1rank ~ corNEOPIrank, data=allcor_dist)
print_lm_result(fit.iv_dv,"corNEOPIrank",summary=T)
# B=0.39, SE=0.06, 95% CI [0.28,0.51], t(229)=6.49, p<.001

fit.mediator <- lm(concrank ~ corNEOPIrank, data=allcor_dist)
print_lm_result(fit.mediator,"corNEOPIrank",summary=T)
# B=0.32, SE=0.06, 95% CI [0.20,0.44], t(229)=5.15, p<.001

fit.meddv.1 <- lm(face1rank ~ concrank + corNEOPIrank, data=allcor_dist)
print_lm_result(fit.meddv.1,"concrank",summary=T) 
# B=0.65, SE=0.05, 95% CI [0.55,0.74], t(228)=13.56, p<.001

med.results.1 <- mediate(fit.mediator, fit.meddv.1, treat='corNEOPIrank', mediator='concrank', boot=T, sims=1000)
med.results.1 %>% summary 


# a: from personality to concept
print(fit.mediator$coefficients["corNEOPIrank"] %>% round(2)) 

# b: from concept to face impressions
print(fit.meddv.1$coefficients["concrank"] %>% round(2)) 

# a * b
print((fit.mediator$coefficients["corNEOPIrank"] * fit.meddv.1$coefficients["concrank"])%>% round(2)) 


##################
### DV2 (face impressions obtained using trait phrases, rather than single trait words)

fit.iv_dv2 <- lm(face2rank ~ corNEOPIrank, data=allcor_dist)
fit.iv_dv2 %>% summary
print_lm_result(fit.iv_dv2,var="corNEOPIrank",summary=T)

fit.meddv.2 <- lm(face2rank ~ concrank + corNEOPIrank, data=allcor_dist)
print_lm_result(fit.meddv.2,var="corNEOPIrank",summary=T)

print_lm_result(fit.meddv.2,var="concrank",summary=T)

med.results.2 <- mediate(fit.mediator, fit.meddv.2, treat='corNEOPIrank', mediator='concrank', boot=T, sims=1000)
med.results.2 %>% summary      


# a: from personality to concept
print(fit.mediator$coefficients["corNEOPIrank"] %>% round(2)) # 0.32

# b: from concept to face impressions
print(fit.meddv.2$coefficients["concrank"] %>% round(2))  # 0.75

# a * b
print((fit.mediator$coefficients["corNEOPIrank"] * fit.meddv.2$coefficients["concrank"])%>% round(2)) # 0.24


########################################################################################
# Mediation analysis via multiple regression models

##################
### Mediation via multiple regressions involving DV1  (face impressions obtained using trait words)

fit.iv_dv1 <- lm(rankface1 ~ rankcorNEOPI + rankSimASJP + rankSimTFL + rankSimHHI, allcor_dist)
print_lm_result(fit.iv_dv1,"rankcorNEOPI",digit=2,summary=T)
# b=0.32, SE=0.09, 95% CI [0.14,0.51], t(134)=3.42, p=.001

View(round(   summary(fit.iv_dv1)$coefficients   ,3))
confint(fit.iv_dv1) %>% round(3) %>% View # 95% CI 


fit.mediator <- lm(rankconc ~ rankcorNEOPI + rankSimASJP + rankSimTFL + rankSimHHI, data=allcor_dist)
print_lm_result(fit.mediator,"rankcorNEOPI",summary=T)
# b=0.32, SE=0.09, 95% CI [0.14,0.51], t(134)=3.47, p=.001

View(round(   summary(fit.mediator)$coefficients   ,3))
confint(fit.mediator) %>% round(3) %>% View  # 95% CI 

fit.meddv.1 <- lm(rankface1 ~ rankconc + 
                              rankcorNEOPI + rankSimASJP + rankSimTFL + rankSimHHI, data=allcor_dist)
print_lm_result(fit.meddv.1,"rankconc",summary=T)
# b=0.70, SE=0.06, 95% CI [0.58,0.82], t(133)=11.06, p<.001

print_lm_result(fit.meddv.1,"rankcorNEOPI",summary=T)

View(round(   summary(fit.meddv.1)$coefficients   ,3))
confint(fit.meddv.1) %>% round(3) %>% View # 95% CI 


# formal mediation test
med.results.1 <- mediate(fit.mediator, fit.meddv.1, treat='rankcorNEOPI', mediator='rankconc', boot=T, sims=1000)
med.results.1 %>% summary  

# a: from personality to concept
print(fit.mediator$coefficients["rankcorNEOPI"] %>% round(2))

# b: from concept to face impressions
print(fit.meddv.1$coefficients["rankconc"] %>% round(2))

# a * b
print((fit.mediator$coefficients["rankcorNEOPI"] * fit.meddv.1$coefficients["rankconc"])%>% round(2))

# `d`. DV ~ IV + Covariates (trait phrases used to collect face judgments to compute DV)
fit.dv.1 <- lm(rankface1 ~ rankcorNEOPI + rankSimASJP + rankSimTFL + rankSimHHI, data=allcor_dist)
View(round(   summary(fit.dv.1)$coefficients   ,3))


##################
### Mediation via multiple regression involving DV2 (complementary DV)
### (face impressions obtained using trait phrases, rather than single trait words)

fit.iv_dv2 <- lm(rankface2 ~ rankcorNEOPI + rankSimASJP + rankSimTFL + rankSimHHI, allcor_dist)
print_lm_result(fit.iv_dv2,"rankcorNEOPI",summary=T)
# b=0.29, SE=0.09, 95% CI [0.1,0.47], t(134)=3.08, p=.003

print_lm_result(fit.iv_dv2,"rankcorNEOPI",digit=2,summary=T)
# B=0.29, SE=0.09, 95% CI [0.1,0.47], t(134)=3.08, p=.003
View(round(   summary(fit.iv_dv2)$coefficients   ,3))
confint(fit.iv_dv2) %>% round(3) %>% View # 95% CI 


fit.meddv.2 <- lm(rankface2 ~ rankconc + rankcorNEOPI + rankSimASJP + rankSimTFL + rankSimHHI, data=allcor_dist) 
print_lm_result(fit.meddv.2,"rankcorNEOPI",summary=T)
# b=0.04, SE=0.06, 95% CI [-0.08,0.17], t=0.67, p=.505

View(round(   summary(fit.meddv.2)$coefficients   ,3))


med.results.2 <- mediate(fit.mediator, fit.meddv.2, treat='rankcorNEOPI', mediator='rankconc', boot=T, sims=1000)
med.results.2 %>% summary 


# a: from personality to concept
print(fit.mediator$coefficients["rankcorNEOPI"] %>% round(2)) # 0.32

# b: from concept to face impressions
print(fit.meddv.2$coefficients["rankconc"] %>% round(2))  # 0.76

# a * b (indirect effect)
print((fit.mediator$coefficients["rankcorNEOPI"] * fit.meddv.2$coefficients["rankconc"])%>% round(2)) # 0.25

