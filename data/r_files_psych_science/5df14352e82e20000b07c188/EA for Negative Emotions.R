# (1) ANALYSIS PREP ####

## Load general libraries
library(foreign)
library(psych)
library(plyr)

## Read in files
lab.df <- read.spss(file.choose(), use.value.labels = FALSE, to.data.frame = TRUE)  # Select "Lab File.sav"


# (2) COMPUTE VARIABLES ####

# (2a) Compute Predictor Variables ####

## Target's appeasement emotions
with(lab.df, describe(cbind(L_CL_A_emo_16, L_CL_A_emo_15, L_CL_A_emo_14)))  # descriptives
lab.df$target.appease <- with(lab.df, rowMeans(cbind(L_CL_A_emo_16, L_CL_A_emo_15, L_CL_A_emo_14), na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_CL_A_emo_16, L_CL_A_emo_15, L_CL_A_emo_14)))  # check reliability

## Agent's perception of target's appeasement emotions
with(lab.df, describe(cbind(L_C_P_emo_16, L_C_P_emo_15, L_C_P_emo_14)))  # descriptives
lab.df$agent.perc.appease <- with(lab.df, rowMeans(cbind(L_C_P_emo_16, L_C_P_emo_15, L_C_P_emo_14), na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_C_P_emo_16, L_C_P_emo_15, L_C_P_emo_14)))  # check reliability

# Agent's appeasement emotions
with(lab.df, describe(cbind(L_C_A_emo_16, L_C_A_emo_15, L_C_A_emo_14)))  # descriptives
lab.df$agent.appease <- with(lab.df, rowMeans(cbind(L_C_A_emo_16, L_C_A_emo_15, L_C_A_emo_14, na.rm = T)))  # create composite
with(lab.df, alpha(cbind(L_C_A_emo_16, L_C_A_emo_15, L_C_A_emo_14)))  # check reliability

# Target's perception of agent's appeasement emotions 
with(lab.df, describe(cbind(L_CL_P_emo_16, L_CL_P_emo_15, L_CL_P_emo_14)))  # descriptives
lab.df$target.perc.appease <- with(lab.df, rowMeans(cbind(L_CL_P_emo_16, L_CL_P_emo_15, L_CL_P_emo_14),na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_CL_P_emo_16, L_CL_P_emo_15, L_CL_P_emo_14)))  # check reliability

# Target's dominance emotions
with(lab.df, describe(cbind(L_CL_A_emo_1, L_CL_A_emo_2, 
                            L_CL_A_emo_10, L_CL_A_emo_3, 
                            L_CL_A_emo_9, L_CL_A_emo_5)))  # descriptives
lab.df$target.dominance <- with(lab.df, rowMeans(cbind(L_CL_A_emo_1, L_CL_A_emo_2, 
                                                      L_CL_A_emo_10, L_CL_A_emo_3, 
                                                      L_CL_A_emo_9, L_CL_A_emo_5), na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_CL_A_emo_1, L_CL_A_emo_2, 
                         L_CL_A_emo_10, L_CL_A_emo_3, 
                         L_CL_A_emo_9, L_CL_A_emo_5)))  # check reliability

# Agent's perception of target's dominance emotions
with(lab.df, describe(cbind(L_C_P_emo_1, L_C_P_emo_2, 
                            L_C_P_emo_10, L_C_P_emo_3, 
                            L_C_P_emo_9, L_C_P_emo_5)))  # descriptives
lab.df$agent.perc.dominance <- with(lab.df, rowMeans(cbind(L_C_P_emo_1, L_C_P_emo_2, 
                                                          L_C_P_emo_10, L_C_P_emo_3, 
                                                          L_C_P_emo_9, L_C_P_emo_5), na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_C_P_emo_1, L_C_P_emo_2, 
                         L_C_P_emo_10, L_C_P_emo_3, 
                         L_C_P_emo_9, L_C_P_emo_5)))  # check reliability

# Agent's dominance emotions 
with(lab.df, describe(cbind(L_C_A_emo_1, L_C_A_emo_2, 
                            L_C_A_emo_10, L_C_A_emo_3, 
                            L_C_A_emo_9, L_C_A_emo_5)))  # descriptives
lab.df$agent.dominance <- with(lab.df, rowMeans(cbind(L_C_A_emo_1, L_C_A_emo_2, 
                                                     L_C_A_emo_10, L_C_A_emo_3, 
                                                     L_C_A_emo_9, L_C_A_emo_5), na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_C_A_emo_1, L_C_A_emo_2, 
                         L_C_A_emo_10, L_C_A_emo_3, 
                         L_C_A_emo_9, L_C_A_emo_5)))  # check reliability

# Target's perception of agent's dominance emotions 
with(lab.df, describe(cbind(L_CL_P_emo_1, L_CL_P_emo_2, 
                            L_CL_P_emo_10, L_CL_P_emo_3, 
                            L_CL_P_emo_9, L_CL_P_emo_5)))  # descriptives
lab.df$target.perc.dominance <- with(lab.df, rowMeans(cbind(L_CL_P_emo_1, L_CL_P_emo_2, 
                                                           L_CL_P_emo_10, L_CL_P_emo_3, 
                                                           L_CL_P_emo_9, L_CL_P_emo_5), na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_CL_P_emo_1, L_CL_P_emo_2, 
                         L_CL_P_emo_10, L_CL_P_emo_3, 
                         L_CL_P_emo_9, L_CL_P_emo_5)))  # check reliability


## (2b) Compute Criteria Variables ####

# Agent relationship quality (self-reported)
lab.df$agent.rs <- lab.df$L_C_outcom_7  # rename agent relationship quality variables
lab.df$agent.close <- lab.df$L_C_outcom_8
lab.df$agent.tension <- 8 - lab.df$L_C_outcom_9
lab.df$agent.conflict <- 8 - lab.df$L_C_outcom_10
with(lab.df, describe(cbind(agent.rs, agent.close, agent.tension, agent.conflict)))  # descriptives
with(lab.df, alpha(cbind(agent.rs, agent.close, agent.tension, agent.conflict), check.keys = T))  # check reliability
lab.df$agent.rq <- with(lab.df, rowMeans(cbind(agent.rs, agent.close, agent.tension, agent.conflict), na.rm = T))  # create composite

# Target relationship quality (self-reported)
lab.df$target.rs <- lab.df$L_CL_outco_7  # rename target relationship quality variables
lab.df$target.close <- lab.df$L_CL_outco_8
lab.df$target.tension <- 8 - lab.df$L_CL_outco_9
lab.df$target.conflict <- 8 - lab.df$L_CL_outco_10
with(lab.df, describe(cbind(target.rs, target.close, target.tension, target.conflict)))  #  descriptives
with(lab.df, alpha(cbind(target.rs, target.close, target.tension, target.conflict), check.keys = T))  # check reliability
lab.df$target.rq <- with(lab.df, rowMeans(cbind(target.rs, target.close, target.tension, target.conflict), na.rm = T))  # create composite

# Load library for interrater reliability
library(irr)

# Agent relationship satisfaction (objective code)
lab.df$agent.rs.obj <- with(lab.df, rowMeans(cbind(Rel_Happy_Satisfied_Person_NK_CS,
                                                   Rel_Happy_Satisfied_Person_RR_CS,
                                                   Rel_Happy_Satisfied_Person_SY_CS,
                                                   Rel_Happy_Satisfied_Person_TM_CS), na.rm = TRUE))
with(lab.df, icc(cbind(Rel_Happy_Satisfied_Person_NK_CS,
                       Rel_Happy_Satisfied_Person_RR_CS,
                       Rel_Happy_Satisfied_Person_SY_CS,
                       Rel_Happy_Satisfied_Person_TM_CS), model = "twoway", type = "agreement",unit = "average"))

# Target relationship satisfaction (objective code)
lab.df$target.rs.obj <- with(lab.df, rowMeans(cbind(Rel_Happy_Satisified_Person_ECL_CL,
                                                    Rel_Happy_Satisified_Person_FM_CL,
                                                    Rel_Happy_Satisified_Person_NK_CL,
                                                    Rel_Happy_Satisified_Person_SY_CL), na.rm = TRUE))
with(lab.df, icc(cbind(Rel_Happy_Satisified_Person_ECL_CL,
                       Rel_Happy_Satisified_Person_FM_CL,
                       Rel_Happy_Satisified_Person_NK_CL,
                       Rel_Happy_Satisified_Person_SY_CL), model = "twoway", type = "agreement", unit = "average"))

# Couple conflict (objective code)
lab.df$couple.conflict.obj <- with(lab.df, rowMeans(cbind(Conflict_ECL_CL,
                                                          Conflict_FM_CL,
                                                          Conflict_NK_CL,
                                                          Conflict_SY_CL), na.rm = TRUE))
with(lab.df, icc(cbind(Conflict_ECL_CL,
                       Conflict_FM_CL,
                       Conflict_NK_CL,
                       Conflict_SY_CL), model = "twoway", type = "agreement", unit = "average"))

# Target motivation to change
lab.df$target.change.motivation <- lab.df$L_CL_effor  # rename variable
describe(lab.df$target.change.motivation)  # check descriptives

# Create Couple Relationship Quality Composite
lab.df$r.conflict.obj <- 6 - lab.df$couple.conflict.obj  # reverse score conflict

lab.df$z.agent.rq <- scale(lab.df$agent.rq) 
lab.df$z.agent.rs.obj <- scale(lab.df$agent.rs.obj) 
lab.df$z.target.rq <- scale(lab.df$target.rq) 
lab.df$z.target.rs.obj <- scale(lab.df$target.rs.obj) 
lab.df$z.r.conflict.obj<- scale(lab.df$r.conflict.obj) 

with(lab.df, describe(cbind(z.agent.rq, z.agent.rs.obj,
                            z.target.rq, z.target.rs.obj, z.r.conflict.obj)))

lab.df$z.subj.couple.rq <- with(lab.df, rowMeans(cbind(z.agent.rq, z.target.rq), na.rm = T))
lab.df$z.obj.couple.rq <- with(lab.df, rowMeans(cbind(z.agent.rs.obj, z.target.rs.obj, z.r.conflict.obj), na.rm = T))
lab.df$z.couple.rq <- with(lab.df, rowMeans(cbind(z.subj.couple.rq, z.obj.couple.rq), na.rm = T))

with(lab.df, cor.test(z.subj.couple.rq, z.obj.couple.rq))

with(lab.df, describe(cbind(z.subj.couple.rq, z.obj.couple.rq, z.couple.rq)))


## (2c) Compute Control Variables ####

# Baseline Relationship Quality
lab.df$baseline.rq <- with(lab.df, rowMeans(cbind(l_neut_rs_1, l_neut_rs_2, 8 - l_neut_rs_3, 8 - l_neut_rs_4)))
with(lab.df, corr.test(cbind(baseline.rq, agent.rq, target.rq), use="complete.obs", adjust="none"))

lab.df$r.l_neut_rs_3 <- 8 - lab.df$l_neut_rs_3
lab.df$r.l_neut_rs_4 <- 8 - lab.df$l_neut_rs_4
with(lab.df, alpha(cbind(l_neut_rs_1, l_neut_rs_2, r.l_neut_rs_3, r.l_neut_rs_4)))  # Check reliability

lab.df$c.baseline.rq <- lab.df$baseline.rq - mean(lab.df$baseline.rq, na.rm = T)

p.baseline.rq <- c()
for (i in 1:(nrow(lab.df)/2)){
  p.baseline.rq <- c(p.baseline.rq, lab.df$baseline.rq[2*i], lab.df$baseline.rq[2*i-1])
}  # create partner RQ variable

lab.df$p.baseline.rq <- p.baseline.rq  # Add partner baseline RQ to lab.df

with(lab.df, cor.test(baseline.rq, p.baseline.rq))  # Chekc correlation

lab.df$couple.baseline.rq <- with(lab.df, rowMeans(cbind(baseline.rq, p.baseline.rq), na.rm = T))
lab.df$c.couple.baseline.rq <- lab.df$couple.baseline.rq - mean(lab.df$couple.baseline.rq, na.rm = T)
describe(lab.df$couple.baseline.rq)
describe(lab.df$c.couple.baseline.rq)

# Issue severity
lab.df$severity.obj <- with(lab.df, rowMeans(cbind(lab.df$Issue_Severity_NK_CS,
                                                   lab.df$Issue_Severity_RR_CS,
                                                   lab.df$Issue_Severity_SY_CS,
                                                   lab.df$Issue_Severity_TM_CS), na.rm = TRUE))
with(lab.df, icc(cbind(lab.df$Issue_Severity_NK_CS,
                       lab.df$Issue_Severity_RR_CS,
                       lab.df$Issue_Severity_SY_CS,
                       lab.df$Issue_Severity_TM_CS), model = "twoway", type = "agreement",unit = "average"))

describe(lab.df$severity.obj)

lab.df$c.severity.obj <- lab.df$severity.obj - mean(lab.df$severity.obj, na.rm = T)


## (3) VARIABLE DESCRIPTIVES AND CORRELATIONS #####

with(lab.df, describe(cbind(agent.appease, agent.perc.appease,
                            agent.dominance, agent.perc.dominance,
                            target.appease, target.perc.appease,
                            target.dominance, target.perc.dominance,
                            target.change.motivation, couple.rq, baseline.rq, severity.obj)))

corr.results <- with(lab.df, corr.test(cbind(agent.appease, agent.perc.appease,
                                             agent.dominance, agent.perc.dominance,
                                             target.appease, target.perc.appease,
                                             target.dominance, target.perc.dominance,
                                             target.change.motivation, couple.rq, baseline.rq, severity.obj), use="complete.obs", adjust="none"))
print(corr.results, digits = 4)


## (4) COMPUTE VARIABLES FOR POLYNOMIAL REGRESSION ####

# Agent Perceived Appeasement Emotion Variables

# Center predictor terms on grand mean across both congruence variables
lab.df$grand.M <- with(lab.df, mean(c(target.appease, agent.perc.appease), na.rm = T))
lab.df$c.target.appease <- lab.df$target.appease - lab.df$grand.M
lab.df$c.agent.perc.appease <- lab.df$agent.perc.appease - lab.df$grand.M

# Compute higher order terms based on centered predictors
lab.df$c.target.appease.sq <- lab.df$c.target.appease^2
lab.df$c.agent.perc.appease.sq <- lab.df$c.agent.perc.appease^2
lab.df$c.ag.perc.tar.appease.int <- lab.df$c.target.appease * lab.df$c.agent.perc.appease


# Target Perceived Appeasement Emotion Variables

# Center predictor terms on grand mean across both congruence variables
lab.df$grand.M <- with(lab.df, mean(c(agent.appease, target.perc.appease), na.rm = T))
lab.df$c.agent.appease <- lab.df$agent.appease - lab.df$grand.M
lab.df$c.target.perc.appease <- lab.df$target.perc.appease - lab.df$grand.M

# Compute higher order terms based on centered predictors
lab.df$c.agent.appease.sq <- lab.df$c.agent.appease^2
lab.df$c.target.perc.appease.sq <- lab.df$c.target.perc.appease^2
lab.df$c.tar.perc.ag.appease.int <- lab.df$c.agent.appease * lab.df$c.target.perc.appease


# Agent Perceived Dominance Emotion Variables

# Center predictor terms on grand mean across both congruence variables
lab.df$grand.M <- with(lab.df, mean(c(target.dominance, agent.perc.dominance), na.rm = T))
lab.df$c.target.dominance <- lab.df$target.dominance - lab.df$grand.M
lab.df$c.agent.perc.dominance <- lab.df$agent.perc.dominance - lab.df$grand.M

# Compute higher order terms based on centered predictors
lab.df$c.target.dominance.sq <- lab.df$c.target.dominance^2
lab.df$c.agent.perc.dominance.sq <- lab.df$c.agent.perc.dominance^2
lab.df$c.ag.perc.tar.dominance.int <- lab.df$c.target.dominance * lab.df$c.agent.perc.dominance


# Target Perceived Dominance Emotion Variables

# Center predictor terms on grand mean across both congruence variables
lab.df$grand.M <- with(lab.df, mean(c(agent.dominance, target.perc.dominance), na.rm = T))
lab.df$c.agent.dominance <- lab.df$agent.dominance - lab.df$grand.M
lab.df$c.target.perc.dominance <- lab.df$target.perc.dominance - lab.df$grand.M

# Compute higher order terms based on centered predictors
lab.df$c.agent.dominance.sq <- lab.df$c.agent.dominance^2
lab.df$c.target.perc.dominance.sq <- lab.df$c.target.perc.dominance^2
lab.df$c.tar.perc.ag.dominance.int <- lab.df$c.agent.dominance * lab.df$c.target.perc.dominance


# Create Aggregates
lab.df$dyad.id <- as.numeric(substr(lab.df$Couple_ID,1,3))  # remove letters from coupleID
lab.df$dyad.id   # check dyadic variables

agg.lab.df <- ddply(lab.df, c("dyad.id"), summarize, 
                    target.appease.M = mean(target.appease, na.rm = T),
                    agent.perc.appease.M = mean(agent.perc.appease, na.rm = T),
                    target.dominance.M = mean(target.dominance, na.rm = T),
                    agent.perc.dominance.M = mean(agent.perc.dominance, na.rm = T),
                    agent.appease.M = mean(agent.appease, na.rm = T),
                    target.perc.appease.M = mean(target.perc.appease, na.rm = T),
                    agent.dominance.M = mean(agent.dominance, na.rm = T),
                    target.perc.dominance.M = mean(target.perc.dominance, na.rm = T))  # create aggregates by couple ID

full.lab.df <- merge(lab.df, agg.lab.df, by ="dyad.id")  # merge aggregates with main df


# Aggregate Agent Perceived Appeasement Emotion Variables

# Center predictor terms on grand mean across both congruence variables
full.lab.df$grand.M <- with(full.lab.df, mean(c(target.appease.M, agent.perc.appease.M), na.rm = T))
full.lab.df$c.target.appease.M <- full.lab.df$target.appease.M - full.lab.df$grand.M
full.lab.df$c.agent.perc.appease.M <- full.lab.df$agent.perc.appease.M - full.lab.df$grand.M

# Compute higher order terms based on centered predictors
full.lab.df$c.target.appease.sq.M <- full.lab.df$c.target.appease.M^2
full.lab.df$c.agent.perc.appease.sq.M <- full.lab.df$c.agent.perc.appease.M^2
full.lab.df$c.ag.perc.tar.appease.int.M <- full.lab.df$c.agent.perc.appease.M * full.lab.df$c.target.appease.M


# Aggregate Target Perceived Appeasement Emotion Variables

# Center predictor terms on grand mean across both congruence variables
full.lab.df$grand.M <- with(full.lab.df, mean(c(agent.appease.M, target.perc.appease.M), na.rm = T))
full.lab.df$c.agent.appease.M <- full.lab.df$agent.appease.M - full.lab.df$grand.M
full.lab.df$c.target.perc.appease.M <- full.lab.df$target.perc.appease.M - full.lab.df$grand.M

# Compute higher order terms based on centered predictors
full.lab.df$c.agent.appease.sq.M <- full.lab.df$c.agent.appease.M^2
full.lab.df$c.target.perc.appease.sq.M <- full.lab.df$c.target.perc.appease.M^2
full.lab.df$c.tar.perc.ag.appease.int.M <- full.lab.df$c.target.perc.appease.M * full.lab.df$c.agent.appease.M


# Aggregate Agent Perceived Domninance Emotion Variables

# Center predictor terms on grand mean across both congruence variables
full.lab.df$grand.M <- with(full.lab.df, mean(c(target.dominance.M, agent.perc.dominance.M), na.rm = T))
full.lab.df$c.target.dominance.M <- full.lab.df$target.dominance.M - full.lab.df$grand.M
full.lab.df$c.agent.perc.dominance.M <- full.lab.df$agent.perc.dominance.M - full.lab.df$grand.M

# Compute higher order terms based on centered predictors
full.lab.df$c.target.dominance.sq.M <- full.lab.df$c.target.dominance.M^2
full.lab.df$c.agent.perc.dominance.sq.M <- full.lab.df$c.agent.perc.dominance.M^2
full.lab.df$c.ag.perc.tar.dominance.int.M <- full.lab.df$c.agent.perc.dominance.M * full.lab.df$c.target.dominance.M


# Aggregate Target Perceived Domninance Emotion Variables

# Center predictor terms on grand mean across both congruence variables
full.lab.df$grand.M <- with(full.lab.df, mean(c(agent.dominance.M, target.perc.dominance.M), na.rm = T))
full.lab.df$c.agent.dominance.M <- full.lab.df$agent.dominance.M - full.lab.df$grand.M
full.lab.df$c.target.perc.dominance.M <- full.lab.df$target.perc.dominance.M - full.lab.df$grand.M

# Compute higher order terms based on centered predictors
full.lab.df$c.agent.dominance.sq.M <- full.lab.df$c.agent.dominance.M^2
full.lab.df$c.target.perc.dominance.sq.M <- full.lab.df$c.target.perc.dominance.M^2
full.lab.df$c.tar.perc.ag.dominance.int.M <- full.lab.df$c.target.perc.dominance.M * full.lab.df$c.agent.dominance.M


## (5) AGENT EMPATHIC ACCURACY FOR TARGET'S APPEASEMENT EMOTIONS ####

# LIBRARIES FOR MAIN ANALYSIS
library(RSA)
library(lme4)
library(lmerTest)
source("MultilevelRSA.r")  # This file with ML-RSA functions (Nestler, Humberg, & Schönbrodt, 2018) can be downloaded at osf.io/jhyu9

## (5a) Couple Relationship Quality ####

## Polynomial Model 

poly.model <- lmer(z.couple.rq ~ c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.appease, c.agent.perc.appease, z.couple.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 0.141288, x = 0.028819 , y = -0.04667, x2 = -0.010214, xy = 0.017744, y2 = -0.015488,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Target Appeasement Emotions", ylab = "Agent Perceived\nAppeasement Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results



## Polynomial Model (Baseline Couple Relationship Quality Control)

poly.model <- lmer(z.couple.rq ~ c.couple.baseline.rq + c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Baseline Couple Relationship Quality Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


## Polynomial Model (Issue Severity Control)

poly.model <- lmer(z.couple.rq ~ c.severity.obj + c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Issue Severity Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 



## (5b) Target's Motivation to Change ####

## Polynomial Model 
poly.model <- lmer(target.change.motivation ~ c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.appease, c.agent.perc.appease, target.change.motivation))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.685237, x = 0.113610, y = -0.061069, x2 = 0.004476, xy = -0.038088, y2 = 0.015506,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Target Appeasement Emotions", ylab = "Agent Perceived\nAppeasement Emotions", zlab = "Target Motivation to Change",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## Polynomial Model (Baseline Couple Relationship Quality Control)
poly.model <- lmer(target.change.motivation ~ c.couple.baseline.rq + c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Baseline Couple Relationship Quality Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 



## Polynomial Model (Issue Severity Control)
poly.model <- lmer(target.change.motivation ~ c.severity.obj + c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Issue Severity Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 



## (6) TARGET EMPATHIC ACCURACY FOR AGENT'S APPEASEMENT EMOTIONS ####

# (6a) Couple Relationship Quality ####

## Polynomial Model 
poly.model <- lmer(z.couple.rq ~ c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, z.couple.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 0.08588, x = 0.05120 , y = -0.04716, x2 = -0.02524 , xy = 0.04270, y2 = -0.01999,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## Polynomial Model (Baseline Couple Relationship Quality Control)
poly.model <- lmer(z.couple.rq ~ c.couple.baseline.rq + c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Baseline Couple Relationship Quality Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


## Polynomial Model (Issue Severity Control)
poly.model <- lmer(z.couple.rq ~ c.severity.obj + c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Issue Severity Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


# RSA Plot (Issue Severity Control)
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, z.couple.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 0.050358, x = 0.098175 , y = -0.073055, x2 = -0.033090, xy = 0.027726, y2 = -0.008081,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results



## (6b) Target's Motivation to Change ####

## Polynomial Model 
poly.model <- lmer(target.change.motivation ~ c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, target.change.motivation))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.520118, x = 0.290687 , y = -0.039298, x2 = -0.033898, xy = 0.005019, y2 = 0.010613,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Target Change Motivation",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## Polynomial Model (Baseline Couple Relationship Quality Control)
poly.model <- lmer(target.change.motivation ~ c.couple.baseline.rq + c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Baseline Couple Relationship Quality Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


## Polynomial Model (Issue Severity Control)
poly.model <- lmer(target.change.motivation ~ c.severity.obj + c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Issue Severity Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 



## (7) AGENT'S EMPATHIC ACCURACY FOR TARGET'S DOMINANCE EMOTIONS ####

# (7a) Couple Relationship Quality ####

## Polynomial Model 
poly.model <- lmer(z.couple.rq ~ c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.dominance, c.agent.perc.dominance, z.couple.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = -0.127048, x = -0.082553 , y = -0.020166, x2 = 0.010000, xy = -0.014251, y2 = 0.003994,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Target Dominance Emotions", ylab = "Agent Perceived\nDominance Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## Polynomial Model (Baseline Couple Relationship Quality Control)
poly.model <- lmer(z.couple.rq ~ c.couple.baseline.rq + c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Baseline Couple Relationship Quality Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


## Polynomial Model (Issue Severity Control)
poly.model <- lmer(z.couple.rq ~ c.severity.obj + c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Issue Severity Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


## (7b) Target's Motivation to Change ####

## Polynomial Model 
poly.model <- lmer(target.change.motivation ~ c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.dominance, c.agent.perc.dominance, target.change.motivation))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.54245, x = -0.08761 , y = 0.20756, x2 = 0.02558, xy = -0.03476, y2 = -0.03339,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Target Dominance Emotions", ylab = "Agent Perceived\nDominance Emotions", zlab = "Target Motivation to Change",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## Polynomial Model (Couple Baseline Relationship Quality Control)
poly.model <- lmer(target.change.motivation ~ c.couple.baseline.rq + c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Couple Baseline Relationship Quality Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


## Polynomial Model (Issue Severity Control)
poly.model <- lmer(target.change.motivation ~ c.severity.obj + c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Issue Severity Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 



## (8) TARGET EMPATHIC ACCURACY FOR AGENT'S DOMINANCE EMOTIONS ####

# (8a) Couple Relationship Quality ####

## Polynomial Model 
poly.model <- lmer(z.couple.rq ~ c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance","c.target.perc.dominance",
                                             "c.agent.dominance.sq","c.tar.perc.ag.dominance.int",
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.dominance, c.target.perc.dominance, z.couple.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = -0.017798, x = -0.072412, y = -0.008241, x2 = -0.001868, xy = 0.013945, y2 = -0.005632,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Agent Dominance Emotions", ylab = "Target Perceived\nDominance Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## Polynomial Model (Baseline Couple Relationship Quality Control)
poly.model <- lmer(z.couple.rq ~ c.couple.baseline.rq + c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Baseline Couple Relationship Quality Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance","c.target.perc.dominance",
                                             "c.agent.dominance.sq","c.tar.perc.ag.dominance.int",
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


## Polynomial Model (Issue Severity Control)
poly.model <- lmer(z.couple.rq ~ c.severity.obj + c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Issue Severity Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance","c.target.perc.dominance",
                                             "c.agent.dominance.sq","c.tar.perc.ag.dominance.int",
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


## (9b) Target's Motivation to Change ####

## Polynomial Model 
poly.model <- lmer(target.change.motivation ~ c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance","c.target.perc.dominance",
                                             "c.agent.dominance.sq","c.tar.perc.ag.dominance.int",
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.dominance, c.target.perc.dominance, target.change.motivation))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.42265, x = 0.10503, y = 0.02373, x2 = -0.01018, xy = -0.08895, y2 = 0.03688,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Agent Dominance Emotions", ylab = "Target Perceived\nDominance Emotions", zlab = "Target Motivation to Change",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## Polynomial Model (Baseline Couple Relationship Quality Control)
poly.model <- lmer(target.change.motivation ~ c.couple.baseline.rq + c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Baseline Couple Relationship Quality Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance","c.target.perc.dominance",
                                             "c.agent.dominance.sq","c.tar.perc.ag.dominance.int",
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

## Polynomial Model (Issue Severity Control)
poly.model <- lmer(target.change.motivation ~ c.severity.obj + c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters (Issue Severity Control)
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance","c.target.perc.dominance",
                                             "c.agent.dominance.sq","c.tar.perc.ag.dominance.int",
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 


# SUPPLEMENTAL MATERIALS  #################################

# Hypothetical Plots in Paper ####

##### Strict Congruence Pattern Hypothetical Plot

plotRSA(x = 0, y = 0, x2 = -0.02, y2 = -0.02, xy = 0.04, b0 = 0.8,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Actual Partner Emotions", ylab = "Perceived Partner Emotions", zlab = "Criterion",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE)  # plot results


##### Broad Congruence (Rising Ridge) Hypothetical Plot

plotRSA(x = .06, y = .05, x2 = -0.02, y2 = -0.02, xy = 0.04, b0 = 0,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Actual Partner Emotions", ylab = "Perceived Partner Emotions", zlab = "Criterion",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE)  # plot results


## (1S) AGENT EMPATHIC ACCURACY FOR TARGET'S APPEASEMENT EMOTIONS ####

## (1Sa) Agent Relationship Quality (self-report) ####

## Polynomial Model 
poly.model <- lmer(agent.rq ~ c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1  | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.appease, c.agent.perc.appease, agent.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 4.922, x = -0.007222 , y = -0.1514, x2 = 0.007127, xy = 0.008092, y2 = -0.0132,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Target Appeasement Emotions", ylab = "Agent Perceived\nAppeasement Emotions", zlab = "Agent Relationship Quality (SR)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (1Sb) Agent Relationship Satisfaction (objective code) ####

## Polynomial Model 
poly.model <- lmer(agent.rs.obj ~ c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.appease, c.agent.perc.appease, agent.rs.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 3.725, x = .02097, y = .00637, x2 = -.0002797, xy = -.005013, y2 = -.008695,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Target Appeasement Emotions", ylab = "Agent Perceived\nAppeasement Emotions", zlab = "Agent Relationship Satisfaction (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (1Sc) Target Relationship Quality (self-report) ####

## Polynomial Model 
poly.model <- lmer(target.rq ~ c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.appease, c.agent.perc.appease, target.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.023939, x = -0.008573 , y = -0.015064, x2 = -0.023695, xy = 0.037879, y2 = -0.028599,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Target Appeasement Emotions", ylab = "Agent Perceived\nAppeasement Emotions", zlab = "Target Relationship Quality (SR)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (1Sd) Target Relationship Satisfaction (objective code) ####

## Polynomial Model 
poly.model <- lmer(target.rs.obj ~ c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.appease, c.agent.perc.appease, target.rs.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 4.105624, x = 0.069595, y =  -0.032194, x2 = -0.014948, xy = 0.012375, y2 = -0.005794,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Target Appeasement Emotions", ylab = "Agent Perceived\nAppeasement Emotions", zlab = "Target Relationship Satisfaction (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results



## (1Sd) Couple Conflict (objective code) ####

## Polynomial Model 
poly.model <- lmer(couple.conflict.obj ~ c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.appease, c.agent.perc.appease, couple.conflict.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 1.585866, x = -0.097372, y = 0.054010, x2 = 0.023951, xy = -0.012233, y2 = 0.003114,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Target Appeasement Emotions", ylab = "Agent Perceived\nAppeasement Emotions", zlab = "Couple Conflict (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (2S) TARGET EMPATHIC ACCURACY FOR AGENT’S APPEASEMENT EMOTIONS ####

## (2Sa) Agent Relationship Quality (self-report) ####

## Polynomial Model 
poly.model <- lmer(agent.rq ~ c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, agent.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.077312, x = 0.033312 , y = -0.115552, x2 = -0.022129, xy = 0.065165, y2 = -0.029993,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Agent Relationship Quality (SR)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (2Sb) Agent Relationship Satisfaction (objective code) ####

## Polynomial Model 
poly.model <- lmer(agent.rs.obj ~ c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, agent.rs.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 3.563, x = .02808, y = .04596, x2 = .009017, xy = .0002663, y2 = -.01407,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Agent Relationship Satisfaction (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (2Sc) Target Relationship Quality (self-report) ####

## Polynomial Model 
poly.model <- lmer(target.rq ~c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, target.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.10432, x = 0.11470 , y = -0.08557, x2 = -0.06555, xy = 0.09667, y2 = -0.05208,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Target Relationship Quality (SR)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (2Sd) Target Relationship Satisfaction (objective code) ####

## Polynomial Model 
poly.model <- lmer(target.rs.obj ~  c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, target.rs.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 3.969173, x = 0.023055, y = -0.026245, x2 = -0.013441, xy = 0.030284, y2 = -0.007966,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Target Relationship Satisfaction (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results



## (2Sd) Couple Conflict (objective code) ####

## Polynomial Model 
poly.model <- lmer(couple.conflict.obj ~ c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, couple.conflict.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 1.840, x = -.1071, y = .09086, x2 = .04323, xy = -.03358, y2 = -.001224,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Couple Conflict (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results



## (3S) AGENT EMPATHIC ACCURACY FOR TARGET'S DOMINANCE EMOTIONS ####

## (3Sa) Agent Relationship Quality (self-report) ####

## Polynomial Model 
poly.model <- lmer(agent.rq ~ c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.dominance, c.agent.perc.dominance, agent.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 4.754818, x = -0.021031 , y = -0.318288, x2 = 0.016473, xy = 0.002811, y2 = 0.010354,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Target Dominance Emotions", ylab = "Agent Perceived\nDominance Emotions", zlab = "Agent Relationship Quality (SR)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (3Sb) Agent Relationship Satisfaction (objective code) ####

## Polynomial Model 
poly.model <- lmer(agent.rs.obj ~ c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.dominance, c.agent.perc.dominance, agent.rs.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 3.506938, x = 0.031637, y = -0.03527, x2 = -0.012049, xy = 0.027799, y2 = -0.011007,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Target Dominance Emotions", ylab = "Agent Perceived\nDominance Emotions", zlab = "Agent Relationship Satisfaction (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (3Sc) Target Relationship Quality (self-report) ####

## Polynomial Model 
poly.model <- lmer(target.rq ~ c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.dominance, c.agent.perc.dominance, target.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 4.611, x = -.2851 , y = -.00009472, x2 = .05606, xy = -.05263, y2 = .02001,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Target Dominance Emotions", ylab = "Agent Perceived\nDominance Emotions", zlab = "Target Relationship Quality (SR)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (3Sd) Target Relationship Satisfaction (objective code) ####

## Polynomial Model 
poly.model <- lmer(target.rs.obj ~ c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.dominance, c.agent.perc.dominance, target.rs.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 3.806750, x = -0.070545, y =  0.078974, x2 = 0.014711, xy = -0.033823, y2 = 0.008804,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Target Dominance Emotions", ylab = "Agent Perceived\nDominance Emotions", zlab = "Target Relationship Satisfaction (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results



## (3Sd) Couple Conflict (objective code) ####

## Polynomial Model 
poly.model <- lmer(couple.conflict.obj ~ c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.dominance, c.agent.perc.dominance, couple.conflict.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 1.944211, x = 0.144383, y = -0.114256, x2 = -0.021561, xy = 0.046122, y2 = -0.019800,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Target Dominance Emotions", ylab = "Agent Perceived\nDominance Emotions", zlab = "Couple Conflict (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (4S) TARGET EMPATHIC ACCURACY FOR AGENT’S DOMINANCE EMOTIONS ####

## (4Sa) Agent Relationship Quality (self-report) ####

## Polynomial Model 
poly.model <- lmer(agent.rq ~ c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance", "c.target.perc.dominance", 
                                             "c.agent.dominance.sq", "c.tar.perc.ag.dominance.int", 
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.dominance, c.target.perc.dominance, agent.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 4.772577, x = -0.505089 , y = 0.098763, x2 = 0.032987, xy = -0.00810, y2 = 0.009459,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Agent Dominance Emotions", ylab = "Target Perceived\nDominance Emotions", zlab = "Agent Relationship Quality (SR)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (4Sb) Agent Relationship Satisfaction (objective code) ####

## Polynomial Model 
poly.model <- lmer(agent.rs.obj ~ c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance", "c.target.perc.dominance", 
                                             "c.agent.dominance.sq", "c.tar.perc.ag.dominance.int", 
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.dominance, c.target.perc.dominance, agent.rs.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 3.590558, x = -0.065855, y = 0.050166, x2 = -0.011941, xy = 0.028036, y2 = -0.008565,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Agent Dominance Emotions", ylab = "Target Perceived\nDominance Emotions", zlab = "Agent Relationship Satisfaction (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (4Sc) Target Relationship Quality (self-report) ####

## Polynomial Model 
poly.model <- lmer(target.rq ~ c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance", "c.target.perc.dominance", 
                                             "c.agent.dominance.sq", "c.tar.perc.ag.dominance.int", 
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.dominance, c.target.perc.dominance, target.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 4.767146, x = 0.008718 , y = -0.158225, x2 = -0.019329, xy = 0.023842, y2 = 0.010014,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Agent Dominance Emotions", ylab = "Target Perceived\nDominance Emotions", zlab = "Target Relationship Quality (SR)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (4Sd) Target Relationship Satisfaction (objective code) ####

## Polynomial Model 
poly.model <- lmer(target.rs.obj ~  c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance", "c.target.perc.dominance", 
                                             "c.agent.dominance.sq", "c.tar.perc.ag.dominance.int", 
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.dominance, c.target.perc.dominance, target.rs.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 3.945, x = .05741, y =-.0009019, x2 = -.004643, xy = .006086, y2 = -.006109,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Agent Dominance Emotions", ylab = "Target Perceived\nDominance Emotions", zlab = "Target Relationship Satisfaction (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results



## (4Sd) Couple Conflict (objective code) ####

## Polynomial Model 
poly.model <- lmer(couple.conflict.obj ~ c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance", "c.target.perc.dominance", 
                                             "c.agent.dominance.sq", "c.tar.perc.ag.dominance.int", 
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.dominance, c.target.perc.dominance, couple.conflict.obj))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 1.778054, x = -0.111845, y = 0.052844, x2 = -0.005742, xy = 0.012749, y2 = 0.006830,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 5),
        xlab = "Agent Dominance Emotions", ylab = "Target Perceived\nDominance Emotions", zlab = "Couple Conflict (OC)",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results



## (5S) AGENT EMPATHIC ACCURACY FOR TARGET'S APPEASEMENT EMOTIONS ####

## Couple relaionship quality composite
full.lab.df$couple.rq.z <- with(full.lab.df, rowMeans(cbind(z.agent.rq, z.agent.rs.obj, z.target.rq, z.target.rs.obj, z.r.conflict.obj), na.rm = T))

## Polynomial Model 

poly.model <- lmer(couple.rq.z ~ c.target.appease + c.agent.perc.appease + 
                     c.target.appease.sq +  c.ag.perc.tar.appease.int + c.agent.perc.appease.sq + 
                     c.target.appease.M + c.agent.perc.appease.M + 
                     c.target.appease.sq.M +  c.ag.perc.tar.appease.int.M + c.agent.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.appease","c.agent.perc.appease",
                                             "c.target.appease.sq","c.ag.perc.tar.appease.int",
                                             "c.agent.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.appease, c.agent.perc.appease, couple.rq.z))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 0.1651, x = 0.03636 , y = -0.04824, x2 = -0.01091, xy = 0.01587, y2 = -0.01346,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Target Appeasement Emotions", ylab = "Agent Perceived\nAppeasement Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (6S) TARGET EMPATHIC ACCURACY FOR AGENT'S APPEASEMENT EMOTIONS ####

## Polynomial Model 
poly.model <- lmer(couple.rq.z ~ c.agent.appease + c.target.perc.appease + 
                     c.agent.appease.sq +  c.tar.perc.ag.appease.int + c.target.perc.appease.sq + 
                     c.agent.appease.M + c.target.perc.appease.M + 
                     c.agent.appease.sq.M +  c.tar.perc.ag.appease.int.M + c.target.perc.appease.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.appease", "c.target.perc.appease", 
                                             "c.agent.appease.sq", "c.tar.perc.ag.appease.int", 
                                             "c.target.perc.appease.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.appease, c.target.perc.appease, couple.rq.z))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 0.086670, x = 0.049330 , y = -0.047368, x2 = -0.023325 , xy = 0.039189, y2 = -0.016840,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Agent Appeasement Emotions", ylab = "Target Perceived\nAppeasement Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (7S) AGENT'S EMPATHIC ACCURACY FOR TARGET'S DOMINANCE EMOTIONS ####

## Polynomial Model 
poly.model <- lmer(couple.rq.z ~ c.target.dominance + c.agent.perc.dominance + 
                     c.target.dominance.sq +  c.ag.perc.tar.dominance.int + c.agent.perc.dominance.sq + 
                     c.target.dominance.M + c.agent.perc.dominance.M + 
                     c.target.dominance.sq.M +  c.ag.perc.tar.dominance.int.M + c.agent.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.dominance","c.agent.perc.dominance",
                                             "c.target.dominance.sq","c.ag.perc.tar.dominance.int",
                                             "c.agent.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.target.dominance, c.agent.perc.dominance, couple.rq.z))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = -0.119409, x = -0.080432, y = -0.005790, x2 = 0.009910, xy = -0.017103, y2 = 0.005075,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Target Dominance Emotions", ylab = "Agent Perceived\nDominance Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (8S) TARGET EMPATHIC ACCURACY FOR AGENT'S DOMINANCE EMOTIONS ####

# Couple Relationship Quality ####

## Polynomial Model 
poly.model <- lmer(couple.rq.z ~ c.agent.dominance + c.target.perc.dominance + 
                     c.agent.dominance.sq +  c.tar.perc.ag.dominance.int + c.target.perc.dominance.sq + 
                     c.agent.dominance.M + c.target.perc.dominance.M + 
                     c.agent.dominance.sq.M +  c.tar.perc.ag.dominance.int.M + c.target.perc.dominance.sq.M +
                     (1 | dyad.id), REML = TRUE, data = full.lab.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.dominance","c.target.perc.dominance",
                                             "c.agent.dominance.sq","c.tar.perc.ag.dominance.int",
                                             "c.target.perc.dominance.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(full.lab.df, (cbind(c.agent.dominance, c.target.perc.dominance, z.couple.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 0.004582, x = -0.053529, y = -0.007825, x2 = -0.002849, xy = 0.014368, y2 = -0.006318,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Agent Dominance Emotions", ylab = "Target Perceived\nDominance Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (9S) VARIABLE COMPUTATION FOR EMPATHIC ACCURACY FOR POSITIVE EMOTIONS ####

# (9Sa) Compute Predictor Variables ####

# Target's positive emotions
with(lab.df, describe(cbind(L_CL_A_emo_17, L_CL_A_emo_18, L_CL_A_emo_19)))  # descriptives
lab.df$target.pa <- with(lab.df, rowMeans(cbind(L_CL_A_emo_17, L_CL_A_emo_18, L_CL_A_emo_19), na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_CL_A_emo_17, L_CL_A_emo_18, L_CL_A_emo_19)))  # check reliability

# Agent's perception of target's positive emotions
with(lab.df, describe(cbind(L_C_P_emo_17, L_C_P_emo_18, L_C_P_emo_19)))  # descriptives
lab.df$agent.perc.pa <- with(lab.df, rowMeans(cbind(L_C_P_emo_17, L_C_P_emo_18, L_C_P_emo_19), na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_C_P_emo_17, L_C_P_emo_18, L_C_P_emo_19)))  # check reliability

# Agent's positive emotions
with(lab.df, describe(cbind(L_C_A_emo_17, L_C_A_emo_18, L_C_A_emo_19)))  # descriptives
lab.df$agent.pa <- with(lab.df, rowMeans(cbind(L_C_A_emo_17, L_C_A_emo_18, L_C_A_emo_19, na.rm = T)))  # create composite
with(lab.df, alpha(cbind(L_C_A_emo_17, L_C_A_emo_18, L_C_A_emo_19)))  # check reliability

# Target's perception of agent's positive emotions 
with(lab.df, describe(cbind(L_CL_P_emo_17, L_CL_P_emo_18, L_CL_P_emo_19)))  # descriptives
lab.df$target.perc.pa <- with(lab.df, rowMeans(cbind(L_CL_P_emo_17, L_CL_P_emo_18, L_CL_P_emo_19),na.rm = T))  # create composite
with(lab.df, alpha(cbind(L_CL_P_emo_17, L_CL_P_emo_18, L_CL_P_emo_19)))  # check reliability

## (9Sb) DESCRIPTIVES FOR POSITIVE EMOTION VARIABLES ####
with(lab.df, describe(cbind(target.pa, agent.perc.pa, agent.pa, target.perc.pa), na.rm = T))
pa.corr <-  with(lab.df, corr.test(cbind(target.pa, agent.perc.pa, agent.pa, target.perc.pa,
                                         z.couple.rq, target.change.motivation), use="complete.obs", adjust="none"))
print(pa.corr, digits = 4)


## (9Sc) COMPUTE VARIABLES FOR POLYNOMIAL REGRESSION ####

# Agent Perceived Positive Emotion Variables

# Center predictor terms on grand mean across both congruence variables
lab.df$grand.M <- with(lab.df, mean(c(target.pa, agent.perc.pa), na.rm = T))
lab.df$c.target.pa <- lab.df$target.pa- lab.df$grand.M
lab.df$c.agent.perc.pa <- lab.df$agent.perc.pa - lab.df$grand.M

# Compute higher order terms based on centered predictors
lab.df$c.target.pa.sq <- lab.df$c.target.pa^2
lab.df$c.agent.perc.pa.sq <- lab.df$c.agent.perc.pa^2
lab.df$c.ag.perc.tar.pa.int <- lab.df$c.target.pa * lab.df$c.agent.perc.pa


# Target Perceived Positive Emotion Variables

# Center predictor terms on grand mean across both congruence variables
lab.df$grand.M <- with(lab.df, mean(c(agent.pa, target.perc.pa), na.rm = T))
lab.df$c.agent.pa <- lab.df$agent.pa - lab.df$grand.M
lab.df$c.target.perc.pa <- lab.df$target.perc.pa - lab.df$grand.M

# Compute higher order terms based on centered predictors
lab.df$c.agent.pa.sq <- lab.df$c.agent.pa^2
lab.df$c.target.perc.pa.sq <- lab.df$c.target.perc.pa^2
lab.df$c.tar.perc.ag.pa.int <- lab.df$c.agent.pa * lab.df$c.target.perc.pa


# Create Aggregates
lab.df$dyad.id <- as.numeric(substr(lab.df$Couple_ID,1,3))  # remove letters from coupleID
lab.df$dyad.id   # check dyadic variables

agg.lab.df <- ddply(lab.df, c("dyad.id"), summarize, 
                    target.pa.M = mean(target.pa, na.rm = T),
                    agent.perc.pa.M = mean(agent.perc.pa, na.rm = T),
                    agent.pa.M = mean(agent.pa, na.rm = T),
                    target.perc.pa.M = mean(target.perc.pa, na.rm = T))  # create aggregates by couple ID

pa.df <- merge(lab.df, agg.lab.df, by ="dyad.id")  # merge aggregates with main df


# Aggregate Agent Perceived Positive Emotion Variables

# Center predictor terms on grand mean across both congruence variables
pa.df$grand.M <- with(pa.df, mean(c(target.pa.M, agent.perc.pa.M), na.rm = T))
pa.df$c.target.pa.M <- pa.df$target.pa.M - pa.df$grand.M
pa.df$c.agent.perc.pa.M <- pa.df$agent.perc.pa.M - pa.df$grand.M

# Compute higher order terms based on centered predictors
pa.df$c.target.pa.sq.M <- pa.df$c.target.pa.M^2
pa.df$c.agent.perc.pa.sq.M <- pa.df$c.agent.perc.pa.M^2
pa.df$c.ag.perc.tar.pa.int.M <- pa.df$c.agent.perc.pa.M * pa.df$c.target.pa.M


# Aggregate Target Perceived Positive Emotion Variables

# Center predictor terms on grand mean across both congruence variables
pa.df$grand.M <- with(pa.df, mean(c(agent.pa.M, target.perc.pa.M), na.rm = T))
pa.df$c.agent.pa.M <- pa.df$agent.pa.M - pa.df$grand.M
pa.df$c.target.perc.pa.M <- pa.df$target.perc.pa.M - pa.df$grand.M

# Compute higher order terms based on centered predictors
pa.df$c.agent.pa.sq.M <- pa.df$c.agent.pa.M^2
pa.df$c.target.perc.pa.sq.M <- pa.df$c.target.perc.pa.M^2
pa.df$c.tar.perc.ag.pa.int.M <- pa.df$c.target.perc.pa.M * pa.df$c.agent.pa.M


## (10S) AGENT EMPATHIC ACCURACY FOR TARGET'S POSITIVE EMOTIONS  ####

## (10Sa) Couple Relationship Quality ####

## Polynomial Model 
poly.model <- lmer(z.couple.rq ~ c.target.pa + c.agent.perc.pa + 
                     c.target.pa.sq +  c.ag.perc.tar.pa.int + c.agent.perc.pa.sq + 
                     c.target.pa.M + c.agent.perc.pa.M + 
                     c.target.pa.sq.M +  c.ag.perc.tar.pa.int.M + c.agent.perc.pa.sq.M +
                     (1 | dyad.id), REML = TRUE, data = pa.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.pa", "c.agent.perc.pa", 
                                             "c.target.pa.sq", "c.ag.perc.tar.pa.int", 
                                             "c.agent.perc.pa.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(pa.df, (cbind(c.target.pa, c.agent.perc.pa, z.couple.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = -0.035961, x = 0.006826, y = 0.019111, x2 = 0.010728, xy = -0.007694, y2 = -0.001324,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Target Positive Emotions", ylab = "Agent Perceived\nPositive Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (10Sb) Target's Motivation to Change ####

## Polynomial Model 
poly.model <- lmer(target.change.motivation ~ c.target.pa + c.agent.perc.pa + 
                     c.target.pa.sq +  c.ag.perc.tar.pa.int + c.agent.perc.pa.sq + 
                     c.target.pa.M + c.agent.perc.pa.M + 
                     c.target.pa.sq.M +  c.ag.perc.tar.pa.int.M + c.agent.perc.pa.sq.M +
                     (1 | dyad.id), REML = TRUE, data = pa.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.target.pa", "c.agent.perc.pa", 
                                             "c.target.pa.sq", "c.ag.perc.tar.pa.int", 
                                             "c.agent.perc.pa.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(pa.df, (cbind(c.target.pa, c.agent.perc.pa, target.change.motivation))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.324325, x = 0.115979, y = 0.066727, x2 = 0.009667, xy = -0.015410, y2 = 0.004620,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Target Positive Emotions", ylab = "Agent Perceived\nPositive Emotions", zlab = "Target Change Motivation",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (11S) TARGET EMPATHIC ACCURACY FOR AGENT'S POSITIVE EMOTIONS  ####

## (11Sa) Couple Relationship Quality ####

## Polynomial Model 
poly.model <- lmer(z.couple.rq ~ c.agent.pa + c.target.perc.pa + 
                     c.agent.pa.sq +  c.tar.perc.ag.pa.int + c.target.perc.pa.sq + 
                     c.agent.pa.M + c.target.perc.pa.M + 
                     c.agent.pa.sq.M +  c.tar.perc.ag.pa.int.M + c.target.perc.pa.sq.M +
                     (1 | dyad.id), REML = TRUE, data = pa.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.pa", "c.target.perc.pa", 
                                             "c.agent.pa.sq", "c.tar.perc.ag.pa.int", 
                                             "c.target.perc.pa.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(pa.df, (cbind(c.agent.pa, c.target.perc.pa, z.couple.rq))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 0.099907, x = 0.041571, y = 0.005798, x2 = 0.007481, xy = 0.002893, y2 = 0.001787,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(-4, 2),
        xlab = "Agent Positive Emotions", ylab = "Target Perceived\nPositive Emotions", zlab = "Couple Relationship Quality",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = FALSE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results


## (11Sb) Target's Motivation to Change ####

## Polynomial Model 
poly.model <- lmer(target.change.motivation ~ c.agent.pa + c.target.perc.pa + 
                     c.agent.pa.sq +  c.tar.perc.ag.pa.int + c.target.perc.pa.sq + 
                     c.agent.pa.M + c.target.perc.pa.M + 
                     c.agent.pa.sq.M +  c.tar.perc.ag.pa.int.M + c.target.perc.pa.sq.M +
                     (1 | dyad.id), REML = TRUE, data = pa.df)
summary(poly.model)


## RSA Parameters
MLRSA_AverageSurface(poly.model, name_vars=c("c.agent.pa", "c.target.perc.pa", 
                                             "c.agent.pa.sq", "c.tar.perc.ag.pa.int", 
                                             "c.target.perc.pa.sq"), random_vars=c(NA, NA, NA, NA, NA)) 

# RSA Plot
rsa.df <- data.frame(with(pa.df, (cbind(c.agent.pa, c.target.perc.pa, target.change.motivation))))  # Create dataframe for RSA plot
rsa.plot.df <- rsa.df[complete.cases(rsa.df), ]  # Retain complete cases only

plotRSA(b0 = 5.56676, x = 0.25133, y = -0.02542, x2 = -0.02790, xy = -0.05013, y2 = 0.03157,
        xlim =c(-7.5, 7.5), ylim =c(-7.5, 7.5), zlim = c(1, 7),
        xlab = "Agent Positive Emotions", ylab = "Target Perceived\nPositive Emotions", zlab = "Target Change Motivation",
        cex.axesLabel = 1.3, cex.main = 1.3, param = FALSE, legend = TRUE, project=c("points", "PA2", "LOC", "LOIC", "hull"),
        points = list(data = rsa.plot.df, show = FALSE, value = "raw", jitter = 0, color = "black", cex = 0.5, out.mark = FALSE))  # plot results