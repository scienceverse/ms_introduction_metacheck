#####################################################################################################################
#                                       Load excel file: Make sure to adjust working directory as needed            #                   
#####################################################################################################################

script.dir <- " "
setwd(script.dir)
library(readxl)
raw.data <- read_excel("Bilingual_Meta_Table_Final_1130.xlsx")

# load the metafor package. This is needed to run all the analyses below. 

library(metafor)

## Calculate effect size using means and SDs. Wrote to CSV so effect size can be manually coded (direction) depending on the outcome measure. 
## All other effect sizes were calculated using practical meta effect size calculator

raw.data.effectsize <- escalc(measure = "SMD", vtype = "UB", m1i = MonoMean, m2i = BiMean, sd1i = MonoSD, sd2i = BiSD, n1i = MonoN, n2i = BiN, data = raw.data) 
write.csv(raw.data.effectsize,file= "ES.csv")


#Generating models for comparison. On Raw Data  

One.level.model   <- rma.mv(yi, vi, data = raw.data, method = "REML") # Generating models
print(One.level.model, digits= 3)
Two.level.model   <- rma.mv(yi, vi, data = raw.data, method = "REML", random = ~ 1 | Participants)
print(Two.level.model, digits= 3)
Three.level.model  <- rma.mv(yi, vi, data = raw.data, method = "REML", random = ~ 1 |Study/Participants)
print(Three.level.model, digits=3)
Four.level.model <- rma.mv(yi, vi, data = raw.data, method = "REML", random = ~ 1 |LabGroup/Study/Participants)
print(Four.level.model, digits=3)

anova(One.level.model, Two.level.model) # Comparing models
anova(Two.level.model, Three.level.model)
anova(Three.level.model, Four.level.model)


#########################################################################
# Trim data to remove outlier effect sizes greater than and less 3      #
#########################################################################

trim.data <- subset(raw.data, yi >= -3 & yi <= 3) 

#########################################################################
# Model the trimmed data                                                #
#########################################################################

One.level.model   <- rma.mv(yi, vi, data = trim.data, method = "REML") # Generating models
print(One.level.model, digits= 3)
Two.level.model   <- rma.mv(yi, vi, data = trim.data, method = "REML", random = ~ 1 | Participants)
print(Two.level.model, digits= 3)
Three.level.model   <- rma.mv(yi, vi, data = trim.data, method = "REML", random = ~ 1 |Study/Participants)
print(Three.level.model, digits= 3)
Four.level.model  <- rma.mv(yi, vi, data = trim.data, method = "REML", random = ~ 1 |LabGroup/Study/Participants)
print(Four.level.model, digits=3)

anova(One.level.model, Two.level.model) # Comparing models
anova(Two.level.model, Three.level.model)
anova(Three.level.model, Four.level.model)

#################################################################################
#Create profile plot for the four.level.model. This is used to assess model fit #
#################################################################################

par(mfrow=c(2,1))
profile(Four.level.model, sigma2=1)
profile(Four.level.model, sigma2=2)
profile(Four.level.model, sigma2=3)


#######################################################################################################################
#            Creating function for the four-level model using raw data as default                                                 #
#######################################################################################################################

final_model <- function(mod = NULL, dat = raw.data) {
  model <- rma.mv(yi, vi, data = dat, method = "REML", random = ~ 1 | LabGroup/Study/Participants, mods = mod) 
  return(model)
}

overall.raw <- final_model(dat = raw.data) # Overall effect
print(overall.raw)

pet.raw <- final_model(mod = ~ sqrt(vi), dat = raw.data) # Pet-test
print(pet.raw)

peese.raw <- final_model(mod = ~ vi, dat = raw.data) # Peese-test
print(peese.raw)


###################################################################################### 
# Creating function for the four-level model using trimmed data as default           # 
######################################################################################

final_model <- function(mod = NULL, dat = trim.data) {
  model <- rma.mv(yi, vi, data = dat, method = "REML", random = ~ 1 | LabGroup/Study/Participants, mods = mod) 
  return(model)
}

overall.trim <- final_model(dat = trim.data) # Overall effect
print(overall.trim)

###Calculate total I2 ###

res <- rma.mv(yi, vi, data = trim.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/trim.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Calculate I2 for between-cluster heterogeneity ###

100 * res$sigma2 / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate ICC of the true effects. 

round(overall.trim$sigma2[1] / sum(overall.trim$sigma2), 3)

### Estimate total heterogeneity/Tau 2 

tau <-round(sum(overall.trim$sigma2), 3)

##############################################################################################################
#      Pet-Peese test on the overall effect.                                                                 #
##############################################################################################################

# Pet-test

pet.trim <- final_model(mod = ~ sqrt(vi), dat = trim.data) 
print(pet.trim)

# Peese-test

peese.trim <- final_model(mod = ~ vi, dat = trim.data) 
print(peese.trim)



##############################################################################################################
#             Executive function domain as a moderator                                                       #   
##############################################################################################################

domain <- final_model(mod = ~ factor(Domain))  
print(domain)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = trim.data, method = "REML",mod = ~ factor(Domain),  random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/trim.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate ICC of the true effects. 

round(res$sigma2[1] / sum(res$sigma2), 3)

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

### PET-PESE for domain ###

domain.pet <- final_model(mod = ~ factor(Domain) + sqrt(vi)) # Pet-test
print(domain.pet)

domain.peese <- final_model(mod = ~ factor(Domain) + vi -1) # Peese-test
print(domain.peese)

#######################################################################################
# Run seperate anaylses for immersion studies removed                                 #
#######################################################################################
immersion.removed.data <- subset(trim.data, Immersion_Education == "N") 

overall.immersion.removed <- final_model(dat = immersion.removed.data)
print(overall.immersion.removed)

#######################################################################################
# Run seperate anaylses for each executive function domain                            #
#######################################################################################


###Create files for each domain ###

inh.data <- subset(trim.data, Domain == "Inhibition") 
sel.data <- subset(trim.data, Domain == "Selection")
shi.data <- subset(trim.data, Domain == "Switching")
att.data <- subset(trim.data, Domain == "Attention")
plan.data <- subset(trim.data, Domain == "Planning")
wm.data  <- subset(trim.data, Domain == "WM")# verbal working memory 
vm.data  <- subset(trim.data, Domain == "Visuospatial WM") # non verbal working memory 
global.data  <- subset(trim.data, Domain == "Global EF")
reward.data  <- subset(trim.data, Domain == "Reward-based decision making")
dual.data  <- subset(trim.data, Domain == "Dual-Task")

# create subset for executive attention (non-verbal working memory, flexbility, selection)
executive.attention <- subset(trim.data, Domain == "Switching" | Domain == "Visuospatial WM" | Domain == "Selection")
executive.other <- subset(trim.data, Domain == "Inhibition" | Domain == "WM" | Domain == "Attention" | Domain == "Planning" | Domain == "Global EF" | Domain == "Reward-based decision making" | Domain == "Dual-Task")

# create a subset including all the domains included in moderator analyses 
executive.mod.domains <- subset(trim.data, Domain == "Switching" | Domain == "Visuospatial WM" | Domain == "Selection" | Domain == "Inhibition" | Domain == "WM" | Domain == "Attention")

############################ Executive Attention ##################################################


overall.executive.attention <- final_model(dat = executive.attention) # Overall effect executive attention 
print(overall.executive.attention)

domain.pet <- final_model(dat = executive.attention, mod = ~ sqrt(vi)) # Pet-test
print(domain.pet)

domain.peese <- final_model(dat = executive.attention, mod = ~ vi) # Peese-test
print(domain.peese)



###Calculate I2 ###

res <- rma.mv(yi, vi, data = executive.attention, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/executive.attention$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

############################ Other Executive Function Domains ##################################################

overall.executive.other <- final_model(dat = executive.other) # Overall effect other executive function domains
print(overall.executive.other)

res <- rma.mv(yi, vi, data = executive.other, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/executive.other$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

domain.pet <- final_model(dat = executive.other, mod = ~ sqrt(vi)) # Pet-test
print(domain.pet)

domain.peese <- final_model(dat = executive.other, mod = ~ vi) # Peese-test
print(domain.peese)



############################ Response Inhibition ##################################################


overall.inh <- final_model(dat = inh.data) # Overall effect
print(overall.inh)


###Calculate I2 ###

res <- rma.mv(yi, vi, data = inh.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/inh.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)



############################ Inhibition GNG ##################################################

inh.gng <- subset(inh.data, Outcome_Task == "GNG") 

overall.inh.gng<- final_model(dat = inh.gng) 
print(overall.inh.gng)


###Calculate I2 ###

res <- rma.mv(yi, vi, data = inh.gng, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/inh.gng$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#####################################################################################################################
# Selection                        
######################################################################################################################

overall.sel <- final_model(dat = sel.data) # Overall effect
print(overall.sel)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#################################################
#Stroop                                         #
#################################################

sel.stroop.congruent <- subset(sel.data, Outcome_Task == "Stroop" & Outcome_Type =="Congruent")
sel.stroop.c.RT <-subset(sel.stroop.congruent, SubMeasure == "RT")
sel.stroop.incongruent <- subset(sel.data, Outcome_Task == "Stroop" & Outcome_Type =="Incongruent")
sel.stroop.i.RT <-subset(sel.stroop.incongruent, SubMeasure == "RT")
sel.stroop.congruent <- subset(sel.data, Outcome_Task == "Stroop" & Outcome_Type =="Congruent")
sel.stroop.c.Acc <-subset(sel.stroop.congruent, SubMeasure == "Acc")
sel.stroop.incongruent <- subset(sel.data, Outcome_Task == "Stroop" & Outcome_Type =="Incongruent")
sel.stroop.i.Acc <-subset(sel.stroop.incongruent, SubMeasure == "Acc")

###################################################################
# Stroop: RT congurent trials                                     #
###################################################################

stroop.c.rt <- final_model(dat = sel.stroop.c.RT) # Congruent trials RT Stroop task 
print(stroop.c.rt)


###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.stroop.c.RT, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.stroop.c.RT$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


###################################################################
# Stroop: RT incongurent trials                                   #
###################################################################

stroop.i.rt <- final_model(dat = sel.stroop.i.RT) # Congruent trials RT Stroop task 
print(stroop.i.rt)


###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.stroop.i.RT, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.stroop.i.RT$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


###################################################################
# Stroop: Acc Congurent trials                                   #
###################################################################
stroop.c.acc <- final_model(dat = sel.stroop.c.Acc) # Congruent trials Acc Stroop task 
print(stroop.c.acc)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.stroop.c.Acc, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.stroop.c.Acc$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

###################################################################
# Stroop: Acc Incongurent trials                                   #
###################################################################
stroop.i.Acc <- final_model(dat = sel.stroop.i.Acc) # Incongruent trials RT Stroop task 
print(stroop.i.Acc)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.stroop.i.Acc, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead"))
W <- diag(1/sel.stroop.i.Acc$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)



#################################################
# Flanker                                        #
##################################################


sel.flanker.congruent <- subset(sel.data, Outcome_Task == "Flanker" & Outcome_Type =="Congruent")
sel.flanker.c.RT <-subset(sel.flanker.congruent, SubMeasure == "RT")
sel.flanker.incongruent <- subset(sel.data, Outcome_Task == "Flanker" & Outcome_Type =="Incongruent")
sel.flanker.i.RT <-subset(sel.flanker.incongruent, SubMeasure == "RT")
sel.flanker.congruent <- subset(sel.data, Outcome_Task == "Flanker" & Outcome_Type =="Congruent")
sel.flanker.c.Acc <-subset(sel.flanker.congruent, SubMeasure == "Acc")
sel.flanker.incongruent <- subset(sel.data, Outcome_Task == "Flanker" & Outcome_Type =="Incongruent")
sel.flanker.i.Acc <-subset(sel.flanker.incongruent, SubMeasure == "Acc")


###################################################################
# Flanker: RT congurent trials                                    #
###################################################################

flanker.c.rt <- final_model(dat = sel.flanker.c.RT) # Congruent trials RT Stroop task 
print(flanker.c.rt)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.flanker.c.RT, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.flanker.c.RT$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

###################################################################
# Flanker: RT incongurent trials                                    #
###################################################################

flanker.i.rt <- final_model(dat = sel.flanker.i.RT) # Congruent trials RT Flanker task 
print(flanker.i.rt)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.flanker.i.RT, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.flanker.i.RT$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

###################################################################
# Flanker: Acc congurent trials                                    #
###################################################################

flanker.c.acc <- final_model(dat = sel.flanker.c.Acc) # Congruent trials RT Stroop task 
print(flanker.c.acc)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.flanker.c.Acc, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.flanker.c.Acc$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

###################################################################
# Flanker: Acc incongurent trials                                 #
###################################################################

flanker.i.acc <- final_model(dat = sel.flanker.i.Acc) # Congruent trials RT Stroop task 
print(flanker.i.acc)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.flanker.i.Acc, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.flanker.i.Acc$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

#################################################
# Simon                                         #
#################################################

sel.simon.congruent <- subset(sel.data, Outcome_Task == "Simon" & Outcome_Type =="Congruent")
sel.simon.c.RT <-subset(sel.simon.congruent, SubMeasure == "RT")
sel.simon.incongruent <- subset(sel.data, Outcome_Task == "Simon" & Outcome_Type =="Incongruent")
sel.simon.i.RT <-subset(sel.simon.incongruent, SubMeasure == "RT")
sel.simon.congruent <- subset(sel.data, Outcome_Task == "Simon" & Outcome_Type =="Congruent")
sel.simon.c.Acc <-subset(sel.simon.congruent, SubMeasure == "Acc")
sel.simon.incongruent <- subset(sel.data, Outcome_Task == "Simon" & Outcome_Type =="Incongruent")
sel.simon.i.Acc <-subset(sel.simon.incongruent, SubMeasure == "Acc")



###################################################################
# Simon: RT congurent trials                                      #
###################################################################

simon.c.rt <- final_model(dat = sel.simon.c.RT)  
print(simon.c.rt)


###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.simon.c.RT, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.simon.c.RT$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

###################################################################
# Simon: RT incongurent trials                                    #
###################################################################

### optimizer did not reach convergence with inital model. Had to adjust optimizer settings.

simon.i.rt <- rma.mv(yi, vi, data = sel.simon.i.RT, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead"))
print(simon.i.rt)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.simon.i.RT, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead"))
W <- diag(1/sel.simon.i.RT$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

###################################################################
# Simon: Acc congurent trials                                    #
###################################################################

simon.c.acc <- final_model(dat = sel.simon.c.Acc) 
print(simon.c.acc)


###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.simon.c.Acc, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.simon.c.Acc$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

###################################################################
# Simon : Acc incongurent trials                                 #
###################################################################

simon.i.acc <- final_model(dat = sel.simon.i.Acc) # Incongruent trials Acc Stroop task 
print(simon.i.acc)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = sel.simon.i.Acc, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/sel.simon.i.Acc$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#####################################################################################################################
# WM: Verbal                                                                                                        
#####################################################################################################################

overall.wm <- final_model(dat = wm.data) # Overall effect
print(overall.wm)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = wm.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/wm.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#################################################
# N-back                                        #
#################################################

wm.nback.data <- subset(wm.data, Outcome_Task == "N-back")
wm.nback <- final_model(dat = wm.nback.data) # Overall effect
print(wm.nback)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = wm.nback.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/wm.nback.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

#################################################
# Forward Digit Span                            #
#################################################

wm.fspan.data <- subset(wm.data, Outcome_Task == "Forward Digit Span")
wm.fspan <- final_model(dat = wm.fspan.data) 
print(wm.fspan)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = wm.fspan.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/wm.fspan.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#################################################
# Backward Digit Span                           #
#################################################

wm.bspan.data <- subset(wm.data, Outcome_Task == "Backward Digit Span")
wm.bspan <- final_model(dat = wm.bspan.data) 
print(wm.bspan)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = wm.bspan.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/wm.bspan.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#####################################################################################################################
# Flexibility 
######################################################################################################################

overall.shi <- final_model(dat = shi.data) # Overall effect
print(overall.shi)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = shi.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/shi.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#################################################
# DCCS                                          #
#################################################

shi.dccs.data <- subset(shi.data, Outcome_Task == "DCCS")
shi.dccs <- final_model(dat = shi.dccs.data) 
print(shi.dccs)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = shi.dccs.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/shi.dccs.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

#################################################
# TMT                                          #
#################################################

shi.tmt.data <- subset(shi.data, Outcome_Task == "TMT")
shi.tmt <- final_model(dat = shi.tmt.data) 
print(shi.tmt)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = shi.tmt.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/shi.tmt.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#################################################
# WCST                                         #
#################################################

shi.wcst.data <- subset(shi.data, Outcome_Task == "WCST")
shi.wcst <- final_model(dat = shi.wcst.data) 
print(shi.wcst)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = shi.wcst.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/shi.wcst.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

#####################################################################################################################
# Attention                       
######################################################################################################################

overall.att <- final_model(dat = att.data) # Overall effect
print(overall.att)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = att.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/att.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#################################################
#ANT                                            #
#################################################

att.ant <- subset(att.data, Outcome_Task == "ANT")

overall.att.ant <- final_model(dat = att.ant) 
print(overall.att.ant)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = att.ant, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/att.ant$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

#####################################################################################################################
# Planning                       
######################################################################################################################

overall.plan <- final_model(dat = plan.data) # Overall effect
print(overall.plan)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = plan.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/plan.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#####################################################################################################################
# VM                   
######################################################################################################################

  overall.vm <- final_model(dat = vm.data) # Overall effect
  print(overall.vm)
  
  ###Calculate I2 ###
  
  res <- rma.mv(yi, vi, data = vm.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
  W <- diag(1/vm.data$vi)
  X <- model.matrix(res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))
  
  ### Estimate total heterogeneity/Tau 2 
  
  round(sum(res$sigma2), 3)


#####################################################################################################################
# Global EF              
######################################################################################################################

overall.global <- final_model(dat = global.data) # Overall effect
print(overall.global)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = global.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/global.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)

#####################################################################################################################
# Reward Based           
######################################################################################################################

overall.reward <- final_model(dat = reward.data) # Overall effect
print(overall.reward)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = reward.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/reward.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


#####################################################################################################################
# Dual-task      
######################################################################################################################

overall.dual <- final_model(dat = dual.data) # Overall effect
print(overall.dual)

###Calculate I2 ###

res <- rma.mv(yi, vi, data = dual.data, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/dual.data$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)



#######################################################################################
# MoDERATOR ANAYLSES                                                                  # 
#######################################################################################

####################################
# Age (Age.c = age mean centered) # 
####################################

age.mod <- final_model(mod = ~ Age.c) 
print(age.mod)

# Effect of Age at each level of the included domains after controlling for domain) 

age.mod.domains <- final_model(dat = executive.mod.domains, mod = ~ Age.c:Domain) 
print(age.mod.domains)

#############################################
# Verbal vs. Non-verbal tasks               # 
#############################################

verbal.mod <- final_model(mod = ~ factor(Verbal_NonVerbal) - 1) 
print(verbal.mod)

###### individual analyses used to get k values #### 
verbal.data <- subset(trim.data, Verbal_NonVerbal == "Verbal")
verbal <- final_model(dat = verbal.data) 
print(verbal)

non_verbal.data <- subset(trim.data, Verbal_NonVerbal == "Non-verbal")
non_verbal <- final_model(dat = non_verbal.data) 
print(non_verbal)

both.data <- subset(trim.data, Verbal_NonVerbal == "Both")
both <- final_model(dat = both.data) 
print(both)
###################################
# Geographic Location             #
###################################

location.mod <- final_model(mod = ~ factor(Location) - 1) 
print(location.mod)


########################################################
# Bilingial prof as a moderator (emergent vs. balanced) #                                                                      
########################################################

prof.mod <- final_model(mod = ~ factor(Proficiency) -1) 
print(prof.mod)

prof.mod <- final_model(dat = executive.mod.domains, mod = ~ Proficiency:Domain) 
print(prof.mod)


#####################################################################################################################################
#    Measurement of Language status: LU = language use questionnaire, RV = measurement of receptive vocabulary, SR = self-nomination #                                      #   
#####################################################################################################################################

measure_LS.mod <- final_model(mod = ~ factor(Prof)-1) 
print(measure_LS.mod)

measure.LS.domain.mod <- final_model(dat = executive.mod.domains, mod = ~ factor(Prof):Domain) 
print(measure.LS.domain.mod)

###### individual analyses used to get k values #### 
LU.data <- subset(trim.data, Prof == "LU") 
RV.data <- subset(trim.data, Prof == "RV")
SR.data <- subset(trim.data, Prof == "SR")

prof.mod.LU <- final_model(dat = LU.data) 
print(prof.mod.LU)

prof.mod.RV <- final_model(dat = RV.data) 
print(prof.mod.RV)

prof.mod.SR <- final_model(dat = SR.data) 
print(prof.mod.SR)

###########################################################################
#     Publication year moderator analysis                                 #   
###########################################################################

pubyear.mod <- final_model(mod = ~ Year) # overall 
print(pubyear.mod)

# publication year by domain 

pubyear.mod.domain <- final_model(dat = executive.mod.domains, mod = ~ Year:Domain) 
print(pubyear.mod.domain)

###########################################################################
#     Study quality as a moderator:                                       #   
###########################################################################

# Moderating effect of study quality on the overall effect ### 

quality.mod <- final_model(mod = ~ Quality) 
print(quality.mod)

# Quality by domain

qual.mod <- final_model(dat = executive.mod.domains, mod = ~ Quality:Domain) 
print(qual.mod)


###########################################################################
#   Studies of high methodological quality sub analysis                   #  
##########################################################################


q.high <- subset(trim.data, SES== 1 & Equivalence == 1 & StudyQuality >= 12)
quality.high <- final_model(dat = q.high) 
print(quality.high)                

###Calculate I2 ###

res <- rma.mv(yi, vi, data = q.high, method = "REML", random = ~ 1 | LabGroup/Study/Participants)
W <- diag(1/q.high$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(res$sigma2) / (sum(res$sigma2) + (res$k-res$p)/sum(diag(P)))

### Estimate total heterogeneity/Tau 2 

round(sum(res$sigma2), 3)


####################################################################################################################################################
#               Matched samples and Equivalence as a moderator. 1= use of matached samples or equivalence testing on at least 1 variable. 0=None   #
####################################################################################################################################################

equiv.mod <- final_model(mod = ~ factor(Equivalence)-1) 
print(equiv.mod)

#  by domain 

equiv.mod.domain <- final_model(executive.mod.domains, mod = ~ Equiv:Domain - 1)
print(equiv.mod.domain)

##############################################################################################################################################################################  
#SES measured as a moderator. Y=Yes, authors objectively measured SES either using income or parent education as a proxy. N=No, SES was not objectively measured             #
##############################################################################################################################################################################

ses.mod <- final_model(mod = ~ factor(SES_Measured)-1)  # overall effect
print(ses.mod)

# testing the interaction between SES and geographic location. 

ses.location.mod <- final_model(mod = ~ SES_Measured: Location - 1)  # overall effect
print(ses.location.mod)

#testing interaction between SES and executive function domains. 

ses.mod.domain <- final_model(executive.mod.domains, mod = ~ SES_Measured:Domain -1)
print(ses.mod.domain)

##############################################################################################################################################################################  
#Multiple regression model: Publication Year, Age, Quality, Measurement of SES, and use of Matched samples in one analysis
##############################################################################################################################################################################

multiple.mod4 <- final_model(mod = ~ Age.c + Year + Prof_DC + (Quality * SES * Equivalence)) 
print(multiple.mod4)

ses.yes <- subset(trim.data, SES== 1)
quality.ses <- final_model(mod = ~ Quality, dat = ses.yes) 
print(quality.ses)

ses.no <- subset(trim.data, SES== 0)
quality.ses.no <- final_model(mod = ~ Quality, dat = ses.no) 
print(quality.ses.no)


################# Egger's regression test ####################################


# Need to read in Eggers file. In this file Vi is replaced by wi (per recommendation of Pustejovsky & Rodger, 2019)

raw.data <- read_excel("Bilingual_Meta_Table_Final_1130_Egger.xlsx")

trim.data <- subset(raw.data, yi >= -3 & yi <= 3) 


# create file using raw data for each domain. 

inh.data <- subset(raw.data, Domain == "Inhibition") 
sel.data <- subset(raw.data, Domain == "Selection")
shi.data <- subset(raw.data, Domain == "Switching")
att.data <- subset(raw.data, Domain == "Attention")
plan.data <- subset(raw.data, Domain == "Planning")
wm.data  <- subset(raw.data, Domain == "WM")
vm.data  <- subset(raw.data, Domain == "Visuospatial WM")
global.data  <- subset(raw.data, Domain == "Global EF")
reward.data  <- subset(raw.data, Domain == "Reward-based decision making")
dual.data  <- subset(raw.data, Domain == "Dual-Task")
executive.attention <- subset(raw.data, Domain == "Switching" | Domain == "Visuospatial WM" | Domain == "Selection")
executive.other <- subset(raw.data, Domain == "Inhibition" | Domain == "WM" | Domain == "Attention" | Domain == "Planning" | Domain == "Global EF" | Domain == "Reward-based decision making" | Domain == "Dual-Task")


Egger.overall <- rma.mv(yi,vi, data = raw.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.overall)

Egger.executive.attention <- rma.mv(yi,vi, data = executive.other, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.executive.attention)

Egger.executive.other <- rma.mv(yi,vi, data = executive.attention, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.executive.other)

Egger.inh <- rma.mv(yi,vi,data =inh.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.inh)

Egger.sel <- rma(yi,vi,data =sel.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.sel)

Egger.shi <- rma.mv(yi,vi,data =shi.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead"))  
print(Egger.shi)

Egger.wm <-  rma.mv(yi,vi,data =wm.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.wm)

Egger.vm <- rma.mv(yi,vi,data =vm.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.vm)


Egger.global <- rma.mv(yi,vi,data = global.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants) 
print(Egger.global)

Egger.reward <- rma.mv(yi,vi,data = reward.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants) 
print(Egger.reward)

Egger.dual  <- rma.mv(yi,vi,data = dual.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants) 
print(Egger.dual)

Egger.plan  <- rma.mv(yi,vi,data = plan.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants) 
print(Egger.plan)

Egger.att <- rma.mv(yi,vi,data = att.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.att)


# create file using trimmed data for each domain. 

inh.data <- subset(trim.data, Domain == "Inhibition") 
sel.data <- subset(trim.data, Domain == "Selection")
shi.data <- subset(trim.data, Domain == "Switching")
att.data <- subset(trim.data, Domain == "Attention")
plan.data <- subset(trim.data, Domain == "Planning")
wm.data  <- subset(trim.data, Domain == "WM")
vm.data  <- subset(trim.data, Domain == "Visuospatial WM")
global.data  <- subset(trim.data, Domain == "Global EF")
reward.data  <- subset(trim.data, Domain == "Reward-based decision making")
dual.data  <- subset(trim.data, Domain == "Dual-Task")
executive.attention <- subset(trim.data, Domain == "Switching" | Domain == "Visuospatial WM" | Domain == "Selection")
executive.other <- subset(trim.data, Domain == "Inhibition" | Domain == "WM" | Domain == "Attention" | Domain == "Planning" | Domain == "Global EF" | Domain == "Reward-based decision making" | Domain == "Dual-Task")

Egger.overall <- rma.mv(yi,vi, data = trim.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.overall)

Egger.executive.attention <- rma.mv(yi,vi, data = executive.other, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.executive.attention)

Egger.executive.other <- rma.mv(yi,vi, data = executive.attention, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.executive.other)

Egger.inh <- rma.mv(yi,vi,data =inh.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.inh)

Egger.sel <- rma(yi,vi,data =sel.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.sel)

Egger.shi <- rma.mv(yi,vi,data =shi.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead"))  
print(Egger.shi)

Egger.wm <-  rma.mv(yi,vi,data =wm.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.wm)

Egger.vm <- rma.mv(yi,vi,data =vm.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.vm)


Egger.global <- rma.mv(yi,vi,data = global.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants) 
print(Egger.global)

Egger.reward <- rma.mv(yi,vi,data = reward.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants) 
print(Egger.reward)

Egger.dual  <- rma.mv(yi,vi,data = dual.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants) 
print(Egger.dual)

Egger.plan  <- rma.mv(yi,vi,data = plan.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants) 
print(Egger.plan)

Egger.att <- rma.mv(yi,vi,data = att.data, mod = ~ sqrtwi, method = "REML", random = ~ 1 | LabGroup/Study/Participants, control=list(optimizer="optim", optmethod="Nelder-Mead")) 
print(Egger.att)

# Funnel plots on raw data 

global <- rma(yi,vi, data=global.data, measure="SMD", vtype="UB", method="REML")
regtest(global)
funnel(global, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(global, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")

reward <- rma(yi,vi, data=reward.data, measure="SMD", vtype="UB", method="REML")
funnel(reward, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(reward, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")

Dual <- rma(yi,vi, data=dual.data, measure="SMD", vtype="UB", method="REML")
funnel(Dual, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(Dual, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")

overall <- rma(yi,vi, data=raw.data, measure="SMD", vtype="UB", method="REML")
regtest(overall)
funnel(overall, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(overall, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")

inh <- rma(yi, vi =wi,  data=inh.data, measure="SMD", vtype="UB", method="REML")
funnel(inh, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(inh, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")

sel <- rma(yi,vi, data=sel.data, measure="SMD", vtype="UB", method="REML")
regtest(sel)
funnel(sel, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(sel, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")

wm <- rma(yi,vi, data=wm.data, measure="SMD", vtype="UB", method="REML")
funnel(wm, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(wm, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")


vm <- rma(yi,vi, data=vm.data, measure="SMD", vtype="UB", method="REML")
funnel(vm, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(vm, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")

shi <- rma(yi,vi, data=shi.data, measure="SMD", vtype="UB", method="REML")
funnel(shi, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(shi, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")


att <- rma(yi,sei=sqrtwi, data=att.data, measure="SMD", vtype="UB", method="REML")
funnel(att, yaxis="seinv", main="Inverse Standard Error", level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")
funnel(att, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), refline=0, xlab="Hedge's g")


# Cat Plot### 

yi <- trim.data$yi
vi <- trim.data$vi

forest(yi, vi,
       xlim=c(-2.5,3.5),        ### adjust horizontal plot region limits
       subset=order(yi),        ### order by size of yi
       slab=NA, annotate=FALSE, ### remove study labels and annotations
       efac=0,                  ### remove vertical bars at end of CIs
       pch=19,                  ### changing point symbol to filled circle
       col="gray40",            ### change color of points/CIs
       psize=2,                 ### increase point size
       cex.lab=1, cex.axis=1,   ### increase size of x-axis title/labels
       lty=c("solid","blank"))  ### remove horizontal line at top of plot

### draw points one more time to make them easier to see
points(sort(yi), k:1, pch=19, cex=0.5)

### add summary polygon at bottom and text
addpoly(res, mlab="", annotate=FALSE, cex=1)
text(-2, -2, "RE Model", pos=4, offset=0, cex=1)