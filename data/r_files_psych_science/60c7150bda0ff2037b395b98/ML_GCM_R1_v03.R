#-------------------------------------------------------------------------#

####------------SLS Cohorts - ML Growth Curve Models-----------------####

#-------------------------------------------------------------------------#

Daten.long1 <- read.csv ("IALSA_long_R1v02.csv", header=T, sep=",") 
Daten.long1$X <- NULL

library (psych)

describe (Daten.long1$C)
describe (Daten.long1$A)

apply(Daten.long1[c("C",
                    "A",
                    "E",	
                    "N",
                    "O")], 2, describe)

# z and T metric

names(Daten.long1)

Daten.long1[(c(27:31))]  <- scale (Daten.long1[,(c(27:31))],center=T, scale=T)

apply(Daten.long1[c("C",
                    "A",
                    "E",	
                    "N",
                    "O")], 2, describe)

Daten.long1[(c(27:31))] <- (Daten.long1[,(c(27:31))]*10+50)

apply(Daten.long1[c("C",
                    "A",
                    "E",	
                    "N",
                    "O")], 2, describe)



############--------ML GCM-----------#########

library (lme4)
library (lmerTest)

#####1) Empty model####
model.0C <- lmer(C ~ 1 + (1| IDNUMR), REML = FALSE , data = Daten.long1)
model.0A <- lmer(A ~ 1 + (1| IDNUMR), REML = FALSE , data = Daten.long1)
model.0E <- lmer(E ~ 1 + (1| IDNUMR), REML = FALSE , data = Daten.long1)
model.0O <- lmer(O ~ 1 + (1| IDNUMR), REML = FALSE , data = Daten.long1)
model.0N <- lmer(N ~ 1 + (1| IDNUMR), REML = FALSE , data = Daten.long1)

# ICC

icc.lmer <- function(mod){
  
  lvl1 <- attr(VarCorr(mod), "sc")^2
  lvl2 <- attr(VarCorr(mod)[[1]], "stddev")["(Intercept)"]^2
  icc <- lvl2/(lvl1+lvl2)
  
  oneran <- length(attr(VarCorr(mod)[[1]], "stddev"))==1
  onefix <- length(fixef(mod))==1
  isnullmod <- oneran * onefix
  
  if(isnullmod){
    names(icc) <- "icc"
  }else{
    names(icc) <- "residual.icc"
    cat("Warning: 'mod' is not an empty model, residual ICC is shown.\n\n")
  }
  return(icc)
}

summary (model.0C)
summary (model.0A)
summary (model.0E)
summary (model.0O)
summary (model.0N)

icc.lmer (model.0C)
icc.lmer (model.0A)
icc.lmer (model.0E)
icc.lmer (model.0O)
icc.lmer (model.0N)

##########----------------time is centered as an inviduals midpoint of study = MOS ------------##############

#rescale time to (0=0, 7=1, 14=2, 21=3, 28 = 4, 35=5, 42=6, 45=6.4, 49=7)

library (car)
Daten.long1$timescaled <- recode (Daten.long1$time, recodes='0=0; 7=1; 14=2; 21=3; 28=4; 35=5; 42=6; 45=6.4; 49=7')

table (Daten.long1$time)
table (Daten.long1$timescaled)

table (Daten.long1$MOS)
#rescale middle of study to (0=0; 3.5=0.5; 7=1; 10.5=1.5; 14=2; 17.5=2.5; 21=3; 22.5=3.2; 24.5=3.5)
Daten.long1$MOSscaled <- recode (Daten.long1$MOS, recodes= '0=0; 3.5=0.5; 7=1; 10.5=1.5; 14=2; 17.5=2.5; 21=3; 22.5=3.2; 24.5=3.5')
table (Daten.long1$MOSscaled)

#timec = timeinstudy-MOS

Daten.long1$timec <- Daten.long1$time-Daten.long1$MOS
Daten.long1$timec_scaled <- Daten.long1$timescaled-Daten.long1$MOSscaled

table(is.na(Daten.long1$timec))
table(is.na(Daten.long1$timec_scaled))

describe (Daten.long1$timec)
describe (Daten.long1$timec_scaled)
hist (Daten.long1$timec)
hist (Daten.long1$timec_scaled)
hist (Daten.long1$timescaled)

hist (Daten.long1$timec_scaled, main=paste(""), breaks=15, xlim= c(-4, 4), ylim=c(0,4000), xlab = "Time in Study", ylab = "Frequency")

#---------------------------------------------------------------------------------------------------------------------------------#

####2)GCM  ####

####time is re-scaled (time_scaled= changed time metric (0,1,2,...) and centered to the middle of study- MOS_scaled)####
#these analyses are used for models without covariates


lmer.varred <- function(...){
  require(lme4)
  get.lvl1 <- function(x){round(attr(VarCorr(x), "sc")^2, 3)}
  get.lvl2 <- function(x){round(VarCorr(x)[[1]][1,1], 3)}
  
  modlist <- list(...)
  headnames <- lapply(1:length(modlist), function(x){paste("model ", x, ": ", sep="")})
  dfrnames <- lapply(1:length(modlist), function(x){paste("model ", x, sep="")})
  fml <- lapply(modlist, formula)
  
  lvl1.res <- lapply(modlist, get.lvl1)
  lvl2.res <- lapply(modlist, get.lvl2)
  total.res <- lapply(modlist, function(x){get.lvl1(x)+get.lvl2(x)})
  
  header0 <- paste(headnames, fml, "\n", sep="", collapse="")
  
  header1 <- "\nResidual variances:\n"
  dfr1 <- rbind(lvl1.res, lvl2.res, total.res)
  colnames(dfr1) <- dfrnames
  rownames(dfr1) <- c("Level 1 Res.","Level 2 Res.","Total Res.")
  
  header2 <- "\nAbsolute reduction of residual variances:\n"
  header3 <- "\nRelative reduction of residual variances:\n"
  dfr3 <- dfr2 <- dfr1; dfr3[,1] <- dfr2[,1] <- 0
  if(length(modlist)>1){
    for(i in 2:length(modlist)){dfr2[,i] <- -(unlist(dfr1[,i]) - unlist(dfr1[,i-1]))
    dfr3[,i] <- -((unlist(dfr1[,i]) - unlist(dfr1[,i-1]))/unlist(dfr1[,i-1]))}
  }
  
  cat(header0, header1, paste(capture.output(dfr1), "\n", sep=""),
      header2, paste(capture.output(dfr2), "\n", sep=""),
      header3, paste(capture.output(dfr3), "\n", sep=""))
}

#####Agreeableness####

model1At <- lmer(A ~ 1+timec_scaled+
                   (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1At)


model1At2 <- lmer(A ~ 1+(timec_scaled+I(timec_scaled^2))+
                    (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1At2)

model1ARSt <- lmer(A ~ 1+timec_scaled+
                     (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1ARSt)


anova (model1ARSt,model1At) 

model1A <- lmer(A ~ 1+timec_scaled + ageMc + ageMc*timec_scaled+
                  (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1A)

model1A1 <- lmer(A ~ 1+timec_scaled + ageMc +
                  (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1A1) 

model1Aa <- lmer(A ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + ageMc*timec_scaled+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Aa)

model1Ab <- lmer(A ~ 1+timec_scaled + ageMc + ageMc*timec_scaled+
                   (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ab)

FE_a <- as.data.frame(round(coef(summary(model1A)),3)) #fixed effects
RE_a <- as.data.frame(VarCorr(model1A)) # random effects


#Cohort
model1Ac <- lmer(A ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + timec_scaled*ybMc + timec_scaled*ybMc*ageMc+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ac) 

model1Ac1 <- lmer(A ~ 1+timec_scaled + ageMc + ybMc + + ageMc*ybMc+ timec_scaled*ybMc +
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ac1) 


FE_ac <- as.data.frame(round(coef(summary(model1Ac1)),3)) #fixed effects
RE_ac <- as.data.frame(VarCorr(model1Ac1)) # random effects

lmer.varred (model1A,model1Ac1)


#####Conscientiousness####

model1Ct <- lmer(C ~ 1+timec_scaled+
                   (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ct)


model1Ct2 <- lmer(C ~ 1+(timec_scaled+I(timec_scaled^2))+
                    (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ct2)

model1CRSt <- lmer(C ~ 1+timec_scaled+
                     (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1CRSt)


anova (model1CRSt,model1Ct) 

model1CRSt2 <- lmer(C ~ 1+(timec_scaled+I(timec_scaled^2))+
                      (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1CRSt2)


model1C <- lmer(C ~ 1+timec_scaled + ageMc + ageMc*timec_scaled+
                  (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1C) 

model1Ca <- lmer(C ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + ageMc*timec_scaled+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ca)


model1Cb <- lmer(C ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + ageMc*timec_scaled+ (ageMc*I(timec_scaled^2))+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Cb)

model1Cc <- lmer(C ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + 
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Cc)

model1Cd <- lmer(C ~ 1+timec_scaled + ageMc + ageMc*timec_scaled+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Cd)

FE_C <- as.data.frame(round(coef(summary(model1Cd)),3)) #fixed effects
RE_C <- as.data.frame(VarCorr(model1Cd)) # random effects


#Cohort
model1Cd1 <- lmer(C ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + timec_scaled*ybMc + 
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Cd1) 

model1Cd2 <- lmer(C ~ 1+timec_scaled + ageMc + ybMc+ ageMc*ybMc + 
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Cd2)

model1Cd3 <- lmer(C ~ 1+timec_scaled + ageMc + ybMc+ 
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Cd3)

FE_Cc <- as.data.frame(round(coef(summary(model1Cd3)),3)) #fixed effects
RE_Cc <- as.data.frame(VarCorr(model1Cd3)) 

lmer.varred (model1Cd,model1Cd3)


#####Extraversion####

model1Et <- lmer(E ~ 1+timec_scaled+
                   (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Et)


model1Et2 <- lmer(E ~ 1+(timec_scaled+I(timec_scaled^2))+
                    (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Et2)

model1ERSt <- lmer(E ~ 1+timec_scaled+
                     (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1ERSt)


anova (model1ERSt,model1Et) 

model1ERSt2 <- lmer(E ~ 1+(timec_scaled+I(timec_scaled^2))+
                      (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1ERSt2)


model1E <- lmer(E ~ 1+timec_scaled + ageMc + ageMc*timec_scaled+
                  (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1E) 

model1Ea <- lmer(E ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + ageMc*timec_scaled+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ea) 


model1Eb <- lmer(E ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + ageMc*timec_scaled+ (ageMc*I(timec_scaled^2))+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Eb) 

FE_E <- as.data.frame(round(coef(summary(model1E)),3)) #fixed effects
RE_E <- as.data.frame(VarCorr(model1E)) # random effects

#Cohort
model1Ec <- lmer(E ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + timec_scaled*ybMc +  timec_scaled*ybMc*ageMc+ 
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ec) 

model1Ec1 <- lmer(E ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled +
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ec1) 

FE_Ec <- as.data.frame(round(coef(summary(model1Ec1)),3))
RE_Ec <- as.data.frame(VarCorr(model1Ec1)) # random effects

lmer.varred (model1E,model1Ec1)

#####Neuroticism####

model1Nt <- lmer(N ~ 1+timec_scaled+
                   (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Nt)


model1Nt2 <- lmer(N ~ 1+(timec_scaled+I(timec_scaled^2))+
                    (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Nt2)

model1NRSt <- lmer(N ~ 1+timec_scaled+
                     (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1NRSt)


anova (model1NRSt,model1Nt)

model1NRSt2 <- lmer(N ~ 1+(timec_scaled+I(timec_scaled^2))+
                      (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1NRSt2) 

model1N <- lmer(N ~ 1+timec_scaled + ageMc + ageMc*timec_scaled+
                  (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1N) 

model1Na <- lmer(N ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + ageMc*timec_scaled+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Na)


model1Nb <- lmer(N ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + ageMc*timec_scaled+ (ageMc*I(timec_scaled^2))+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Nb) 

FE_N <- as.data.frame(round(coef(summary(model1N)),3)) #fixed effects
RE_N <- as.data.frame(VarCorr(model1N)) # random effects


#Cohort
model1Nc <- lmer(N ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + timec_scaled*ybMc +  timec_scaled*ybMc*ageMc+ 
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Nc)

model1Nc1 <- lmer(N ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + timec_scaled*ybMc +
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Nc1) 

model1Nc2 <- lmer(N ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc +
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Nc2) 

FE_Nc <- as.data.frame(round(coef(summary(model1Nc2)),3))
RE_Nc <- as.data.frame(VarCorr(model1Nc2)) # random effects

lmer.varred (model1N,model1Nc1)


#####Openness####

model1Ot <- lmer(O ~ 1+timec_scaled+
                   (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ot)

model1Ot2 <- lmer(O ~ 1+(timec_scaled+I(timec_scaled^2))+
                    (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ot2)

model1ORSt <- lmer(O ~ 1+timec_scaled+
                     (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1ORSt)


anova (model1ORSt,model1Ot) 

model1ORSt2 <- lmer(O ~ 1+(timec_scaled+I(timec_scaled^2))+
                      (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1ORSt2) 

model1O <- lmer(O ~ 1+timec_scaled + ageMc + ageMc*timec_scaled+
                  (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1O) 

model1Oa <- lmer(O ~ 1+(timec_scaled + I(timec_scaled^2)) + ageMc + ageMc*timec_scaled+
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Oa)


model1Ob <- lmer(O ~ 1+timec_scaled + ageMc + ageMc*timec_scaled+
                  (1| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ob) 

model1Ox <- lmer(O ~ 1+timec_scaled + ageMc + 
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ox) 

FE_O <- as.data.frame(round(coef(summary(model1O)),3)) #fixed effects
RE_O <- as.data.frame(VarCorr(model1O)) # random effects


#Cohort
model1Oc <- lmer(O ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + timec_scaled*ybMc +  timec_scaled*ybMc*ageMc+ 
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Oc) 

model1Oc1 <- lmer(O ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + 
                   (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Oc1) 

FE_Oc <- as.data.frame(round(coef(summary(model1Oc1)),3))
RE_Oc <- as.data.frame(VarCorr(model1Oc1)) # random effects

lmer.varred (model1O,model1Oc1)



###Covariates (educ, sex)####  ######
#time is re-scaled (time_scaled= changed time metric (0,1,2,...) and centered to the middle of study- MOS_scaled)#
#these analyses are used for models with covariates

#center sex

Daten.long1$sexc <- scale (Daten.long1$sex, center =T, scale =F)
describe (Daten.long1$sexc)
describe (Daten.long1$sex)

#####Agreeableness####

#Cohort

model1Ac1 <- lmer(A ~ 1+timec_scaled + ageMc + ybMc + + ageMc*ybMc+ timec_scaled*ybMc + sexc + educ_c+ 
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ac1) 

FE_ac <- as.data.frame(round(coef(summary(model1Ac1)),3)) #fixed effects
RE_ac <- as.data.frame(VarCorr(model1Ac1)) # random effects


lmer.varred (model1A,model1Ac1)


####Conscientiousness####

model1Ccd3 <- lmer(C ~ 1+timec_scaled + ageMc + ybMc+  sexc + educ_c+ sexc*ybMc+
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ccd3)

FE_Cc <- as.data.frame(round(coef(summary(model1Ccd3)),3)) #fixed effects
RE_Cc <- as.data.frame(VarCorr(model1Ccd3)) 

lmer.varred (model1Cd,model1Ccd3)


Data_long_female <- subset (Daten.long1, Daten.long1$sex==1)
Data_long_male <- subset (Daten.long1, Daten.long1$sex==0)

model1Ccd3_fem <- lmer(C ~ 1+timec_scaled + ageMc + ybMc + educ_c+ 
                     (1+timec_scaled| IDNUMR), REML = FALSE , data = Data_long_female)
summary (model1Ccd3_fem)

model1Ccd3_mal <- lmer(C ~ 1+timec_scaled + ageMc + ybMc + educ_c+ 
                         (1+timec_scaled| IDNUMR), REML = FALSE , data = Data_long_male)
summary (model1Ccd3_mal)


####Extraversion ####

model1Ec1 <- lmer(E ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + sexc + educ_c+
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ec1) 

FE_Ec <- as.data.frame(round(coef(summary(model1Ec1)),3))
RE_Ec <- as.data.frame(VarCorr(model1Ec1)) # random effects

model1Ec2 <- lmer(E ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + sexc + educ_c+ sexc*ybMc+
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Ec2) 


lmer.varred (model1E,model1Ec1)

####Neuroticism#####

#Cohort

model1Nc2 <- lmer(N ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + sexc + educ_c+ sexc*ybMc+
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Nc2) 

FE_Nc <- as.data.frame(round(coef(summary(model1Nc2)),3))
RE_Nc <- as.data.frame(VarCorr(model1Nc2)) 

lmer.varred (model1N,model1Nc2)

####Openness####

#Cohort#

model1Oc1 <- lmer(O ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + sexc + educ_c+
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Oc1) 

FE_Oc <- as.data.frame(round(coef(summary(model1Oc1)),3))
RE_Oc <- as.data.frame(VarCorr(model1Oc1)) # random effects

model1Oc2 <- lmer(O ~ 1+timec_scaled + ageMc + ybMc + ageMc*timec_scaled + ageMc*ybMc + sexc + educ_c+ sexc*ybMc+
                    (1+timec_scaled| IDNUMR), REML = FALSE , data = Daten.long1)
summary (model1Oc2) 

lmer.varred (model1O,model1Oc1)






