

#### ANALYSIS SCRIPT ACCOMPANYING THE ARTICLE 
#### "The dispositional essence of proactive social preferences: The dark core of personality vis-à-vis 58 traits" (Psychological Science)
#### Benjamin E. Hilbig, Isabel Thielmann, Ingo Zettler, Morten Moshagen 
#### License: CC-By Attribution 4.0 International


#load relevant packages
library(psych)
library(stringr)
library(lavaan)
library(semTools)
library(car)

# read data
data <- read.csv2("data_osf.csv")


############ sample descriptives ############

table(data$sex_t5)
describe(data$age_t5)
describe(data$svo_payment)
describe(data$svo_angle)

# check for any influence of demographics on choices
cor.test(data$age_t5, data$svo_angle)
data$sex <- ifelse(data$sex_t5 == "male", 0, 1)
cohen.d(data$svo_angle, group = data$sex)


############ candidate trait selection ############

# get relevance-ratings 
ratings <- read.csv2("ratings.csv", header = TRUE)
#head(ratings)

# count number of clearly positive & negative ratings
ratings$counts.1 <- rowSums(ratings[, c('rater1', 'rater2', 'rater3', 'rater4')] == "1")
ratings$counts.0 <- rowSums(ratings[, c('rater1', 'rater2', 'rater3', 'rater4')] == "0")

#retain trait unless 3 raters agree it is irrelevant
ratings$set2 <- ifelse(ratings$counts.0 > 2, 0, 1)

#report how many and which traits were excluded
length(ratings$set2)- sum(ratings$set2)
excluded <- subset(ratings, set2 ==0)
excluded$construct

#define a strict set (for additional analyses): only if 3 raters agree it is relevant
ratings$set1 <- ifelse(ratings$counts.1 > 3, 1, 0)
sum(ratings$set1)

#final set of candidate traits
constructs <- subset(ratings, (set2==1))



############ main analyses to determine R² and R²-change ############


#define raw latent model structure to be re-run with candidate traits inserted (placeholder: ALT)

model.raw <- ' 

### D ###
  
D16 =~ d1+d6+d2+d11+d34+d55+d17+d50+d68+d24+d30+d48+d38+d40+d57+d54

### ALT ###

ALT =~ XXXX

### Regression ###

svo_angle ~ YYYY

'

# prepare results vectors

  #fit 
  rmseas <-vector()
  srmrs <- vector()
  
  #reliability
  omegas.D <-vector()
  omegas.ALT <- vector()
  
  #zero-order associations
  corrs.D.ALT <- vector()
  corrs.SVO.D <- vector()
  corrs.SVO.ALT <- vector()
  
  #effective sample size
  Ns <- vector()
  
  #explained variance
  R2s.D <- vector()
  R2s.ALT <- vector()
  R2s.both <- vector()



# create loop to re-run for each model comparison

i=1

while(i <= length(constructs$Indicators)) {
  
  construct = constructs$construct[i]  
  indicatornames = constructs$Indicators[i]
  
   print(i)
   print(construct)
  
  # insert indicators to create full model
  
    model.withiindicators <- str_replace(model.raw, "XXXX", indicatornames)
  
  # specify model to estimate reliabilities, laodings, and zero-order correlations (i.e., without regression); and double-check effective sampe size
  
    model.base <- str_replace(model.withiindicators , "svo_angle ~ YYYY", "SVO =~ svo_angle")
  
    fit.base <- sem(model.base, data=data)

    rmseas[i] <- as.numeric(fitMeasures(fit.base, fit.measures = "rmsea"))
    srmrs[i] <- as.numeric(fitMeasures(fit.base, fit.measures = "srmr"))
    
    rel <- reliability(fit.base)	
    omegas.D[i] <- as.numeric(rel[2,1])
    omegas.ALT[i] <- as.numeric(rel[2,2])
    
    corrs.D.ALT[i] <- as.numeric(lavInspect(fit.base, what="cor.lv" )[1,2])
    corrs.SVO.D[i] <- as.numeric(lavInspect(fit.base, what="cor.lv" )[1,3])
    corrs.SVO.ALT[i] <- as.numeric(lavInspect(fit.base, what="cor.lv" )[2,3])
  
    Ns[i] <- lavInspect(fit.base, what="nobs")
  
  # specify model with (only) D as predictor in regression and determine R²
  
    model.D <- str_replace(model.withiindicators , "YYYY", "D16")

    fit <- sem(model.D, data=data)
  
    R2s.D[i] <- as.numeric(inspect(fit, what="r2")["svo_angle"])
  
    
  # (for additional results) specify model with (only) candidate trait as predictor in regression and determine R²
    
    model.ALT <- str_replace(model.withiindicators , "YYYY", "ALT")
    
    fit <- sem(model.ALT, data=data)
    
    R2s.ALT[i] <- as.numeric(inspect(fit, what="r2")["svo_angle"])    
    
    
    
  # specify model with both D and the candidate trait as predictors and determine R²
  
    model.both <- str_replace(model.withiindicators , "YYYY", "D16 + ALT")

    fit3 <- sem(model.both, data=data)

    R2s.both[i] <- as.numeric(inspect(fit3, what="r2")["svo_angle"])
  
# go to next trait
  
  i=i+1
  
}


results.D16 <- cbind(constructs, Ns, rmseas, srmrs, omegas.D, omegas.ALT, corrs.D.ALT, corrs.SVO.D, corrs.SVO.ALT, R2s.D,  R2s.ALT, R2s.both)

#compute increase in R² (candidate > D)
results.D16$dR2.ALT = round((R2s.both - R2s.D),4)
results.D16$dR2.ALT[results.D16$dR2.ALT <0] <- 0

#compute increase in R² (D > candidate)
results.D16$dR2.D = round((R2s.both - R2s.ALT),4)
results.D16$dR2.D[results.D16$dR2.D <0] <- 0

#save results
write.csv(results.D16, "res.D16.csv", row.names=FALSE)

############ report summary results R²-change ############

#fits
round((median(results.D16$rmseas)),4)
round((median(results.D16$srmrs)),4)

# reliabilities
round((median(results.D16$omegas.D)),3)
round((median(results.D16$omegas.ALT)),3)

# zero-order latent correlations and R²
round((median(abs(results.D16$corrs.D.ALT))),3)
range(abs(results.D16$corrs.D.ALT))

round((median(abs(results.D16$corrs.SVO.D))),3)
round((median(results.D16$R2s.D)),3)


round((median(abs(results.D16$corrs.SVO.ALT))),3)
range(abs(results.D16$corrs.SVO.ALT))

#mult R² (D + candidate traits)
round((median(results.D16$R2s.both)),3)
range(abs(results.D16$R2s.both))


#R² increase by adding candidate traits (beyond D)
round((mean(results.D16$dR2.ALT)),3)
round((median(results.D16$dR2.ALT)),3)
round((range(results.D16$dR2.ALT)),4)


# candidate traits explaining incremental variance equal (or greater) to a small effect size
sum(results.D16$dR2.ALT >= .0182) 
positives <- subset(results.D16, dR2.ALT >= .0182, select=c(construct, corrs.D.ALT, corrs.SVO.ALT, corrs.SVO.D, dR2.ALT))
head(positives)

#create figure
bins <- seq(0,.04,.001)
hist(results.D16$dR2.ALT, col="#dddddd", ylim=c(0, 30), xlim=c(0,.04), breaks=bins, xlab="", ylab="", main="")
abline(v = median(results.D16$dR2.ALT), col = "#aaaaaa",lwd = 2,lty = 2)
abline(v = .0182, col = "#333333",lwd = 2,lty = 3)


#check whether incremental variance explained depends on reliability
cor.test(results.D16$omegas.ALT,results.D16$dR2.ALT)



# check whether main results change in a relevant way when considering only the strict set of 37 certainly relevant traits
results.D16.sub <- subset(results.D16, (set1==1))

round((median(abs(results.D16.sub$corrs.D.ALT))),3)
round((median(abs(results.D16.sub$corrs.SVO.ALT))),3)


round((mean(results.D16.sub$dR2.ALT)),3)
round((median(results.D16.sub$dR2.ALT)),3)
round((range(results.D16.sub$dR2.ALT)),3)
sum(results.D16.sub$dR2.ALT >= .0182) 

positives.sub <- subset(results.D16.sub, dR2.ALT >= .0182)
positives.sub$construct



# additional results for incremental variance explained D beyond candidate traits

#R² increase by adding D (beyond candidate traits)
round((mean(results.D16$dR2.D)),3)
round((median(results.D16$dR2.D)),3)
round((range(results.D16$dR2.D)),3)

# candidate traits for which D explains incremental variance less than a small effect size
sum(results.D16$dR2.D >= .0182) 
negatives <- subset(results.D16, dR2.D < .0182, select=c(construct, corrs.D.ALT, corrs.SVO.ALT, corrs.SVO.D, dR2.D))
negatives

round((mean(negatives$dR2.D)),3)
round((median(negatives$dR2.D)),3)
round((range(negatives$dR2.D)),3)

round((median(abs(negatives$corrs.D.ALT))),3)
round((median(abs(negatives$corrs.SVO.ALT))),3)
round((median(abs(negatives$corrs.SVO.D))),3)



############ redo with D70 ############

############ main analyses to determine R² and R²-change ############


#define raw latent model structure to be re-run with candidate traits inserted (placeholder: ALT)

#define latent model structure

model.raw <- ' 

### D biactor ###
  
  D70 =~ NA*d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15+d16+d17+d18+d19+d20+d21+d22+d23+d24+d25+d26+d27+d28+d29+d30+d31+d32+d33+d34+d35+d36+d37+d38+d39+d40+d41+d42+d43+d44+d45+d46+d47+d48+d49+d50+d51+d52+d53+d54+d55+d56+d57+d58+d59+d60+d61+d62+d63+d64+d65+d66+d67+d68+d69+d70
  dvin =~ d3+d6+d20+d21+d24+d29+d32+d45+d50+d53+d55+d57+d59+d60+d62+d64+d67
  dsad =~ NA*d8+d9+d10+d17+d18+d26+d33+1*d34+d37+d40+d42+d48+d51+d52+d54+d56+d66+d70
  dcal =~ d1+d4+d11+d15+d19+d28+d30+d35+d38+d44+d46+d58+d65+d68+d69
  dnar =~ d5+d7+d13+1*d22+d23+d25+d39+d41+d49+d61+d63
  ddec =~ NA*d2+d12+d14+d16+d27+1*d31+d36+d43+d47

  D70 ~~ 1*D70

  dcal + ddec + dnar + dsad + dvin ~~ 0*D70
  ddec + dnar + dsad + dvin ~~ 0*dcal
  dnar + dsad + dvin ~~ 0*ddec
  dsad + dvin ~~ 0*dnar
  dvin ~~ 0*dsad

### ALT ###

ALT =~ XXXX

### Regression ###

svo_angle ~ YYYY

#ZZZZ

'


# prepare results vectors

#fit 
rmseas <-vector()
srmrs <- vector()

#reliability
omegas.D <-vector()
omegas.ALT <- vector()

#zero-order associations
corrs.D.ALT <- vector()
corrs.SVO.D <- vector()
corrs.SVO.ALT <- vector()

#effective sample size
Ns <- vector()

#explained variance
R2s.D <- vector()
R2s.ALT <- vector()
R2s.both <- vector()



# create loop to re-run for each model comparison

i=1

while(i <= length(constructs$Indicators)) {
  
  construct = constructs$construct[i]  
  indicatornames = constructs$Indicators[i]
  
  # print(i)
   print(construct)
  
  # insert indicators to create full model
  
  model.withiindicators <- str_replace(model.raw, "XXXX", indicatornames)
  
  # specify model to estimate reliabilities, laodings, and zero-order correlations (i.e., without regression); and double-check effective sampe size
  
  model.base <- str_replace(model.withiindicators , "svo_angle ~ YYYY", "SVO =~ svo_angle")
  
  fit.base <- sem(model.base, data=data)
  
  rmseas[i] <- as.numeric(fitMeasures(fit.base, fit.measures = "rmsea"))
  srmrs[i] <- as.numeric(fitMeasures(fit.base, fit.measures = "srmr"))
  
  rel <- reliability(fit.base)	
  omegas.D[i] <- as.numeric(rel[2,1])
  omegas.ALT[i] <- as.numeric(rel[2,7])
  
  corrs.D.ALT[i] <- as.numeric(lavInspect(fit.base, what="cor.lv" )[1,7])
  corrs.SVO.D[i] <- as.numeric(lavInspect(fit.base, what="cor.lv" )[1,8])
  corrs.SVO.ALT[i] <- as.numeric(lavInspect(fit.base, what="cor.lv" )[8,7])
  
  Ns[i] <- lavInspect(fit.base, what="nobs")
  
  # specify model with (only) D as predictor in regression and determine R²
  
  model.D <- str_replace(model.withiindicators , "YYYY", "D70")
  #allow specific factors to correlate with SVO
  model.D <- str_replace(model.D , "#ZZZZ", "svo_angle ~~  dvin + dsad + dcal + dnar + ddec")
  
  fit <- sem(model.D, data=data)
  
  R2s.D[i] <- as.numeric(inspect(fit, what="r2")["svo_angle"])
  
  
  # (for additional results) specify model with (only) candidate trait as predictor in regression and determine R²
  
  model.ALT <- str_replace(model.withiindicators , "YYYY", "ALT")
  
  fit <- sem(model.ALT, data=data)
  
  R2s.ALT[i] <- as.numeric(inspect(fit, what="r2")["svo_angle"])    
  
  
  # specify model with both D and the candidate trait as predictors and determine R²
  
  model.both <- str_replace(model.withiindicators , "YYYY", "D70 + ALT")
  #allow specific factors to correlate with SVO
  model.both <- str_replace(model.both , "#ZZZZ", "svo_angle ~~  dvin + dsad + dcal + dnar + ddec")
  
  fit3 <- sem(model.both, data=data)
  
  R2s.both[i] <- as.numeric(inspect(fit3, what="r2")["svo_angle"])
  
  # go to next trait
  
  i=i+1
  
}


results.D70 <- cbind(constructs, Ns, rmseas, srmrs, omegas.D, omegas.ALT, corrs.D.ALT, corrs.SVO.D, corrs.SVO.ALT, R2s.D, R2s.ALT, R2s.both)




#compute increase in R² (candidate > D)
results.D70$dR2.ALT = round((R2s.both - R2s.D),4)
results.D70$dR2.ALT[results.D70$dR2.ALT <0] <- 0

#compute increase in R² (D > candidate)
results.D70$dR2.D = round((R2s.both - R2s.ALT),4)
results.D70$dR2.D[results.D70$dR2.D <0] <- 0


#save results
write.csv(results.D70, "res.D70.csv", row.names=FALSE)


############ report summary results R²-change ############

#fits
round((median(results.D70$rmseas)),4)
round((median(results.D70$srmrs)),4)

# reliabilities
round((median(results.D70$omegas.D)),3)
round((median(results.D70$omegas.ALT)),3)

# zero-order latent correlations 
round((median(abs(results.D70$corrs.D.ALT))),3)
range(abs(results.D70$corrs.D.ALT))

round((median(abs(results.D70$corrs.SVO.D))),3)

round((median(abs(results.D70$corrs.SVO.ALT))),3)
range(abs(results.D70$corrs.SVO.ALT))

#mult R² (D + candidate traits)
round((median(results.D70$R2s.both)),3)
range(abs(results.D70$R2s.both))


#R² increase by adding candidate traits (beyond D)
round((mean(results.D70$dR2.ALT)),3)
round((median(results.D70$dR2.ALT)),3)
round((range(results.D70$dR2.ALT)),4)

# candidate traits explaining incremental variance equal (or greater) to a small effect size
sum(results.D70$dR2.ALT >= .0182) 
positives <- subset(results.D70, dR2.ALT >= .0182, select=c(construct, corrs.D.ALT, corrs.SVO.ALT, corrs.SVO.D, dR2.ALT))
head(positives)


# check whether main results change in a relevant way when considering only the strict set of 37 certainly relevant traits
results.D70.sub <- subset(results.D70, (set1==1))

round((median(abs(results.D70.sub$corrs.D.ALT))),3)
round((median(abs(results.D70.sub$corrs.SVO.ALT))),3)


round((mean(results.D70.sub$dR2.ALT)),3)
round((median(results.D70.sub$dR2.ALT)),3)
round((range(results.D70.sub$dR2.ALT)),3)
sum(results.D70.sub$dR2.ALT >= .0182) 

positives.sub <- subset(results.D70.sub, dR2.ALT >= .0182)
positives.sub$construct


# additional results for D > candidate traits

#R² increase by adding D (beyond candidate traits)
round((mean(results.D70$dR2.D)),3)
round((median(results.D70$dR2.D)),3)
round((range(results.D70$dR2.D)),3)

# candidate traits for which D explains incremental variance less than a small effect size
sum(results.D70$dR2.D >= .0182) 
negatives <- subset(results.D70, dR2.D < .0182, select=c(construct, corrs.D.ALT, corrs.SVO.ALT, corrs.SVO.D, dR2.D))
head(negatives)

round((mean(negatives$dR2.D)),3)
round((median(negatives$dR2.D)),3)
round((range(negatives$dR2.D)),3)

round((median(abs(negatives$corrs.D.ALT))),3)
round((median(abs(negatives$corrs.SVO.ALT))),3)
round((median(abs(negatives$corrs.SVO.D))),3)

