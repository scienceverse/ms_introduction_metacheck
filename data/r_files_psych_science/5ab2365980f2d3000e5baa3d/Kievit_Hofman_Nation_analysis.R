#' ------------------------------------------------------------------
#' ------------------------------------------------------------------
#' This script replicates the analyses in the following paper
#' Mutualistic coupling between vocabulary and reasoning in young children: A replication and extension of Kievit et al. (2017)
#' Authors: Rogier A. Kievit, Abe Hofman & Kate Nation
#' Script author: Rogier Kievit, October 2018 
#' 
#' The analyses are run on a cohort of 227 children age 6-8, using only the 3
#' waves that have data on both relevant domains. Cohort lead is Professor Kate
#' Nation at Oxford
# ------------------------------------------------------------------

rm(list = ls())

# load packages
library(e1701)
library(lavaan)
library(ggplot2)
library(qpcR)
library(reshape2)
library(psych)

# ------------------------------------------------------------------
#Read in data
cogdat<-read.csv('cogdat.csv',sep=',')

# ------------------------------------------------------------------
# Descriptives 
# Gender balance: Number of girls (0)
sum(cogdat$Gender == 0) 

# (Supplementary Table 1)
apply(cogdat, 2, function(x) sum(!is.na(x))) # not missing observation
apply(cogdat, 2, range, na.rm = TRUE) # range
apply(cogdat[,-7], 2, mean, na.rm = TRUE) # mean scores
apply(cogdat[,-7], 2, sd, na.rm = TRUE) # sd scores
apply(cogdat[,-7], 2, e1071::skewness, na.rm = TRUE) # skewness
apply(cogdat[,-7], 2, e1071::kurtosis, na.rm = TRUE) # kurtosis

# ------------------------------------------------------------------
# Model fitting in same order as Kievit et al. 2018 
# (with cross-temporal equality constraints as default whenever tenable)

# Reliability correction. Here we impose error variances based on the suggested correction as noted in Brown, 2006, p. 139
# to constrain the error variance of single indicators to S^2(1-Rxx)); where S^2 is the variance 
# of the observed score and Rxx is the manual-derived split half reliability 

#Doing so for each timepoint, based on the manual-reported split half reliability yields the following
var(cogdat$t1ssvocab,na.rm=T)
var(cogdat$t1ssmatrix,na.rm=T)

39.35973*(1-.89) #voc  .89 
20.98639*(1-.92) #mat  .92

var(cogdat$t2ssvocab,na.rm=T)
var(cogdat$t2ssmatrix,na.rm=T)

60.40562*(1-.89)  #voc .89
39.32092*(1-.92)  #mat  .92

var(cogdat$t3ssvocab,na.rm=T)
var(cogdat$t3ssmatrix,na.rm=T)
61.44958*(1-.89)  #voc .89
40.71807*(1-.92)  #mat  .92


#Fitting the mutualism model with reliability adjustment 
mutualism <- '

t1ssvoclv=~1*t1ssvocab
t2ssvoclv=~1*t2ssvocab
t3ssvoclv=~1*t3ssvocab
t1ssmatlv=~1*t1ssmatrix
t2ssmatlv=~1*t2ssmatrix
t3ssmatlv=~1*t3ssmatrix


t2ssvoclv ~ 1 * t1ssvoclv
t3ssvoclv ~ 1 * t2ssvoclv

etavoc1 =~ 1 * t2ssvoclv
etavoc2 =~ 1 * t3ssvoclv

etavoc1 ~ vocint1 * 1   
etavoc2 ~ vocint2 * 1 # only exception to cross-wave equality constraints 

t1ssvoclv ~ 1
t2ssvoclv ~ 0 * 1
t3ssvoclv ~ 0 * 1

etavoc1 ~~ varetavoc1 * etavoc1
etavoc2 ~~ varetavoc1 * etavoc2

t2ssmatlv ~ 1 * t1ssmatlv
t3ssmatlv ~ 1 * t2ssmatlv

etamat1 =~ 1 * t2ssmatlv
etamat2 =~ 1 * t3ssmatlv

etamat1 ~ matint1 * 1   
etamat2 ~ matint1 * 1   

t1ssmatlv ~ 1
t2ssmatlv ~ 0 * 1
t3ssmatlv ~ 0 * 1

# variances/covariances
etamat1 ~~ varetamat1 * etamat1
etamat2 ~~ varetamat1 * etamat2

t1ssmatlv ~~ t1ssvoclv

etamat1 ~ negmat * t1ssmatlv + couplinga * t1ssvoclv
etamat2 ~ negmat * t2ssmatlv + couplinga * t2ssvoclv

etavoc1 ~ negvoc * t1ssvoclv + couplingb * t1ssmatlv
etavoc2 ~ negvoc * t2ssvoclv + couplingb * t2ssmatlv

etamat1 ~~ a * etavoc1
etamat2 ~~ a * etavoc2

t1ssvocab~~ 4.32957*t1ssvocab
t2ssvocab~~ 6.644618*t2ssvocab
t3ssvocab~~ 6.759454*t3ssvocab
t1ssmatrix~~1.678911*t1ssmatrix
t2ssmatrix~~3.14567*t2ssmatrix
t3ssmatrix~~3.257446*t3ssmatrix

'

est_mutualism <- growth(mutualism, data = cogdat, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(est_mutualism, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

#Fitting the investment model with reliability adjustment
investment <- '

t1ssvoclv=~1*t1ssvocab
t2ssvoclv=~1*t2ssvocab
t3ssvoclv=~1*t3ssvocab
t1ssmatlv=~1*t1ssmatrix
t2ssmatlv=~1*t2ssmatrix
t3ssmatlv=~1*t3ssmatrix


t2ssvoclv ~ 1 * t1ssvoclv
t3ssvoclv ~ 1 * t2ssvoclv

etavoc1 =~ 1 * t2ssvoclv
etavoc2 =~ 1 * t3ssvoclv

etavoc1 ~ vocint1 * 1   
etavoc2 ~ vocint2 * 1 # only exception to cross-wave equality constraints 

t1ssvoclv ~ 1
t2ssvoclv ~ 0 * 1
t3ssvoclv ~ 0 * 1

etavoc1 ~~ varetavoc1 * etavoc1
etavoc2 ~~ varetavoc1 * etavoc2

t2ssmatlv ~ 1 * t1ssmatlv
t3ssmatlv ~ 1 * t2ssmatlv

etamat1 =~ 1 * t2ssmatlv
etamat2 =~ 1 * t3ssmatlv

etamat1 ~ matint1 * 1   
etamat2 ~ matint1 * 1   

t1ssmatlv ~ 1
t2ssmatlv ~ 0 * 1
t3ssmatlv ~ 0 * 1

# variances/covariances
etamat1 ~~ varetamat1 * etamat1
etamat2 ~~ varetamat1 * etamat2

t1ssmatlv ~~ t1ssvoclv

etamat1 ~ negmat * t1ssmatlv + 0 * t1ssvoclv
etamat2 ~ negmat * t2ssmatlv + 0 * t2ssvoclv

etavoc1 ~ negvoc * t1ssvoclv + couplingb * t1ssmatlv
etavoc2 ~ negvoc * t2ssvoclv + couplingb * t2ssmatlv

etamat1 ~~ a * etavoc1
etamat2 ~~ a * etavoc2

t1ssvocab~~ 4.32957*t1ssvocab
t2ssvocab~~ 6.644618*t2ssvocab
t3ssvocab~~ 6.759454*t3ssvocab
t1ssmatrix~~1.678911*t1ssmatrix
t2ssmatrix~~3.14567*t2ssmatrix
t3ssmatrix~~3.257446*t3ssmatrix

'

est_investment <- growth(investment, data = cogdat, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(est_investment, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)


# Fit the g-factor model
gfactor <- '
# Measurement model
gft1 =~ fl1 * t1ssmatrix + fl2 * t1ssvocab
gft2 =~ fl1 * t2ssmatrix + fl2 * t2ssvocab
gft3 =~ fl1 * t3ssmatrix + fl2 * t3ssvocab

# LCS specification
gft2 ~ 1 * gft1
gft3 ~ 1 * gft2

etag1 =~ 1 * gft2
etag2 =~ 1 * gft3

gft2 ~~ 0 * gft2
gft3 ~~ 0 * gft3

# Latent change intercepts
etag1 ~ etaint1 * 1
etag2 ~ etaint1 * 1
gft1 ~ 1
gft2 ~ 0 * 1
gft3 ~ 0 * 1
etag1 ~~ etag1
etag2 ~~ etag2

# Structural model
etag1 ~ selffb1 * gft1
etag2 ~ selffb1 * gft2

# Residual variances
t1ssmatrix ~~ resvarmat * t1ssmatrix
t2ssmatrix ~~ resvarmat * t2ssmatrix
t3ssmatrix ~~ resvarmat * t3ssmatrix
t1ssvocab ~~ resvarvoc * t1ssvocab
t2ssvocab ~~ resvarvoc * t2ssvocab
t3ssvocab ~~ resvarvoc * t3ssvocab

# Residual covariances
t1ssmatrix ~~ rescovmat * t2ssmatrix
t3ssmatrix ~~ rescovmat * t2ssmatrix
t3ssmatrix ~~ rescovmat * t1ssmatrix

t1ssvocab ~~ rescovvoc * t2ssvocab
t2ssvocab ~~ rescovvoc * t3ssvocab
t3ssvocab ~~ rescovvoc * t1ssvocab'

est_gfactor <- growth(gfactor, data = cogdat, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(est_gfactor, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# ------------------------------------------------------------------------
# Model comparison
# Mutualism model fits better than investment

anova(est_investment, est_mutualism)
# Mutualism model fits better than gfactor
c(AIC(est_gfactor), AIC(est_mutualism))
c(BIC(est_gfactor), BIC(est_mutualism))


## Respecify as a parallel process model
parallel_process_mutualism <- ' 
imat =~ 1 * t1ssmatrix + 1 * t2ssmatrix + 1 * t3ssmatrix
smat =~ 0 * t1ssmatrix + 1 * t2ssmatrix + 2 * t3ssmatrix

ivoc =~ 1 * t1ssvocab + 1 * t2ssvocab + 1 * t3ssvocab
svoc =~ 0 * t1ssvocab + 1 * t2ssvocab + 2 * t3ssvocab

t1ssmatrix ~~ varmat1 * t1ssmatrix
t2ssmatrix ~~ varmat2 * t2ssmatrix
t3ssmatrix ~~ varmat3 * t3ssmatrix

t1ssvocab ~~ varvoc1 * t1ssvocab
t2ssvocab ~~ varvoc2 * t2ssvocab
t3ssvocab ~~ varvoc3 * t3ssvocab

smat ~ ivoc
svoc ~ imat
'

##### Fits well - coupling explaining ~ 18-26% of the slope variance
est_parallel_process_mutualism <- growth(parallel_process_mutualism, data = cogdat, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(est_parallel_process_mutualism, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

#parallel process model with extra parameters worsens fit
parallel_process_mut_extra <- ' 
imat =~ 1 * t1ssmatrix + 1 * t2ssmatrix + 1 * t3ssmatrix
smat =~ 0 * t1ssmatrix + 1 * t2ssmatrix + 2 * t3ssmatrix

ivoc =~ 1 * t1ssvocab + 1 * t2ssvocab + 1 * t3ssvocab
svoc =~ 0 * t1ssvocab + 1 * t2ssvocab + 2 * t3ssvocab

t1ssmatrix ~~ varmat1 * t1ssmatrix
t2ssmatrix ~~ varmat2 * t2ssmatrix
t3ssmatrix ~~ varmat3 * t3ssmatrix

t1ssvocab ~~ varvoc1 * t1ssvocab
t2ssvocab ~~ varvoc2 * t2ssvocab
t3ssvocab ~~ varvoc3 * t3ssvocab

smat ~ ivoc
svoc ~ imat

smat ~ imat
svoc ~ ivoc

'

##### Fits worse than model which omits within-domain driving parameters
est_parallel_process_mut_extra <- growth(parallel_process_mut_extra, data = cogdat, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(est_parallel_process_mut_extra, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

anova(est_parallel_process_mut_extra,est_parallel_process_mutualism)


# ------------------------------------------------------------------------
# Multigroup model for boys and girls: equality constraints don't adversely affect fit
# Equality constraints are hand coded by appending 'a' and 'b' for the freely 
# estimated model.

mutualism_constrained_lv_mg <- '

t1ssvoclv=~1*t1ssvocab
t2ssvoclv=~1*t2ssvocab
t3ssvoclv=~1*t3ssvocab
t1ssmatlv=~1*t1ssmatrix
t2ssmatlv=~1*t2ssmatrix
t3ssmatlv=~1*t3ssmatrix

t2ssvoclv ~ 1 * t1ssvoclv
t3ssvoclv ~ 1 * t2ssvoclv

etavoc1 =~ 1 * t2ssvoclv
etavoc2 =~ 1 * t3ssvoclv

etavoc1 ~ c(vocint1,vocint1) *  1   
etavoc2 ~ c(vocint2,vocint2) * 1 # only exception to cross-wave equality constraints 

t1ssvoclv ~ c(mvoc,mvoc) *1
t2ssvoclv ~ 0 * 1
t3ssvoclv ~ 0 * 1

t2ssmatlv ~ 1 * t1ssmatlv
t3ssmatlv ~ 1 * t2ssmatlv

etamat1 =~ 1 * t2ssmatlv
etamat2 =~ 1 * t3ssmatlv

etamat1 ~ c(matint1,matint1)  * 1   
etamat2 ~ c(matint1,matint1)  * 1   

t1ssmatlv ~c(mmat,mmat) * 1
t2ssmatlv ~ 0 * 1
t3ssmatlv ~ 0 * 1

##### Variances

etamat1 ~~ c(varetamat1,varetamat1) * etamat1
etamat2 ~~ c(varetamat1,varetamat1) * etamat2
etavoc1 ~~ c(varetavoc1,varetavoc1) * etavoc1
etavoc2 ~~ c(varetavoc1,varetavoc1) * etavoc2

t1ssvoclv~~c(varvoc,varvoc) * t1ssvoclv
t1ssmatlv~~c(varmat,varmat) * t1ssmatlv


##### Covariances
etamat1~~c(a,a) * etavoc1
etamat2~~c(a,a) * etavoc2
t1ssmatrix~~c(cov,cov) * t1ssvocab


##### Structural model
etamat1~c(negmat,negmat) * t1ssmatrix+c(couplinga,couplinga) * t1ssvocab
etamat2~c(negmat,negmat) * t2ssmatrix+c(couplinga,couplinga) * t2ssvocab

etavoc1~c(negvoc,negvoc) * t1ssvocab+c(couplingb,couplingb) * t1ssmatrix
etavoc2~c(negvoc,negvoc) * t2ssvocab+c(couplingb,couplingb) * t2ssmatrix


#Residual variances
t1ssvocab~~ 4.32957*t1ssvocab
t2ssvocab~~ 6.644618*t2ssvocab
t3ssvocab~~ 6.759454*t3ssvocab
t1ssmatrix~~1.678911*t1ssmatrix
t2ssmatrix~~3.14567*t2ssmatrix
t3ssmatrix~~3.257446*t3ssmatrix


'

est_mutualism_constrained_lv_mg <- growth(mutualism_constrained_lv_mg, data=cogdat, estimator='mlr',fixed.x=FALSE,group='Gender',missing='fiml')
summary(est_mutualism_constrained_lv_mg, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

mutualism_free_lv_mg <- '

t1ssvoclv=~1*t1ssvocab
t2ssvoclv=~1*t2ssvocab
t3ssvoclv=~1*t3ssvocab
t1ssmatlv=~1*t1ssmatrix
t2ssmatlv=~1*t2ssmatrix
t3ssmatlv=~1*t3ssmatrix

t2ssvoclv ~ 1 * t1ssvoclv
t3ssvoclv ~ 1 * t2ssvoclv

etavoc1 =~ 1 * t2ssvoclv
etavoc2 =~ 1 * t3ssvoclv

etavoc1 ~ c(vocint1a,vocint1b) *  1   
etavoc2 ~ c(vocint2a,vocint2b) * 1 # only exception to cross-wave equality constraints 

t1ssvoclv ~ c(mvoca,mvocb) *1
t2ssvoclv ~ 0 * 1
t3ssvoclv ~ 0 * 1

t2ssmatlv ~ 1 * t1ssmatlv
t3ssmatlv ~ 1 * t2ssmatlv

etamat1 =~ 1 * t2ssmatlv
etamat2 =~ 1 * t3ssmatlv

etamat1 ~ c(matint1a,matint1b)  * 1   
etamat2 ~ c(matint1a,matint1b)  * 1   

t1ssmatlv ~c(mmata,mmatb) * 1
t2ssmatlv ~ 0 * 1
t3ssmatlv ~ 0 * 1

##### Variances

etamat1 ~~ c(varetamat1a,varetamat1b) * etamat1
etamat2 ~~ c(varetamat1a,varetamat1b) * etamat2
etavoc1 ~~ c(varetavoc1a,varetavoc1b) * etavoc1
etavoc2 ~~ c(varetavoc1a,varetavoc1b) * etavoc2

t1ssvoclv~~c(varvoca,varvocb) * t1ssvoclv
t1ssmatlv~~c(varmata,varmatb) * t1ssmatlv


##### Covariances
etamat1~~c(a,a) * etavoc1
etamat2~~c(a,a) * etavoc2
t1ssmatrix~~c(cov,cov) * t1ssvocab


##### Structural model
etamat1~c(negmata,negmatb) * t1ssmatrix+c(couplinga_a,couplinga_b) * t1ssvocab
etamat2~c(negmata,negmatb) * t2ssmatrix+c(couplinga_a,couplinga_b) * t2ssvocab

etavoc1~c(negvoca,negvocb) * t1ssvocab+c(couplingb_a,couplingb_b) * t1ssmatrix
etavoc2~c(negvoca,negvocb) * t2ssvocab+c(couplingb_a,couplingb_b) * t2ssmatrix


#Residual variances
t1ssvocab~~ 4.32957*t1ssvocab
t2ssvocab~~ 6.644618*t2ssvocab
t3ssvocab~~ 6.759454*t3ssvocab
t1ssmatrix~~1.678911*t1ssmatrix
t2ssmatrix~~3.14567*t2ssmatrix
t3ssmatrix~~3.257446*t3ssmatrix

'

est_mutualism_free_lv_mg <- growth(mutualism_free_lv_mg, data=cogdat, estimator='mlr',fixed.x=FALSE,group='Gender',missing='fiml')
summary(est_mutualism_free_lv_mg, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

#No significant improvement upon freely estimating key parameters
anova(est_mutualism_constrained_lv_mg,est_mutualism_free_lv_mg)


####Fisher transformation of standardized coupling effects
#### the first parameter is always NSPN, the second the Oxford sample

#self-feedback reasoning 
r.test(785,-.59, -0.258, n2=227)
  r.test(785,-.602, -0.258, n2=227)
r.test(785,-.59, -0.383, n2=227)
  r.test(785,-.602, -0.383, n2=227)
  
#self-feedback voc
r.test(785,-0.37, -0.189, n2=227)
  r.test(785,-0.3602, -0.189, n2=227)
r.test(785,-0.37, -0.227, n2=227)
  r.test(785,-0.3602, -0.227, n2=227)

#coupling reas to voc
r.test(785,.144,0.222,n2=227)
  r.test(785,.155,0.222,n2=227)
r.test(785,.144,0.279,n2=227)
  r.test(785,.155,0.279,n2=227)

#coupling voc to reas
r.test(785,0.203,0.357,n2=227)
r.test(785,0.203,0.507,n2=227)

#Parallel process
r.test(785,.144,0.433,n2=227)
      r.test(785,.155,0.433,n2=227)
r.test(785,0.203,0.51,n2=227)

#residual
resid_NSPN<-.1003
resid_Oxford<-.299
r.test(785,resid_NSPN,resid_Oxford,n2=227)

# ------------------------------------------------------------------------
# Plots
# ------------------------------------------------------------------------

# Model comparison: Compute akaike weights according to Wagenmakers and Farrell

aic_vecall <- c(AIC(est_gfactor), AIC(est_mutualism), AIC(est_investment))
bic_vecall <- c(BIC(est_gfactor), BIC(est_mutualism), BIC(est_investment))
valuesweights <- akaike.weights(aic_vecall)$weights

modeldat <- data.frame(Model = factor(rep(c('g factor', 'Mutualism', 'Investment'), 2)),
                       IC = factor(rep(c('AIC','BIC'), each = 3)),
                       values = c(aic_vecall, bic_vecall))
                      
ggplot(modeldat, aes(IC, values, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_cartesian(ylim = c(7240, 7450)) +
  xlab('Information criterion') +
  theme_grey(base_size = 22)

ggsave('ICscolnew.jpg', height = 5, width = 8)

# As normalized weights
Model <- factor(c('g factor','Mutualism','Investment'))
modeldatweights <- data.frame(valuesweights, Model)
ggplot(modeldatweights, aes(Model, valuesweights, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab('Likelihood') + xlab('') +
  ggtitle('Akaike weights') +
  theme_grey(base_size = 22)

ggsave('Akaikeweightscolnew.jpg', height = 5, width = 8)

# Create figure: Vector plot

# Creating matrix of model implied bivariate pairs
dimlength <- 6
t1mat <- seq(from = 0, to = 18, length.out = dimlength)
t1voc <- seq(from = 10, to = 40, length.out = dimlength)
scorevecmut <- data.frame(rep(t1mat, times = dimlength))
scorevecmut$voc <- rep(t1voc, each = dimlength)
colnames(scorevecmut)[1:2] <- c('t1mat', 't1voc')

# Computing model implied scores at T2
scorevecmut$t2voc <- scorevecmut$t1voc + 3.476 + (- 0.171 * scorevecmut$t1voc) + (0.272 * scorevecmut$t1mat)
scorevecmut$t2mat <- scorevecmut$t1mat - 0.593 + (- 0.268 * scorevecmut$t1mat) + (0.273 * scorevecmut$t1voc)

# Computing model implied scores at T3
scorevecmut$t3voc <- scorevecmut$t2voc + 0.928 + (- 0.171 * scorevecmut$t2voc) + (0.272 * scorevecmut$t2mat)
scorevecmut$t3mat <- scorevecmut$t2mat - 0.593 + (- 0.268 * scorevecmut$t2mat) + (0.273 * scorevecmut$t2voc)

# Pick random subset of raw data to not clutter figure too much
subsamplesize <- 150
set.seed(31415)
samplerows <- sample(1:227, subsamplesize,replace = FALSE)
subsetplot <- cogdat[samplerows, 1:2]
colnames(subsetplot) <- c('Mat', 'Voc')

# Plotting vector field from t1 to t3
ggplot(subsetplot, aes(Voc, Mat)) + 
  geom_jitter(alpha=.5,size=2) + 
  geom_segment(aes(x = t1voc, y = t1mat, xend = t3voc, yend = t3mat),
               arrow = arrow(length = unit(0.2, "cm")),
               data = scorevecmut) +
  xlab('Vocabulary') + ylab('Matrix Reasoning') +
  coord_cartesian(ylim = c(0, 25), xlim=c(8, 45)) + 
  annotate("rect", xmin = 10, xmax = 40, ymin = 11, ymax = 23,alpha =.2, fill = 'red') +
  annotate("rect", xmin = 16, xmax = 25, ymin = 0, ymax = 25, alpha = .2, fill='blue') + 
  theme_bw(base_size = 19) +
  theme(panel.grid.minor = element_blank()) +
  ylim(0, 25) +
  stat_ellipse(lty = 2, level = 0.90, alpha = .7)

ggsave('vectornew.tiff', width = 10, height = 7,compression='lzw')




# Plotting the raw data

# Figure 1: Behavioural scores 
# Matrix
datmat <- data.frame(id = as.factor(rep(1:nrow(cogdat), 3)),
                     age = rep(c(6.22, 7.22, 8.22), each = nrow(cogdat)),
                     matrix = c(cogdat$t1ssmatrix, cogdat$t2ssmatrix, cogdat$t3ssmatrix))

ggplot(datmat, aes(age, matrix, group = id))  + 
  geom_point(alpha = .7, size = 2, col = 'dodgerblue') +
  geom_line(alpha = .7, col = 'dodgerblue', lwd = 1) +
  ylab('Matrix reasoning score') +
  theme(text = element_text(size = 22)) +
  ylim(0, 30) +
  xlab('Age')

ggsave('mat.jpg')

# Vocab
datvoc <- data.frame(id = as.factor(rep(1:nrow(cogdat), 3)),
                     age = rep(c(6.22, 7.22, 8.22), each = nrow(cogdat)),
                     vocab = c(cogdat$t1ssvocab, cogdat$t2ssvocab, cogdat$t3ssvocab))

ggplot(datvoc, aes(age, vocab, group = id)) +
  geom_point(alpha = .7, size = 2, col = '#ff7f7f') +
  geom_line(alpha = .7, col = '#ff7f7f', lwd = 1) +
  ylab('Vocabulary score') +
  theme(text = element_text(size = 22)) +
  ylim(0, 50) +
  xlab('Age')

ggsave('voc.jpg')


#Robustness analyses: Below we show similar results are obtained when either omitting the reliability adjustment
#or using a different analytic strategy (the RICLPM)


# Fit the Mutualism model without adjustment
mutualism_raw <- '
t2ssvocab ~ 1 * t1ssvocab
t3ssvocab ~ 1 * t2ssvocab

etavoc1 =~ 1 * t2ssvocab
etavoc2 =~ 1 * t3ssvocab

etavoc1 ~ vocint1 * 1   
etavoc2 ~ vocint2 * 1 # only exception to cross-wave equality constraints 

t1ssvocab ~ 1
t2ssvocab ~ 0 * 1
t3ssvocab ~ 0 * 1

etavoc1 ~~ varetavoc1 * etavoc1
etavoc2 ~~ varetavoc1 * etavoc2

t2ssmatrix ~ 1 * t1ssmatrix
t3ssmatrix ~ 1 * t2ssmatrix

etamat1 =~ 1 * t2ssmatrix
etamat2 =~ 1 * t3ssmatrix

etamat1 ~ matint1 * 1   
etamat2 ~ matint1 * 1   

t1ssmatrix ~ 1
t2ssmatrix ~ 0 * 1
t3ssmatrix ~ 0 * 1

# variances/covariances
etamat1 ~~ varetamat1 * etamat1
etamat2 ~~ varetamat1 * etamat2

t1ssmatrix ~~ t1ssvocab

etamat1 ~ negmat * t1ssmatrix + couplinga * t1ssvocab
etamat2 ~ negmat * t2ssmatrix + couplinga * t2ssvocab

etavoc1 ~ negvoc * t1ssvocab + couplingb * t1ssmatrix
etavoc2 ~ negvoc * t2ssvocab + couplingb * t2ssmatrix

etamat1 ~~ a * etavoc1
etamat2 ~~ a * etavoc2'

est_mutualism_raw <- growth(mutualism_raw, data = cogdatMI, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(est_mutualism_raw, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)

# Fit the Investment model without adjustment
investment_raw <- '
t2ssvocab ~ 1 * t1ssvocab
t3ssvocab ~ 1 * t2ssvocab

etavoc1 =~ 1 * t2ssvocab
etavoc2 =~ 1 * t3ssvocab

etavoc1 ~ vocint1 * 1   
etavoc2 ~ vocint2 * 1 # only exception to cross-wave equality constraints 

t1ssvocab ~ 1
t2ssvocab ~ 0 * 1
t3ssvocab ~ 0 * 1

etavoc1 ~~ varetavoc1 * etavoc1
etavoc2 ~~ varetavoc1 * etavoc2

t2ssmatrix ~ 1 * t1ssmatrix
t3ssmatrix ~ 1 * t2ssmatrix

etamat1 =~ 1 * t2ssmatrix
etamat2 =~ 1 * t3ssmatrix

etamat1 ~ matint1 * 1   
etamat2 ~ matint1 * 1   

t1ssmatrix ~ 1
t2ssmatrix ~ 0 * 1
t3ssmatrix ~ 0 * 1

# variances/covariances
etamat1 ~~ varetamat1 * etamat1
etamat2 ~~ varetamat1 * etamat2

t1ssmatrix ~~ t1ssvocab

etamat1 ~ negmat * t1ssmatrix
etamat2 ~ negmat * t2ssmatrix

etavoc1 ~ negvoc * t1ssvocab + couplingb * t1ssmatrix
etavoc2 ~ negvoc * t2ssvocab + couplingb * t2ssmatrix

etamat1 ~~ a * etavoc1
etamat2 ~~ a * etavoc2'

est_investment_raw <- growth(investment_raw, data=cogdat, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(est_investment_raw, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

anova(est_investment_raw,est_mutualism_raw)
#Fitting as RICLPM with thanks to J. Flournoy for sharing code

riclpmModel <- 
  '
#means
kappa =~ 1*t1ssmatrix + 1*t2ssmatrix + 1*t3ssmatrix
omega =~ 1*t1ssvocab + 1*t2ssvocab + 1*t3ssvocab

#intercepts
t1ssmatrix ~ mu1*1 #intercepts
t2ssmatrix ~ mu2*1
t3ssmatrix ~ mu3*1
t1ssvocab ~ pi1*1
t2ssvocab ~ pi2*1
t3ssvocab ~ pi3*1

#(co)variances
kappa ~~ kappa #variance
omega ~~ omega #variance
kappa ~~ omega #covariance

#latent variables for autoregressive and cross-lagged effects
p1 =~ 1*t1ssmatrix #each factor loading set to 1
p2 =~ 1*t2ssmatrix
p3 =~ 1*t3ssmatrix
q1 =~ 1*t1ssvocab
q2 =~ 1*t2ssvocab
q3 =~ 1*t3ssvocab

#structural model
p3 ~ alpha*p2 + beta*q2
p2 ~ alpha*p1 + beta*q1

q3 ~ delta*q2 + gamma*p2
q2 ~ delta*q1 + gamma*p1

#Variances
p1 ~~ u*p1 #variance
p2 ~~ u*p2
p3 ~~ u*p3
q1 ~~ v*q1 #variance
q2 ~~ v*q2
q3 ~~ v*q3

p1 ~~ cov*q1 #p1 and q1 covariance
p2 ~~ cov*q2 #p2 and q2 covariance
p3 ~~ cov*q3 #p2 and q2 covariance'

fit_riclpmModel <- lavaan(riclpmModel, data = cogdat,estimator='mlr',
                          missing = 'ML',
                          int.ov.free = F,
                          int.lv.free = F,
                          auto.fix.first = F,
                          auto.fix.single = F,
                          auto.cov.lv.x = F,
                          auto.cov.y = F,
                          auto.var = F)

summary(fit_riclpmModel, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = TRUE)



# ------------------------------------------------------------------------
# (not in paper for reasons of space)
# ------------------------------------------------------------------------
# The g factor fits poorly (improper solution). Various
# attemps at resuscitation don't help
# Alternative: fit a g factor model with only latent variables (no lcs)
# fits much better (despite std covariances just exceeding 1)
# Suggest g measurement itself ok, but not a good developmental model. 

gfactorlvonly <- '
gft1 =~ flm * t1ssmatrix + flv * t1ssvocab
gft2 =~ flm * t2ssmatrix + flv * t2ssvocab
gft3 =~ flm * t3ssmatrix + flv * t3ssvocab

t1ssmatrix ~ 0 * 1
t2ssmatrix ~ 0 * 1
t3ssmatrix ~ 0 * 1

gft1 ~ 1
gft2 ~ 1
gft3 ~ 1

# Residual covariances

t1ssmatrix ~~ rescovmat1 * t2ssmatrix
t3ssmatrix ~~ rescovmat2 * t2ssmatrix
t3ssmatrix ~~ rescovmat2 * t1ssmatrix

t1ssvocab ~~ rescovvoc1 * t2ssvocab
t2ssvocab ~~ rescovvoc2 * t3ssvocab
t3ssvocab ~~ rescovvoc2 * t1ssvocab

# Residual variances

t1ssmatrix ~~ resvarmat * t1ssmatrix
t2ssmatrix ~~ resvarmat * t2ssmatrix
t3ssmatrix ~~ resvarmat * t3ssmatrix
t1ssvocab ~~ resvarvoc1 * t1ssvocab
t2ssvocab ~~ resvarvoc2 * t2ssvocab
t3ssvocab ~~ resvarvoc2 * t3ssvocab'

fitgfactorlvonly <- cfa(gfactorlvonly, data = cogdat, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(fitgfactorlvonly, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
cov2cor(inspect(fitgfactorlvonly, "cov.lv")) 

