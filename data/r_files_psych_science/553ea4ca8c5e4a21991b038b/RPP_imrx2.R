################################################################################
##################          REPLICATION R CODES               ##################
################################################################################
#
##@@ INSTRUCTIONS @@##
# Please do not remove any comments from this file, but feel free to add as many
# comments to R code as you like!
#
# Replace all ---USER INPUT--- sections with relevant input (including the - 
# symbols).
#
# When you are done, save this file as RPP_YYYY.R, where YYYY is the OSF code of 
# the project.
# e.g., The first study, Tracing attention and the activation...., should be
# called RPP_qwkum.R (since https://osf.io/qwkum/ is the project site)
#
# For any questions, please contact me at sacha.epskamp@gmail.com
#
##@@ GENERAL INFORMATION @@##
#@ Study Title: Replication of Exline et al. (2008, JPSP, Study 7)
#@ Coder name: Pia Tio
#@ Coder e-mail: piatio@gmail.com
#@ Type of statistic: F-statistic
#@ Type of effect-size: eta-squared
#@ OSF link project: https://osf.io/imrx2/
#@ OSF link replication report: https://osf.io/es7ub/
#
##@@ REQUIRED PACKAGES @@##
  library("httr")
  library("RCurl")
  source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  library("car")
  library("MASS")
  library("lsr")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
  file <- getOSFfile('https://osf.io/yjc35/')
  datap <-read.csv(file, header = TRUE)

  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL differences between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
  #code gender and conditions as factors
  datap$sex = as.factor(datap$sex)
  datap$condition = as.factor(datap$condition)
  
  #males and females count
  gentable = table(datap$sex)
  gentable
  
  #ethnicity count
  sum(datap$ethnicity_1, na.rm = TRUE) #33
  sum(datap$ethnicity_2, na.rm = TRUE) #11
  sum(datap$ethnicity_3, na.rm = TRUE) #12
  sum(datap$ethnicity_4, na.rm = TRUE) #2
  sum(datap$ethnicity_5, na.rm = TRUE) #82
  sum(datap$ethnicity_6, na.rm = TRUE) #1
  sum(datap$ethnicity_7, na.rm = TRUE) #2
  
  #age
  datap$age = as.numeric(as.character(datap$age))
  mean(datap$age); sd(datap$age)
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object
  #contrast code in order to get correct ANOVA results (this is something we learned in psych 252)
  contrasts(datap$sex) = c(-1,1)
  contrasts(datap$condition) = c(-1,1)
  #contrasts(datap$condition) = c(0,1)
  #checking out vengefulness by condition and gender data
  table = aggregate(datap$vengefulness, by=list(datap$condition,datap$sex),FUN=mean)
  table

  #just checking male vs. females
  table2 = aggregate(datap$vengefulness, by=list(datap$sex),FUN=mean)
  table2
  table2sd = aggregate(datap$vengefulness, by=list(datap$sex),FUN=sd)
  table2sd
  table3 = aggregate(datap$TRIMscore, by=list(datap$condition), FUN=mean)
  table3
  table3sd = aggregate(datap$TRIMscore, by=list(datap$condition), FUN=sd)
  table3sd

  newTable = matrix(table$x,nrow=2,ncol=2)
  newTable

  x = barplot(newTable,beside = TRUE, names.arg = c("male","female"))

  resA = lm(datap$vengefulness~datap$condition)
  summary(resA)

  #Two ways to test the ANOVA of condition x gender on vengefulness
  res1 = lm(datap$vengefulness~datap$condition*datap$sex)
  summary(res1)

  res1.1 = Anova(lm(datap$vengefulness~datap$condition*datap$sex),type="III")
  Result = (res1.1)
  print(Result)
  
##@@ STATISTIC @@##
  Result[[3]][4]
  
##@@ P-VALUE @@##
  Result[[4]][4]
  
##@@ SAMPLE SIZE @@##
  nrow(datap)
  
##@@ EFFECT SIZE @@##
  effect = etaSquared(lm(datap$vengefulness~datap$condition*datap$sex), type = 3)
  effect[3]

##@@ AGREEMENT WITH AUTHORS @@##
  TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---