#analyses for current dataset.
library("sjmisc")#this package is used for standardizing (std) and centering (center) predictors.
library(readxl)#used for loading data.
library("robustbase")#used for robust regression.
#cagr <- read_excel("[name and location]")#load the dataset
##NB changed previous line to next
cagr <- read_excel("current.xlsx")#load the dataset

##NB installed package "snakecase"

##NB Table 1
cat("Table 1\n")
for (v in c("Age", "CAG_Repeat", "riskc", "zdom", "zcont", "zcons")) {
  for (cond in c(1, 0)) {
    cat(" cond=", cond, sep="")
    cat(" M=", sprintf("%.3f", mean(subset(cagr, DrugCondition == cond)[[v]])), sep="")
    cat(" SD=", sprintf("%.3f", sd(subset(cagr, DrugCondition == cond)[[v]])), sep="")
  }
  cat("\n")
}

#run model with two-way interaction (drug x personality risk) predicting aggression.
#NB: Table 3, Analysis A
summary(cagdr <- lmrob(Agg_Prov ~ std(riskc)*center(DrugCondition), data = cagr))
#to follow-up significant interaction, we transform the personality risk variable to test the conditional effect of drug at +1 or -1 SD of personality risk. 
#NB: Table 3, Analysis B
summary(cagdr <- lmrob(Agg_Prov ~ I(std(riskc)+1)*center(DrugCondition), data = cagr))#effect of drug at low personality risk.
summary(cagdr <- lmrob(Agg_Prov ~ I(std(riskc)-1)*center(DrugCondition), data = cagr))#effect of drug at high personality risk.
#run model with three-way interaction predicting aggression.
#NB: Table 4, Analysis A
summary(cagdr <- lmrob(Agg_Prov ~ std(CAG_Repeat)*std(riskc)*center(DrugCondition), data = cagr))
#to follow-up significant three-way interaction, we transform the CAG repeat length variable to test the two-way drugXrisk interaction at +1 or -1 SD of CAG repeat length. 
#NB: Table 4, Analysis B
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*std(riskc)*center(DrugCondition), data = cagr))#DrugxRisk at LOW cag.
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)-1)*std(riskc)*center(DrugCondition), data = cagr))#DrugxRisk at HIGH cag.
#to further follow-up three-way interaction, we transform both personality risk and CAG repeat length to test the conditional effect of drug at each combination of low/high personality risk and CAG repeat length.
#NB: Table 4, Analysis C
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)-1)*I(std(riskc)+1)*center(DrugCondition), data = cagr))#drug effect at high CAG low risk
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*I(std(riskc)+1)*center(DrugCondition), data = cagr))#drug effect at low CAG low risk
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*I(std(riskc)-1)*center(DrugCondition), data = cagr))#drug effect at low CAG high risk
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)-1)*I(std(riskc)-1)*center(DrugCondition), data = cagr))#drug effect at high CAG high risk
#run model with two-way interaction (drug x CAG Repeat Length) predicting aggression.
#NB: Table 5, Analysis A
summary(cagdr <- lmrob(Agg_Prov ~ std(CAG_Repeat)*center(DrugCondition), data = cagr))
#to follow-up significant interaction, we transform CAG Repeat Length to test the conditional effect of drug at +1 or -1 SD of CAG repeat length. 
#NB: Table 5, Analysis B
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*center(DrugCondition), data = cagr))#effect of drug at low CAG repeat length.
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)-1)*center(DrugCondition), data = cagr))#effect of drug at high CAG repeat length.
#test if there is a conditional effect of Drug at high risk/low CAG on reward (FeelGood).
#NB: p. 488, text, right-hand side
summary(cagdr <- lmrob(FeelGood ~ I(std(CAG_Repeat)+1)*I(std(riskc)-1)*center(DrugCondition), data=subset(cagr,FeelGood > 0)))
#test if there is a conditional effect of Drug, at high risk, low CAG, on anger (Angry)
#NB: p. 488, text, right-hand side
summary(cagdr <- lmrob(Angry ~ I(std(CAG_Repeat)+1)*I(std(riskc)-1)*center(DrugCondition), data=subset(cagr,Angry > 0)))
#NB: Next line apparently not used in article
summary(cagdr <- lmrob(Angry ~ I(std(CAG_Repeat)+1)*I(std(riskc)-1)*center(DrugCondition), data=subset(cagr,FeelGood > 0)))#same analysis but within same subset on which reward analysis was conducted.
#conditional effect of drug within subset of participants that have reward data. 
#NB: p. 489, text, left-hand side, last line
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*I(std(riskc)-1)*center(DrugCondition), data=subset(cagr,FeelGood > 0)))
#conditional effects of drug and reward (at high risk, low CAG) when both are entered simultaneously in the model.
#NB: p. 489, text, right-hand side, last line (wraps to p. 490; this model generates two reported results)
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*I(std(riskc)-1)*center(DrugCondition)+I(std(CAG_Repeat)+1)*I(std(riskc)-1)*std(FeelGood),data=subset(cagr,FeelGood > 0)))
#Supplementary analyses: examining two- and three- way interactions involving the individual personality moderators (note that coefficients for higher order interactions give same p values when the individual variables comprising them are transformed to +1 or -1 SD).
#NB: Table S1, Analysis A
summary(cagdr <- lmrob(Agg_Prov ~ I(std(zdom)-1)*center(DrugCondition), data = cagr))#drug x dominance 2 way.
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*I(std(zdom)-1)*center(DrugCondition), data = cagr))#drug x dominance three-way.
#NB: Table S1, Analysis B
summary(cagdr <- lmrob(Agg_Prov ~ I(std(zcont)+1)*center(DrugCondition), data = cagr))#drug x self control 2 way.
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*I(std(zcont)+1)*center(DrugCondition), data = cagr))#drug x self control three-way.
#NB: Table S1, Analysis C
summary(cagdr <- lmrob(Agg_Prov ~ I(std(zcons)-1)*center(DrugCondition), data = cagr))#drug x self construal 2 way.
summary(cagdr <- lmrob(Agg_Prov ~ I(std(CAG_Repeat)+1)*I(std(zcons)-1)*center(DrugCondition), data = cagr))#drug x self construal three-way.
#Supplementary analyses: examining three- way interactions involving the individual personality moderators when they are entered into the model simultaneously.
#NB: Supplement, page 2
summary(cagdr <- lmrob(Agg_Prov ~ std(CAG_Repeat)*std(zdom)*center(DrugCondition)+std(CAG_Repeat)*std(zcont)*center(DrugCondition)+std(CAG_Repeat)*std(zcons)*center(DrugCondition), data = cagr))
#Supplementary analyses: adding participant guess as a covariate to the model.
summary(cagdr <- lmrob(Agg_Prov ~ center(Guess)+std(CAG_Repeat)*std(riskc)*center(DrugCondition), data = cagr))

