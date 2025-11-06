
# ---- load readxl package
library("xlsx")
library(readxl)
library("lme4")

# ---- load the dataframe
all <- read_excel("choose_adv.xlsx")
dr <- read_excel("dr_choose_adv.xlsx")
other <- read_excel("other_choose_adv.xlsx")

# ---- create the model objects
summary(glmer(formula = choose_adv ~ trial*age_years + (1|subject), data = all, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))))
summary(glmer(formula = choose_adv ~ trial*age_years + (1|subject), data = dr, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))))
summary(glmer(formula = choose_adv ~ trial*age_years + (1|subject), data = other, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))))
