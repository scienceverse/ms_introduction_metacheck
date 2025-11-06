###############Libraries################################

#wrangling
library(tidyverse)
library(datawizard)

#plotting
library(ggplot2)
library(plotly)
library(sjPlot)
library(sjmisc)

#lm, SEM, and ML
library(lavaan)
library(interpretCI)
library(caret)
options(scipen=999) 

#read data
library(readr)

################## Extract and clean ####################

## ***read in data *** ##
data <- tibble(read_csv("data.csv"))

# target ID not necessary in target-level data
data$stimulus<-NULL

#scale all relevant continuous variables (for standardized coefficients)
data <- 
  data %>% 
  mutate(MproportionHuman=standardise(MproportionHuman), #DV
         Old=standardise(Old),
         Alive_in_eyes=standardise(Alive_in_eyes),
         Attractive=standardise(Attractive),
         Distinctive=standardise(Distinctive),
         Expressive=standardise(Expressive),
         Eye_contact=standardise(Eye_contact),
         Familiar=standardise(Familiar),
         Familiar=standardise(Familiar),
         Gen_happy=standardise(Gen_happy),
         Image_quality=standardise(Image_quality),
         Congruent_light_shadows=standardise(Congruent_light_shadows),
         Memorable=standardise(Memorable),
         Proportional=standardise(Proportional),
         Skin_smooth=standardise(Skin_smooth),
         Symmetrical=standardise(Symmetrical))

#######################################################################
#### Analysis 1: what explains proportion of selections as "human" ####
#######################################################################

mod1<-
  lm(MproportionHuman~
     Old+
     Alive_in_eyes+
     Attractive+
     Distinctive+
     Expressive+
     Eye_contact+
     Familiar+
     Gen_happy+
     Image_quality+
     Congruent_light_shadows+
     Memorable+
     Proportional+
     Skin_smooth+
     Symmetrical, 
   data = data)

summary(mod1)

plot_model(mod1, type = "est", 
           vline.color = "red",
           sort.est = TRUE,
           show.values = TRUE, value.offset = .4,
           dot.size = 3)+ylab("Standardized coefficients")+theme_bw()

#########################################################################
#### Analysis 2: attributes mediating accurate selections as "human" ####
#########################################################################

lens.model<-'

#STEP 1: stim_type predicts each attribute; assign each value to calculate indirect effects  

Proportional~aa*stim_type
Old~ab*stim_type
Alive_in_eyes~ac*stim_type
Attractive~ad*stim_type
Distinctive~ae*stim_type
Expressive~af*stim_type
Eye_contact~ag*stim_type
Familiar~ah*stim_type
Gen_happy~ai*stim_type
Image_quality~aj*stim_type
Congruent_light_shadows~ak*stim_type
Memorable~al*stim_type
Skin_smooth~am*stim_type
Symmetrical~an*stim_type

#STEP 2: Each attribute predicts MproportionHuman; assign each value to calculate indirect effects  

MproportionHuman~
ba*Proportional+
bb*Old+
bc*Alive_in_eyes+
bd*Attractive+
be*Distinctive+
bf*Expressive+
bg*Eye_contact+
bh*Familiar+
bi*Gen_happy+
bj*Image_quality+
bk*Congruent_light_shadows+
bl*Memorable+
bm*Skin_smooth+
bn*Symmetrical

# NOTE: here I am adding stim_type as an *additional* predictor in the above model predicting "MproportionHuman". I separate it out to remember that this is a direct effect, not an indirect effect

MproportionHuman~cp*stim_type

#STEP 3: calculate instantaneous indirect effects through each mediator, use a and b pathways already asigned (above)
ab1:=aa*ba #note: indirect via Proportional
ab2:=ab*bb #note: indirect via Old
ab3:=ac*bc #note: indirect via Alive_in_eyes
ab4:=ad*bd #note: indirect via Attractive
ab5:=ae*be #note: indirect via Distinctive
ab6:=af*bf #note: indirect via Expressive
ab7:=ag*bg #note: indirect via Eye_contact
ab8:=ah*bh #note: indirect via Familiar
ab9:=ai*bi #note: indirect via Gen_happy
ab10:=aj*bj #note: indirect via Image_quality
ab11:=ak*bk #note: indirect via Congruent_light_shadows
ab12:=al*bl #note: indirect via Memorable
ab13:=am*bm #note: indirect via Skin_smooth
ab14:=an*bn #note: indirect via Symmetrical

#STEP 4: Calculate total effect (i.e., direct effect + all indirect effects)
total:=cp+ab1+ab2+ab3+ab4+ab5+ab6+ab7+ab8+ab9+ab10+ab11+ab12+ab13+ab14

# NOTE: all predictors are allowed to correlate in a multiple mediation model. 

Proportional~~Old
Proportional~~Alive_in_eyes
Proportional~~Attractive
Proportional~~Distinctive
Proportional~~Expressive
Proportional~~Eye_contact
Proportional~~Familiar
Proportional~~Gen_happy
Proportional~~Image_quality
Proportional~~Congruent_light_shadows
Proportional~~Memorable
Proportional~~Skin_smooth
Proportional~~Symmetrical

Old~~Alive_in_eyes
Old~~Attractive
Old~~Distinctive
Old~~Expressive
Old~~Eye_contact
Old~~Familiar
Old~~Gen_happy
Old~~Image_quality
Old~~Congruent_light_shadows
Old~~Memorable
Old~~Skin_smooth
Old~~Symmetrical

Alive_in_eyes~~Attractive
Alive_in_eyes~~Distinctive
Alive_in_eyes~~Expressive
Alive_in_eyes~~Eye_contact
Alive_in_eyes~~Familiar
Alive_in_eyes~~Gen_happy
Alive_in_eyes~~Image_quality
Alive_in_eyes~~Congruent_light_shadows
Alive_in_eyes~~Memorable
Alive_in_eyes~~Skin_smooth
Alive_in_eyes~~Symmetrical

Attractive~~Distinctive
Attractive~~Expressive
Attractive~~Eye_contact
Attractive~~Familiar
Attractive~~Gen_happy
Attractive~~Image_quality
Attractive~~Congruent_light_shadows
Attractive~~Memorable
Attractive~~Skin_smooth
Attractive~~Symmetrical

Distinctive~~Expressive
Distinctive~~Eye_contact
Distinctive~~Familiar
Distinctive~~Gen_happy
Distinctive~~Image_quality
Distinctive~~Congruent_light_shadows
Distinctive~~Memorable
Distinctive~~Skin_smooth
Distinctive~~Symmetrical

Expressive~~Eye_contact
Expressive~~Familiar
Expressive~~Gen_happy
Expressive~~Image_quality
Expressive~~Congruent_light_shadows
Expressive~~Memorable
Expressive~~Skin_smooth
Expressive~~Symmetrical

Eye_contact~~Familiar
Eye_contact~~Gen_happy
Eye_contact~~Image_quality
Eye_contact~~Congruent_light_shadows
Eye_contact~~Memorable
Eye_contact~~Skin_smooth
Eye_contact~~Symmetrical

Familiar~~Gen_happy
Familiar~~Image_quality
Familiar~~Congruent_light_shadows
Familiar~~Memorable
Familiar~~Skin_smooth
Familiar~~Symmetrical

Gen_happy~~Image_quality
Gen_happy~~Congruent_light_shadows
Gen_happy~~Memorable
Gen_happy~~Skin_smooth
Gen_happy~~Symmetrical

Image_quality~~Congruent_light_shadows
Image_quality~~Memorable
Image_quality~~Skin_smooth
Image_quality~~Symmetrical

Congruent_light_shadows~~Memorable
Congruent_light_shadows~~Skin_smooth
Congruent_light_shadows~~Symmetrical

Memorable~~Skin_smooth
Memorable~~Symmetrical

Skin_smooth~~Symmetrical

'

lens.model.fit<-sem(lens.model, data=data)
summary(lens.model.fit, standardized = T, rsquare = T, fit.measures = T)



#####################
#### ML approach ####
#####################

## set mtry to standard starting point; note: no model tuning. 
tunegrid <- expand.grid(.mtry=round(sqrt(ncol(data))))

MyTrainControl=trainControl(
  method = "cv",
  number=10, #10-fold CV
  savePredictions = TRUE,
  classProbs = TRUE,
  verboseIter = TRUE,
)

mod.rf<-
  train(stim_type~
          Proportional+
          Old+
          Alive_in_eyes+
          Attractive+
          Distinctive+
          Expressive+
          Eye_contact+
          Familiar+
          Gen_happy+
          Image_quality+
          Congruent_light_shadows+
          Memorable+
          Skin_smooth+
          Symmetrical, 
        data = data,
        method = "rf",  #random forest
        tuneGrid=tunegrid, #mtry = 4
        trControl = MyTrainControl) # 10-fold CV

# results:
confusionMatrix(mod.rf, norm = "none")
mod.rf$results
propCI(n = 200, p = mod.rf$results$Accuracy, alpha = 0.05)



