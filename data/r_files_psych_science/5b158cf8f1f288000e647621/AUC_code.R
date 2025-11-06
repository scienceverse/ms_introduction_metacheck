
#Colloff, M. F., Wade, K. A., Strange, D., & Wixted, J. T. (2018) Psychological Science. 
#Filler Siphoning Theory Does Not Predict the Effect of Lineup Fairness on the Ability to Discriminate Innocent from Guilty Suspects:
#Reply to Smith, Wells, Smalarz, and Lampinen (2017). 

#This code has been tested using RStudio Versions 0.98.1103 and 1.0.136 on Mac OS 10.13.4

# Load pROC package
install.packages("pROC")
library("pROC")

#set working directory (add directory location before "/Analysis" e.g., Documents/Analysis)
setwd("~/Analysis")

#See Laura Mickes' website for a tutorial: http://www.mickeslab.com/handy/roc-tutorial/

###########################################################################################
#Yes Responses: Main pAUC analysis reported in the reply
#Collapsed over both graffiti and mugging videos (Figure 2A)
###########################################################################################
library(readxl)
Block <- read_excel("Block.xlsx")
DoNothing <- read_excel("DoNothing.xlsx")

#smallest FAR = 0.23 (Block)
# Compute partial area under the curve for Block
roc(controls = Block$FalseID, cases =
              Block$CorrectID[!is.na(Block$CorrectID)], direction = "<", ci = T, partial.auc =
              c(1, 0.77))

# Compute partial area under the curve for DoNothing
roc(controls = DoNothing$FalseID, cases =
              DoNothing$CorrectID[!is.na(DoNothing$CorrectID)], direction = "<", ci = T, partial.auc =
              c(1, 0.77))

# Make ROC object for Block
Block <- roc(controls=Block$FalseID,
            cases=Block$CorrectID[!is.na(Block$CorrectID)], direction = "<")

# Make ROC object for DoNothing
DoNothing <- roc(controls=DoNothing$FalseID,
            cases=DoNothing$CorrectID[!is.na(DoNothing$CorrectID)], direction = "<")

# Compare Block & DoNothing ROC curves
roc.test(Block, DoNothing, reuse.auc=FALSE, paired=FALSE, partial.auc=c(1, 0.77),
         partial.auc.focus="sp", method="bootstrap")



###########################################################################################
#pAUC: Preregistered Analyses on Graffiti and Mugging Stimuli (available: https://osf.io/nr24b/)
###########################################################################################

######################################
#Mugging video
######################################
BlockMugging <- read_excel("BlockMugging.xlsx")
DoNothingMugging <- read_excel("DoNothingMugging.xlsx")

#Smallest FAR = 0.22 (Block)

# Compute partial area under the curve for BlockMugging
roc(controls = BlockMugging$FalseID, cases =
      BlockMugging$CorrectID[!is.na(BlockMugging$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.78))

# Compute partial area under the curve for DoNothingMugging
roc(controls = DoNothingMugging$FalseID, cases =
      DoNothingMugging$CorrectID[!is.na(DoNothingMugging$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.78))

# Make ROC objects for BlockMugging
BlockMugging <- roc(controls=BlockMugging$FalseID,
                                      cases=BlockMugging$CorrectID[!is.na(BlockMugging$CorrectID)], direction = "<")

# Make ROC objects for DoNothingMugging
DoNothingMugging <- roc(controls=DoNothingMugging$FalseID,
                                  cases=DoNothingMugging$CorrectID[!is.na(DoNothingMugging$CorrectID)], direction = "<")

# Compare BlockMugging & DoNothingMugging ROC curves
roc.test(BlockMugging, DoNothingMugging, reuse.auc=FALSE, paired=FALSE, partial.auc=c(1, 0.78),
         partial.auc.focus="sp", method="bootstrap")


######################################
#Graffiti Video
######################################

BlockGraffiti <- read_excel("BlockGraffiti.xlsx")
DoNothingGraffiti <- read_excel("DoNothingGraffiti.xlsx")

#smallest FAR = 0.24 (Block)

# Compute partial area under the curve for BlockGraffiti
roc(controls = BlockGraffiti$FalseID, cases =
      BlockGraffiti$CorrectID[!is.na(BlockGraffiti$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.76))

# Compute partial area under the curve for DoNothingGraffiti
roc(controls = DoNothingGraffiti$FalseID, cases =
      DoNothingGraffiti$CorrectID[!is.na(DoNothingGraffiti$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.76))

# Make ROC objects for BlockGraffiti
BlockGraffiti <- roc(controls=BlockGraffiti$FalseID,
                     cases=BlockGraffiti$CorrectID[!is.na(BlockGraffiti$CorrectID)], direction = "<")


# Make ROC objects for DoNothingGraffiti
DoNothingGraffiti <- roc(controls=DoNothingGraffiti$FalseID,
                         cases=DoNothingGraffiti$CorrectID[!is.na(DoNothingGraffiti$CorrectID)], direction = "<")

# Compare BlockGraffiti & DoNothingGraffiti ROC curves
roc.test(BlockGraffiti, DoNothingGraffiti, reuse.auc=FALSE, paired=FALSE, partial.auc=c(1, 0.76),
         partial.auc.focus="sp", method="bootstrap")


###########################################################################################
#pAUC: Preregistered Analyses on Showup 1 and 2 (available: https://osf.io/nr24b/)
###########################################################################################

######################################
#Showup1 test
######################################

BlockShowup1 <- read_excel("BlockShowup1.xlsx")
DoNothingShowup1 <- read_excel("DoNothingShowup1.xlsx")

#Smallest FAR = 0.21 (Block)

# Compute partial area under the curve for BlockShowup1
roc(controls = BlockShowup1$FalseID, cases =
      BlockShowup1$CorrectID[!is.na(BlockShowup1$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.79))

# Compute partial area under the curve for DoNothingShowup1
roc(controls = DoNothingShowup1$FalseID, cases =
      DoNothingShowup1$CorrectID[!is.na(DoNothingShowup1$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.79))

# Make ROC objects for BlockShowup1
BlockShowup1 <- roc(controls=BlockShowup1$FalseID,
                           cases=BlockShowup1$CorrectID[!is.na(BlockShowup1$CorrectID)], direction = "<")

# Make ROC objects for DoNothingShowup1
DoNothingShowup1 <- roc(controls=DoNothingShowup1$FalseID,
                       cases=DoNothingShowup1$CorrectID[!is.na(DoNothingShowup1$CorrectID)], direction = "<")

# Compare BlockShowup1 & DoNothingShowup1 ROC curves
roc.test(BlockShowup1, DoNothingShowup1, reuse.auc=FALSE, paired=FALSE, partial.auc=c(1, 0.79),
         partial.auc.focus="sp", method="bootstrap")


######################################
#Showup2 test
######################################

BlockShowup2 <- read_excel("BlockShowup2.xlsx")
DoNothingShowup2 <- read_excel("DoNothingShowup2.xlsx")

#Smallest FAR in = 0.24 (Block)

# Compute partial area under the curve for BlockShowup2
roc(controls = BlockShowup2$FalseID, cases =
      BlockShowup2$CorrectID[!is.na(BlockShowup2$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.76))

# Compute partial area under the curve for DoNothingShowup2
roc(controls = DoNothingShowup2$FalseID, cases =
      DoNothingShowup2$CorrectID[!is.na(DoNothingShowup2$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.76))

# Make ROC objects for BlockShowup2
BlockShowup2 <- roc(controls=BlockShowup2$FalseID,
                          cases=BlockShowup2$CorrectID[!is.na(BlockShowup2$CorrectID)], direction = "<")

# Make ROC objects for DoNothingShowup2
DoNothingShowup2 <- roc(controls=DoNothingShowup2$FalseID,
                      cases=DoNothingShowup2$CorrectID[!is.na(DoNothingShowup2$CorrectID)], direction = "<")

# Compare BlockShowup2 & DoNothingShowup2 ROC curves
roc.test(BlockShowup2, DoNothingShowup2, reuse.auc=FALSE, paired=FALSE, partial.auc=c(1, 0.76),
         partial.auc.focus="sp", method="bootstrap")


######################################
#Practice Effects - Block
######################################

BlockShowup1 <- read_excel("BlockShowup1.xlsx")
BlockShowup2 <- read_excel("BlockShowup2.xlsx")

#Smallest FAR = 0.21 (Showup1)

# Compute partial area under the curve for BlockShowup1
roc(controls = BlockShowup1$FalseID, cases =
      BlockShowup1$CorrectID[!is.na(BlockShowup1$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.79))

# Compute partial area under the curve for BlockShowup2
roc(controls = BlockShowup2$FalseID, cases =
      BlockShowup2$CorrectID[!is.na(BlockShowup2$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.79))

# Make ROC objects for BlockShowup1
BlockShowup1 <- roc(controls=BlockShowup1$FalseID,
                      cases=BlockShowup1$CorrectID[!is.na(BlockShowup1$CorrectID)], direction = "<")

# Make ROC objects for BlockShowup2
BlockShowup2 <- roc(controls=BlockShowup2$FalseID,
                      cases=BlockShowup2$CorrectID[!is.na(BlockShowup2$CorrectID)], direction = "<")


# Compare BlockShowup1 & BlockShowup2 ROC curves
roc.test(BlockShowup1, BlockShowup2, reuse.auc=FALSE, paired=FALSE, partial.auc=c(1, 0.79),
         partial.auc.focus="sp", method="bootstrap")

######################################
#Practice Effects - DoNothing
######################################

DoNothingShowup1 <- read_excel("DoNothingShowup1.xlsx")
DoNothingShowup2 <- read_excel("DoNothingShowup2.xlsx")

# Smallest FAR = 0.39 (Showup 2)

# Compute partial area under the curve for DoNothingShowup1
roc(controls = DoNothingShowup1$FalseID, cases =
      DoNothingShowup1$CorrectID[!is.na(DoNothingShowup1$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.61))

# Compute partial area under the curve for DoNothingShowup2
roc(controls = DoNothingShowup2$FalseID, cases =
      DoNothingShowup2$CorrectID[!is.na(DoNothingShowup2$CorrectID)], direction = "<", ci = T, partial.auc =
      c(1, 0.61))

# Make ROC objects for DoNothingShowup1
DoNothingShowup1 <- roc(controls=DoNothingShowup1$FalseID,
                      cases=DoNothingShowup1$CorrectID[!is.na(DoNothingShowup1$CorrectID)], direction = "<")

# Make ROC objects for DoNothingShowup2
DoNothingShowup2 <- roc(controls=DoNothingShowup2$FalseID,
                      cases=DoNothingShowup2$CorrectID[!is.na(DoNothingShowup2$CorrectID)], direction = "<")

# Compare DoNothingShowup1 & DoNothingShowup2 ROC curves
roc.test(DoNothingShowup1, DoNothingShowup2, reuse.auc=FALSE, paired=FALSE, partial.auc=c(1, 0.61),
         partial.auc.focus="sp", method="bootstrap")


###########################################################################################
#Yes and No Responses: Full Area Under the Curve (AUC) Analysis: 
#Collapsed over both graffiti and mugging videos (SOM; Figure S1)
###########################################################################################

BlockFullROC <- read_excel("BlockFullROC.xlsx")
DoNothingFullROC <- read_excel("DoNothingFullROC.xlsx")

# Compute area under the curve for BlockFullROC
roc(controls = BlockFullROC$FalseID, cases =
      BlockFullROC$CorrectID[!is.na(BlockFullROC$CorrectID)], direction = "<", ci = T)

# Compute partial area under the curve for DoNothingFullROC
roc(controls = DoNothingFullROC$FalseID, cases =
      DoNothingFullROC$CorrectID[!is.na(DoNothingFullROC$CorrectID)], direction = "<", ci = T)

# Make ROC objects for BlockFullROC
BlockFullROC <- roc(controls=BlockFullROC$FalseID,
                          cases=BlockFullROC$CorrectID[!is.na(BlockFullROC$CorrectID)], direction = "<")

# Make ROC objects for DoNothingFullROC
DoNothingFullROC <- roc(controls=DoNothingFullROC$FalseID,
                        cases=DoNothingFullROC$CorrectID[!is.na(DoNothingFullROC$CorrectID)], direction = "<")

# Compare BlockFullROC & DoNothingFullROC ROC curves
roc.test(BlockFullROC, DoNothingFullROC, reuse.auc=FALSE, paired=FALSE, method="bootstrap")
