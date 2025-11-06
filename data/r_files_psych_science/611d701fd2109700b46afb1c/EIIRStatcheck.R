# A script to make sure that our stats are reported accurately.
# Note that the effect sizes, MSE, and the BF10/01 values are extracted from the text bc the program doesn't check for them.

library(statcheck)

study_1Main <- "Results revealed a significant main effect of the learning condition, F(2, 150) = 8.44, p < .001"
study_2MainLearning <- "There was a main effect of Learning Type with participants in the Active Learning condition (M = 73%) having slightly higher overall accuracy than the participants in the Incidental Learning condition (M = 68%), F(1, 166) = 4.12, p = .04"
study_2MainImage <- "There was also a main effect of Image Type, F(1, 166) = 56.3, p < .001"
study_2MainTime <- "Moreover, there was a main effect of Time Point, F(3, 498) = 3.75, p = .01"
study_2ImageTimeInteraction <- "Importantly, there was a significant interaction between Image Type and Time Point, F(3, 498) = 4.658, p = .003"
study_2ImageTimeLinear <- "To explore this effect further, a linear contrast was computed, which revealed a significant linear trend in the interaction between Image Type and Time Point, F(1, 166) = 11.74, p = .001"
study_2ImageTimeDifference <- " The difference was 24% at T1, t(166.78) = 7.83, p < .001; 17% at T2 t(166.78) = 5.44, p < .001; 14% at T3 t(167.46) = 4.26, p < .001; and, 9% at T4 t(166.31) = 2.76, p = .003."
study_2LearningImageTimeInteraction <- "There was no significant three-way Learning Type x Image Type x Time Point interaction, F(3, 498) = .50, p = .68"
study_2SimpleEffMA <- "Simple effects tests confirmed that the effect of time point was not significant in the Member Average condition, F(3, 231) = .80, p =.50"
study_2SimpleEffNonMA <- "but significant for the Non-member Average condition, F(3, 267) = 6.75, p < .001"
study_2IdentitySequenceInteraction <- "The results revealed a significant interaction between Identity and Sequence, F(3, 504) = 8.765, p < .001"
study_2IdentitySequenceLearningInteraction <- "The results also revealed a significant 3-way interaction among Identity, Sequence, and Training Condition, F(3, 504) = 2.94, p = .03"

# Intermission: I apologize for the terribly long variable names.

# Statcheck for the F tests:
fTests <- c(study_1Main, study_2MainLearning, 
            study_2MainImage, 
            study_2MainTime,
            study_2ImageTimeInteraction,
            study_2ImageTimeLinear,
            study_2LearningImageTimeInteraction,
            study_2SimpleEffMA,
            study_2SimpleEffNonMA,
            study_2IdentitySequenceInteraction,
            study_2IdentitySequenceLearningInteraction)

statcheckFtests <- statcheck(x = fTests, stat = "F", pEqualAlphaSig = FALSE)

# Statcheck for the t tests
tTests <- c(study_2ImageTimeDifference)

statchecktTests <- statcheck(x=tTests, stat = "t", OneTailedTests = TRUE, pEqualAlphaSig = FALSE, OneTailedTxt = FALSE)
