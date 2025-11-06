print("Running zeroOrder.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(haven)
###############
## Load data ##
###############
loopr_S1 <- 
  read_sav("Data Analysis/Data - Survey 1.sav") %>%
  rename_with(~ sub("S1_", "", .x))
loopr_S2 <- 
  read_sav("Data Analysis/Data - Survey 2.sav") %>%
  rename_with(~ sub("S2_", "", .x)) %>%
  rename("RiskySuicide" = "rRiskySuicide")
######################
## Define variables ##
######################
outcomes_S1 <-
  c("Gratitude", "Forgiveness", "Inspiration", "Humor", 
    "IdentityIntegrationConsolidation", "LifeSatisfaction", "PositiveAffect",
    "NegativeAffect", "Happiness", "IdentityAchievement", "IdentityDiffusion",
    "IdentityForeclosure", "IdentityMoratorium", "ExistentialWellBeing",
    "CognitiveOrientationTowardsSpirituality", "Religiousness",
    "ExistentialPhenomenologicalDimension", "ParanormalBeliefs", 
    "RightWingAuthoritarianism", "Conservatism", "PeersAcceptanceFriendship",
    "Attractiveness", "PeerStatus", "RomanticDissolution",
    "RomanticSatisfaction", "RomanticConflict", "RomanticAbuse", 
    "FamilyMotherAffectional", "FamilyFatherAffectional",
    "FamilyMotherFunctional", "FamilyFatherFunctional", "AntisocialBehavior",
    "CriminalBehavior")
outcomes_S2 <- 
  c("PersonalityDisordersMistrust", "PersonalityDisordersManipulativeness",
    "PersonalityDisordersAggression", "PersonalityDisordersSelfHarm",
    "PersonalityDisordersEccentricPerceptions",
    "PersonalityDisordersDependency", "PersonalityDisordersExhibitionism",
    "PersonalityDisordersEntitlement", "PersonalityDisordersDetachment",
    "PersonalityDisordersImpulsivity", "PersonalityDisordersPropriety",
    "RealisticInterests", "InvestigativeInterests", "ArtisticInterests",
    "SocialInterests", "EnterprisingInterests", "ConventionalInterests",
    "JobAttainment", "ExtrinsicSuccess", "IntrinsicSuccess", 
    "OccupationalSatisfaction", "OccupationalCommitment", 
    "OccupationalPerformanceTurnover", "OccupationalPerformanceStatusChange",
    "OccupationalPerformanceSalary", "OccupationalInvolvement", "Leadership",
    "FinancialSecurity", "Volunteerism", "ActiveCoping", "AvoidantCoping",
    "SeekingSupport", "Resilience", "RiskyActivity", "RiskyEating",
    "RiskyAlcoholDrunkPrevalence", "RiskyTobaccoPrevalence", 
    "RiskyDrugsPrevalence", "RiskyDriving", "RiskyViolence", "RiskySuicide",
    "HeartDisease", "SubstanceAbuse", "Anxiety", "Depression")
#############################
## Zero-Order Correlations ##
#############################
corr_s1 <- # Create correlation table for survey 1
  loopr_S1 %>%
  select(
    ., 
    all_of(outcomes_S1)) %>%
  cor(use = "pairwise.complete.obs") 
write.csv(
  corr_s1, 
  "Data Analysis/Results/corr_s1.csv")
corr_s2 <- # Create correlation table for survey 2
  loopr_S2 %>%
  select(
    ., 
    all_of(outcomes_S2)) %>%
  cor(use = "pairwise.complete.obs") 
write.csv(
  corr_s2, 
  "Data Analysis/Results/corr_s2.csv")
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("zeroOrder.R finished running!")
Sys.sleep(5)
