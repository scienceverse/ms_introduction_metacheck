print("Running replications.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
######################
## Define variables ##
######################
outcomes_S1 <-
  c("Gratitude", "Forgiveness", "Inspiration", "Humor", 
    "IdentityIntegrationConsolidation", "LifeSatisfaction", "PositiveAffect",
    "NegativeAffect", "Happiness", "IdentityAchievement", "IdentityDiffusion",
    "IdentityForeclosure", "IdentityMoratorium", "ExistentialWellBeing",
    "composite_COTS_Religiousness", "ExistentialPhenomenologicalDimension",
    "ParanormalBeliefs", "RightWingAuthoritarianism", "Conservatism",
    "PeersAcceptanceFriendship", "Attractiveness", "PeerStatus", 
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
    "composite_JobAttainment_ExtrinsicSuccess",
    "composite_OccupationalSatisfaction_IntrinsicSuccess",
    "OccupationalCommitment", "OccupationalPerformanceSalary",
    "OccupationalInvolvement", "Leadership", "FinancialSecurity",
    "Volunteerism", "ActiveCoping", "AvoidantCoping", "SeekingSupport",
    "Resilience", "RiskyActivity", "RiskyDriving", "RiskyViolence")
log_outcome_S1 <- 
  "RomanticDissolution"
log_outcome_S2 <- 
  c("OccupationalPerformanceTurnover", "OccupationalPerformanceStatusChange",
    "RiskyEating", "RiskyAlcoholDrunkPrevalence", "RiskyTobaccoPrevalence",
    "RiskyDrugsPrevalence", "RiskySuicide", "HeartDisease", "SubstanceAbuse",
    "Anxiety", "Depression")
supp_outcome_S1 <- 
  "DatingVariety"
supp_outcome_S2 <- 
  c("PersonalityDisordersWorkaholism", "RiskySex")
##############################################
## Check for Replication - Primary Analyses ##
##############################################
replications_linear <-   # Remove null/spurious effect tests
  read_csv("Data Analysis/Results/results.csv") %>%
  filter(str_detect(interpretation, "Meaningful")) %>%
  write_csv("Data Analysis/Results/replications.csv")
replications_log <-   # Remove null/spurious effect tests
  read_csv("Data Analysis/Results/results_log.csv") %>%
  filter(str_detect(interpretation_log, "Meaningful")) %>%
  write_csv("Data Analysis/Results/replications_log.csv")
replications.S1 <-   # Create data frame for robustness tests (S1)
  replications_linear %>%
  filter(outcome %in% outcomes_S1) %>%
  select(
    ., 
    c("outcome", "trait1", "trait2", "interpretation")) %>%
  write_csv("Data Analysis/Results/replications.S1.csv")
replications.S2 <-   # Create data frame for robustness tests (S2)
  replications_linear %>%
  filter(outcome %in% outcomes_S2) %>%
  select(
    ., 
    c("outcome", "trait1", "trait2", "interpretation")) %>%
  write_csv("Data Analysis/Results/replications.S2.csv")
replications_log.S1 <-# Create df for robustness tests (S1) - binary outcomes
  replications_log %>%
  filter(outcome %in% log_outcome_S1) %>%
  select(
    ., 
    c("outcome", "trait1", "trait2", "interpretation_log")) %>%
  write_csv("Data Analysis/Results/replications_log.S1.csv")
replications_log.S2 <- # Create df for robustness tests (S2) - binary outcomes
  replications_log %>%
  filter(outcome %in% log_outcome_S2) %>%
  select(
    ., 
    c("outcome", "trait1", "trait2", "interpretation_log")) %>%
  write_csv("Data Analysis/Results/replications_log.S2.csv")
###########################################################
## Check for Replication - Supplemental Outcome Analyses ##
###########################################################
replications_supp <-   # Remove null/spurious effect tests
  read_csv("Data Analysis/Results/results_supp.csv") %>%
  filter(str_detect(interpretation_supp, "Meaningful")) %>%
  write_csv("Data Analysis/Results/replications_supp.csv")
replications_supp.S1 <- # Create df for robustness tests (S1) - supp outcomes
  replications_supp %>%
  filter(outcome %in% supp_outcome_S1) %>%
  select(
    ., 
    c("outcome", "trait1", "trait2", "interpretation_supp")) %>%
  write_csv("Data Analysis/Results/replications_supp.S1.csv")
replications_supp.S2 <-   # Create df for robustness tests (S2) - supp outcomes
  replications_supp %>%
  filter(outcome %in% supp_outcome_S2) %>%
  select(
    ., 
    c("outcome", "trait1", "trait2", "interpretation_supp")) %>%
  write_csv("Data Analysis/Results/replications_supp.S2.csv")
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("replications.R finished running!")
Sys.sleep(5)  