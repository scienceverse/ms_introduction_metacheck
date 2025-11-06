print("Running split.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
###############
## Load data ##
###############
loopr_S1 <-
  read_csv("Data Analysis/Results/loopr_S1_composites.csv")
loopr_S2 <-
  read_csv("Data Analysis/Results/loopr_S2_composites.csv")
######################
## Define variables ##
######################
traits <- 
  c("PersonalityExtraversion", "PersonalityAgreeableness",
    "PersonalityConscientiousness", "PersonalityNegativeEmotionality", 
    "PersonalityOpenMindedness")
outcomes_S1 <-
  c("Gratitude", "Forgiveness", "Inspiration", "Humor", 
    "IdentityIntegrationConsolidation", "LifeSatisfaction", "PositiveAffect",
    "NegativeAffect", "Happiness", "IdentityAchievement", "IdentityDiffusion",
    "IdentityForeclosure", "IdentityMoratorium", "ExistentialWellBeing",
    "composite_COTS_Religiousness", "ExistentialPhenomenologicalDimension",
    "ParanormalBeliefs", "RightWingAuthoritarianism", "Conservatism",
    "PeersAcceptanceFriendship", "Attractiveness", "PeerStatus",
    "RomanticDissolution", "RomanticSatisfaction", "RomanticConflict",
    "RomanticAbuse", "FamilyMotherAffectional", "FamilyFatherAffectional",
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
    "OccupationalCommitment", "OccupationalPerformanceTurnover",
    "OccupationalPerformanceStatusChange", "OccupationalPerformanceSalary",
    "OccupationalInvolvement", "Leadership", "FinancialSecurity",
    "Volunteerism", "ActiveCoping", "AvoidantCoping", "SeekingSupport",
    "Resilience", "RiskyActivity", "RiskyEating", 
    "RiskyAlcoholDrunkPrevalence", "RiskyTobaccoPrevalence",
    "RiskyDrugsPrevalence", "RiskyDriving", "RiskyViolence", "RiskySuicide",
    "HeartDisease", "SubstanceAbuse", "Anxiety", "Depression")
supp_outcome_S2 <- 
  c("PersonalityDisordersWorkaholism", "RiskySex")
##################
## Split Sample ##
##################
set.seed(30605) # Set seed for reproducibility
for (i in outcomes_S1) {  # For each outcome in survey 1, split into 2 parts
  temp <- 
    loopr_S1 %>%
    select(
      .,
      c("CaseID", all_of(traits), all_of(i), "Sex", "Age_1", "Race_1",
        "HispanicLatino")) %>%
    mutate(
      NonHispanicWhite = ifelse( # create binary ethnicity var
        Race_1 == 1 & HispanicLatino == 0, 
        1,
        0)) %>% 
    filter(complete.cases(.))
  A <- # Select random half of survey 1 responses
    slice_sample(temp, prop = .5) 
  B <- # Select remaining responses
    setdiff(temp, A) 
  assign(paste0(i, "_A"), A)
  assign(paste0(i, "_B"), B)
}
for (i in outcomes_S1) {
  write_csv(
    get(paste0(i, "_A")), 
    paste0("Data Analysis/Splits/", i, "_A.csv"))
  write_csv(
    get(paste0(i, "_B")), 
    paste0("Data Analysis/Splits/", i, "_B.csv"))
}
for (i in outcomes_S2) {   # For each outcome in survey 2, split into 2 parts
  temp <- 
    loopr_S2 %>%
    select(
      .,
      c("CaseID", all_of(traits), all_of(i), "Sex", "Age_1", "Race_1",
        "HispanicLatino")) %>%
    mutate(
      NonHispanicWhite = ifelse( # create binary ethnicity var
        Race_1 == 1 & HispanicLatino == 0, 
        1,
        0)) %>% 
    filter(complete.cases(.))
  A <- # Select random half of survey 1 responses
    slice_sample(temp, prop = .5)
  B <- # Select remaining responses
    setdiff(temp, A)
  assign(paste0(i, "_A"), A)
  assign(paste0(i, "_B"), B)
}
for (i in outcomes_S2) {
  write_csv(
    get(paste0(i, "_A")), 
    paste0("Data Analysis/Splits/", i, "_A.csv"))
  write_csv(
    get(paste0(i, "_B")), 
    paste0("Data Analysis/Splits/", i, "_B.csv"))
}
# For DatingVariety outcome, split into 2 parts
temp <- 
  loopr_S1 %>%
  select(
    .,
    c("CaseID", all_of(traits), "DatingVariety", "Sex", "Age_1", "Race_1",
      "HispanicLatino")) %>%
  mutate(
    NonHispanicWhite = ifelse( # create binary ethinicity var
      Race_1 == 1 & HispanicLatino == 0, 
      1,
      0)) %>% 
  filter(complete.cases(.))
DatingVariety_A <- # Select random half 
  slice_sample(temp, prop = .5)
DatingVariety_B <- # Select remaining
  setdiff(temp, DatingVariety_A)
write_csv(
  DatingVariety_A, 
  "Data Analysis/Splits/DatingVariety_A.csv")
write_csv(
  DatingVariety_B, 
  "Data Analysis/Splits/DatingVariety_B.csv")
for (i in supp_outcome_S2) { # For each supp outcome in survey 2, split 
  temp <- 
    loopr_S2 %>%
    select(
      .,
      c("CaseID", all_of(traits), all_of(i), "Sex", "Age_1", "Race_1",
        "HispanicLatino")) %>%
    mutate(
      NonHispanicWhite = ifelse( # create binary ethnicity var
        Race_1 == 1 &  HispanicLatino == 0, 
        1, 
        0)) %>% 
    filter(complete.cases(.))
  A <- # Select random half
    slice_sample(temp, prop = .5)
  B <- # Select remaining
    setdiff(temp, A)
  assign(paste0(i, "_A"), A)
  assign(paste0(i, "_B"), B)
}
for (i in supp_outcome_S2) {
  write_csv(
    get(paste0(i, "_A")), 
    paste0("Data Analysis/Splits/", i, "_A.csv"))
  write_csv(
    get(paste0(i, "_B")), 
    paste0("Data Analysis/Splits/", i, "_B.csv"))
}
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("split.R finished running!")
Sys.sleep(5)
