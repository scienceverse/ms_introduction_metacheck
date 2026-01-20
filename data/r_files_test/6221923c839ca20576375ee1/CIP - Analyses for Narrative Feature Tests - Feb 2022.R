# Manuscript Analyses for "Early Impacts of College, Interrupted: Considering 
    # First-Year Students’ Narratives about COVID and Reports of Adjustment 
    # during College Shutdowns"
# Last Updated: 2_27_22

library(readr)
CIP <- read_csv("CIP Masked Data - T1 Themes and Self-Reports.csv")

library(psych)
citation('psych')

describe(CIP)

attach(CIP) # without this command you need to specify the dataset for each analysis

# Preliminary Analyses ----

# CoherenceComposite.1 is the combination of LifeImpact.Theme.1, LifeImpact.Context.1,
# and LifeImpact.Chronology.1

library(lsr)
citation('lsr')

# ANOVAs of Gender with Narrative Themes
summary(aov(CoherenceComposite.1 ~ as.factor(Gender.1)))
summary(aov(LifeImpact.Fact.1 ~ as.factor(Gender.1)))
    etaSquared(aov(LifeImpact.Fact.1 ~ as.factor(Gender.1)))
summary(aov(LifeImpact.Interpret.1 ~ as.factor(Gender.1)))
    etaSquared(aov(LifeImpact.Interpret.1 ~ as.factor(Gender.1)))
summary(aov(LifeImpact.Agency.1 ~ as.factor(Gender.1)))
summary(aov(LifeImpact.Communion.1 ~ as.factor(Gender.1)))
    etaSquared(aov(LifeImpact.Communion.1 ~ as.factor(Gender.1)))
summary(aov(LifeImpact.Growth.1 ~ as.factor(Gender.1)))

# ANOVAs of Racial Group (5 Larger Groups) with Narrative Themes
summary(aov(CoherenceComposite.1 ~ as.factor(Trimmed.Race.Cats)))
summary(aov(LifeImpact.Fact.1 ~ as.factor(Trimmed.Race.Cats)))
summary(aov(LifeImpact.Interpret.1 ~ as.factor(Trimmed.Race.Cats)))
summary(aov(LifeImpact.Agency.1 ~ as.factor(Trimmed.Race.Cats)))
summary(aov(LifeImpact.Communion.1 ~ as.factor(Trimmed.Race.Cats)))
summary(aov(LifeImpact.Growth.1 ~ as.factor(Trimmed.Race.Cats)))

# T-Tests given Pell Grant Status
t.test(CoherenceComposite.1 ~ DidReceivePellAY1920)
t.test(LifeImpact.Fact.1 ~ DidReceivePellAY1920)
t.test(LifeImpact.Interpret.1 ~ DidReceivePellAY1920)
t.test(LifeImpact.Agency.1 ~ DidReceivePellAY1920)
t.test(LifeImpact.Communion.1 ~ DidReceivePellAY1920)
t.test(LifeImpact.Growth.1 ~ DidReceivePellAY1920)

# Testing baseline differences given one-year follow-up availability

t.test(CoherenceComposite.1 ~ T4Availability)
t.test(LifeImpact.Fact.1 ~ T4Availability)
t.test(LifeImpact.Interpret.1 ~ T4Availability)
t.test(LifeImpact.Agency.1 ~ T4Availability)
t.test(LifeImpact.Communion.1 ~ T4Availability)
t.test(LifeImpact.Growth.1 ~ T4Availability)

t.test(Needs.Competence.1 ~ T4Availability)
t.test(Needs.Autonomy.1 ~ T4Availability)
t.test(Needs.Relatedness.1 ~ T4Availability)
t.test(College.Self.Efficacy.1 ~ T4Availability)
t.test(Belonging.1 ~ T4Availability)
t.test(TotalRyff.1 ~ T4Availability)
t.test(DIDS.CM.1 ~ T4Availability)
t.test(DIDS.EB.1 ~ T4Availability)
t.test(DIDS.RE.1 ~ T4Availability)
t.test(DIDS.IC.1 ~ T4Availability)
t.test(DIDS.ED.1 ~ T4Availability)
t.test(Depression34.1 ~ T4Availability)
t.test(Anxiety34.1 ~ T4Availability)
t.test(Social_Anxiety34.1 ~ T4Availability)
t.test(Academics34.1 ~ T4Availability)
t.test(Eating34.1 ~ T4Availability)
t.test(Hostility34.1 ~ T4Availability)
t.test(Alcohol34.1 ~ T4Availability)

# Correlation Tests ----
# include BH correction for multiple tests

Baseline.Correlation.Frame <- data.frame(Covidstress.1, LifeImpact.Fact.1, 
    LifeImpact.Interpret.1, CoherenceComposite.1, LifeImpact.Agency.1,
    LifeImpact.Communion.1, LifeImpact.Growth.1, Needs.Competence.1, 
    Needs.Autonomy.1, Needs.Relatedness.1, College.Self.Efficacy.1, 
    Belonging.1, TotalRyff.1, DIDS.CM.1, DIDS.EB.1, DIDS.RE.1, DIDS.IC.1,
    DIDS.ED.1, EPSI.Identity.1, Depression34.1, Anxiety34.1, Social_Anxiety34.1,
    Academics34.1, Eating34.1, Hostility34.1, Alcohol34.1)

corr.test(Baseline.Correlation.Frame, adjust = "BH")
print(corr.test(Baseline.Correlation.Frame, adjust = "BH", ci = TRUE), short = FALSE)


FollowUp.Correlation.Frame <- data.frame(Covidstress.1, LifeImpact.Fact.1, 
    LifeImpact.Interpret.1, CoherenceComposite.1, LifeImpact.Agency.1,
    LifeImpact.Communion.1, LifeImpact.Growth.1, Needs.Competence.4, 
    Needs.Autonomy.4, Needs.Relatedness.4, College.Self.Efficacy.4, 
    Belonging.4, TotalRyff.4, DIDS.CM.4, DIDS.EB.4, DIDS.RE.4, DIDS.IC.4,
    DIDS.ED.4, EPSI.Identity.4, Depression34.4, Anxiety34.4, Social_Anxiety34.4,
    Academics34.4, Eating34.4, Hostility34.4, Alcohol34.4)

corr.test(FollowUp.Correlation.Frame, adjust = "BH")
print(corr.test(FollowUp.Correlation.Frame, adjust = "BH", ci = TRUE), short = FALSE)


# Regression Tests ----

library(rsq) # for testing partial r^2
citation('rsq')
library(konfound) # provides model-level sensitivity analyses
citation('konfound')

# For these tests, syntax focuses on the effect sizes for narrative growth
    # This syntax can be repurposed to consider other model effects

# Commonly used cutoffs for f2 (partial r2) effect sizes are .02 (small), 
    # .15 (medium), and .35 (large)

# Regressions for Baseline Outcomes (Spring 2020) ----

Baseline.Competence.1 <- lm(Needs.Competence.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.Competence.1)
Baseline.Competence.2 <- lm(Needs.Competence.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.Competence.2)
    rsq.partial(Baseline.Competence.2)
Baseline.Competence.3 <- lm(Needs.Competence.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.Competence.3)
    rsq.partial(Baseline.Competence.3)
    konfound(Baseline.Competence.3, test_all = TRUE)

Baseline.Autonomy.1 <- lm(Needs.Autonomy.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.Autonomy.1)
Baseline.Autonomy.2 <- lm(Needs.Autonomy.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.Autonomy.2)
    rsq.partial(Baseline.Autonomy.2)
Baseline.Autonomy.3 <- lm(Needs.Autonomy.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.Autonomy.3)
    rsq.partial(Baseline.Autonomy.3)
    konfound(Baseline.Autonomy.3, test_all = TRUE)
    
Baseline.Relatedness.1 <- lm(Needs.Relatedness.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.Relatedness.1)
Baseline.Relatedness.2 <- lm(Needs.Relatedness.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.Relatedness.2)
    rsq.partial(Baseline.Relatedness.2)
Baseline.Relatedness.3 <- lm(Needs.Relatedness.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.Relatedness.3)
    rsq.partial(Baseline.Relatedness.3)
    konfound(Baseline.Competence.3, test_all = TRUE)
    
Baseline.SelfEfficacy.1 <- lm(College.Self.Efficacy.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.SelfEfficacy.1)
Baseline.SelfEfficacy.2 <- lm(College.Self.Efficacy.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.SelfEfficacy.2)
    rsq.partial(Baseline.SelfEfficacy.2)
Baseline.SelfEfficacy.3 <- lm(College.Self.Efficacy.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
    summary(Baseline.SelfEfficacy.3)
    rsq.partial(Baseline.SelfEfficacy.3)
    konfound(Baseline.SelfEfficacy.3, test_all = TRUE)

Baseline.Belonging.1 <- lm(Belonging.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.Belonging.1)
Baseline.Belonging.2 <- lm(Belonging.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.Belonging.2)
    rsq.partial(Baseline.Belonging.2)
Baseline.Belonging.3 <- lm(Belonging.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.Belonging.3)
    rsq.partial(Baseline.Belonging.3)
    konfound(Baseline.Belonging.3, test_all = TRUE)

Baseline.WellBeing.1 <- lm(TotalRyff.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.WellBeing.1)
Baseline.WellBeing.2 <- lm(TotalRyff.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.WellBeing.2)
    rsq.partial(Baseline.WellBeing.2)
Baseline.WellBeing.3 <- lm(TotalRyff.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.WellBeing.3)
    rsq.partial(Baseline.WellBeing.3)
    konfound(Baseline.WellBeing.3, test_all = TRUE)
    
Baseline.CommitmentMaking.1 <- lm(DIDS.CM.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.CommitmentMaking.1)
Baseline.CommitmentMaking.2 <- lm(DIDS.CM.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.CommitmentMaking.2)
    rsq.partial(Baseline.CommitmentMaking.2)
Baseline.CommitmentMaking.3 <- lm(DIDS.CM.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.CommitmentMaking.3)
    rsq.partial(Baseline.CommitmentMaking.3)
    konfound(Baseline.CommitmentMaking.3, test_all = TRUE)

Baseline.ExpInBreadth.1 <- lm(DIDS.EB.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.ExpInBreadth.1)
Baseline.ExpInBreadth.2 <- lm(DIDS.EB.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.ExpInBreadth.2)
rsq.partial(Baseline.ExpInBreadth.2)
Baseline.ExpInBreadth.3 <- lm(DIDS.EB.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.ExpInBreadth.3)
    rsq.partial(Baseline.ExpInBreadth.3)
    konfound(Baseline.ExpInBreadth.3, test_all = TRUE)    
    
Baseline.RuminativeExpl.1 <- lm(DIDS.RE.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.RuminativeExpl.1)
Baseline.RuminativeExpl.2 <- lm(DIDS.RE.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.RuminativeExpl.2)
    rsq.partial(Baseline.RuminativeExpl.2)
Baseline.RuminativeExpl.3 <- lm(DIDS.RE.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.RuminativeExpl.3)
    rsq.partial(Baseline.RuminativeExpl.3)
    konfound(Baseline.RuminativeExpl.3, test_all = TRUE)

Baseline.IdentCommitment.1 <- lm(DIDS.IC.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.IdentCommitment.1)
Baseline.IdentCommitment.2 <- lm(DIDS.IC.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.IdentCommitment.2)
    rsq.partial(Baseline.IdentCommitment.2)
Baseline.IdentCommitment.3 <- lm(DIDS.IC.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.IdentCommitment.3)
    rsq.partial(Baseline.IdentCommitment.3)
    konfound(Baseline.IdentCommitment.3, test_all = TRUE)    

Baseline.ExpInDepth.1 <- lm(DIDS.ED.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.ExpInDepth.1)
Baseline.ExpInDepth.2 <- lm(DIDS.ED.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.ExpInDepth.2)
    rsq.partial(Baseline.ExpInDepth.2)
Baseline.ExpInDepth.3 <- lm(DIDS.ED.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.ExpInDepth.3)
    rsq.partial(Baseline.ExpInDepth.3)
    konfound(Baseline.ExpInDepth.3, test_all = TRUE)

Baseline.GlobalID.1 <- lm(EPSI.Identity.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.GlobalID.1)
Baseline.GlobalID.2 <- lm(EPSI.Identity.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.GlobalID.2)
    rsq.partial(Baseline.GlobalID.2)
Baseline.GlobalID.3 <- lm(EPSI.Identity.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.GlobalID.3)
    rsq.partial(Baseline.GlobalID.3)
    konfound(Baseline.GlobalID.3, test_all = TRUE)   
    
Baseline.Depression.1 <- lm(Depression34.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.Depression.1)
Baseline.Depression.2 <- lm(Depression34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.Depression.2)
    rsq.partial(Baseline.Depression.2)
Baseline.Depression.3 <- lm(Depression34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.Depression.3)
    rsq.partial(Baseline.Depression.3)
    konfound(Baseline.Depression.3, test_all = TRUE)

Baseline.Anxiety.1 <- lm(Anxiety34.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.Anxiety.1)
Baseline.Anxiety.2 <- lm(Anxiety34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.Anxiety.2)
    rsq.partial(Baseline.Anxiety.2)
Baseline.Anxiety.3 <- lm(Anxiety34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.Anxiety.3)
    rsq.partial(Baseline.Anxiety.3)
    konfound(Baseline.Anxiety.3, test_all = TRUE)        

Baseline.SocialAnxiety.1 <- lm(Social_Anxiety34.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.SocialAnxiety.1)
Baseline.SocialAnxiety.2 <- lm(Social_Anxiety34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.SocialAnxiety.2)
    rsq.partial(Baseline.SocialAnxiety.2)
Baseline.SocialAnxiety.3 <- lm(Social_Anxiety34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.SocialAnxiety.3)
    rsq.partial(Baseline.SocialAnxiety.3)
    konfound(Baseline.SocialAnxiety.3, test_all = TRUE)    

Baseline.AcademicDistress.1 <- lm(Academics34.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.AcademicDistress.1)
Baseline.AcademicDistress.2 <- lm(Academics34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.AcademicDistress.2)
    rsq.partial(Baseline.AcademicDistress.2)
Baseline.AcademicDistress.3 <- lm(Academics34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.AcademicDistress.3)
    rsq.partial(Baseline.AcademicDistress.3)
    konfound(Baseline.AcademicDistress.3, test_all = TRUE)   
    
Baseline.EatingConcerns.1 <- lm(Eating34.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.EatingConcerns.1)
Baseline.EatingConcerns.2 <- lm(Eating34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.EatingConcerns.2)
    rsq.partial(Baseline.EatingConcerns.2)
Baseline.EatingConcerns.3 <- lm(Eating34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.EatingConcerns.3)
    rsq.partial(Baseline.EatingConcerns.3)
    konfound(Baseline.EatingConcerns.3, test_all = TRUE)   

Baseline.Hostility.1 <- lm(Hostility34.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.Hostility.1)
Baseline.Hostility.2 <- lm(Hostility34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.Hostility.2)
    rsq.partial(Baseline.Hostility.2)
Baseline.Hostility.3 <- lm(Hostility34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.Hostility.3)
    rsq.partial(Baseline.Hostility.3)
    konfound(Baseline.Hostility.3, test_all = TRUE)       

Baseline.SubstanceUse.1 <- lm(Alcohol34.1 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(Baseline.SubstanceUse.1)
Baseline.SubstanceUse.2 <- lm(Alcohol34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1)
summary(Baseline.SubstanceUse.2)
    rsq.partial(Baseline.SubstanceUse.2)
Baseline.SubstanceUse.3 <- lm(Alcohol34.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(Baseline.SubstanceUse.3)
    rsq.partial(Baseline.SubstanceUse.3)
    konfound(Baseline.SubstanceUse.3, test_all = TRUE)       

# Regressions for Follow-Up Outcomes (Spring 2021) ---- 
    
FollowUp.Competence.1 <- lm(Needs.Competence.4 ~ CisMale_Dummy)
summary(FollowUp.Competence.1)
FollowUp.Competence.2 <- lm(Needs.Competence.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.Competence.2)
    rsq.partial(FollowUp.Competence.2)
FollowUp.Competence.3 <- lm(Needs.Competence.4 ~ CisMale_Dummy + Covidstress.1 + 
    LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.Competence.3)
    rsq.partial(FollowUp.Competence.3)
    konfound(FollowUp.Competence.3, test_all = TRUE)
    
FollowUp.Autonomy.1 <- lm(Needs.Autonomy.4 ~ CisMale_Dummy )
summary(FollowUp.Autonomy.1)
FollowUp.Autonomy.2 <- lm(Needs.Autonomy.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.Autonomy.2)
    rsq.partial(FollowUp.Autonomy.2)
FollowUp.Autonomy.3 <- lm(Needs.Autonomy.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.Autonomy.3)
    rsq.partial(FollowUp.Autonomy.3)
    konfound(FollowUp.Autonomy.3, test_all = TRUE)
    
FollowUp.Relatedness.1 <- lm(Needs.Relatedness.4 ~ CisMale_Dummy)
summary(FollowUp.Relatedness.1)
FollowUp.Relatedness.2 <- lm(Needs.Relatedness.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.Relatedness.2)
    rsq.partial(FollowUp.Relatedness.2)
FollowUp.Relatedness.3 <- lm(Needs.Relatedness.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.Relatedness.3)
    rsq.partial(FollowUp.Relatedness.3)
    konfound(FollowUp.Competence.3, test_all = TRUE)
    
FollowUp.SelfEfficacy.1 <- lm(College.Self.Efficacy.4 ~ CisMale_Dummy)
summary(FollowUp.SelfEfficacy.1)
FollowUp.SelfEfficacy.2 <- lm(College.Self.Efficacy.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.SelfEfficacy.2)
    rsq.partial(FollowUp.SelfEfficacy.2)
FollowUp.SelfEfficacy.3 <- lm(College.Self.Efficacy.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.SelfEfficacy.3)
    rsq.partial(FollowUp.SelfEfficacy.3)
    konfound(FollowUp.SelfEfficacy.3, test_all = TRUE)
    
FollowUp.Belonging.1 <- lm(Belonging.4 ~ CisMale_Dummy )
summary(FollowUp.Belonging.1)
FollowUp.Belonging.2 <- lm(Belonging.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.Belonging.2)
    rsq.partial(FollowUp.Belonging.2)
FollowUp.Belonging.3 <- lm(Belonging.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.Belonging.3)
    rsq.partial(FollowUp.Belonging.3)
    konfound(FollowUp.Belonging.3, test_all = TRUE)
    
FollowUp.WellBeing.1 <- lm(TotalRyff.4 ~ CisMale_Dummy)
summary(FollowUp.WellBeing.1)
FollowUp.WellBeing.2 <- lm(TotalRyff.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.WellBeing.2)
    rsq.partial(FollowUp.WellBeing.2)
FollowUp.WellBeing.3 <- lm(TotalRyff.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.WellBeing.3)
    rsq.partial(FollowUp.WellBeing.3)
    konfound(FollowUp.WellBeing.3, test_all = TRUE)
    
FollowUp.CommitmentMaking.1 <- lm(DIDS.CM.4 ~ CisMale_Dummy)
summary(FollowUp.CommitmentMaking.1)
FollowUp.CommitmentMaking.2 <- lm(DIDS.CM.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.CommitmentMaking.2)
    rsq.partial(FollowUp.CommitmentMaking.2)
FollowUp.CommitmentMaking.3 <- lm(DIDS.CM.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.CommitmentMaking.3)
    rsq.partial(FollowUp.CommitmentMaking.3)
    konfound(FollowUp.CommitmentMaking.3, test_all = TRUE)
    
FollowUp.ExpInBreadth.1 <- lm(DIDS.EB.4 ~ CisMale_Dummy)
summary(FollowUp.ExpInBreadth.1)
FollowUp.ExpInBreadth.2 <- lm(DIDS.EB.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.ExpInBreadth.2)
    rsq.partial(FollowUp.ExpInBreadth.2)
FollowUp.ExpInBreadth.3 <- lm(DIDS.EB.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.ExpInBreadth.3)
    rsq.partial(FollowUp.ExpInBreadth.3)
    konfound(FollowUp.ExpInBreadth.3, test_all = TRUE)    
    
FollowUp.RuminativeExpl.1 <- lm(DIDS.RE.4 ~ CisMale_Dummy )
summary(FollowUp.RuminativeExpl.1)
FollowUp.RuminativeExpl.2 <- lm(DIDS.RE.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.RuminativeExpl.2)
    rsq.partial(FollowUp.RuminativeExpl.2)
FollowUp.RuminativeExpl.3 <- lm(DIDS.RE.4 ~ CisMale_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.RuminativeExpl.3)
    rsq.partial(FollowUp.RuminativeExpl.3)
    konfound(FollowUp.RuminativeExpl.3, test_all = TRUE)
    
FollowUp.IdentCommitment.1 <- lm(DIDS.IC.4 ~ CisMale_Dummy)
summary(FollowUp.IdentCommitment.1)
    FollowUp.IdentCommitment.2 <- lm(DIDS.IC.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.IdentCommitment.2)
    rsq.partial(FollowUp.IdentCommitment.2)
FollowUp.IdentCommitment.3 <- lm(DIDS.IC.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.IdentCommitment.3)
    rsq.partial(FollowUp.IdentCommitment.3)
    konfound(FollowUp.IdentCommitment.3, test_all = TRUE)    
    
FollowUp.ExpInDepth.1 <- lm(DIDS.ED.4 ~ CisMale_Dummy)
summary(FollowUp.ExpInDepth.1)
FollowUp.ExpInDepth.2 <- lm(DIDS.ED.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.ExpInDepth.2)
    rsq.partial(FollowUp.ExpInDepth.2)
FollowUp.ExpInDepth.3 <- lm(DIDS.ED.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.ExpInDepth.3)
    rsq.partial(FollowUp.ExpInDepth.3)
    konfound(FollowUp.ExpInDepth.3, test_all = TRUE)
    
FollowUp.GlobalID.1 <- lm(EPSI.Identity.4 ~ CisMale_Dummy)
summary(FollowUp.GlobalID.1)
FollowUp.GlobalID.2 <- lm(EPSI.Identity.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.GlobalID.2)
    rsq.partial(FollowUp.GlobalID.2)
FollowUp.GlobalID.3 <- lm(EPSI.Identity.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.GlobalID.3)
    rsq.partial(FollowUp.GlobalID.3)
    konfound(FollowUp.GlobalID.3, test_all = TRUE)
    
FollowUp.Depression.1 <- lm(Depression34.4 ~ CisMale_Dummy)
summary(FollowUp.Depression.1)
FollowUp.Depression.2 <- lm(Depression34.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.Depression.2)
    rsq.partial(FollowUp.Depression.2)
FollowUp.Depression.3 <- lm(Depression34.4 ~ CisMale_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.Depression.3)
    rsq.partial(FollowUp.Depression.3)
    konfound(FollowUp.Depression.3, test_all = TRUE)
    
FollowUp.Anxiety.1 <- lm(Anxiety34.4 ~ CisMale_Dummy)
summary(FollowUp.Anxiety.1)
FollowUp.Anxiety.2 <- lm(Anxiety34.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.Anxiety.2)
    rsq.partial(FollowUp.Anxiety.2)
FollowUp.Anxiety.3 <- lm(Anxiety34.4 ~ CisMale_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.Anxiety.3)
    rsq.partial(FollowUp.Anxiety.3)
    konfound(FollowUp.Anxiety.3, test_all = TRUE)        
    
FollowUp.SocialAnxiety.1 <- lm(Social_Anxiety34.4 ~ CisMale_Dummy)
summary(FollowUp.SocialAnxiety.1)
FollowUp.SocialAnxiety.2 <- lm(Social_Anxiety34.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.SocialAnxiety.2)
    rsq.partial(FollowUp.SocialAnxiety.2)
FollowUp.SocialAnxiety.3 <- lm(Social_Anxiety34.4 ~ CisMale_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.SocialAnxiety.3)
    rsq.partial(FollowUp.SocialAnxiety.3)
    konfound(FollowUp.SocialAnxiety.3, test_all = TRUE)    
    
FollowUp.AcademicDistress.1 <- lm(Academics34.4 ~ CisMale_Dummy + GenderQueer_Dummy)
summary(FollowUp.AcademicDistress.1)
FollowUp.AcademicDistress.2 <- lm(Academics34.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.AcademicDistress.2)
    rsq.partial(FollowUp.AcademicDistress.2)
FollowUp.AcademicDistress.3 <- lm(Academics34.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.AcademicDistress.3)
    rsq.partial(FollowUp.AcademicDistress.3)
    konfound(FollowUp.AcademicDistress.3, test_all = TRUE)   
    
FollowUp.EatingConcerns.1 <- lm(Eating34.4 ~ CisMale_Dummy)
summary(FollowUp.EatingConcerns.1)
FollowUp.EatingConcerns.2 <- lm(Eating34.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.EatingConcerns.2)
    rsq.partial(FollowUp.EatingConcerns.2)
FollowUp.EatingConcerns.3 <- lm(Eating34.4 ~ CisMale_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.EatingConcerns.3)
    rsq.partial(FollowUp.EatingConcerns.3)
    konfound(FollowUp.EatingConcerns.3, test_all = TRUE)   
    
FollowUp.Hostility.1 <- lm(Hostility34.4 ~ CisMale_Dummy)
summary(FollowUp.Hostility.1)
FollowUp.Hostility.2 <- lm(Hostility34.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.Hostility.2)
    rsq.partial(FollowUp.Hostility.2)
FollowUp.Hostility.3 <- lm(Hostility34.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.Hostility.3)
    rsq.partial(FollowUp.Hostility.3)
    konfound(FollowUp.Hostility.3, test_all = TRUE)       
    
FollowUp.SubstanceUse.1 <- lm(Alcohol34.4 ~ CisMale_Dummy)
summary(FollowUp.SubstanceUse.1)
FollowUp.SubstanceUse.2 <- lm(Alcohol34.4 ~ CisMale_Dummy + Covidstress.1)
summary(FollowUp.SubstanceUse.2)
    rsq.partial(FollowUp.SubstanceUse.2)
FollowUp.SubstanceUse.3 <- lm(Alcohol34.4 ~ CisMale_Dummy +
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + LifeImpact.Growth.1)
summary(FollowUp.SubstanceUse.3)
    rsq.partial(FollowUp.SubstanceUse.3)
    konfound(FollowUp.SubstanceUse.3, test_all = TRUE)       

    
    
# Testing Interaction Effects between Growth and Indicators of Vulnerability ----
# For space, only previously supported models were shown here, but others can
    # be re-tested using similar syntax
    
Baseline.SelfEfficacy.Int <- lm(College.Self.Efficacy.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    + Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + scale(LifeImpact.Growth.1) + 
    scale(Whether.AtRisk) + scale(LifeImpact.Growth.1):scale(Whether.AtRisk))
    summary(Baseline.SelfEfficacy.Int)
    rsq.partial(Baseline.SelfEfficacy.Int)
    konfound(Baseline.SelfEfficacy.Int, test_all = TRUE)

FollowUp.IdentCommitment.Int <- lm(DIDS.IC.4 ~ CisMale_Dummy + 
    Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 + scale(LifeImpact.Growth.1) + 
    scale(Whether.AtRisk) + scale(LifeImpact.Growth.1):scale(Whether.AtRisk))
    summary(FollowUp.IdentCommitment.Int)
    rsq.partial(FollowUp.IdentCommitment.Int)
    konfound(FollowUp.IdentCommitment.Int, test_all = TRUE)
    
# Testing Indirect/Mediation Effects, focusing on Growth as a Mediating Variable----
    
    # Selig, J. P., & Preacher, K. J. (2008, June). Monte Carlo method for 
        # assessing mediation: An interactive tool for creating confidence intervals 
        # for indirect effects [Computer software]. Available from http://quantpsy.org/.
    
# First affirming the ties between COVID stressors and narrated growth
    # both measures from baseline in this set of models
    # retaining gender and other narrative features as covariates
    
Baseline.Growth <- lm(LifeImpact.Growth.1 ~ CisMale_Dummy + GenderQueer_Dummy +
    + Covidstress.1 + LifeImpact.Fact.1 + LifeImpact.Interpret.1 + CoherenceComposite.1
    + LifeImpact.Agency.1 + LifeImpact.Communion.1 )
summary(Baseline.Growth)
    rsq.partial(Baseline.Growth)

# These 'path a'  effects would be used for part of the mediation tests
    # taking the unstandardized estimate and squared standard error in this case
    # see http://quantpsy.org/medmc/medmc.htm
    
# Given the estimation approach for these models, the provided model results can
    # differ slightly from final numbers presented in our manuscript, but the 
    # overall evidence should not differ substantially or shift the overall 
    # meaning behind findings
    
# Test for Baseline Competence Need Fulfillment
require(MASS)
a=-0.23468
b=0.1849
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0065950641
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
         main='Distribution of Indirect Effect for Baseline Competence Need Fulfillment')
    
    
# Test for Baseline Autonomy Need Fulfillment

require(MASS)
a=-0.23468
b=0.286
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0046744569
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Autonomy Need Fulfillment')


# Test for Baseline Relatedness Need Fulfillment

require(MASS)
a=-0.23468
b=0.1512
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0065950641
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Relatedness Need Fulfillment')

# Test for Baseline College Self-Efficacy

require(MASS)
a=-0.23468
b=0.28504
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0134652816
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline College Self-Efficacy')

# Test for Baseline College Belonging

require(MASS)
a=-0.23468
b=0.10546
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0489958225
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline College Belonging')

# Test for Baseline Well-Being

require(MASS)
a=-0.23468
b=0.16509
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0022610025
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Well-Being')

# Test for Baseline DIDS - Commitment-Making 

require(MASS)
a=-0.23468
b=0.10471
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0041357761
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline DIDS Commitment-Making')

# Test for Baseline DIDS - Exploration in Breadth 

require(MASS)
a=-0.23468
b=0.08985
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0025664356
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for DIDS Exploration-in-Breadth')

# Test for Baseline DIDS - Ruminative Exploration

require(MASS)
a=-0.23468
b=-0.0578
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.003513407076
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline DIDS Ruminative Exploration')

# Test for Baseline DIDS - Identification with Commitment

require(MASS)
a=-0.23468
b=0.14365
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.003328251481
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline DIDS Identification with Commitment')

# Test for Baseline DIDS - Exploration in Breadth 

require(MASS)
a=-0.23468
b=0.035883
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.001657141264
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline DIDS Exploration-in-Breadth')

# Test for EPSI Global Identity Development

require(MASS)
a=-0.23468
b=0.212017
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.002717953956
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Global Identity Development')

# Test for Baseline Depression

require(MASS)
a=-0.23468
b=-0.27513
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0035236096
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Depression')

# Test for Baseline (Generalized) Anxiety

require(MASS)
a=-0.23468
b=-0.10291
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.002624615361
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Anxiety')

# Test for Baseline Social Anxiety

require(MASS)
a=-0.23468
b=-0.1817
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0042419169
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Social Anxiety')

# Test for Baseline Academic Distress

require(MASS)
a=-0.23468
b=-0.1852
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0042523441
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Academic Distress')

# Test for Baseline Eating Concerns

require(MASS)
a=-0.23468
b=-0.18556
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0054420129
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Eating Concerns')

# Test for Baseline Hostility

require(MASS)
a=-0.23468
b=-0.16224
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0024512401
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Hostility')

# Test for Baseline Substance Use

require(MASS)
a=-0.23468
b=-0.07041
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0027342441
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Baseline Substance Use')

# Test for Follow-Up Competence Fulfillment

require(MASS)
a=-0.23468
b=0.20395
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0145082025
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Competence Need Fulfillment')

# Test for Follow-Up Autonomy Fulfillment

require(MASS)
a=-0.23468
b=0.24311
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0114425809
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Autonomy Need Fulfillment')

# Test for Follow-Up Relatedness Fulfillment

require(MASS)
a=-0.23468
b=0.22918
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0112529664
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Relatedness Need Fulfillment')

# Test for Follow-Up College Self-Efficacy

require(MASS)
a=-0.23468
b=0.39199
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0223113969
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up College Self-Efficacy')

# Test for Follow-Up College Belonging

require(MASS)
a=-0.23468
b=0.794
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.10549504
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up College Belonging')

# Test for Follow-Up Well-Being

require(MASS)
a=-0.23468
b=0.16166
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0047720464
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Well-Being')

# Test for Follow-Up DIDS Commitment-Making

require(MASS)
a=-0.23468
b=0.06002
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0081920601
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up DIDS Commitment and Identification')

# Test for Follow-Up DIDS Exploration-in-Breadth

require(MASS)
a=-0.23468
b=-0.01043
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0056942116
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Exploration-in-Breadth')

# Test for Follow-Up DIDS Ruminative Exploration

require(MASS)
a=-0.23468
b=-0.020027
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.007974311401
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Ruminative Exploration')

# Test for Follow-Up DIDS Identification with Commitment

require(MASS)
a=-0.23468
b=0.035647
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.007405463025
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Identification with Commitment')

# Test for Follow-Up DIDS Exploration-in-Depth

require(MASS)
a=-0.23468
b=0.052806
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.003728934225
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Exploration-in-Depth')

# Test for Follow-Up EPSI Global Identity Development

require(MASS)
a=-0.23468
b=0.162433
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.005720804496
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='darkred',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Global Identity Development')

# Test for Follow-Up 

require(MASS)
a=-0.23468
b=-0.1294
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0070358544
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Depression')

# Test for Follow-Up (Generalized) Anxiety

require(MASS)
a=-0.23468
b=-0.088665
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.005796690496
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Anxiety')

# Test for Follow-Up Social Anxiety

require(MASS)
a=-0.23468
b=-0.236406
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.008132973489
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Social Anxiety')

# Test for Follow-Up Academic Distress

require(MASS)
a=-0.23468
b=-0.187183
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.009601648144
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Academic Distress')

# Test for Follow-Up Eating Concerns 

require(MASS)
a=-0.23468
b=-0.145972
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.011034452025
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Eating Concerns')

# Test for Follow-Up Hostility

require(MASS)
a=-0.23468
b=-0.122928
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.004070312401
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Hostility')

# Test for Follow-Up Substance Use

require(MASS)
a=-0.23468
b=-0.038279
rep=20000
conf=95
pest=c(a,b)
acov <- matrix(c(
    0.002330765284, 0,
    0, 0.0053173264
),2,2)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ab <- mcmc[,1]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ab,low)
UL=quantile(ab,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)

hist(ab,breaks='FD',col='forestgreen',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect for Follow-up Substance Use')
