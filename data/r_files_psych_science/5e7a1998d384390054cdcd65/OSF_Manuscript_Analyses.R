######################################################################
### Manuscript Analysis for                                        ###
### The Latent Genetic Structure of Impulsivity and its Relation   ###
### to Internalizing Psychopathology                               ###
### Authors: Daniel E. Gustavson  /  Sandra Sanchez-Roige          ###
######################################################################

### INSTALL PACKAGES ###
#install.packages("devtools")
library(devtools)
#install_github("MichelNivard/GenomicSEM")
require(GenomicSEM)
#warnings()


#####################################
#######         Aim 1       #########
####  All Impulsivity Facets  #######
#####################################
####       CREATE MATRICES      ##############################################################################
#### NOTE: some  summary statistics files are only available through collaboration with 23andMe
#### Code to create matrices for data analyses are provided here 
#### The matrices loaded in below are located on the OSF page
###############################################################################################################
# traits <- c("Negative_Urgency_UPPS.sumstats.gz", "Positive_Urgency_UPPS.sumstats.gz", 
#             "SS_UPPS.sumstats.gz","Premeditation_UPPS.sumstats.gz", "23andMe_BISlog10_21495.sumstats.gz", 
#             "GPC-2.EXTRAVERSION.full.sumstats.gz", "23andMe_Perseverance_quantnorm_UPPSP_22861.sumstats.gz")
# sample.prev <- c(NA, NA, NA, NA, NA, NA, NA)
# population.prev <- c(NA, NA, NA, NA, NA, NA, NA)
# ld <- ("eur_w_ld_chr/")
# wld <- ("eur_w_ld_chr/")
# trait.names <- c("UPPSP_NU", "UPPSP_PU", "UPPSP_SS", "UPPSP_Premeditation", "BIS_Total", "Extraversion",
#                  "UPPSP_Perseverance")
# LDSCoutputA1 <- ldsc(traits, sample.prev, population.prev, ld, wld, trait.names)
#save(LDSCoutputA1, file="Aim1_Matrices.RData")
load( file="Aim1_Matrices.RData")
cov2cor(LDSCoutputA1$S)
LDSCoutputA1$S


##### TEST MODELS ####
Model1 <- 'F1_PRE =~ NA*BIS_Total + a*BIS_Total + a*UPPSP_Premeditation 
F3_EXT =~ NA*UPPSP_SS + c*UPPSP_SS + c*Extraversion
F4_Persev =~ 1*UPPSP_Perseverance
F2_NU =~ 1*UPPSP_NU 
F6_PU =~ 1*UPPSP_PU
UPPSP_Perseverance ~~ 0*UPPSP_Perseverance
UPPSP_PU ~~ 0*UPPSP_PU
UPPSP_NU ~~ 0*UPPSP_NU
F1_PRE ~~ F2_NU + F3_EXT + F4_Persev
F2_NU ~~ F3_EXT + F4_Persev
F3_EXT ~~ F4_Persev
F6_PU ~~ F1_PRE + F2_NU + F3_EXT + F4_Persev
F1_PRE ~~ 1*F1_PRE
F3_EXT ~~ 1*F3_EXT
Extraversion ~~ x*Extraversion
x > 0.001
'

Model1_output <- usermodel(LDSCoutputA1, model = Model1)
Model1_output




### Model 2 - Collapse Positive and Negative Urgency
Model2 <- 'F1_PRE =~ NA*BIS_Total + a*BIS_Total + a*UPPSP_Premeditation 
F2_URG =~ NA*UPPSP_NU + b*UPPSP_NU + b*UPPSP_PU
F3_EXT =~ NA*UPPSP_SS + c*UPPSP_SS + c*Extraversion
F4_Persev =~ 1*UPPSP_Perseverance
UPPSP_Perseverance ~~ 0*UPPSP_Perseverance
F1_PRE ~~ F2_URG + F3_EXT + F4_Persev
F2_URG ~~ F3_EXT + F4_Persev
F3_EXT ~~ F4_Persev
F1_PRE ~~ 1*F1_PRE
F2_URG ~~ 1*F2_URG
F3_EXT ~~ 1*F3_EXT
Extraversion ~~ x*Extraversion
x > 0.001
'
Model2_output <- usermodel(LDSCoutputA1, model = Model2)
Model2_output


### Model 3 - Collapse Lack of Premeditation and Lack of Perseverance
Model3 <- 'F1_PRE =~ NA*BIS_Total + BIS_Total + UPPSP_Premeditation + UPPSP_Perseverance
F3_EXT =~ NA*UPPSP_SS + c*UPPSP_SS + c*Extraversion
F2_NU =~ 1*UPPSP_NU 
F6_PU =~ 1*UPPSP_PU
UPPSP_PU ~~ 0*UPPSP_PU
UPPSP_NU ~~ 0*UPPSP_NU
F1_PRE ~~ F2_NU + F3_EXT 
F2_NU ~~ F3_EXT 
F6_PU ~~ F1_PRE + F2_NU + F3_EXT
F1_PRE ~~ 1*F1_PRE
F3_EXT ~~ 1*F3_EXT
Extraversion ~~ x*Extraversion
BIS_Total ~~ y*BIS_Total
x > 0.001
y > 0.0
'

Model3_output <- usermodel(LDSCoutputA1, model = Model3)
Model3_output

#### Model 4 - Common Factor ###
model4_output <- commonfactor(LDSCoutputA1)
model4_output

#####################################
#######    Aim 2 and 3      #########
####  Add Delay Discounting   #######
####     and Internalizing    #######
#####################################
### Note: factor loadings and genetic correlations for Subjective Well-Being are negative in these matrices/analyses
### but are reported as positive in the main text. We describe this reverse scoring procedure in the Method

# traits <- c("Negative_Urgency_UPPS.sumstats.gz", "Positive_Urgency_UPPS.sumstats.gz", 
#             "SS_UPPS.sumstats.gz","Premeditation_UPPS.sumstats.gz", "23andMe_BISlog10_21495.sumstats.gz", 
#             "GPC-2.EXTRAVERSION.full.sumstats.gz", "23andMe_Perseverance_quantnorm_UPPSP_22861.sumstats.gz",
#             "23andMe_MCQ_k.23127.sumstats.gz","PGC_UKB_depression_genome-wide.sumstats.gz", "sumstats_neuroticism_ctg_format.sumstats.gz", "SWB_Full.sumstats.gz")
# sample.prev <- c(NA, NA, NA, NA, NA, NA, NA, NA, .3414, NA, NA)
# population.prev <- c(NA, NA, NA, NA, NA, NA, NA, .15, NA, NA, NA)
# ld <- ("eur_w_ld_chr/")
# wld <- ("eur_w_ld_chr/")
# trait.names <- c("UPPSP_NU", "UPPSP_PU", "UPPSP_SS", "UPPSP_Premeditation", "BIS_Total", "Extraversion",
#                  "UPPSP_Perseverance","Delay_Disc","MDD", "Neuroticism", "SWB")
# LDSCoutputA4 <- ldsc(traits, sample.prev, population.prev, ld, wld, trait.names)
# save(LDSCoutputA4, file="Aim2and3_Matrices.RData")
load( file="Aim2and3_Matrices.RData")
cov2cor(LDSCoutputA4$S)

# Supplemental Table 1 and Figure 2
# fullcorrmatrix <- cov2cor(LDSCoutputA4$S)
# write.csv(fullcorrmatrix, "fullcorrmatrix.csv")

# library(corrplot)
# rownames <- c("UPPS-P NU", "UPPS-P PU", "UPPS-P SS" , "Extraversion", "UPPS-P Premed", "BIS Total", "UPPS-P Persev", "Delay Discounting","MDD","Neuroticism", "SWB")
# colnames <- c("", "", "", "", "", "", "", "", "", "", "")
# Reordered correlation matrix outside of R to move extraversion closer to sensation seeking and reverse score subjective wellbeing
# Table1_reordered <- read.csv("fullcorrmatrix_reordered.csv",  header=T,row.names = 1)
# rownames(Table1_reordered) <- rownames
# colnames(Table1_reordered) <- colnames
# corrplot(as.matrix(Table1_reordered), type = "lower", order = "original", 
#          tl.col = "black", tl.srt = 45, cl.pos="r")


### Table 2 Model 
Table2 <- 'F1_PRE =~ NA*BIS_Total + a*BIS_Total + a*UPPSP_Premeditation
F2_URG =~ 1*UPPSP_NU
F3_EXT =~ NA*UPPSP_SS + c*UPPSP_SS + c*Extraversion
F4_Persev =~ 1*UPPSP_Perseverance
F5_Internal =~ NA*MDD + Neuroticism + SWB
F6_PU =~ 1*UPPSP_PU
F7_DD =~ 1*Delay_Disc
UPPSP_Perseverance ~~ 0*UPPSP_Perseverance
UPPSP_NU ~~ 0*UPPSP_NU
UPPSP_PU ~~ 0*UPPSP_PU
Delay_Disc ~~ 0*Delay_Disc
F1_PRE ~~ F2_URG + F3_EXT + F4_Persev
F2_URG ~~ F3_EXT + F4_Persev
F3_EXT ~~ F4_Persev
F5_Internal ~~ F1_PRE + F2_URG + F3_EXT + F4_Persev
F6_PU ~~ F5_Internal + F1_PRE + F2_URG + F3_EXT + F4_Persev
F7_DD ~~ F6_PU + F5_Internal + F1_PRE + F2_URG + F3_EXT + F4_Persev
F1_PRE ~~ 1*F1_PRE
F3_EXT ~~ 1*F3_EXT
F5_Internal ~~ 1*F5_Internal
Extraversion ~~ x*Extraversion
x > 0.001
'

Table2_output <- usermodel(LDSCoutputA4, model = Table2)
Table2_output
Table2_output$modelfit



Table2_collapseDD <- 'F1_PRE =~ NA*BIS_Total + a*BIS_Total + a*UPPSP_Premeditation+ Delay_Disc
F2_URG =~ 1*UPPSP_NU
F3_EXT =~ NA*UPPSP_SS + c*UPPSP_SS + c*Extraversion
F4_Persev =~ 1*UPPSP_Perseverance
F5_Internal =~ NA*MDD + Neuroticism + SWB
F6_PU =~ 1*UPPSP_PU
UPPSP_Perseverance ~~ 0*UPPSP_Perseverance
UPPSP_NU ~~ 0*UPPSP_NU
UPPSP_PU ~~ 0*UPPSP_PU
F1_PRE ~~ F2_URG + F3_EXT + F4_Persev
F2_URG ~~ F3_EXT + F4_Persev
F3_EXT ~~ F4_Persev
F5_Internal ~~ F1_PRE + F2_URG + F3_EXT + F4_Persev
F6_PU ~~ F5_Internal + F1_PRE + F2_URG + F3_EXT + F4_Persev
F1_PRE ~~ 1*F1_PRE
F3_EXT ~~ 1*F3_EXT
F5_Internal ~~ 1*F5_Internal
Extraversion ~~ x*Extraversion
x > 0.001
BIS_Total ~~ y*BIS_Total
y > .001
a<1.0
'

CollapseDD_output <- usermodel(LDSCoutputA4, model = Table2_collapseDD)
CollapseDD_output


