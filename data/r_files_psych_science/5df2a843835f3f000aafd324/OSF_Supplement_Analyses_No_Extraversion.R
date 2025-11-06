######################################################################
### Supplemental Analysis (models with no extraversion)            ###
### Authors: Daniel E. Gustavson  /  Sandra Sanchez-Roige          ###
######################################################################
library(devtools)
require(GenomicSEM)
warnings()

##############################
####  CREATE MATRICES  #######
##############################
# traits <- c("Negative_Urgency_UPPS.sumstats.gz", "Positive_Urgency_UPPS.sumstats.gz", 
#             "SS_UPPS.sumstats.gz","Premeditation_UPPS.sumstats.gz", "23andMe_BISlog10_21495.sumstats.gz", 
#              "23andMe_Perseverance_quantnorm_UPPSP_22861.sumstats.gz")
# sample.prev <- c(NA, NA, NA, NA, NA, NA)
# population.prev <- c(NA, NA, NA, NA, NA, NA)
# ld <- ("eur_w_ld_chr/")
# wld <- ("eur_w_ld_chr/")
# trait.names <- c("UPPSP_NU", "UPPSP_PU", "UPPSP_SS", "UPPSP_Premeditation", "BIS_Total",
#                  "UPPSP_Perseverance")
# LDSCoutputB1 <- ldsc(traits, sample.prev, population.prev, ld, wld, trait.names)
# #save(LDSCoutputB1, file="SUPP_Aim1.RData")
load( file="SUPP_Aim1.RData")
cov2cor(LDSCoutputB1$S)
LDSCoutputB1$S

### Model 1 - 5 Factors
Model1 <- 'F1_PRE =~ NA*BIS_Total + BIS_Total + UPPSP_Premeditation + UPPSP_Perseverance
F3_EXT =~ 1*UPPSP_SS
F2_NU =~ 1*UPPSP_NU 
F6_PU =~ 1*UPPSP_PU
UPPSP_PU ~~ 0*UPPSP_PU
UPPSP_NU ~~ 0*UPPSP_NU
UPPSP_SS ~~ 0*UPPSP_SS
F1_PRE ~~ F2_NU + F3_EXT 
F2_NU ~~ F3_EXT 
F6_PU ~~ F1_PRE + F2_NU + F3_EXT
F1_PRE ~~ 1*F1_PRE
BIS_Total ~~ y*BIS_Total
y > 0.0
'

Model3c_output <- usermodel(LDSCoutputB1, model = Model3c)
Model3c_output

### Model 2 - Collapse Positive and Negative Urgency
Model2 <- 'F1_PRE =~ NA*BIS_Total + a*BIS_Total + a*UPPSP_Premeditation 
F2_URG =~ NA*UPPSP_NU + b*UPPSP_NU + b*UPPSP_PU
F3_EXT =~ 1*UPPSP_SS
UPPSP_SS ~~ 0*UPPSP_SS
F4_Persev =~ 1*UPPSP_Perseverance
UPPSP_Perseverance ~~ 0*UPPSP_Perseverance
F1_PRE ~~ F2_URG + F3_EXT + F4_Persev
F2_URG ~~ F3_EXT + F4_Persev
F3_EXT ~~ F4_Persev
F1_PRE ~~ 1*F1_PRE
F2_URG ~~ 1*F2_URG
'
Model2_output <- usermodel(LDSCoutputB1, model = Model2)
Model2_output

### Model 3 - Collapse Lack of Premeditation and Lack of Perseverence 
Model3 <- 'F1_PRE =~ NA*BIS_Total + a*BIS_Total + a*UPPSP_Premeditation 
F3_EXT =~ 1*UPPSP_SS
F4_Persev =~ 1*UPPSP_Perseverance
F2_NU =~ 1*UPPSP_NU 
F6_PU =~ 1*UPPSP_PU
UPPSP_Perseverance ~~ 0*UPPSP_Perseverance
UPPSP_PU ~~ 0*UPPSP_PU
UPPSP_NU ~~ 0*UPPSP_NU
UPPSP_SS ~~ 0*UPPSP_SS
F1_PRE ~~ F2_NU + F3_EXT + F4_Persev
F2_NU ~~ F3_EXT + F4_Persev
F3_EXT ~~ F4_Persev
F6_PU ~~ F1_PRE + F2_NU + F3_EXT + F4_Persev
F1_PRE ~~ 1*F1_PRE
'

Model3_output <- usermodel(LDSCoutputB1, model = Model3)
Model3_output

### Model 4 - Single Factor
model3d_output <- commonfactor(LDSCoutputB1)
model3d_output
