## This script demonstrates how to implement a custom family to reproduce the hierarchical Bayesian analyses in
# Lindsay, Gambi, & Rabagliati - Preschoolers optimize the timing of their conversational turns through flexible coordination of language comprehension and production
# Psychological Science, 2018
# using a version of brms >= 2.2.0

# Specifically, the code below was tested with versions 2.4.0 and 2.5.0

library(brms) # check which version of brms you have installed.

# Generate some exgaussian data for illustrative purposes (this requires the brms package)
a = data.frame(a =rexgaussian(10000,0,1,2))

# Define a custom family for the reparameterized exgaussian model
# In the current version of BRMS (2.5.0) the main predicted variable
# is the mean of the distribution, which is mu+beta.
# We prefer to use the more standard practice in the response time
# modeling literature, which is to separately report mu, sigma and beta.
# This custom family, when combined with `standard_exg_funs` below, allows
# us to do that. The name of the family is `exp_mod_normal2` which maps to
# the probability density function that Stan will use to do sampling, and
# which is defined in `standard_exg_funs` below. The remaining arguments are
# fairly explanatory, note that sigma and beta are obviously defined to have 
# lower bounds of 0.
three_param_exg <- custom_family(
  "exp_mod_normal2", dpars = c("mu", "sigma","beta"),
  links = c("identity", "log","log"), lb = c(NA, 0, 0),
  type = "real"
)


# `standard_exg_funs` defines the probability density function for exp_mod_normal2. This is
# very similar to the parameterisation in brms, except that there, the sampling
# statement is exp_mod_normal_lpdf(Y | mu - beta, sigma, inv(beta)).
# Note that the only difference is in the definition of mu. The current brms parameterisation
# defines mu as the mean of the exGaussian, so that the mean parameter of the Gaussian component (which is the first argument following | in the statement for
# the probability density function) is equal to mu - beta. Instead, we directly define mu as the mean paramter of the Gaussian component. 

# This function will be added to the functions block of the Stan code that
# BRMS eventually generates; first it is turned into a variable via the function
# stanvar (which prepares user-defined functions to be added to the Stan programme
# block), and then it is added to the Stan code via the stanvars argument 
# of the brm call.

three_param_exg_funs <- "
  real exp_mod_normal2_lpdf(real Y, real mu, real sigma, real beta) {
    return exp_mod_normal_lpdf(Y | mu, sigma, inv(beta));
  }
"
# `stanvars` prepared `standard_exg_funs` to be added to the stan code
three_param_exg_stanvars <- stanvar(scode = three_param_exg_funs, block = "functions")

# These two lines of code illustrate the difference between our custom model
# and the traditional BRMS model. Note the family argument, and the stanvars
# argument for the custom `three_param_exg_stancode` model.
three_param_exg_stancode = make_stancode(a ~ 1 , data = a, family = three_param_exg, stanvars = three_param_exg_stanvars)
print(three_param_exg_stancode)
standard_brms_exg_stancode = make_stancode(a ~ 1 , data = a, family = "exgaussian")
print(standard_brms_exg_stancode)


#### This demonstrates that we can reproduce our original analyses using the new custom family. We do so for the main model (reported in Table 1)
#### comparing adult to children. If you wish to reproduce other analyses, simply adapt the model calls in BRMS_LMER.r and BRMS_LMER_AgeCOnt_oldparAug18.R
#### using the code below as an example.

# For this part, you'll need to have loaded the data from turn taking study already (see BRMS_LMER.r and run the code from line 29 to line 110)

# Define the formula for the brms model that will be fit
expt2.formula.adult <- brmsformula(rt ~ 1  +Age1C  * Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject) , 
                                   beta ~ 1  +Age1C  * Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject) , 
                                   sigma ~ 1  +Age1C * Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject))

# Fit the model using the custom family.
expt2.model_adults_v_kids <- brm(expt2.formula.adult, 
                                 data = tt.expt2, 
                                 family = three_param_exg, 
                                 stanvars = three_param_exg_stanvars ,
                                 chains = 4, 
                                 inits="0")
print(summary(expt2.model_adults_v_kids),digits=3)

# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Eff.Sample  Rhat
# Intercept                                0.477     0.007    0.464    0.490        564 1.005
# sigma_Intercept                         -3.077     0.043   -3.164   -2.995       2724 1.000
# beta_Intercept                          -1.672     0.048   -1.768   -1.582        727 1.002
# Age1C                                    0.028     0.012    0.004    0.054        455 1.003
# Early.Latelate                           0.069     0.004    0.060    0.077       2507 1.000
# PredUnpred                               0.027     0.013    0.003    0.052        460 1.014
# scale_CharacterLength.ms                -0.023     0.001   -0.026   -0.020       4000 0.999
# Right.Wrongwrong                         0.016     0.002    0.012    0.021       4000 1.000
# Age1C:Early.Latelate                    -0.063     0.009   -0.080   -0.047       2432 1.000
# Age1C:PredUnpred                        -0.010     0.026   -0.061    0.042        504 1.005
# Early.Latelate:PredUnpred               -0.026     0.009   -0.042   -0.009       2407 1.002
# Age1C:Early.Latelate:PredUnpred          0.005     0.018   -0.030    0.040       2387 1.000
# sigma_Age1C                             -0.225     0.083   -0.382   -0.061       2097 1.001
# sigma_Early.Latelate                     0.299     0.056    0.191    0.414       4000 1.000
# sigma_PredUnpred                         0.104     0.084   -0.062    0.269       2299 1.003
# sigma_scale_CharacterLength.ms           0.106     0.023    0.062    0.152       4000 1.000
# sigma_Right.Wrongwrong                  -0.016     0.048   -0.109    0.079       4000 1.000
# sigma_Age1C:Early.Latelate              -0.193     0.114   -0.415    0.033       4000 1.000
# sigma_Age1C:PredUnpred                  -0.411     0.161   -0.723   -0.100       2599 1.000
# sigma_Early.Latelate:PredUnpred          0.033     0.111   -0.184    0.256       4000 1.001
# sigma_Age1C:Early.Latelate:PredUnpred   -0.364     0.223   -0.800    0.077       4000 0.999
# beta_Age1C                              -1.189     0.100   -1.388   -0.991        505 1.007
# beta_Early.Latelate                      0.149     0.037    0.077    0.222       4000 1.001
# beta_PredUnpred                          0.254     0.096    0.060    0.438        767 1.006
# beta_scale_CharacterLength.ms           -0.097     0.014   -0.124   -0.070       4000 1.000
# beta_Right.Wrongwrong                    0.048     0.027   -0.004    0.101       4000 1.000
# beta_Age1C:Early.Latelate                0.001     0.075   -0.149    0.148       3185 1.000
# beta_Age1C:PredUnpred                    0.015     0.186   -0.348    0.379        817 1.003
# beta_Early.Latelate:PredUnpred           0.081     0.073   -0.062    0.229       4000 1.000
# beta_Age1C:Early.Latelate:PredUnpred    -0.147     0.146   -0.432    0.139       3267 1.001

save(expt2.model_adults_v_kids, file = "brms_custom_family_Oct18.RData")