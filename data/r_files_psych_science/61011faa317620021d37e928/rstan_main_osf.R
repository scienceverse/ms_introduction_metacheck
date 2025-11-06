#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# rstan_main_osf.R                                                  #
# Author: Yang Hu                                                   #
# Date: Sept. 1, 2018                                               #
# Date of last change: Nov. 10, 2019                                #
# Project: Corruption with tDCS (Corr_tDCS)                         #
# PI: Dr.Yang Hu                                                    #
# Co-PI: Prof.Dr.Jean-Claude Dreher                                 #
# Main Collborators: Remi Philippe, Valentin Guigon, Sasa Zhao      # 
#                                                                   #
# This is a custom R function to run rstan estimation. It is        #
# cleaned and simplified based on the latest version of the code,   #
# and is uploaded to the Open Science Framework (OSF) to reproduce  #
# the main analyses.                                                #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# On the Weiming #1 Server, you need to do the following first:
# (Type in the following command in the terminal)
# 1. module load anaconda/3.7.1
# 2. source activate R_3.6
# 3. module load gcc/7.2.0
# 4. rstudio
rootdir <- "/gpfs/share/home/1906388343/CBCS_Projects/2019_Corruption_tDCS/"
nIter <- 2500
nwarmup <- 1500 # nwarmup <- floor(nIter/2)
models <- c("Corr_tDCS_MC2cond_m2","Corr_tDCS_MC2cond_m3",
            "Corr_tDCS_MC2cond_m4","Corr_tDCS_FSIA_m4")

tDCS_groups <- c("Anodal","Cathodal","Sham")
source(paste0(rootdir,"code/code_rstan/rstan_fit_osf.R"))

for (m in 1:length(models)) {
      model_tmp <- paste(models[m],"_hbm",sep = "")
  
      for (n in 1:length(tDCS_groups)) {
          fitname <- paste0(rootdir,"output/stanfits/",tDCS_groups[n],'_',models[m],"_hbm_",toString(nIter),".RData")
          if (!file.exists(fitname)) {
            # if(substr(models[m], nchar(models[m]), nchar(models[m])) == "z") {
            #   IsZscore <- 1
            # } else {
            #   IsZscore <- 0
            # }
            print(paste0(tDCS_groups[n], " Group: ",model_tmp, " is now being fitted ..."))
            rstan_fit(modelStr = model_tmp, groupStr = tDCS_groups[n], test = T, saveFit = T,
                      nIter = nIter, nwarmup = nwarmup,
                      # suffix = "_ub_5_2500",
                      adapt = 0.8, treedepth = 10, nThinn = 1)
          } else {
            print(paste0(model_tmp, " has been fitted ..."))
          }
      }

}



