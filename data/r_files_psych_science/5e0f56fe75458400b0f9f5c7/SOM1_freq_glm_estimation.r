
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #or: setwd(getSrcDirectory()[1])

# Load needed libraries
library(Rcpp)
library(inline)
library(ggplot2)
library(psych)

#options(repr.matrix.max.cols=50, repr.matrix.max.rows=100) #this option allows to show more lines/rows in jupyter notebook

exp_vector <- c('images','money')
for (i in 1:length(exp_vector)){
  current_exp <- exp_vector[i]

included_ids <- read.csv(paste0('./data/included_ids_list_',current_exp,'.csv'))
print(paste0('Number of included subjects (experiment ', current_exp, ') is: ', nrow(included_ids)))

remfor_all <- read.csv(paste0('./data/remembered_forgotten_allIds_',current_exp,'.csv'))
remfor_pos <- subset(remfor_all,data_type=='positive')
remfor_neg <- subset(remfor_all,data_type=='negative')

remfor_pos_new <- c()
remfor_neg_new <- c()

for (index in 1:nrow(included_ids)){
    id <- included_ids$ID[index]
    print(id)
    remfor_pos_subj <- subset(remfor_pos, ID==id)
    remfor_pos_subj$ID <- index
    remfor_pos_new <- rbind(remfor_pos_new,remfor_pos_subj)
    
    remfor_neg_subj <- subset(remfor_neg, ID==id)
    remfor_neg_subj$ID <- index
    remfor_neg_new <- rbind(remfor_neg_new,remfor_neg_subj)
}

table(remfor_pos_new$ID)
table(remfor_neg_new$ID)

nsubj <- nrow(included_ids)
nsubj

outfile <- paste0('./results/results_memory_bias/frequentist/memory_bias_gains_frequentist_',current_exp,'.csv')

if(!file.exists(outfile)){
    
    df_pos <- data.frame(matrix(nrow=nsubj, ncol=14))
    colnames(df_pos) <- c('ID','NumberTrials','intercept','slope','CI_lower',
                  'CI_upper','CI_lower_intercept','CI_upper_intercept',
                  'intercept_corr','slope_corr','CI_lower_corr',
                  'CI_upper_corr','CI_lower_intercept_corr','CI_upper_intercept_corr')
    df_pos$ID <- 1:tail(nsubj, n=1)

    for (id in 1:nsubj){
      print(id)
      dec_reduced <- subset(remfor_pos_new,ID==id)

      df_pos$NumberTrials[id]=nrow(dec_reduced)
      glm_memory_bias <- glm(dec_reduced$decided_remembered ~ dec_reduced$z_remembered, family=binomial)

      CI=confint(glm_memory_bias)
      df_pos$CI_upper[id]= CI[2,2]
      df_pos$CI_lower[id]=CI[2,1]
      df_pos$CI_upper_intercept[id]=CI[1,1]
      df_pos$CI_lower_intercept[id]=CI[1,2]
      df_pos$intercept[id] <- glm_memory_bias$coefficients[1] # intercept
      df_pos$slope[id] <- glm_memory_bias$coefficients[2] # slope
        
      glm_corrected_mb <- glm(dec_reduced$decided_remembered ~ dec_reduced$corrected_z_value, family=binomial)
        
      CI_corr=confint(glm_corrected_mb)
      df_pos$CI_upper_corr[id]= CI_corr[2,2]
      df_pos$CI_lower_corr[id]=CI_corr[2,1]
      df_pos$CI_upper_intercept_corr[id]=CI_corr[1,1]
      df_pos$CI_lower_intercept_corr[id]=CI_corr[1,2]
      df_pos$intercept_corr[id] <- glm_corrected_mb$coefficients[1] # intercept
      df_pos$slope_corr[id] <- glm_corrected_mb$coefficients[2] # slope   
  }
    write.csv(df_pos,outfile,row.names=F)

}else {
    print('File already exists. Skipping estimation.')
}



outfile <- paste0('./results/results_memory_bias/frequentist/memory_bias_losses_frequentist_',current_exp,'.csv')

if(!file.exists(outfile)){
    df_neg <- data.frame(matrix(nrow=nsubj, ncol=14))
    colnames(df_neg) <- c('ID','NumberTrials','intercept','slope','CI_lower',
                  'CI_upper','CI_lower_intercept','CI_upper_intercept',
                  'intercept_corr','slope_corr','CI_lower_corr',
                  'CI_upper_corr','CI_lower_intercept_corr','CI_upper_intercept_corr')
    df_neg$ID <- 1:tail(nsubj, n=1)

    for (id in 1:nsubj){
      print(id)
      dec_reduced <- subset(remfor_neg_new,ID==id)

      df_neg$NumberTrials[id]=nrow(dec_reduced)
      glm_memory_bias <- glm(dec_reduced$decided_remembered ~ dec_reduced$z_remembered, family=binomial)

      CI=confint(glm_memory_bias)
      df_neg$CI_upper[id]= CI[2,2]
      df_neg$CI_lower[id]=CI[2,1]
      df_neg$CI_upper_intercept[id]=CI[1,1]
      df_neg$CI_lower_intercept[id]=CI[1,2]
      df_neg$intercept[id] <- glm_memory_bias$coefficients[1] # intercept
      df_neg$slope[id] <- glm_memory_bias$coefficients[2] # slope
        
      glm_corrected_mb <- glm(dec_reduced$decided_remembered ~ dec_reduced$corrected_z_value, family=binomial)
        
      CI_corr=confint(glm_corrected_mb)
      df_neg$CI_upper_corr[id]= CI_corr[2,2]
      df_neg$CI_lower_corr[id]=CI_corr[2,1]
      df_neg$CI_upper_intercept_corr[id]=CI_corr[1,1]
      df_neg$CI_lower_intercept_corr[id]=CI_corr[1,2]
      df_neg$intercept_corr[id] <- glm_corrected_mb$coefficients[1] # intercept
      df_neg$slope_corr[id] <- glm_corrected_mb$coefficients[2] # slope   
  }
    write.csv(df_neg,outfile,row.names=F)

}else {
    print('File already exists. Skipping estimation.')
}
}
