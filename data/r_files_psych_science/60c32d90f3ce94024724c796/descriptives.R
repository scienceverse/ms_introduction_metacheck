# Script 2: Descriptives ----------------------------------------------------------------------------------
# This script provides descriptive statistics about the data in order to explore them and ensure that no errors
# were committed in the cleaning process.

# The script performs these tasks:
# 1. Loading libraries and data
# 2. Summary statistics and visualisation
# 3. Assessing relationships between variables

input = ""
output = ""

# 1. Loading libraries and data --------------------------------------------

# 1a. Load libraries ----
library(summarytools)
library(tidyverse)
library(corrplot)
library(gdata)
library(patchwork)
library(xtable)

# 1b. Load data ----
full_data <- read.csv(file=paste(input,"/full_proj_data.csv",sep="")) 
full_data <- subset(full_data, select = -c(X,missing_all_adolescence,missing_threshold,outlier_status,inclusion))
proj_data <- read.csv(file=paste(input,"/proj_data.csv",sep="")) 
proj_data <- subset(proj_data, select = -X)
proj_data_imp <- read.csv(file=paste(input,"/proj_data_imp.csv",sep="")) 
proj_data_imp <- subset(proj_data_imp, select = -X)

# 1c. Load correlation plot function and adapt to spearman ---
# Code from Giacomo Bignardi, github.com/gia01/ccatools

# Confidence Interval Function Required for PlotCorrelationMatrix
.R_ConInt = function(x,y){
  # browser()
  if(base::is.na(stats::cor(x,y,  method = "spearman",use="pairwise.complete.obs"))){
    # return(list(CI="0", N=nrow(na.omit(cbind.data.frame(x,y))), CI_l="0"))
    return(base::list(CI="0", N="0", CI_l="0"))
    
  } else {
    CI = stats::cor.test(x,y)$conf.int[1:2]
    CI = base::gsub("^\\s","",gsub("0\\.", "\\.",base::format(CI, digits=0, nsmall=2, perl=FALSE)))
    N = base::as.numeric(cor.test(x,y)$parameter + 2)
    CI_l =     base::paste0( "[",CI[1],", ",CI[2],"]", sep="")
    out =    base::paste0(N,"\n[",CI[1],", ",CI[2],"]", sep="")
    return(list(CI=out, N=N, CI_l=CI_l))
  }
}

plotcor = function(dat,
                   Variables_Labels=NULL, textadjust=2, includeN=TRUE, reportCI=TRUE,
                   low_colour="#EDCB64", high_colour="#B62A3D", abs_colour=TRUE,
                   cluster_variables = FALSE
){
  if (!base::is.data.frame(dat)) {dat=base::as.data.frame(dat)}
  
  if (cluster_variables) {
    new_order = ccatools:::.sortVar(dat)
    dat = dat[,new_order]
    if (!base::is.null(Variables_Labels)){
      Variables_Labels = Variables_Labels[new_order]
    }
  }
  
  Variables = base::colnames(dat)
  if(is.null(Variables_Labels)){
    Variables_Labels = base::colnames(dat)
  }
  
  matrix_scores = dat
  Mat_Cor =gsub("^0","",gsub("^ +","",gsub("^-0","-", format(cor(matrix_scores, method = "spearman",use="pairwise.complete.obs"), digits=0, nsmall=2)))) #Correlation matrix
  if(abs_colour){
    Mat_Cor_fill = base::abs(stats::cor(matrix_scores,  method = "spearman",use="pairwise.complete.obs"))  #Correlation matrix for table fill
  } else {
    Mat_Cor_fill = (stats::cor(matrix_scores,  method = "spearman",use="pairwise.complete.obs"))  #Correlation matrix for table fill
  }
  
  Mat_Cor_fill[base::lower.tri(Mat_Cor_fill,diag = TRUE)]=NA
  
  #Matrix on Ns per comparison - lower triag
  Mat_N = sapply(Variables, function(x)
    sapply(Variables, function(y)
      base::nrow(stats::na.omit(base::data.frame(dat[,base::unique(c(x,y))])))
    ))
  
  #Confidence interval information
  if(reportCI){
    Mat_CI = sapply(Variables, function(x)
      sapply(Variables, function(y)
        ccatools:::.R_ConInt(as.numeric(dat[,x]),base::as.numeric(dat[,y]))$CI
      ))
    base::diag(Mat_CI) = base::diag(Mat_N)
    Mat_N = Mat_CI
  }
  
  #Create Dataframe For ggplot to Read
  PlotMat = Mat_Cor
  if(includeN){
    PlotMat[base::lower.tri(PlotMat, diag=TRUE)]=Mat_N[base::lower.tri(Mat_N, diag=TRUE)]
  }
  if(!includeN){
    PlotMat[base::lower.tri(PlotMat, diag=TRUE)]=""
  }
  base::colnames(PlotMat) = Variables_Labels ;  base::rownames(PlotMat) = Variables_Labels
  
  PlotMat = base::data.frame(reshape2::melt(PlotMat), stringsAsFactors = FALSE)
  head(PlotMat)
  
  PlotMat$value = (base::as.character(PlotMat$value))
  PlotMat$ValueFill = base::as.numeric(t(c(Mat_Cor_fill)))
  PlotMat$Var2 = base::factor(PlotMat$Var2, levels=rev(base::levels(PlotMat$Var2)))
  
  
  OutPlot =
    ggplot2::ggplot(data = PlotMat, ggplot2::aes(x=Var1, y=Var2,fill=ValueFill))+# + geom_point(aes(size=value^2,alpha=value^4))+
    ggplot2::geom_tile() + ggplot2::labs(x=NULL, y=NULL) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=5*textadjust)) +
    ggplot2::geom_text(ggplot2::aes(label=value), size=1.4*textadjust) +
    jtools::theme_apa() +
    #scale_fill_brewer(palette=1,na.value="grey")+
    ggplot2::scale_fill_continuous(na.value="gray94",low=low_colour, high=high_colour) +
    ggplot2::theme(legend.position = "#DAECED") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,hjust=1)) +
    ggplot2::coord_fixed()
  
  OutPlot
  
  return(OutPlot)
}

# 2. Summary statistics and variable visualisation ---------------------------

# 2a. Get descriptive statistics ----
# Full dataset
dfSummary(full_data[1:round(length(full_data)/2)])
dfSummary(full_data[(round(length(full_data)/2)+1):length(full_data)])

# Project data before imputation
dfSummary(proj_data[1:round(length(proj_data)/2)])
dfSummary(proj_data[(round(length(proj_data)/2)+1):length(proj_data)])

# Project data after imputation
dfSummary(proj_data_imp[1:round(length(proj_data_imp)/2)])
dfSummary(proj_data_imp[(round(length(proj_data_imp)/2)+1):length(proj_data_imp)])


# 2b. Visualise distributions of network variables ----
# Emotional domestic violence
p_dom_viol_emo.1 <- ggplot(proj_data_imp, aes(dom_viol_emo.1)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_emo.2 <- ggplot(proj_data_imp, aes(dom_viol_emo.2)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_emo.3 <- ggplot(proj_data_imp, aes(dom_viol_emo.3)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_emo.4 <- ggplot(proj_data_imp, aes(dom_viol_emo.4)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_emo.5 <- ggplot(proj_data_imp, aes(dom_viol_emo.5)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_dom_viol_emo.1 + p_dom_viol_emo.2 + p_dom_viol_emo.3 + p_dom_viol_emo.4 + p_dom_viol_emo.5

# Physical domestic violence
p_dom_viol_phys.1 <- ggplot(proj_data_imp, aes(dom_viol_phys.1)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_phys.2 <- ggplot(proj_data_imp, aes(dom_viol_phys.2)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_phys.3 <- ggplot(proj_data_imp, aes(dom_viol_phys.3)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_phys.4 <- ggplot(proj_data_imp, aes(dom_viol_phys.4)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_phys.5 <- ggplot(proj_data_imp, aes(dom_viol_phys.5)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_dom_viol_phys.1 + p_dom_viol_phys.2 + p_dom_viol_phys.3 + p_dom_viol_phys.4 + p_dom_viol_phys.5

# Physical cruelty
p_phys_cruel.1 <- ggplot(proj_data_imp, aes(phys_cruel.1)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_cruel.2 <- ggplot(proj_data_imp, aes(phys_cruel.2)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_cruel.3 <- ggplot(proj_data_imp, aes(phys_cruel.3)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_cruel.4 <- ggplot(proj_data_imp, aes(phys_cruel.4)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_cruel.5 <- ggplot(proj_data_imp, aes(phys_cruel.5)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_phys_cruel.1 + p_phys_cruel.2 + p_phys_cruel.3 + p_phys_cruel.4 + p_phys_cruel.5

# Emotional cruelty
p_emo_cruel.1 <- ggplot(proj_data_imp, aes(emo_cruel.1)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_emo_cruel.2 <- ggplot(proj_data_imp, aes(emo_cruel.2)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_emo_cruel.3 <- ggplot(proj_data_imp, aes(emo_cruel.3)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_emo_cruel.4 <- ggplot(proj_data_imp, aes(emo_cruel.4)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_emo_cruel.5 <- ggplot(proj_data_imp, aes(emo_cruel.5)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_emo_cruel.1 + p_emo_cruel.2 + p_emo_cruel.3 + p_emo_cruel.4 + p_emo_cruel.5

# Physical abuse
p_phys_abuse.1 <- ggplot(proj_data_imp, aes(phys_abuse.1)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_abuse.2 <- ggplot(proj_data_imp, aes(phys_abuse.2)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_abuse.3 <- ggplot(proj_data_imp, aes(phys_abuse.3)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_abuse.4 <- ggplot(proj_data_imp, aes(phys_abuse.4)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_abuse.5 <- ggplot(proj_data_imp, aes(phys_abuse.5)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_phys_abuse.1 + p_phys_abuse.2 + p_phys_abuse.3 + p_phys_abuse.4 + p_phys_abuse.5

# Sexual abuse
p_sex_abuse.1 <- ggplot(proj_data_imp, aes(sex_abuse.1)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_sex_abuse.2 <- ggplot(proj_data_imp, aes(sex_abuse.2)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_sex_abuse.3 <- ggplot(proj_data_imp, aes(sex_abuse.3)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_sex_abuse.4 <- ggplot(proj_data_imp, aes(sex_abuse.4)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_sex_abuse.5 <- ggplot(proj_data_imp, aes(sex_abuse.5)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_sex_abuse.1 + p_sex_abuse.2 + p_sex_abuse.3 + p_sex_abuse.4 + p_sex_abuse.5

# Change of caregiver
p_care_change.1 <- ggplot(proj_data_imp, aes(care_change.1)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_care_change.2 <- ggplot(proj_data_imp, aes(care_change.2)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_care_change.3 <- ggplot(proj_data_imp, aes(care_change.3)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_care_change.4 <- ggplot(proj_data_imp, aes(care_change.4)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_care_change.5 <- ggplot(proj_data_imp, aes(care_change.5)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_care_change.1 + p_care_change.2 + p_care_change.3 + p_care_change.4 + p_care_change.5

# Separation from parent
p_separate.1 <- ggplot(proj_data_imp, aes(separate.1)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_separate.2 <- ggplot(proj_data_imp, aes(separate.2)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_separate.3 <- ggplot(proj_data_imp, aes(separate.3)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_separate.4 <- ggplot(proj_data_imp, aes(separate.4)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_separate.5 <- ggplot(proj_data_imp, aes(separate.5)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_separate.1 + p_separate.2 + p_separate.3 + p_separate.4 + p_separate.5

# Financial difficulties score
par(mfrow=c(1,1))
boxplot(proj_data_imp[,c("fin_diff.1","fin_diff.2","fin_diff.3","fin_diff.4","fin_diff.5")],
        ylab="financial difficulties score",
        xlab="timepoint")

# Maternal and paternal caregiver neglect scores
par(mfrow = c(1,2))
boxplot(proj_data_imp[,c("interact_mum.1","interact_mum.2","interact_mum.3","interact_mum.4","interact_mum.5")],ylab="neglect score",xlab="timepoint")
boxplot(proj_data_imp[,c("interact_dad.1","interact_dad.2","interact_dad.3","interact_dad.4","interact_dad.5")],ylab="neglect score",xlab="timepoint")

# Overall adversity
p_dom_viol_emo_cu <- ggplot(proj_data_imp, aes(dom_viol_emo_cu)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_dom_viol_phys_cu <- ggplot(proj_data_imp, aes(dom_viol_phys_cu)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_cruel_cu <- ggplot(proj_data_imp, aes(phys_cruel_cu)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_emo_cruel_cu <- ggplot(proj_data_imp, aes(emo_cruel_cu)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_phys_abuse_cu <- ggplot(proj_data_imp, aes(phys_abuse_cu)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_sex_abuse_cu <- ggplot(proj_data_imp, aes(sex_abuse_cu)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_care_change_cu <- ggplot(proj_data_imp, aes(care_change_cu)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")
p_separate_cu <- ggplot(proj_data_imp, aes(separate_cu)) + geom_bar(aes(y=(..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent)+ylab("relative frequencies")+ guides(fill="none")

p_dom_viol_emo_cu + p_dom_viol_phys_cu + p_phys_cruel_cu + p_emo_cruel_cu + p_phys_abuse_cu + p_sex_abuse_cu + p_care_change_cu + p_separate_cu

# Mental health and cognition variables
par(mfrow = c(1,4))
boxplot(proj_data_imp$intern,ylab="score",xlab="internalising behaviours")
boxplot(proj_data_imp$extern,ylab="score",xlab="externalising behaviours")
boxplot(proj_data_imp$wasi,ylab="score",xlab="wasi")
boxplot(proj_data_imp$stopsig,ylab="total correct trials",xlab="stop signal task")

# 3. Assess relationships between variables   ---------------------------

# 3a. Correlations across timepoints ----
# Domestic violence variables
corr_dom_viol <- cor(proj_data_imp[,c("dom_viol_phys.1","dom_viol_phys.2","dom_viol_phys.3","dom_viol_phys.4","dom_viol_phys.5",
                                      "dom_viol_emo.1","dom_viol_emo.2","dom_viol_emo.3","dom_viol_emo.4","dom_viol_emo.5")])
corrplot(corr_dom_viol)

# Physical cruelty variables
corr_phys_cruel <- cor(proj_data_imp[,c("phys_cruel_dad.1","phys_cruel_dad.2","phys_cruel_dad.3","phys_cruel_dad.4","phys_cruel_dad.5",
                                        "phys_cruel_mum.1","phys_cruel_mum.2","phys_cruel_mum.3","phys_cruel_mum.4","phys_cruel_mum.5")])
corrplot(corr_phys_cruel)

# Emotional cruelty variables
corr_emo_cruel <- cor(proj_data_imp[,c("emo_cruel_dad.1","emo_cruel_dad.2","emo_cruel_dad.3","emo_cruel_dad.4","emo_cruel_dad.5",
                                       "emo_cruel_mum.1","emo_cruel_mum.2","emo_cruel_mum.3","emo_cruel_mum.4","emo_cruel_mum.5")])
corrplot(corr_emo_cruel)

# Abuse variables
corr_abuse <- cor(proj_data_imp[,c("phys_abuse.1","phys_abuse.2","phys_abuse.3","phys_abuse.4","phys_abuse.5",
                                   "sex_abuse.1","sex_abuse.2","sex_abuse.3","sex_abuse.4","sex_abuse.5")])
corrplot(corr_abuse)

# Change of caregiver and separation
corr_care <- cor(proj_data_imp[,c("care_change.1","care_change.2","care_change.3","care_change.4","care_change.5",
                                  "separate_mum.1","separate_mum.2","separate_mum.3","separate_mum.4","separate_mum.5",
                                  "separate_dad.1","separate_dad.2","separate_dad.3","separate_dad.4","separate_dad.5")])
corrplot(corr_care)

# Financial difficulties
corr_findiff <- cor(proj_data_imp[,c("fin_diff_food.1","fin_diff_food.2","fin_diff_food.3","fin_diff_food.4","fin_diff_food.5",
                                     "fin_diff_clothes.1","fin_diff_clothes.2","fin_diff_clothes.3","fin_diff_clothes.4","fin_diff_clothes.5",
                                     "fin_diff_heat.1","fin_diff_heat.2","fin_diff_heat.3","fin_diff_heat.4","fin_diff_heat.5",
                                     "fin_diff_rent.1","fin_diff_rent.2","fin_diff_rent.3","fin_diff_rent.4","fin_diff_rent.5",
                                     "fin_diff_items.1","fin_diff_items.2","fin_diff_items.3","fin_diff_items.4","fin_diff_items.5")], method = "spearman")
corrplot(corr_findiff)

# Maternal caregiver neglect
corr_interact_mum <- cor(proj_data_imp[,c("interact_mum.1a","interact_mum.1b","interact_mum.1c","interact_mum.1d","interact_mum.1e","interact_mum.1f","interact_mum.1g",
                                          "interact_mum.2a","interact_mum.2b","interact_mum.2c","interact_mum.2d","interact_mum.2e","interact_mum.2f","interact_mum.2g","interact_mum.2h","interact_mum.2i",
                                          "interact_mum.3a","interact_mum.3b","interact_mum.3c","interact_mum.3d","interact_mum.3e","interact_mum.3f","interact_mum.3g","interact_mum.3h","interact_mum.3i",
                                          "interact_mum.4a","interact_mum.4b","interact_mum.4c","interact_mum.4d","interact_mum.4e","interact_mum.4f","interact_mum.4g","interact_mum.4h","interact_mum.4i","interact_mum.4j","interact_mum.4k","interact_mum.4l",
                                          "interact_mum.5a","interact_mum.5b","interact_mum.5c","interact_mum.5d","interact_mum.5e","interact_mum.5f","interact_mum.5g","interact_mum.5h","interact_mum.5i","interact_mum.5j","interact_mum.5k","interact_mum.5l","interact_mum.5m","interact_mum.5n","interact_mum.5o","interact_mum.5p","interact_mum.5q","interact_mum.5r")])
corrplot(corr_interact_mum)

# Paternal caregiver neglect
corr_interact_dad <- cor(proj_data_imp[,c("interact_dad.1a","interact_dad.1b","interact_dad.1c","interact_dad.1d","interact_dad.1e","interact_dad.1f","interact_dad.1g","interact_dad.1h","interact_dad.1i",
                                          "interact_dad.2a","interact_dad.2b","interact_dad.2c","interact_dad.2d","interact_dad.2e","interact_dad.2f","interact_dad.2g","interact_dad.2h","interact_dad.2i",
                                          "interact_dad.3a","interact_dad.3b","interact_dad.3c","interact_dad.3d","interact_dad.3e","interact_dad.3f","interact_dad.3g","interact_dad.3h","interact_dad.3i",
                                          "interact_dad.4a","interact_dad.4b","interact_dad.4c","interact_dad.4d","interact_dad.4e","interact_dad.4f","interact_dad.4g","interact_dad.4h","interact_dad.4i","interact_dad.4j","interact_dad.4k","interact_dad.4l",
                                          "interact_dad.5a","interact_dad.5b","interact_dad.5c","interact_dad.5d","interact_dad.5e","interact_dad.5f","interact_dad.5g","interact_dad.5h","interact_dad.5i","interact_dad.5j","interact_dad.5k","interact_dad.5l","interact_dad.5m","interact_dad.5n","interact_dad.5o","interact_dad.5p","interact_dad.5q","interact_dad.5r")])
corrplot(corr_interact_dad)


# 3b. Overall correlations ----
networkvars <- c("extern","intern","stopsig","wasi","dom_viol_emo_cu","dom_viol_phys_cu","phys_cruel_cu",
                    "emo_cruel_cu","phys_abuse_cu","sex_abuse_cu","care_change_cu",
                    "separate_cu","fin_diff_cu","interact_mum_cu","interact_dad_cu")
networkvars_names <- c("externalising", "internalising","stop signal","wasi",
                      "emotional domestic violence","physical domestic violence",
                      "physical cruelty","emotional cruelty","physical abuse",
                      "sexual abuse","caregiver change", "parental separation",
                      "financial difficulties","maternal neglect",
                      "paternal neglect")

plotcor(full_data[,networkvars], reportCI = FALSE, Variables_Labels = networkvars_names)
plotcor(proj_data[,networkvars], reportCI = FALSE, Variables_Labels = networkvars_names)
plotcor(proj_data_imp[,networkvars], reportCI = FALSE, Variables_Labels = networkvars_names)


# 3b. Reliability of scales ----
# Reliability of maternal caregiver neglect score by wave
alpha_int_m <- list()
alpha_int_m[[1]] <- psych::alpha(proj_data_imp[,c("interact_mum.1a","interact_mum.1b","interact_mum.1c","interact_mum.1d","interact_mum.1e","interact_mum.1f","interact_mum.1g")]) # Wave 1
alpha_int_m[[2]] <- psych::alpha(proj_data_imp[,c("interact_mum.2a","interact_mum.2b","interact_mum.2c","interact_mum.2d","interact_mum.2e","interact_mum.2f","interact_mum.2g","interact_mum.2h","interact_mum.2i")]) # Wave 2
alpha_int_m[[3]] <- psych::alpha(proj_data_imp[,c("interact_mum.3a","interact_mum.3b","interact_mum.3c","interact_mum.3d","interact_mum.3e","interact_mum.3f","interact_mum.3g","interact_mum.3h","interact_mum.3i")]) # Wave 3
alpha_int_m[[4]] <- psych::alpha(proj_data_imp[,c("interact_mum.4a","interact_mum.4b","interact_mum.4c","interact_mum.4d","interact_mum.4e","interact_mum.4f","interact_mum.4g","interact_mum.4h","interact_mum.4i","interact_mum.4j","interact_mum.4k","interact_mum.4l")]) # Wave 4
alpha_int_m[[5]] <- psych::alpha(proj_data_imp[,c("interact_mum.5a","interact_mum.5b","interact_mum.5c","interact_mum.5d","interact_mum.5e","interact_mum.5f","interact_mum.5g","interact_mum.5h","interact_mum.5i","interact_mum.5j","interact_mum.5k","interact_mum.5l","interact_mum.5m","interact_mum.5n","interact_mum.5o","interact_mum.5p","interact_mum.5q","interact_mum.5r")]) # Wave 5

# Reliability of paternal caregiver neglect score by wave
alpha_int_d <- list()
alpha_int_d[[1]] <- psych::alpha(proj_data_imp[,c("interact_dad.1a","interact_dad.1b","interact_dad.1c","interact_dad.1d","interact_dad.1e","interact_dad.1f","interact_dad.1g","interact_dad.1h","interact_dad.1i")]) # Wave 1
alpha_int_d[[2]] <- psych::alpha(proj_data_imp[,c("interact_dad.2a","interact_dad.2b","interact_dad.2c","interact_dad.2d","interact_dad.2e","interact_dad.2f","interact_dad.2g","interact_dad.2h","interact_dad.2i")]) # Wave 2
alpha_int_d[[3]] <- psych::alpha(proj_data_imp[,c("interact_dad.3a","interact_dad.3b","interact_dad.3c","interact_dad.3d","interact_dad.3e","interact_dad.3f","interact_dad.3g","interact_dad.3h","interact_dad.3i")]) # Wave 3
alpha_int_d[[4]] <- psych::alpha(proj_data_imp[,c("interact_dad.4a","interact_dad.4b","interact_dad.4c","interact_dad.4d","interact_dad.4e","interact_dad.4f","interact_dad.4g","interact_dad.4h","interact_dad.4i","interact_dad.4j","interact_dad.4k","interact_dad.4l")]) # Wave 4
alpha_int_d[[5]] <- psych::alpha(proj_data_imp[,c("interact_dad.5a","interact_dad.5b","interact_dad.5c","interact_dad.5d","interact_dad.5e","interact_dad.5f","interact_dad.5g","interact_dad.5h","interact_dad.5i","interact_dad.5j","interact_dad.5k","interact_dad.5l","interact_dad.5m","interact_dad.5n","interact_dad.5o","interact_dad.5p","interact_dad.5q","interact_dad.5r")]) # Wave 5

# Reliability of financial difficulties score by wave
alpha_fin <- list()
alpha_fin[[1]] <- psych::alpha(proj_data_imp[,c("fin_diff_food.1","fin_diff_clothes.1","fin_diff_heat.1","fin_diff_rent.1","fin_diff_items.1")]) # Wave 1
alpha_fin[[2]] <- psych::alpha(proj_data_imp[,c("fin_diff_food.2","fin_diff_clothes.2","fin_diff_heat.2","fin_diff_rent.2","fin_diff_items.2")]) # Wave 2
alpha_fin[[3]] <- psych::alpha(proj_data_imp[,c("fin_diff_food.3","fin_diff_clothes.3","fin_diff_heat.3","fin_diff_rent.3","fin_diff_items.3")]) # Wave 3
alpha_fin[[4]] <- psych::alpha(proj_data_imp[,c("fin_diff_food.4","fin_diff_clothes.4","fin_diff_heat.4","fin_diff_rent.4","fin_diff_items.4")]) # Wave 4
alpha_fin[[5]] <- psych::alpha(proj_data_imp[,c("fin_diff_food.5","fin_diff_clothes.5","fin_diff_heat.5","fin_diff_rent.5","fin_diff_items.5")]) # Wave 5


