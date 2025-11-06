# To run a section: ctrl + alt + t
# To run current line / selection: ctrl + enter (alt + enter, stays on current line)
# To run everything : Ctrl + Alt + R
# Comment selection: Ctrl + Shift + C
# ----
library(rstanarm)
library(rstan)
library(brms)
setwd("D:/Data/Gidi/CRT")
dat <- read.csv("CRT_finaldata.csv", header=TRUE)
set.seed(123)

# Prep data:
dat$Subject.ID <- paste(dat$Experiment.ID, dat$Subject.ID, sep="*")
dat$Question.ID <- factor(dat$Question.ID)
dat$ConditionT <- 1*(dat$Condition=="Testosterone")
dat$Group.ID <- paste(dat$Experiment.ID, dat$Condition, dat$Blind, dat$Stress, sep="*")
dat$CQ.ID <- paste(dat$Condition, dat$Question.ID, sep="*Q")
dat$EQ.ID <- paste(dat$Experiment.ID, dat$Question.ID, sep="*Q")
dat$GQ.ID <- paste(dat$Group.ID, dat$Question.ID, sep="*Q")
dat$NCQ.ID <- paste(paste0("Nave",dat$Experiment.ID==4), dat$CQ.ID, sep="*")
dat$Group.nTE <- paste(dat$Experiment.ID, dat$Blind, dat$Stress, sep="*")

nave.sel <- dat$Experiment.ID==4

dat$TrtExp <- 1*(dat$Treatment_Expectancy_dichot=="Testosterone")
dat$TrtExp[nave.sel] <- (dat$Treatment_Expectancy[nave.sel] - 1) / 4
dat$Time <- dat$CRT_Time_minnoon / 60

# aggregate data ----
var_list = c('Experiment.ID','Group.ID','Stress','Blind','ConditionT','Group.nTE')
aggreg_dat = aggregate.data.frame(dat$y,list(dat$Subject.ID),FUN=sum)
aggreg_dat$sum_y = factor(aggreg_dat$x, levels = 0:3, ordered = TRUE)
aggreg_dat[,var_list] = dat[1,var_list]
for(r in 1:nrow(aggreg_dat)){
  orig_dat = dat[dat$Subject.ID == aggreg_dat$Group.1[r],]
  aggreg_dat[r,var_list] = orig_dat[1,var_list]
}

aggreg_13 = aggreg_dat[aggreg_dat$Experiment.ID %in% c(1,2,3),]

aggreg_dat$Experiment.ID = as.factor(aggreg_dat$Experiment.ID)
aggreg_13$Experiment.ID = as.factor(aggreg_13$Experiment.ID)

#############
# Run regression (Hierarchichal: BRMS Package)----
hier_post <- brm(sum_y~ ConditionT +  (1|Stress) + (1|Blind) + (1|Experiment.ID) ,
                 cores=4, chains=4,  iter=5000,
                 data=aggreg_dat, family=cumulative("logit", threshold="flexible"))


hier_post_fixed <- brm(sum_y~ ConditionT +  Experiment.ID ,
                 cores=4, chains=4,  iter=5000,
                 data=aggreg_dat, family=cumulative("logit", threshold="flexible"))

hier_post <- brm(sum_y~ ConditionT  + (1|Group.nTE) + (1|Experiment.ID) ,
                 cores=4, chains=4,  iter=5000,
                 data=aggreg_dat, family=cumulative("logit", threshold="flexible"))

##########
sampling_results = hier_post_fixed


summary(sampling_results)
post_sum = posterior_summary(sampling_results, probs = c(0.025,0.25,0.5,0.75, 0.975))

r.names = row.names(post_sum)

for(i in 1:length(r.names)){
  cat(sprintf("%s : \t", r.names[i] ))
  for(j in 1:5){
    cat(sprintf("%.4f\t",post_sum[r.names[i],2+j]))
  }
  cat("\n")
}

#########
OL.sum = posterior_summary(hier_post, probs = c(0.025,0.25,0.5,0.75, 0.975))

OL.theta = c('b_ConditionT','b_Intercept[1]','b_Intercept[2]','b_Intercept[3]',
             'sd_Experiment.ID__Intercept', 'sd_Group.nTE__Intercept')

proper.names = c('Treatment effect','Threshold #1','Threshold #2','Threshold #3',
                 'Experiment', 'Group')

for(i in 1:length(OL.theta)){
  cat(sprintf("%s : \t", proper.names[i] ))
  for(j in 1:5){
    cat(sprintf("%.4f\t",OL.sum[OL.theta[i],2+j]))
  }
  cat("\n")
}
