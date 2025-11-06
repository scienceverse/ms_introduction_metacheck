
library(ggplot2)
library(psych) #describe function
library(bayestestR) #hdi function
library(plyr)
library(lsr) #cohensD function (effect size)

#setwd('/Users/rweilbaecher/Documents/phD_Project2/data_analysis/data_analysis_public')

options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)

mb_dir <- './results/results_memory_bias/bayesian_pymc3'

#images data
summary_memory_bias_images <- read.csv(paste0(mb_dir,'/summary_estimates_images.csv'))
summary_memory_bias_money <- read.csv(paste0(mb_dir,'/summary_estimates_money.csv'))


participant_intercepts_images <- summary_memory_bias_images[grep("condition_participant_intercept", 
                                                                    summary_memory_bias_images$var_names), ]
participant_intercepts_images$domain <- rep(c('losses','gains'),each=36)

participant_intercepts_money <- summary_memory_bias_money[grep("condition_participant_intercept", 
                                                                    summary_memory_bias_images$var_names), ]
participant_intercepts_money$domain <- rep(c('losses','gains'),each=36)

mb_gains_images <- subset(participant_intercepts_images,domain=='gains')
mb_losses_images <- subset(participant_intercepts_images,domain=='losses')
mb_gains_money <- subset(participant_intercepts_money,domain=='gains')
mb_losses_money <- subset(participant_intercepts_money,domain=='losses')

mb_losses_money

mb_freq_results <- './results/results_memory_bias/frequentist/'

mb_estimates_gains_images_freq <- read.csv(paste0(mb_freq_results,'memory_bias_gains_frequentist_images.csv'))
mb_estimates_losses_images_freq <- read.csv(paste0(mb_freq_results,'memory_bias_losses_frequentist_images.csv'))

mb_estimates_gains_money_freq <- read.csv(paste0(mb_freq_results,'memory_bias_gains_frequentist_money.csv'))
mb_estimates_losses_money_freq <- read.csv(paste0(mb_freq_results,'memory_bias_losses_frequentist_money.csv'))

sb_freq_results <- './results/results_subjective_belief/frequentist/'

sb_estimates_gains_images <- read.csv(paste0(sb_freq_results,'belief_bias_gains_frequentist_images.csv'))
sb_estimates_losses_images <- read.csv(paste0(sb_freq_results,'belief_bias_losses_frequentist_images.csv'))

sb_estimates_gains_money <- read.csv(paste0(sb_freq_results,'belief_bias_gains_frequentist_money.csv'))
sb_estimates_losses_money <- read.csv(paste0(sb_freq_results,'belief_bias_losses_frequentist_money.csv'))

gambles_freq_results <- './results/results_gambles/frequentist/'

gambles_estimates_gains_images <- read.csv(paste0(gambles_freq_results,'gambles_frequentist_gains_images.csv'))
gambles_estimates_losses_images <- read.csv(paste0(gambles_freq_results,'gambles_frequentist_losses_images.csv'))

gambles_estimates_gains_money <- read.csv(paste0(gambles_freq_results,'gambles_frequentist_gains_money.csv'))
gambles_estimates_losses_money <- read.csv(paste0(gambles_freq_results,'gambles_frequentist_losses_money.csv'))


risk_gains_images <- gambles_estimates_gains_images[grep(paste('alpha', collapse = "|"), 
                                                         gambles_estimates_gains_images$X),]
risk_losses_images <- gambles_estimates_losses_images[grep(paste('alpha', collapse = "|"), 
                                                         gambles_estimates_losses_images$X),]

amb_gains_images <- gambles_estimates_gains_images[grep(paste('beta', collapse = "|"), 
                                                         gambles_estimates_gains_images$X),]
amb_losses_images <- gambles_estimates_losses_images[grep(paste('beta', collapse = "|"), 
                                                         gambles_estimates_losses_images$X),]

# perform all the t-tests

#two-step approach
print('Experiment images:')
round(describe(mb_gains_images[,'mean']),digits=2)
round(describe(mb_losses_images[,'mean']),digits=2)

print('Experiment money:')
round(describe(mb_gains_money[,'mean']),digits=2)
round(describe(mb_losses_money[,'mean']),digits=2)

t.test(mb_gains_images[,'mean'],alternative='greater')
eff_size <- round(cohensD(mb_gains_images[,'mean']),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_gains_money[,'mean'],alternative='greater')
eff_size <- round(cohensD(mb_gains_money[,'mean']),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_losses_images[,'mean'],alternative='less')
eff_size <- round(cohensD(mb_losses_images[,'mean']),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_losses_money[,'mean'],alternative='less')
eff_size <- round(cohensD(mb_losses_money[,'mean']),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_gains_images[,'mean'],mb_losses_images[,'mean'], 
       alternative='greater',paired=T)
eff_size <- round(cohensD(mb_gains_images[,'mean'],mb_losses_images[,'mean']),
      digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_gains_money[,'mean'],mb_losses_money[,'mean'], 
       alternative='greater',paired=T)
eff_size <- round(cohensD(mb_gains_money[,'mean'],mb_losses_money[,'mean']),
      digits=2)
print(paste0('Effect size is:',eff_size))


# purely frequentist approach
t.test(mb_estimates_gains_images_freq$intercept_corr,alternative='greater')
eff_size <- round(cohensD(mb_estimates_gains_images_freq$intercept_corr),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_estimates_gains_money_freq$intercept_corr,alternative='greater')
eff_size <- round(cohensD(mb_estimates_gains_money_freq$intercept_corr),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_estimates_losses_images_freq$intercept_corr,alternative='less')
eff_size <- round(cohensD(mb_estimates_losses_images_freq$intercept_corr),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_estimates_losses_money_freq$intercept_corr,alternative='less')
eff_size <- round(cohensD(mb_estimates_losses_money_freq$intercept_corr),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_estimates_gains_images_freq$intercept_corr,mb_estimates_losses_images_freq$intercept_corr,alternative='greater',paired=T)
eff_size <- round(cohensD(mb_estimates_gains_images_freq$intercept_corr,mb_estimates_losses_images_freq$intercept_corr),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_estimates_gains_money_freq$intercept_corr,mb_estimates_losses_money_freq$intercept_corr,alternative='greater',paired=T)
eff_size <- round(cohensD(mb_estimates_gains_money_freq$intercept_corr,mb_estimates_losses_money_freq$intercept_corr),digits=2)
print(paste0('Effect size is:',eff_size))



# perform the purely frequenstist linear model and correlation anaylsis
head(mb_estimates_gains_images_freq)

memory_bias_gains <- c(mb_estimates_gains_images_freq$intercept_corr,mb_estimates_gains_money_freq$intercept_corr)
memory_bias_losses <- c(mb_estimates_losses_images_freq$intercept_corr,mb_estimates_losses_money_freq$intercept_corr)
risk_gains <- c((gambles_estimates_gains_images$alpha)-1,(gambles_estimates_gains_money$alpha)-1) #ATTENTION HERE!!!
risk_losses <- c(1-(gambles_estimates_losses_images$alpha),1-(gambles_estimates_losses_money$alpha))#ATTENTION HERE!!!
amb_gains <- c(-(gambles_estimates_gains_images$beta),-(gambles_estimates_gains_money$beta))#ATTENTION HERE!!!
amb_losses <- c(gambles_estimates_losses_images$beta,gambles_estimates_losses_money$beta)
exp_vec <- c(rep('images',nrow(mb_estimates_gains_images_freq)), rep('money',nrow(mb_estimates_losses_images_freq)))

difference_df <- cbind(memory_bias_gains-memory_bias_losses, risk_gains-risk_losses, amb_gains-amb_losses,exp_vec)
colnames(difference_df) <- c('mb_diff','risk_diff','amb_diff','experiment')

nrow(mb_estimates_gains_images_freq)
nrow(mb_estimates_losses_images_freq)

class(difference_df)
difference_df <- as.data.frame(difference_df)
difference_df$mb_diff <- as.numeric(as.character(difference_df$mb_diff))
difference_df$risk_diff <- as.numeric(as.character(difference_df$risk_diff))
difference_df$amb_diff <- as.numeric(as.character(difference_df$amb_diff))
head(difference_df)
summary(difference_df)

#prepare variables for linear regression
y <- memory_bias_gains - memory_bias_losses
x1 <- exp_vec
x2 <- risk_gains - risk_losses
x3 <- amb_gains - amb_losses

mydata <- as.data.frame(cbind(y,x1,x2,x3))
mydata$y <- as.numeric(as.character(mydata$y))
mydata$x2 <- as.numeric(as.character(mydata$x2))
mydata$x3 <- as.numeric(as.character(mydata$x3))

# Multiple Linear Regression 
fit <- lm(y ~ x1 + x2 + x3, data=mydata)
summary(fit) # show results

summary(mydata)

images_df <- subset(difference_df,experiment=='images')
money_df <- subset(difference_df,experiment=='money')

print('Risk correlations:')
cor.test(images_df$mb_diff,images_df$risk_diff)
cor.test(money_df$mb_diff,money_df$risk_diff)
print('Ambiguity correlations:')
cor.test(images_df$mb_diff,images_df$amb_diff)
cor.test(money_df$mb_diff,money_df$amb_diff)

print('Combined correlations (both experiments):')
cor.test(difference_df$mb_diff,difference_df$risk_diff)
cor.test(difference_df$mb_diff,difference_df$amb_diff)





