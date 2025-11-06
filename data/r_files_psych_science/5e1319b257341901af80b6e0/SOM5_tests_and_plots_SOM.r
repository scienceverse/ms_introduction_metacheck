
library(ggplot2)
library(psych) #describe function
library(bayestestR) #hdi function
library(plyr)
library(cowplot) #plot_grid function
library(lsr) #cohensD function (effect size)

#cexvar=1.5
darker_col <- 0.25
brighter_col <- 0.5

positive_col <- 'blue'#rgb(brighter_col,brighter_col,brighter_col)
negative_col <- 'red'#rgb(darker_col,darker_col,darker_col)

cols <- c("Gains"=positive_col,"Losses"=negative_col)
my_colors <- c('blue','red')#c(rgb(brighter_col,brighter_col,brighter_col,1),rgb(darker_col,darker_col,darker_col,1))
my_colors_half <- c(rgb(0,0,1,0.5),rgb(1,0,0,0.5))#c(rgb(brighter_col,brighter_col,brighter_col,0.5),rgb(darker_col,darker_col,darker_col,0.5))
grey_color <- rgb(0,0,0)

my_interval <- 0.5


my_fontsize_axes <- 35
my_fontsize_axes_sublabels <- my_fontsize_axes-1
my_fontsize_legend <- my_fontsize_axes_sublabels

size_dashed <- 1

mytheme <-   theme(
    panel.ontop = F, 
    axis.title.x = element_text(size=my_fontsize_axes, family = 'Helvetica',vjust=-2), 
    axis.title.y = element_text(size=my_fontsize_axes, family = 'Helvetica',vjust=4),
    axis.text.x = element_text(size=my_fontsize_axes_sublabels, family = 'Helvetica'),
    axis.text.y = element_text(size=my_fontsize_axes_sublabels, family = 'Helvetica'),
    legend.title = element_text(size = my_fontsize_legend, family = 'Helvetica'),
    legend.text = element_text(size = my_fontsize_legend, family = 'Helvetica'),
    plot.margin=unit(c(1,1,1,1),"cm"),
    # Remove panel border
  panel.border = element_blank(),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line
  axis.line = element_line(colour = "black"))

options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)

# setwd('..')
#base_dir <- getwd()
#base_dir <- '/Users/rweilbaecher/Documents/phD_Project2/'
#base_dir

mb_dir <- './results/results_memory_bias/bayesian'

#images data
memory_bias_gains_images <- read.csv(paste0(mb_dir,'/estimates_mb_pos_images.csv'))
memory_bias_losses_images <- read.csv(paste0(mb_dir,'/estimates_mb_neg_images.csv'))

mb_gains_images <- memory_bias_gains_images[-c(1:4),]
mb_losses_images <- memory_bias_losses_images[-c(1:4),]

intercept_lines_mb_images <- seq(1,nrow(mb_gains_images),2) #intercepts
intercept_lines_mb_images <- intercept_lines_mb_images[-length(intercept_lines_mb_images)]

slope_lines_mb_images <- seq(2,nrow(mb_gains_images),2) #slopes
slope_lines_mb_images <- slope_lines_mb_images[-length(slope_lines_mb_images)]

#money data
memory_bias_gains_money <- read.csv(paste0(mb_dir,'/estimates_mb_pos_money.csv'))
memory_bias_losses_money <- read.csv(paste0(mb_dir,'/estimates_mb_neg_money.csv'))

mb_gains_money <- memory_bias_gains_money[-c(1:4),]
mb_losses_money <- memory_bias_losses_money[-c(1:4),]

intercept_lines_mb_money <- seq(1,nrow(mb_gains_money),2)
intercept_lines_mb_money <- intercept_lines_mb_money[-length(intercept_lines_mb_money)]

slope_lines_mb_money <- seq(2,nrow(mb_gains_money),2) #slopes
slope_lines_mb_money <- slope_lines_mb_money[-length(slope_lines_mb_money)]

mean_intercept_money_gains <- mean(mb_gains_money[intercept_lines_mb_money,'mean'])
mean_intercept_images_gains <- mean(mb_gains_images[intercept_lines_mb_images,'mean'])

mean_slope_money_gains <- mean(mb_gains_money[slope_lines_mb_money,'mean'])
mean_slope_images_gains<- mean(mb_gains_images[slope_lines_mb_images,'mean'])

mean_intercept_money_losses <- mean(mb_losses_money[intercept_lines_mb_money,'mean'])
mean_intercept_images_losses <- mean(mb_losses_images[intercept_lines_mb_images,'mean'])

mean_slope_money_losses <- mean(mb_losses_money[slope_lines_mb_money,'mean'])
mean_slope_images_losses<- mean(mb_losses_images[slope_lines_mb_images,'mean'])

sb_dir <- './results/results_subjective_belief/bayesian'

#images data
subjective_belief_gains_images <- read.csv(paste0(sb_dir,'/estimates_sb_pos_images.csv'))
subjective_belief_losses_images <- read.csv(paste0(sb_dir,'/estimates_sb_neg_images.csv'))

rows_out <- c(1:4,nrow(subjective_belief_gains_images),nrow(subjective_belief_gains_images)-1)

sb_gains_images <- subjective_belief_gains_images[-rows_out,]
sb_losses_images <- subjective_belief_losses_images[-rows_out,]

intercept_lines_sb_images <- seq(1,nrow(sb_gains_images),2) #intercepts, odd
slope_lines_sb_images <- seq(2,nrow(sb_gains_images),2) #slopes, even

#money data
subjective_belief_gains_money <- read.csv(paste0(sb_dir,'/estimates_sb_pos_money.csv'))
subjective_belief_losses_money <- read.csv(paste0(sb_dir,'/estimates_sb_neg_money.csv'))

rows_out <- c(1:4,nrow(subjective_belief_gains_money),nrow(subjective_belief_gains_money)-1)

sb_gains_money <- subjective_belief_gains_money[-rows_out,]
sb_losses_money <- subjective_belief_losses_money[-rows_out,]

intercept_lines_sb_money <- seq(1,nrow(sb_gains_money),2) #intercepts, odd
slope_lines_sb_money <- seq(2,nrow(sb_gains_money),2) #slopes, even

sb_pos_fit_images <- readRDS(paste0(sb_dir,'/estimated_lm_positive_images.rds'))
sb_neg_fit_images <- readRDS(paste0(sb_dir,'/estimated_lm_negative_images.rds'))
posterior_pos_images_sb <- as.matrix(sb_pos_fit_images)
posterior_neg_images_sb <- as.matrix(sb_neg_fit_images)

sb_pos_fit_money <- readRDS(paste0(sb_dir,'/estimated_lm_positive_money.rds'))
sb_neg_fit_money <- readRDS(paste0(sb_dir,'/estimated_lm_negative_money.rds'))
posterior_pos_money_sb <- as.matrix(sb_pos_fit_money)
posterior_neg_money_sb <- as.matrix(sb_neg_fit_money)

samples_sb <- nrow(posterior_neg_money_sb)
samples_sb

slopes_neg_images <- posterior_neg_images_sb[((samples_sb/2)+1):(samples_sb),2]
slopes_pos_images <- posterior_pos_images_sb[((samples_sb/2)+1):(samples_sb),2]

slopes_neg_money <- posterior_neg_money_sb[((samples_sb/2)+1):(samples_sb),2]
slopes_pos_money <- posterior_pos_money_sb[((samples_sb/2)+1):(samples_sb),2]

mb_dir <- './results/results_memory_bias/bayesian'

mbsb_pos_fit_images <- readRDS(paste0(mb_dir,'/estimated_glm_pos_images_sb.rds'))
mbsb_neg_fit_images <- readRDS(paste0(mb_dir,'/estimated_glm_neg_images_sb.rds'))
posterior_pos_images_mbsb <- as.matrix(mbsb_pos_fit_images)
posterior_neg_images_mbsb <- as.matrix(mbsb_neg_fit_images)

mbsb_pos_fit_money <- readRDS(paste0(mb_dir,'/estimated_glm_pos_money_sb.rds'))
mbsb_neg_fit_money <- readRDS(paste0(mb_dir,'/estimated_glm_neg_money_sb.rds'))
posterior_pos_money_mbsb <- as.matrix(mbsb_pos_fit_money)
posterior_neg_money_mbsb <- as.matrix(mbsb_neg_fit_money)

samples_mbsb <- nrow(posterior_pos_images_mbsb)
samples_mbsb

intercept_neg_images_mbsb <- posterior_neg_images_mbsb[((samples_mbsb/2)+1):(samples_mbsb),'mu[1]']
intercept_pos_images_mbsb <- posterior_pos_images_mbsb[((samples_mbsb/2)+1):(samples_mbsb),'mu[1]']

intercept_neg_money_mbsb <- posterior_neg_money_mbsb[((samples_mbsb/2)+1):(samples_mbsb),'mu[1]']
intercept_pos_money_mbsb <- posterior_pos_money_mbsb[((samples_mbsb/2)+1):(samples_mbsb),'mu[1]']

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

print('Experiment images:')
round(describe(mb_gains_images[intercept_lines_mb_images,'mean']),digits=2)
round(describe(mb_losses_images[intercept_lines_mb_images,'mean']),digits=2)

print('Experiment money:')
round(describe(mb_gains_money[intercept_lines_mb_money,'mean']),digits=2)
round(describe(mb_losses_money[intercept_lines_mb_money,'mean']),digits=2)

t.test(mb_gains_money[intercept_lines_mb_money,'mean'],alternative='greater')
eff_size <- round(cohensD(mb_gains_money[intercept_lines_mb_money,'mean']),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_gains_money[intercept_lines_mb_money,'mean'],alternative='greater')
eff_size <- round(cohensD(mb_losses_money[intercept_lines_mb_money,'mean']),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_losses_images[intercept_lines_mb_images,'mean'],alternative='less')
eff_size <- round(cohensD(mb_losses_images[intercept_lines_mb_images,'mean']),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_losses_money[intercept_lines_mb_money,'mean'],alternative='less')
eff_size <- round(cohensD(mb_losses_money[intercept_lines_mb_money,'mean']),digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_gains_images[intercept_lines_mb_images,'mean'],mb_losses_images[intercept_lines_mb_images,'mean'], 
       alternative='greater',paired=T)
eff_size <- round(cohensD(mb_gains_images[intercept_lines_mb_images,'mean'],mb_losses_images[intercept_lines_mb_images,'mean']),
      digits=2)
print(paste0('Effect size is:',eff_size))

t.test(mb_gains_money[intercept_lines_mb_money,'mean'],mb_losses_money[intercept_lines_mb_money,'mean'], 
       alternative='greater',paired=T)
eff_size <- round(cohensD(mb_gains_money[intercept_lines_mb_money,'mean'],mb_losses_money[intercept_lines_mb_money,'mean']),
      digits=2)
print(paste0('Effect size is:',eff_size))

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

print('Positive Images MB_SB')
ci_pos_images_sb <- hdi(intercept_pos_images_mbsb,ci=.90)
round(ci_pos_images_sb,digits=2)
round(describe(intercept_pos_images_mbsb),digits=2)
print('Negative Images MB_SB')
ci_neg_images_sb <- hdi(intercept_neg_images_mbsb,ci=.90)
round(ci_neg_images_sb,digits=2)
round(describe(intercept_neg_images_mbsb),digits=2)

print('Positive Money MB_SB')
ci_pos_numbers_sb <- hdi(intercept_pos_money_mbsb,ci=.90)
round(ci_pos_numbers_sb,digits=2)
round(describe(intercept_pos_money_mbsb),digits=2)
print('Negative Money MB_SB')
ci_neg_numbers_sb <- hdi(intercept_neg_money_mbsb,ci=.90)
round(ci_neg_numbers_sb,digits=2)
round(describe(intercept_neg_money_mbsb),digits=2)

percentage_images_neg_sb <- mean(intercept_neg_images_mbsb < 0)
print(paste0('We can conclude that there is a ', percentage_images_neg_sb*100,
      '% chance that the mean for negative is smaller than 0 (images)'))

percentage_images_pos_sb <- mean(intercept_pos_images_mbsb > 0)
print(paste0('We can conclude that there is a ', percentage_images_pos_sb*100,
      '% chance that the mean for positive is greater than 0 (images)'))

percentage_images_sb <- mean(intercept_pos_images_mbsb-intercept_neg_images_mbsb > 0)
print(paste0('We can conclude that there is a ', percentage_images_sb*100,
      '% chance that the mean for positive is greater than the mean for negative (images)'))
print(paste0('This corresponds to an overlap of ',100-percentage_images_sb*100))

percentage_money_neg_sb <- mean(intercept_neg_money_mbsb < 0)
print(paste0('We can conclude that there is a ', percentage_money_neg_sb*100,
      '% chance that the mean for negative is smaller than 0 (money)'))

percentage_money_pos_sb <- mean(intercept_pos_money_mbsb > 0)
print(paste0('We can conclude that there is a ', percentage_money_pos_sb*100,
      '% chance that the mean for positive is greater than 0 (money)'))

percentage_money_sb <- mean(intercept_pos_money_mbsb-intercept_neg_money_mbsb > 0)
print(paste0('We can conclude that there is a ', percentage_money_sb*100,
      '% chance that the mean for positive is greater than the mean for negative (money)'))
print(paste0('This corresponds to an overlap of ',100-percentage_money_sb*100))

intercepts_images_sb <- c(intercept_pos_images_mbsb,intercept_neg_images_mbsb)
domain <- rep(c('gains','lossses'),each=length(intercept_pos_images_mbsb))
images_df_sb <- data.frame(intercepts_images_sb,domain)

mu_images_sb <- ddply(images_df_sb, "domain", summarise, grp.mean=mean(intercepts_images_sb))

mu_images_sb

describe(intercepts_images_sb)

intercepts_money_sb <- c(intercept_pos_money_mbsb,intercept_neg_money_mbsb)
domain <- rep(c('gains','lossses'),each=length(intercept_pos_money_mbsb))
money_df_sb <- data.frame(intercepts_money_sb,domain)

mu_money_sb <- ddply(money_df_sb, "domain", summarise, grp.mean=mean(intercepts_money_sb))

mu_money_sb

describe(intercepts_money_sb)

p_num_post_sb<-ggplot(money_df_sb, aes(x=intercepts_money_sb,color=domain,fill=domain)) + 
    xlim(c(-.4,.6)) +
ylim(c(0,7)) +
    #geom_histogram(aes(y=..density..),position="dodge",alpha=0.5,binwidth = 0.01,show.legend = FALSE)+
    geom_density(alpha=.2,show.legend = FALSE,trim=TRUE)+
    #geom_vline(data=mu_money_sb, aes(xintercept=grp.mean, color=domain),linetype="dashed",show.legend = FALSE)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_pos_numbers_sb$CI_low, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_pos_numbers_sb$CI_high, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_numbers_sb$CI_low, color=my_colors[2], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_numbers_sb$CI_high, color=my_colors[2], linetype=2,size=size_dashed) +
    scale_color_manual(values=my_colors)+
    scale_fill_manual(values=my_colors)+
    labs(x="Memory bias parameter value", y = "Density (Money)")+
    mytheme

p_images_post_sb<-ggplot(images_df_sb, aes(x=intercepts_images_sb,color=domain,fill=domain)) + 
    xlim(c(-0.65,0.65)) +
ylim(c(0,7)) +
    #geom_histogram(aes(y=..density..),position="dodge",alpha=0.5,binwidth = 0.01,show.legend = FALSE)+
    geom_density(alpha=.2,show.legend = TRUE,trim=TRUE)+
    #geom_vline(data=mu_images_sb, aes(xintercept=grp.mean, color=domain),linetype="dashed",show.legend = FALSE)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_pos_images_sb$CI_low, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_pos_images_sb$CI_high, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_images_sb$CI_low, color=my_colors[2], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_images_sb$CI_high, color=my_colors[2], linetype=2,size=size_dashed) +
    scale_color_manual(values=my_colors)+
    scale_fill_manual(values=my_colors)+
    labs(x="Memory bias parameter value", y = "Density (Images)")+
    mytheme+
    #theme(legend.position="top")
    theme(legend.title = element_blank())+#
    theme(legend.position = c(0.1, 0.8))

p_images_post_sb

p <- plot_grid(p_images_post_sb,p_num_post_sb,labels='auto',label_size = my_fontsize_axes*1.5,label_fontfamily = 'Helvetica',
  label_fontface = "bold")
save_plot("./figures/SOM_Fig3_memory_bias_corrected_sb.pdf", p, ncol = 2,nrow=1,base_height = 12,base_width = 12)

round(describe(sb_gains_money[slope_lines_sb_money,'mean']),digits=2)
round(describe(sb_losses_money[slope_lines_sb_money,'mean']),digits=2)

round(hdi(slopes_neg_images,ci=.90),digits=2)
round(hdi(slopes_pos_images,ci=.90),digits=2)
round(hdi(slopes_neg_money,ci=.90),digits=2)
round(hdi(slopes_pos_money,ci=.90),digits=2)

mean(slopes_neg_images < 0)
mean(slopes_pos_images > 0)
mean(slopes_pos_images - slopes_neg_images > 0)

mean(slopes_neg_money < 0)
mean(slopes_pos_money > 0)
mean(slopes_pos_money - slopes_neg_money > 0)

#plot subjective belief raw data

current_exp <- 'images'
subj_belief_all_images <- read.csv(paste0('./data/subjective_value_allIds_',current_exp,'.csv'))
subj_belief_pos_images <- subset(subj_belief_all_images,Mean_rating>=0)
subj_belief_neg_images <- subset(subj_belief_all_images,Mean_rating<=0)

current_exp <- 'money'
subj_belief_all_money <- read.csv(paste0('./data/subjective_value_allIds_',current_exp,'.csv'))
subj_belief_pos_money <- subset(subj_belief_all_money,Number>=0)
subj_belief_neg_money <- subset(subj_belief_all_money,Number<=0)

new_subj_belief_pos_images <- c()
for (subj in unique(subj_belief_all_images$participant)){
    subject_data <- subset(subj_belief_pos_images,participant==subj)
    mean_used<- mean(subject_data$Mean_rating)
    sd_used <- sd(subject_data$Mean_rating)
    subject_data$z_image <- (subject_data$Mean_rating-mean_used)/sd_used
    new_subj_belief_pos_images <- rbind(new_subj_belief_pos_images,subject_data)
}

new_subj_belief_neg_images <- c()
for (subj in unique(subj_belief_neg_images$participant)){
    subject_data <- subset(subj_belief_neg_images,participant==subj)
    mean_used<- mean(subject_data$Mean_rating)
    sd_used <- sd(subject_data$Mean_rating)
    subject_data$z_image <- (subject_data$Mean_rating-mean_used)/sd_used
    new_subj_belief_neg_images <- rbind(new_subj_belief_neg_images,subject_data)
}

new_subj_belief_pos_money <- c()
for (subj in unique(subj_belief_all_money$participant)){
    subject_data <- subset(subj_belief_pos_money,participant==subj)
    mean_used<- mean(subject_data$Number)
    sd_used <- sd(subject_data$Number)
    subject_data$z_number <- (subject_data$Number-mean_used)/sd_used
    new_subj_belief_pos_money <- rbind(new_subj_belief_pos_money,subject_data)
}

new_subj_belief_neg_money <- c()
for (subj in unique(subj_belief_neg_money$participant)){
    subject_data <- subset(subj_belief_neg_money,participant==subj)
    mean_used<- mean(subject_data$Number)
    sd_used <- sd(subject_data$Number)
    subject_data$z_number <- (subject_data$Number-mean_used)/sd_used
    new_subj_belief_neg_money <- rbind(new_subj_belief_neg_money,subject_data)
}

my_breaks <- c(-Inf,seq(-2, 2, my_interval),Inf)
last_label <- length(my_breaks)-1
last_label

prepare_plotting <- function(data,my_breaks, last_label,type='images'){
    if(type=='images'){
        data$cuts<- cut(data$z_image, my_breaks,labels=1:last_label)
        }else if (type=='money'){
        data$cuts<- cut(data$z_number, my_breaks,labels=1:last_label)
    }
    data$dummy <- 1

    means=aggregate(x = data$SubjectiveBelief,by=list(data$cuts),
                       FUN =mean,na.rm=T)
    sds=aggregate(x = data$SubjectiveBelief,by=list(data$cuts),
                       FUN =sd,na.rm=T)
    sums=aggregate(x=data$dummy,by=list(data$cuts),FUN = sum )
    
    plot_data <- means
    names(plot_data)[names(plot_data)=="x"] <- "means"
    if(type=='images'){
        plot_data$ah=seq(-2-my_interval, 2, my_interval)+(my_interval/2)
    }else if (type=='money'){
        plot_data$ah=seq(-2+(my_interval*0), 2-(my_interval*1), my_interval)+(my_interval/2)
    }
    
    errorp <- qnorm(0.975)*sds/sqrt(sums$x)
    plot_data$Lp=means$x-errorp$x
    plot_data$Up=means$x+errorp$x
    return (plot_data)
}

plot_data_images_pos <- prepare_plotting(data=new_subj_belief_pos_images,my_breaks=my_breaks, last_label=last_label)
plot_data_images_neg <- prepare_plotting(data=new_subj_belief_neg_images,my_breaks=my_breaks, last_label=last_label)

summary(new_subj_belief_pos_money)

plot_data_money_pos <- prepare_plotting(data=new_subj_belief_pos_money,my_breaks=my_breaks, last_label=last_label,
                                        type='money')
plot_data_money_neg <- prepare_plotting(data=new_subj_belief_neg_money,my_breaks=my_breaks, last_label=last_label,
                                        type='money')

p_num_raw <- ggplot(plot_data_money_pos, aes(ah, means)) +
  ylim(c(0.2,1)) + xlim(c(-2,2))+
  labs(x="Standardized value", y = "Money\nEstimated memory performance  ")+
  geom_line(aes(col='Gains'),size=0.5) +
  geom_errorbar(mapping=aes(x=plot_data_money_pos$ah,ymax = plot_data_money_pos$Up, 
                            ymin = plot_data_money_pos$Lp), 
                width=0.2, size=0.5, color=positive_col) +
  geom_point(aes(col='Gains'),size=2)+
  mytheme+ 
  theme(legend.position = "none")+
  geom_line(data = plot_data_money_neg, aes(x = ah, y = means,col='Losses'),size=0.5) +
  geom_errorbar(mapping=aes(x=plot_data_money_neg$ah,ymax = plot_data_money_neg$Up, 
                           ymin = plot_data_money_neg$Lp), width=0.2, size=0.5, color=negative_col) +
  geom_point(data = plot_data_money_neg, aes(x = ah, y = means,col='Losses'),size=2)+
  scale_colour_manual('images', values = cols)

p_ima_raw <- ggplot(plot_data_images_pos, aes(ah, means)) +
  ylim(c(0.2,1)) +
  labs(x="Standardized value", y = "Images\nEstimated memory performance  ")+
  geom_line(aes(col='Gains'),size=0.5) +
  geom_errorbar(mapping=aes(x=plot_data_images_pos$ah,ymax = plot_data_images_pos$Up, 
                           ymin = plot_data_images_pos$Lp), 
             width=0.2, size=0.5, color=positive_col) +
  geom_point(aes(col='Gains'),size=2)+
  mytheme+ 
    theme(legend.title = element_blank())+
  theme(legend.position = c(0.2, 0.9))+
  geom_line(data = plot_data_images_neg, aes(x = ah, y = means,col='Losses'),size=0.5) +
  geom_errorbar(mapping=aes(x=plot_data_images_neg$ah,ymax = plot_data_images_neg$Up, 
                           ymin = plot_data_images_neg$Lp), width=0.2, size=0.5, color=negative_col) +
  geom_point(data = plot_data_images_neg, aes(x = ah, y = means,col='Losses'),size=2)+
  scale_colour_manual('images', values = cols)

p <- plot_grid(p_ima_raw, p_num_raw,labels='auto',label_size = my_fontsize_axes*1.5,label_fontfamily = 'Helvetica',
  label_fontface = "bold")
p
save_plot("./figures/SOM_Fig2_subjective_belief.pdf", p, ncol = 2,nrow=1,base_height = 12,base_width = 12)

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


