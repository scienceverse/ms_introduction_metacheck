
library(data.table)
library(psych)
library(plyr)
library(lsr)
library(dfoptim)
library(xtable)
library(ggplot2)
library(rstan)
library(bayesplot)
library(cowplot)
library(bayestestR)
library(bayesboot)
library(grid)
library(devtools)

brighter_col <- 0.5
rgb(brighter_col,brighter_col,brighter_col)

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

size_dashed <- 1 #size of dashed lines

my_colors_half

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

mb_pos_fit_images <- readRDS(paste0(mb_dir,'/estimated_glm_pos_images.rds'))
mb_neg_fit_images <- readRDS(paste0(mb_dir,'/estimated_glm_neg_images.rds'))
posterior_pos_images <- as.matrix(mb_pos_fit_images)
posterior_neg_images <- as.matrix(mb_neg_fit_images)

mb_pos_fit_money <- readRDS(paste0(mb_dir,'/estimated_glm_pos_money.rds'))
mb_neg_fit_money <- readRDS(paste0(mb_dir,'/estimated_glm_neg_money.rds'))
posterior_pos_money <- as.matrix(mb_pos_fit_money)
posterior_neg_money <- as.matrix(mb_neg_fit_money)

gambles_dir <- './results/results_gambles/bayesian'

gambles_estimates_gains_images <- read.csv(paste0(gambles_dir,'/estimates_gains_images.csv'))
gambles_estimates_losses_images <- read.csv(paste0(gambles_dir,'/estimates_losses_images.csv'))

risk_gains_images <- gambles_estimates_gains_images[grep(paste('alpha', collapse = "|"), 
                                                         gambles_estimates_gains_images$X),]
risk_losses_images <- gambles_estimates_losses_images[grep(paste('alpha', collapse = "|"), 
                                                         gambles_estimates_losses_images$X),]

amb_gains_images <- gambles_estimates_gains_images[grep(paste('beta', collapse = "|"), 
                                                         gambles_estimates_gains_images$X),]
amb_losses_images <- gambles_estimates_losses_images[grep(paste('beta', collapse = "|"), 
                                                         gambles_estimates_losses_images$X),]

gambles_estimates_gains_money <- read.csv(paste0(gambles_dir,'/estimates_gains_money.csv'))
gambles_estimates_losses_money <- read.csv(paste0(gambles_dir,'/estimates_losses_money.csv'))

risk_gains_money <- gambles_estimates_gains_money[grep(paste('alpha', collapse = "|"), 
                                                         gambles_estimates_gains_money$X),]
risk_losses_money <- gambles_estimates_losses_money[grep(paste('alpha', collapse = "|"), 
                                                         gambles_estimates_losses_money$X),]

amb_gains_money <- gambles_estimates_gains_money[grep(paste('beta', collapse = "|"), 
                                                         gambles_estimates_gains_money$X),]
amb_losses_money <- gambles_estimates_losses_money[grep(paste('beta', collapse = "|"), 
                                                         gambles_estimates_losses_money$X),]

memory_bias_gains <- c(mb_gains_images[intercept_lines_mb_images,'mean'],mb_gains_money[intercept_lines_mb_money,'mean'])
memory_bias_losses <- c(mb_losses_images[intercept_lines_mb_images,'mean'],mb_losses_money[intercept_lines_mb_money,'mean'])
risk_gains <- c((risk_gains_images$mean)-1,(risk_gains_money$mean)-1) #ATTENTION HERE!!!
risk_losses <- c(1-(risk_losses_images$mean),1-(risk_losses_money$mean))#ATTENTION HERE!!!
amb_gains <- c(-(amb_gains_images$mean),-(amb_gains_money$mean))#ATTENTION HERE!!!
amb_losses <- c(amb_losses_images$mean,amb_losses_money$mean)
exp_vec <- c(rep('images',nrow(amb_losses_images)), rep('money',nrow(amb_losses_money)))

difference_df <- cbind(memory_bias_gains-memory_bias_losses, risk_gains-risk_losses, amb_gains-amb_losses,exp_vec)
colnames(difference_df) <- c('mb_diff','risk_diff','amb_diff','experiment')

nrow(amb_losses_images)
nrow(amb_losses_money)

current_exp <- 'money'
remfor_all_money <- read.csv(paste0('./data/remembered_forgotten_allIds_',current_exp,'.csv'))
remfor_pos_money <- subset(remfor_all_money,data_type=='positive')
remfor_neg_money <- subset(remfor_all_money,data_type=='negative')

current_exp <- 'images'
remfor_all_images <- read.csv(paste0('./data/remembered_forgotten_allIds_',current_exp,'.csv'))
remfor_pos_images <- subset(remfor_all_images,data_type=='positive')
remfor_neg_images <- subset(remfor_all_images,data_type=='negative')

my_breaks <- c(-Inf,seq(-2, 2, my_interval),Inf)
last_label <- length(my_breaks)-1
last_label

remfor_pos_images$dec_rem=factor(remfor_pos_images$decided_remembered,labels=c('dec_for','dec_rem'))
remfor_pos_images$cuts<- cut(remfor_pos_images$z_remembered, my_breaks,labels=1:last_label)
remfor_pos_images$cuts_corr<- cut(remfor_pos_images$corrected_z_value, my_breaks,labels=1:last_label)
remfor_pos_images$dummy <- 1

remfor_pos_images_normal <- subset(remfor_pos_images,cuts!=1 & cuts!=last_label)
remfor_pos_images_corr <- subset(remfor_pos_images,cuts_corr!=1 & cuts_corr!=last_label)

remfor_pos_money$dec_rem=factor(remfor_pos_money$decided_remembered,labels=c('dec_for','dec_rem'))
remfor_pos_money$cuts<- cut(remfor_pos_money$z_remembered, c(-Inf,seq(-2, 2, my_interval),Inf),labels=1:last_label)
remfor_pos_money$cuts_corr<- cut(remfor_pos_money$corrected_z_value, c(-Inf,seq(-2, 2, my_interval),Inf),labels=1:last_label)
remfor_pos_money$dummy <- 1

remfor_pos_money_normal <- subset(remfor_pos_money,cuts!=1 & cuts!=last_label)
remfor_pos_money_corr <- subset(remfor_pos_money,cuts_corr!=1 & cuts_corr!=last_label)

mean_positive_corr_images=data.frame(unclass(table(remfor_pos_images_corr$cuts_corr,remfor_pos_images_corr$dec_rem)))
mean_positive_corr_images$cuts <- 1:last_label
mean_positive_corr_images <- subset(mean_positive_corr_images,cuts!=1 & cuts!=last_label)
mean_positive_corr_images$prop_chosen=mean_positive_corr_images$dec_rem/(mean_positive_corr_images$dec_for+
                                                                         mean_positive_corr_images$dec_rem)
mean_positive_corr_images$ah=seq(-2+(my_interval*0), 2-(my_interval*1), my_interval)+(my_interval/2)

sdp_pos_corr=aggregate(x = remfor_pos_images_corr$decided_remembered,by=list(remfor_pos_images_corr$cuts_corr),
                       FUN =sd,na.rm=T)
sump_pos_corr=aggregate(x=remfor_pos_images_corr$dummy,by=list(remfor_pos_images_corr$cuts_corr),FUN = sum )

errorp_pos_corr <- qnorm(0.975)*sdp_pos_corr$x/sqrt(sump_pos_corr$x)
mean_positive_corr_images$Lp_pos_corr=mean_positive_corr_images$prop_chosen-errorp_pos_corr
mean_positive_corr_images$Up_pos_corr=mean_positive_corr_images$prop_chosen+errorp_pos_corr

mean_positive_corr_money=data.frame(unclass(table(remfor_pos_money_corr$cuts_corr,remfor_pos_money_corr$dec_rem)))
mean_positive_corr_money$cuts <- 1:last_label
mean_positive_corr_money <- subset(mean_positive_corr_money,cuts!=1 & cuts!=last_label)
mean_positive_corr_money$prop_chosen=mean_positive_corr_money$dec_rem/(mean_positive_corr_money$dec_for+
                                                                           mean_positive_corr_money$dec_rem)
mean_positive_corr_money$ah=seq(-2+(my_interval*0), 2-(my_interval*1), my_interval)+(my_interval/2)

sdp_pos_corr=aggregate(x = remfor_pos_money_corr$decided_remembered,by=list(remfor_pos_money_corr$cuts_corr),
                       FUN =sd,na.rm=T)
sump_pos_corr=aggregate(x=remfor_pos_money_corr$dummy,by=list(remfor_pos_money_corr$cuts_corr),FUN = sum )

errorp_pos_corr <- qnorm(0.975)*sdp_pos_corr$x/sqrt(sump_pos_corr$x)
mean_positive_corr_money$Lp_pos_corr=mean_positive_corr_money$prop_chosen-errorp_pos_corr
mean_positive_corr_money$Up_pos_corr=mean_positive_corr_money$prop_chosen+errorp_pos_corr

remfor_neg_images$dec_rem=factor(remfor_neg_images$decided_remembered,labels=c('dec_for','dec_rem'))
remfor_neg_images$cuts<- cut(remfor_neg_images$z_remembered, c(-Inf,seq(-2, 2, my_interval),Inf),labels=1:last_label)
remfor_neg_images$cuts_corr<- cut(remfor_neg_images$corrected_z_value, c(-Inf,seq(-2, 2, my_interval),Inf),labels=1:last_label)
remfor_neg_images$dummy <- 1
remfor_neg_images_normal <- subset(remfor_neg_images,cuts!=1 & cuts!=last_label)
remfor_neg_images_corr <- subset(remfor_neg_images,cuts_corr!=1 & cuts_corr!=last_label)

remfor_neg_money$dec_rem=factor(remfor_neg_money$decided_remembered,labels=c('dec_for','dec_rem'))
remfor_neg_money$cuts<- cut(remfor_neg_money$z_remembered, c(-Inf,seq(-2, 2, my_interval),Inf),labels=1:last_label)
remfor_neg_money$cuts_corr<- cut(remfor_neg_money$corrected_z_value, c(-Inf,seq(-2, 2, my_interval),Inf),labels=1:last_label)
remfor_neg_money$dummy <- 1
remfor_neg_money_normal <- subset(remfor_neg_money,cuts!=1 & cuts!=last_label)
remfor_neg_money_corr <- subset(remfor_neg_money,cuts_corr!=1 & cuts_corr!=last_label)

mean_negative_corr_images=data.frame(unclass(table(remfor_neg_images_corr$cuts_corr,remfor_neg_images_corr$dec_rem)))

mean_negative_corr_images$cuts <- 1:last_label
mean_negative_corr_images <- subset(mean_negative_corr_images,cuts!=1 & cuts!=last_label)
mean_negative_corr_images$prop_chosen=mean_negative_corr_images$dec_rem/(mean_negative_corr_images$dec_for+
                                                                         mean_negative_corr_images$dec_rem)
mean_negative_corr_images$ah=seq(-2+(my_interval*0), 2-(my_interval*1), my_interval)+(my_interval/2)

sdp_neg_corr=aggregate(x = remfor_neg_images_corr$decided_remembered,by=list(remfor_neg_images_corr$cuts_corr),
                       FUN =sd,na.rm=T)
sump_neg_corr=aggregate(x=remfor_neg_images_corr$dummy,by=list(remfor_neg_images_corr$cuts_corr),FUN = sum )

errorp_neg_corr <- qnorm(0.975)*sdp_neg_corr$x/sqrt(sump_neg_corr$x)
mean_negative_corr_images$Lp_neg_corr=mean_negative_corr_images$prop_chosen-errorp_neg_corr
mean_negative_corr_images$Up_neg_corr=mean_negative_corr_images$prop_chosen+errorp_neg_corr

mean_negative_corr_money=data.frame(unclass(table(remfor_neg_money_corr$cuts_corr,remfor_neg_money_corr$dec_rem)))

mean_negative_corr_money$cuts <- 1:last_label
mean_negative_corr_money <- subset(mean_negative_corr_money,cuts!=1 & cuts!=last_label)
mean_negative_corr_money$prop_chosen=mean_negative_corr_money$dec_rem/(mean_negative_corr_money$dec_for+mean_negative_corr_money$dec_rem)
mean_negative_corr_money$ah=seq(-2+(my_interval*0), 2-(my_interval*1), my_interval)+(my_interval/2)

sdp_neg_corr=aggregate(x = remfor_neg_money_corr$decided_remembered,by=list(remfor_neg_money_corr$cuts_corr),
                       FUN =sd,na.rm=T)
sump_neg_corr=aggregate(x=remfor_neg_money_corr$dummy,by=list(remfor_neg_money_corr$cuts_corr),FUN = sum )

errorp_neg_corr <- qnorm(0.975)*sdp_neg_corr$x/sqrt(sump_neg_corr$x)
mean_negative_corr_money$Lp_neg_corr=mean_negative_corr_money$prop_chosen-errorp_neg_corr
mean_negative_corr_money$Up_neg_corr=mean_negative_corr_money$prop_chosen+errorp_neg_corr

#bayes.cor.test(slopes_neg_numbers, posterior_neg_numbers)

total_nsamples <- nrow(posterior_neg_money)
nrow(posterior_neg_images)
total_nsamples

exclude_warmup <- ((total_nsamples/2)+1):(total_nsamples)
length(exclude_warmup)

head(posterior_neg_money)

intercept_neg_images <- posterior_neg_images[((total_nsamples/2)+1):(total_nsamples),'mu[1]']
intercept_pos_images <- posterior_pos_images[((total_nsamples/2)+1):(total_nsamples),'mu[1]']

slope_neg_images <- posterior_neg_images[((total_nsamples/2)+1):(total_nsamples),'mu[2]']
slope_pos_images <- posterior_pos_images[((total_nsamples/2)+1):(total_nsamples),'mu[2]']

intercepts_images <- c(intercept_pos_images,intercept_neg_images)
slopes_images <- c(slope_pos_images,slope_neg_images)
domain <- rep(c('gains','lossses'),each=length(intercept_pos_images))
images_df <- data.frame(intercepts_images,domain)

mu_images <- ddply(images_df, "domain", summarise, grp.mean=mean(intercepts_images))

intercept_neg_money <- posterior_neg_money[((total_nsamples/2)+1):(total_nsamples),'mu[1]']
intercept_pos_money <- posterior_pos_money[((total_nsamples/2)+1):(total_nsamples),'mu[1]']

slope_neg_money <- posterior_neg_money[((total_nsamples/2)+1):(total_nsamples),'mu[2]']
slope_pos_money <- posterior_pos_money[((total_nsamples/2)+1):(total_nsamples),'mu[2]']

intercepts_money <- c(intercept_pos_money,intercept_neg_money)
slopes_money <- c(slope_pos_money,slope_neg_money)
domain <- rep(c('gains','lossses'),each=length(intercept_pos_money))
money_df <- data.frame(intercepts_money,domain)

mu_money <- ddply(money_df, "domain", summarise, grp.mean=mean(intercepts_money))

print('Positive Images Slope')
round(hdi(slope_pos_images,ci=.90),digits=2)
round(describe(slope_pos_images),digits=2)
print('Negative Images Slope')
round(hdi(slope_neg_images,ci=.90),digits=2)
round(describe(slope_neg_images),digits=2)
print('Positive Money Slope')
round(hdi(slope_pos_money,ci=.90),digits=2)
round(describe(slope_pos_money),digits=2)
print('Negative Money Slope')
round(hdi(slope_neg_money,ci=.90),digits=2)
round(describe(slope_neg_money),digits=2)

print('Positive Images Intercept')
ci_pos_images <- hdi(intercept_pos_images,ci=.90)
round(ci_pos_images,digits=2)
round(describe(intercept_pos_images),digits=2)
print('Negative Images Intercept')
ci_neg_images <- hdi(intercept_neg_images,ci=.90)
round(ci_neg_images,digits=2)
round(describe(intercept_neg_images),digits=2)
print('Positive Money Intercept')
ci_pos_money <- hdi(intercept_pos_money,ci=.90)
round(ci_pos_money,digits=2)
round(describe(intercept_pos_money),digits=2)
print('Negative Money Intercept')
ci_neg_money <- hdi(intercept_neg_money,ci=.90)
round(ci_neg_money,digits=2)
round(describe(intercept_neg_money),digits=2)

percentage_images_neg <- mean(intercept_neg_images < 0)
print(paste0('We can conclude that there is a ', percentage_images_neg*100,
      '% chance that the mean for positive is smaller than 0 (images)'))

percentage_images_pos <- mean(intercept_pos_images > 0)
print(paste0('We can conclude that there is a ', percentage_images_pos*100,
      '% chance that the mean for positive is greater than 0 (images)'))

percentage_images <- mean(intercept_pos_images-intercept_neg_images > 0)
print(paste0('We can conclude that there is a ', percentage_images*100,
      '% chance that the mean for positive is greater than the mean for negative (images)'))

percentage_money_neg <- mean(intercept_neg_money < 0)
print(paste0('We can conclude that there is a ', percentage_money_neg*100,
      '% chance that the mean for positive is smaller than 0 (money)'))

percentage_money_pos <- mean(intercept_pos_money > 0)
print(paste0('We can conclude that there is a ', percentage_money_pos*100,
      '% chance that the mean for positive is greater than 0 (money)'))

percentage_money <- mean(intercept_pos_money-intercept_neg_money > 0)
print(paste0('We can conclude that there is a ', percentage_money*100,
      '% chance that the mean for positive is greater than the mean for negative (money)'))

p_num_post<-ggplot(money_df, aes(x=intercepts_money,color=domain,fill=domain)) + 
    xlim(c(-0.4,0.65)) +
    #geom_histogram(aes(y=..density..),position="dodge",alpha=0.5,binwidth = 0.01,show.legend = FALSE)+
    geom_density(alpha=.2,show.legend = FALSE,trim=TRUE)+
    #geom_vline(data=mu_money, aes(xintercept=grp.mean, color=domain),linetype="dashed",show.legend = FALSE,size=size_dashed)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_pos_money$CI_low, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_pos_money$CI_high, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_money$CI_low, color=my_colors[2], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_money$CI_high, color=my_colors[2], linetype=2,size=size_dashed) +
    scale_color_manual(values=my_colors)+
    scale_fill_manual(values=my_colors)+
    labs(x="Memory bias parameter value", y = "Density")+
    mytheme

p_images_post<-ggplot(images_df, aes(x=intercepts_images,color=domain,fill=domain)) + 
    xlim(c(-0.4,0.65)) +
    #geom_histogram(aes(y=..density..),position="dodge",alpha=0.5,binwidth = 0.01,show.legend = FALSE)+
    geom_density(alpha=.2,show.legend = FALSE,trim=TRUE)+
    #geom_vline(data=mu_images, aes(xintercept=grp.mean, color=domain),linetype="dashed",show.legend = FALSE,size=size_dashed)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_pos_images$CI_low, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_pos_images$CI_high, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_images$CI_low, color=my_colors[2], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_images$CI_high, color=my_colors[2], linetype=2,size=size_dashed) +
    scale_color_manual(values=my_colors)+
    scale_fill_manual(values=my_colors)+
    labs(x="Memory bias parameter value", y = "Density")+
    mytheme

p_num_raw <- ggplot(mean_positive_corr_money, aes(ah, prop_chosen)) +
  ylim(c(-0.02,1)) +
  labs(x="Standardized value remembered option", y = "Money\nP(choose remembered)  ")+
  geom_line(aes(col='Gains'),size=0.5) +
  geom_errorbar(mapping=aes(x=mean_positive_corr_money$ah,ymax = mean_positive_corr_money$Up_pos_corr, 
                            ymin = mean_positive_corr_money$Lp_pos_corr), 
                width=0.2, size=0.5, color=positive_col) +
  geom_point(aes(col='Gains'),size=2)+
  geom_vline(xintercept = 0, lty=2,color=grey_color,size=size_dashed) +
  geom_hline(yintercept = 0.5,lty=2,color=grey_color,size=size_dashed) +
  mytheme+ 
  theme(legend.position = "none")+
  geom_line(data = mean_negative_corr_money, aes(x = ah, y = prop_chosen,col='Losses'),size=0.5) +
  geom_errorbar(mapping=aes(x=mean_negative_corr_money$ah,ymax = mean_negative_corr_money$Up_neg_corr, 
                            ymin = mean_negative_corr_money$Lp_neg_corr), width=0.2, size=0.5, color=negative_col) +
  geom_point(data = mean_negative_corr_money, aes(x = ah, y = prop_chosen,col='Losses'),size=2)+
  scale_colour_manual('money', values = cols)

p_images_raw <- ggplot(mean_positive_corr_images, aes(ah, prop_chosen)) +
  ylim(c(-0.02,1)) +

  labs(x="Standardized value remembered option", y = "Images\nP(choose remembered)")+
  geom_line(aes(col='Gains'),size=0.5) + # 95% confidence interval
  geom_errorbar(mapping=aes(x=mean_positive_corr_images$ah,ymax = mean_positive_corr_images$Up_pos_corr, 
                            ymin = mean_positive_corr_images$Lp_pos_corr), width=0.2, size=0.5,color=positive_col) +
  geom_point(aes(col='Gains'),size=2)+
  geom_vline(xintercept = 0, lty=2,color=grey_color,size=size_dashed) +
  geom_hline(yintercept = 0.5,lty=2,color=grey_color,size=size_dashed) +
  mytheme +   
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.2, 0.8))+

  geom_line(data = mean_negative_corr_images, aes(x = ah, y = prop_chosen,col='Losses'),size=0.5) +
  geom_errorbar(mapping=aes(x=mean_negative_corr_images$ah,ymax = mean_negative_corr_images$Up_neg_corr, 
                            ymin = mean_negative_corr_images$Lp_neg_corr), width=0.2, size=0.5,color=negative_col) +
  geom_point(data = mean_negative_corr_images, aes(x = ah, y = prop_chosen,col='Losses'),size=2)+
  scale_colour_manual('images', values = cols)

#p <- plot_grid(p_num_raw, p_num_post,p_images_raw, p_images_post,labels = "AUTO")
p <- plot_grid(p_images_raw, p_images_post,p_num_raw, p_num_post,labels='auto',label_size = my_fontsize_axes*1.5,label_fontfamily = 'Helvetica',
  label_fontface = "bold")
p
save_plot("./figures/Fig2_memory_bias_raw_post_both.pdf", p, ncol = 2,nrow=2,base_height = 12,base_width = 12)

class(difference_df)
difference_df <- as.data.frame(difference_df)
difference_df$mb_diff <- as.numeric(as.character(difference_df$mb_diff))
difference_df$risk_diff <- as.numeric(as.character(difference_df$risk_diff))
difference_df$amb_diff <- as.numeric(as.character(difference_df$amb_diff))
head(difference_df)
summary(difference_df)

exp_colors <- c(rgb(0,150,0,max=255),rgb(255,140,0,max=255))

diff_risk_mb <- ggplot(difference_df, aes(x=risk_diff, y=mb_diff)) +
    geom_smooth(aes(color=experiment,fill=experiment),method="lm") + #with se
    geom_point(aes(color=experiment,fill=experiment),size=6,shape = 21, colour = "black") +
    xlim(-1.5,2)+
    xlab("Risk attitude \n(gains - losses)")+
    ylab("Memory bias on choice \n(gains - losses)") +
    mytheme +
    theme(legend.title = element_blank())+
    theme(legend.position = c(0.2, 0.8))+
    scale_color_manual(values = exp_colors) +
    scale_fill_manual(values = exp_colors)+
    geom_vline(xintercept=0, linetype=2, color='black',size=size_dashed)

diff_amb_mb <- ggplot(difference_df, aes(x=amb_diff, y=mb_diff)) +
    geom_smooth(aes(color=experiment,fill=experiment),method="lm") + #with se
    geom_point(aes(color=experiment,fill=experiment),size=6,shape = 21, colour = "black") +
    xlim(-1.5,2)+
    xlab("Ambiguity attitude \n(gains - losses)")+
    ylab("Memory bias on choice \n(gains - losses)") +
    mytheme +
    theme(legend.title = element_blank())+
    theme(legend.position = 'none')+
    scale_color_manual(values = exp_colors) +
    scale_fill_manual(values = exp_colors)+
    geom_vline(xintercept=0, linetype=2, color='black',size=size_dashed)

#p <- plot_grid(diff_risk_mb,diff_amb_mb, labels = "AUTO")
p <- plot_grid(diff_risk_mb,diff_amb_mb, labels = c('c','d'),label_size = my_fontsize_axes*1.5,label_fontfamily = 'Helvetica',
  label_fontface = "bold")
p
save_plot("./figures/Fig3_lottery_results_only.pdf", p, ncol = 2,nrow=1,base_height = 12,base_width=12)

my_fontsize_axes*1.5

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

cor.test(difference_df$mb_diff,difference_df$risk_diff)
cor.test(difference_df$mb_diff,difference_df$amb_diff)


