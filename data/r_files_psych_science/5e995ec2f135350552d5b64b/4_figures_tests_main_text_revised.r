# # Plot Figures 2 and 3 manuscript "Memory as uncertainty"
## also prints out the 90% HDIs as text files for the memory bias, the linear regression and the correlations
## as well as the Bayes Factors for the correlations
# ### revised code after joint estimation gains and losses (with difference parameter)

# read in the needed libraries
library(ggplot2)
library(psych)
library(bayestestR)
library(cowplot)
library(grid)


#define ci level
ci_level=0.90

# prepare the plotting settings
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
size_lines <- 2 #size of main lines in subplots a and c (raw data)

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

mb_dir <- './results/results_memory_bias/bayesian_pymc3'

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

memory_bias_fits_money <- read.csv(paste0(mb_dir,'/h_estimates_money.csv')) #takes a while to read in (large file)

memory_bias_fits_images <- read.csv(paste0(mb_dir,'/h_estimates_images.csv')) #takes a while to read in (large file)

ci_neg_money = hdi(memory_bias_fits_money$intercept_negative_mu,ci=ci_level)
ci_pos_money = hdi(memory_bias_fits_money$intercept_positive_mu,ci=ci_level)

ci_neg_images = hdi(memory_bias_fits_images$intercept_negative_mu,ci=ci_level)
ci_pos_images = hdi(memory_bias_fits_images$intercept_positive_mu,ci=ci_level)

ci_difference_images <- hdi(memory_bias_fits_images$intercept_difference,ci=ci_level)
ci_difference_money <- hdi(memory_bias_fits_money$intercept_difference,ci=ci_level)

ci_neg_money_slope = hdi(memory_bias_fits_money$slope_negative_mu,ci=ci_level)
ci_pos_money_slope = hdi(memory_bias_fits_money$slope_positive_mu,ci=ci_level)

ci_neg_images_slope = hdi(memory_bias_fits_images$slope_negative_mu,ci=ci_level)
ci_pos_images_slope = hdi(memory_bias_fits_images$slope_positive_mu,ci=ci_level)

ci_difference_images_slope <- hdi(memory_bias_fits_images$slope_difference,ci=ci_level)
ci_difference_money_slope <- hdi(memory_bias_fits_money$slope_difference,ci=ci_level)

percentage_images_neg <- mean(memory_bias_fits_money$intercept_negative_mu < 0)
percentage_images_pos <- mean(memory_bias_fits_money$intercept_positive_mu > 0)
percentage_images <- mean(memory_bias_fits_images$intercept_difference > 0)

percentage_money_neg <- mean(memory_bias_fits_money$intercept_negative_mu < 0)
percentage_money_pos <- mean(memory_bias_fits_money$intercept_positive_mu > 0)
percentage_money <- mean(memory_bias_fits_money$intercept_difference > 0)


for (i in 1:1){
    sink(paste0(mb_dir,'/HDI_memory_bias.txt'))
    print('Here are the 90% HDI levels for the memory bias (intercepts):')
    cat('\n')
    print(paste0('images positive: '))
    print(ci_pos_images)
    print(paste0('images negative: '))
    print(ci_neg_images)
    print('difference images:')
    print(ci_difference_images)
    cat('\n')
    print(paste0('money positive: '))
    print(ci_pos_money)
    print(paste0('money negative: '))
    print(ci_neg_money)
    print('difference money:')
    print(ci_difference_money)
    cat('\n')
    print('Here are the 90% HDI levels for the memory bias (slopes):')
    cat('\n')
    print(paste0('images positive: '))
    print(ci_pos_images_slope)
    print(paste0('images negative: '))
    print(ci_neg_images_slope)
    print('difference images:')
    print(ci_difference_images_slope)
    cat('\n')
    print(paste0('money positive: '))
    print(ci_pos_money_slope)
    print(paste0('money negative: '))
    print(ci_neg_money_slope)
    print('difference money:')
    print(ci_difference_money_slope)
    cat('\n')
    print('Here is the overlap with 0 for the images experiment:')
    cat('\n')
    print(paste0('We can conclude that there is a ', percentage_images_neg*100,
      '% chance that the mean for positive is smaller than 0 (images)'))
    print(paste0('We can conclude that there is a ', percentage_images_pos*100,
      '% chance that the mean for positive is greater than 0 (images)'))
    print(paste0('We can conclude that there is a ', percentage_images*100,
      '% chance that the mean for positive is greater than the mean for negative (images)'))
    print(paste0('The percentage of overap is: ',round(100-(percentage_images*100),digits=2)))
    cat('\n')
    print('Here is the overlap with 0 for the money experiment:')
    cat('\n')
    print(paste0('We can conclude that there is a ', percentage_money_neg*100,
      '% chance that the mean for positive is smaller than 0 (money)'))
    print(paste0('We can conclude that there is a ', percentage_money_pos*100,
      '% chance that the mean for positive is greater than 0 (money)'))
    print(paste0('We can conclude that there is a ', percentage_money*100,
      '% chance that the mean for positive is greater than the mean for negative (money)'))
    print(paste0('The percentage of overap is: ',round(100-(percentage_money*100),digits=2)))
    sink()
    }

group_posteriors_money = as.data.frame(c(memory_bias_fits_money$intercept_negative_mu, 
                                   memory_bias_fits_money$intercept_positive_mu))
names(group_posteriors_money) = 'intercepts_money'
group_posteriors_money$domain = rep(c('losses','gains'),each=nrow(memory_bias_fits_money))

group_posteriors_images = as.data.frame(c(memory_bias_fits_images$intercept_negative_mu, 
                                   memory_bias_fits_images$intercept_positive_mu))
names(group_posteriors_images) = 'intercepts_images'
group_posteriors_images$domain = rep(c('losses','gains'),each=nrow(memory_bias_fits_images))

describe(group_posteriors_images$intercepts_images)
describe(group_posteriors_money$intercepts_money)

# PLOT FIGURE 2
p_num_post<-ggplot(group_posteriors_money, aes(x=intercepts_money,color=domain,fill=domain)) + 
    xlim(c(-0.55,0.70)) +
    #geom_histogram(aes(y=..density..),position="dodge",alpha=0.5,binwidth = 0.01,show.legend = FALSE)+
    geom_density(alpha=.2,show.legend = FALSE,trim=TRUE,adjust=5)+
    #geom_vline(data=mu_money, aes(xintercept=grp.mean, color=domain),linetype="dashed",show.legend = FALSE,size=size_dashed)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_pos_money$CI_low, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_pos_money$CI_high, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_money$CI_low, color=my_colors[2], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_money$CI_high, color=my_colors[2], linetype=2,size=size_dashed) +
    scale_color_manual(values=my_colors)+
    scale_fill_manual(values=my_colors)+
    labs(x="Memory bias group posterior", y = "Density")+
    mytheme

p_images_post<-ggplot(group_posteriors_images, aes(x=intercepts_images,color=domain,fill=domain)) + 
    xlim(c(-0.55,0.70)) +
    #geom_histogram(aes(y=..density..),position="dodge",alpha=0.5,binwidth = 0.01,show.legend = FALSE)+
    geom_density(alpha=.2,show.legend = FALSE,trim=TRUE,adjust=5)+
    #geom_vline(data=mu_images, aes(xintercept=grp.mean, color=domain),linetype="dashed",show.legend = FALSE,size=size_dashed)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_pos_images$CI_low, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_pos_images$CI_high, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_images$CI_low, color=my_colors[2], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_images$CI_high, color=my_colors[2], linetype=2,size=size_dashed) +
    scale_color_manual(values=my_colors)+
    scale_fill_manual(values=my_colors)+
    labs(x="Memory bias group posterior", y = "Density")+
    mytheme

p_num_raw <- ggplot(mean_positive_corr_money, aes(ah, prop_chosen)) +
  ylim(c(-0.02,1)) +
  labs(x="Standardized value remembered option", y = "Money\nP(choose remembered)  ")+
  geom_line(aes(col='Gains'),size=size_lines) +
  geom_errorbar(mapping=aes(x=mean_positive_corr_money$ah,ymax = mean_positive_corr_money$Up_pos_corr, 
                            ymin = mean_positive_corr_money$Lp_pos_corr), 
                width=0.2, size=size_lines, color=positive_col) +
  geom_point(aes(col='Gains'),size=2)+
  geom_vline(xintercept = 0, lty=2,color=grey_color,size=size_dashed) +
  geom_hline(yintercept = 0.5,lty=2,color=grey_color,size=size_dashed) +
  mytheme+ 
  theme(legend.position = "none")+
  geom_line(data = mean_negative_corr_money, aes(x = ah, y = prop_chosen,col='Losses'),size=size_lines) +
  geom_errorbar(mapping=aes(x=mean_negative_corr_money$ah,ymax = mean_negative_corr_money$Up_neg_corr, 
                            ymin = mean_negative_corr_money$Lp_neg_corr), width=0.2, size=size_lines, color=negative_col) +
  geom_point(data = mean_negative_corr_money, aes(x = ah, y = prop_chosen,col='Losses'),size=2)+
  scale_colour_manual('money', values = cols)

p_images_raw <- ggplot(mean_positive_corr_images, aes(ah, prop_chosen)) +
  ylim(c(-0.02,1)) +

  labs(x="Standardized value remembered option", y = "Images\nP(choose remembered)")+
  geom_line(aes(col='Gains'),size=size_lines) + # 95% confidence interval
  geom_errorbar(mapping=aes(x=mean_positive_corr_images$ah,ymax = mean_positive_corr_images$Up_pos_corr, 
                            ymin = mean_positive_corr_images$Lp_pos_corr), width=0.2, size=size_lines,color=positive_col) +
  geom_point(aes(col='Gains'),size=2)+
  geom_vline(xintercept = 0, lty=2,color=grey_color,size=size_dashed) +
  geom_hline(yintercept = 0.5,lty=2,color=grey_color,size=size_dashed) +
  mytheme +   
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.2, 0.8))+

  geom_line(data = mean_negative_corr_images, aes(x = ah, y = prop_chosen,col='Losses'),size=size_lines) +
  geom_errorbar(mapping=aes(x=mean_negative_corr_images$ah,ymax = mean_negative_corr_images$Up_neg_corr, 
                            ymin = mean_negative_corr_images$Lp_neg_corr), width=0.2, size=size_lines,color=negative_col) +
  geom_point(data = mean_negative_corr_images, aes(x = ah, y = prop_chosen,col='Losses'),size=2)+
  scale_colour_manual('images', values = cols)

p <- plot_grid(p_images_raw, p_images_post,p_num_raw, p_num_post,labels='auto',label_size = my_fontsize_axes*1.5,label_fontfamily = 'Helvetica',
  label_fontface = "bold")
save_plot("./figures/Fig2_memory_bias_raw_post_both_revised.pdf", p, ncol = 2,nrow=2,base_height = 12,base_width = 12)


describe(memory_bias_fits_images$intercept_difference)
describe(memory_bias_fits_money$intercept_difference)

#alternative: plot difference only
difference_color <- rgb(138,43,226,max=255)

p_images_post_difference<-ggplot(memory_bias_fits_images, aes(x=intercept_difference,fill=difference_color)) + 
    xlim(c(-0.30,0.9)) +
    geom_density(alpha=.2,show.legend = FALSE,trim=TRUE,adjust=5)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_difference_images$CI_low, color=difference_color, linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_difference_images$CI_high, color=difference_color, linetype=2,size=size_dashed) +
    scale_color_manual(values=difference_color)+
    scale_fill_manual(values=difference_color)+
    labs(x="Memory bias difference posterior", y = "Density")+
    mytheme


p_money_post_difference<-ggplot(memory_bias_fits_money, aes(x=intercept_difference,fill=difference_color)) + 
    xlim(c(-0.30,0.6)) +
    geom_density(alpha=.2,show.legend = FALSE,trim=TRUE,adjust=5)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_difference_money$CI_low, color=difference_color, linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_difference_money$CI_high, color=difference_color, linetype=2,size=size_dashed) +
    scale_color_manual(values=difference_color)+
    scale_fill_manual(values=difference_color)+
    labs(x="Memory bias difference posterior", y = "Density")+
    mytheme

diff_both <- plot_grid(p_images_post_difference, p_money_post_difference,labels='auto',label_size = my_fontsize_axes*1.5,label_fontfamily = 'Helvetica',
  label_fontface = "bold")
save_plot("./figures/Fig2b_memory_bias_post_both_diff.pdf", diff_both, ncol = 2,nrow=2,base_height = 12,base_width = 12)


mb_dir <- './results/results_memory_bias/bayesian_pymc3'

#images data
summary_memory_bias_images <- read.csv(paste0(mb_dir,'/summary_estimates_images.csv'))
summary_memory_bias_money <- read.csv(paste0(mb_dir,'/summary_estimates_money.csv'))

mb_gains_images <- summary_memory_bias_images[grep("intercept_positive__",summary_memory_bias_images$var_names), ]
mb_losses_images <- summary_memory_bias_images[grep("intercept_negative__",summary_memory_bias_images$var_names), ]

mb_gains_money <- summary_memory_bias_money[grep("intercept_positive__", summary_memory_bias_money$var_names), ]
mb_losses_money <- summary_memory_bias_money[grep("intercept_negative__", summary_memory_bias_money$var_names), ]



# PLOT FIGURE 3
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

memory_bias_gains <- c(mb_gains_images[,'mean'],mb_gains_money[,'mean'])
memory_bias_losses <- c(mb_losses_images[,'mean'],mb_losses_money[,'mean'])
risk_gains <- c((risk_gains_images$mean)-1,(risk_gains_money$mean)-1) #ATTENTION HERE!!!
risk_losses <- c(1-(risk_losses_images$mean),1-(risk_losses_money$mean))#ATTENTION HERE!!!
amb_gains <- c(-(amb_gains_images$mean),-(amb_gains_money$mean))#ATTENTION HERE!!!
amb_losses <- c(amb_losses_images$mean,amb_losses_money$mean)
exp_vec <- c(rep('images',nrow(amb_losses_images)), rep('money',nrow(amb_losses_money)))

difference_df <- cbind(memory_bias_gains-memory_bias_losses, risk_gains-risk_losses, amb_gains-amb_losses,exp_vec)
colnames(difference_df) <- c('mb_diff','risk_diff','amb_diff','experiment')

difference_df <- as.data.frame(difference_df)
difference_df$mb_diff <- as.numeric(as.character(difference_df$mb_diff))
difference_df$risk_diff <- as.numeric(as.character(difference_df$risk_diff))
difference_df$amb_diff <- as.numeric(as.character(difference_df$amb_diff))


write.csv(difference_df,'./results/difference_df.csv')

exp_colors <- c(rgb(0,150,0,max=255),rgb(255,140,0,max=255))

diff_risk_mb <- ggplot(difference_df, aes(x=risk_diff, y=mb_diff)) +
    geom_smooth(aes(color=experiment,fill=experiment),method="lm") + #with se
    geom_point(aes(color=experiment,fill=experiment),size=6,shape = 21, colour = "black") +
    xlim(-1.5,2)+
    xlab("Risk attitude \n(gains - losses)")+
    ylab("Memory bias on choice \n(gains - losses)") +
    mytheme +
    theme(legend.title = element_blank())+
    theme(legend.position = c(0.8, 0.8))+
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
save_plot("./figures/Fig3_lottery_results_only_revised.pdf", p, ncol = 2,nrow=1,base_height = 12,base_width=12)



linreg_dir <- './results/results_linear_regression/bayesian'
estimates_linreg <- read.csv(paste0(linreg_dir,'/estimates_linreg.csv')) #takes a while to read in (large file)

hdi_intercept = hdi(estimates_linreg$Intercept,ci=ci_level)
hdi_exp = hdi(estimates_linreg$experiment.T.money,ci=ci_level)

hdi_risk = hdi(estimates_linreg$risk_diff,ci=ci_level)
hdi_amb = hdi(estimates_linreg$amb_diff,ci=ci_level)


for (i in 1:1){
    sink(paste0(linreg_dir,'/HDI_linreg.txt'))
    print('Here are the 90% HDI levels for the multiple linear regression:')
    cat('\n')
    print(paste0('intercept: '))
    print(hdi_intercept)
    print(paste0('experiment: '))
    print(hdi_exp)
    print('risk:')
    print(hdi_risk)
    print('amb:')
    print(hdi_amb)    
    sink()
    }

bf01_risk_linreg = 1 / bayesfactor_parameters(posterior=estimates_linreg$risk_diff, prior = distribution_normal(nrow(estimates_linreg),0,1),
  direction = "two-sided",verbose = TRUE)

bf01_amb_linreg = 1/bayesfactor_parameters(posterior=estimates_linreg$amb_diff, prior = distribution_normal(nrow(estimates_linreg), 0,1),
  direction = "two-sided",verbose = TRUE)

for (i in 1:1){
    sink(paste0(linreg_dir,'/BFs_mult_linreg.txt'))
    print('Here are the BF01 for the multiple linear regression:')
    cat('\n')
    print(paste0('risk linreg: '))
    print(bf01_risk_linreg)
    print(paste0('amb linreg: '))
    print(bf01_amb_linreg)
    sink()
    }

corr_dir <- './results/results_correlations/bayesian'

corr_images_risk <- read.csv(paste0(corr_dir,'/estimates_corr_risk_images.csv')) #takes a while to read in (large file)
corr_images_amb <- read.csv(paste0(corr_dir,'/estimates_corr_amb_images.csv'))

corr_money_risk <- read.csv(paste0(corr_dir,'/estimates_corr_risk_money.csv')) #takes a while to read in (large file)
corr_money_amb <- read.csv(paste0(corr_dir,'/estimates_corr_amb_money.csv'))

hdi_images_risk = hdi(corr_images_risk$r,ci=ci_level)
hdi_images_amb = hdi(corr_images_amb$r,ci=ci_level)

hdi_money_risk = hdi(corr_money_risk$r,ci=ci_level)
hdi_money_amb = hdi(corr_money_amb$r,ci=ci_level)


for (i in 1:1){
    sink(paste0(corr_dir,'/HDI_correlations.txt'))
    print('Here are the 90% HDI levels for the correlations:')
    cat('\n')
    print(paste0('risk images: '))
    print(hdi_images_risk)
    print(paste0('amb images: '))
    print(hdi_images_amb)
    cat('\n')
    print('risk money:')
    print(hdi_money_risk)
    print('amb money:')
    print(hdi_money_amb)    
    sink()
    }

cor.test(difference_df$mb_diff,difference_df$risk_diff)

# https://richarddmorey.github.io/BayesFactor/#lincor
#bf = correlationBF(y = difference_df$mb_diff, x = difference_df$risk_diff)
#bf

#1/bf

#https://easystats.github.io/bayestestR/reference/bayesfactor_savagedickey.html
#https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.html
bf01_risk_images = 1 / bayesfactor_parameters(posterior=corr_images_risk$r, prior = distribution_uniform(nrow(corr_images_risk), min = -1, max = 1),
  direction = "two-sided",verbose = TRUE)

bf01_risk_money = 1/bayesfactor_parameters(posterior=corr_money_risk$r, prior = distribution_uniform(nrow(corr_money_risk), -1, 1),
  direction = "two-sided",verbose = TRUE)

bf01_amb_images = 1 / bayesfactor_parameters(posterior=corr_images_amb$r, prior = distribution_uniform(nrow(corr_images_amb), -1, 1),
  direction = "two-sided",verbose = TRUE)

bf01_amb_money = 1 / bayesfactor_parameters(posterior=corr_money_amb$r, prior = distribution_uniform(nrow(corr_money_amb), -1, 1),
  direction = "two-sided",verbose = TRUE)

for (i in 1:1){
    sink(paste0(corr_dir,'/BFs_correlations.txt'))
    print('Here are the BF01 for the correlations:')
    cat('\n')
    print(paste0('risk images: '))
    print(bf01_risk_images)
    print(paste0('amb images: '))
    print(bf01_risk_money)
    cat('\n')
    print('risk money:')
    print(bf01_amb_images)
    print('amb money:')
    print(bf01_amb_money)    
    sink()
    }

cor.test(difference_df$mb_diff,difference_df$amb_diff)

difference_df_images = subset(difference_df,experiment=='images')
cor.test(difference_df_images$mb_diff,difference_df_images$risk_diff)
cor.test(difference_df_images$mb_diff,difference_df_images$amb_diff)

difference_df_money = subset(difference_df,experiment=='money')
cor(difference_df_money$mb_diff,difference_df_money$risk_diff)
cor(difference_df_money$mb_diff,difference_df_money$amb_diff)

cov(difference_df_money$mb_diff,difference_df_money$risk_diff)
cov(difference_df_money$mb_diff,difference_df_money$amb_diff)

sessionInfo()


