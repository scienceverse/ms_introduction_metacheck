
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

#setwd('/Users/rweilbaecher/Documents/phD_Project2/data_analysis/data_analysis_public')

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




