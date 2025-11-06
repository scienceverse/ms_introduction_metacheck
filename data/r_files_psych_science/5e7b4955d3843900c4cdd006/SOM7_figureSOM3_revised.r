#!/usr/bin/env python
# coding: utf-8

# # Plot Figure SOM3 manuscript "Memory as uncertainty"
# ### revised code after joint estimation gains and losses

# In[1]:


library(ggplot2)
library(psych)
library(bayestestR)
library(cowplot)
library(grid)


# In[2]:


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

mb_dir <- './results/results_memory_bias_sb/bayesian_pymc3'


# In[3]:


memory_bias_fits_money <- read.csv(paste0(mb_dir,'/h_estimates_money.csv')) #takes a while to read in (large file)

memory_bias_fits_images <- read.csv(paste0(mb_dir,'/h_estimates_images.csv')) #takes a while to read in (large file)


# In[4]:


ci_neg_money = hdi(memory_bias_fits_money$condition_intercept_mu__0,ci=.90)
ci_pos_money = hdi(memory_bias_fits_money$condition_intercept_mu__1,ci=.90)

ci_neg_images = hdi(memory_bias_fits_images$condition_intercept_mu__0,ci=.90)
ci_pos_images = hdi(memory_bias_fits_images$condition_intercept_mu__1,ci=.90)


# In[ ]:





# In[5]:


#myvars <- c("condition_intercept_mu__0", "condition_intercept_mu__1")
#group_posteriors <- memory_bias_fits_images[myvars]
#describe(group_posteriors)


# In[6]:


group_posteriors_money = as.data.frame(c(memory_bias_fits_money$condition_intercept_mu__0, 
                                   memory_bias_fits_money$condition_intercept_mu__1))
names(group_posteriors_money) = 'intercepts_money'
group_posteriors_money$domain = rep(c('losses','gains'),each=nrow(memory_bias_fits_money))

group_posteriors_images = as.data.frame(c(memory_bias_fits_images$condition_intercept_mu__0, 
                                   memory_bias_fits_images$condition_intercept_mu__1))
names(group_posteriors_images) = 'intercepts_images'
group_posteriors_images$domain = rep(c('losses','gains'),each=nrow(memory_bias_fits_images))


# In[7]:


p_num_post_sb<-ggplot(group_posteriors_money, aes(x=intercepts_money,color=domain,fill=domain)) + 
    xlim(c(-0.55,0.65)) +
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
    labs(x="Memory bias group posterior", y = "Density (Money)")+
    mytheme

p_images_post_sb<-ggplot(group_posteriors_images, aes(x=intercepts_images,color=domain,fill=domain)) + 
    xlim(c(-0.55,0.65)) +
    #geom_histogram(aes(y=..density..),position="dodge",alpha=0.5,binwidth = 0.01,show.legend = FALSE)+
    geom_density(alpha=.2,show.legend = TRUE,trim=TRUE,adjust=5)+
    #geom_vline(data=mu_images, aes(xintercept=grp.mean, color=domain),linetype="dashed",show.legend = FALSE,size=size_dashed)+
    geom_vline(xintercept=0,color=grey_color,linetype=1,size=size_dashed)+
    # HDI 
    geom_vline(xintercept=ci_pos_images$CI_low, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_pos_images$CI_high, color=my_colors[1], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_images$CI_low, color=my_colors[2], linetype=2,size=size_dashed) +
    geom_vline(xintercept=ci_neg_images$CI_high, color=my_colors[2], linetype=2,size=size_dashed) +
    scale_color_manual(values=my_colors)+
    scale_fill_manual(values=my_colors)+
    labs(x="Memory bias group posterior", y = "Density (Images)")+
    mytheme +
    theme(legend.title = element_blank())+
    theme(legend.position = c(0.1, 0.8))

p <- plot_grid(p_images_post_sb,p_num_post_sb,labels='auto',label_size = my_fontsize_axes*1.5,label_fontfamily = 'Helvetica',
  label_fontface = "bold")
save_plot("./figures/SOM_Fig3_memory_bias_corrected_sb_revised.pdf", p, ncol = 2,nrow=1,base_height = 12,base_width = 12)


# In[ ]:




