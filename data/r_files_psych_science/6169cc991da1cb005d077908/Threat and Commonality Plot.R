#Threat and Commonality Plot

library(tidyverse)
library(ggridges)

####Data Prep####
dat <- read.csv("Merged_long_updated.csv")
d <- dat %>% 
  filter(perspective == "self_percentage") %>% 
  filter(study == "USA Evangelical" | study == "Israel" | study == "West Bank") %>% 
  filter(ingrp0 == 1)
d$study <- factor(d$study, levels = rev(c("USA Evangelical", "Israel", 'West Bank')))

####Adjust Data for Plot####
long <- gather(d, measure, score, c(threat_composite,commonality_composite))
long$measure <- factor(long$measure, levels = c("threat_composite", "commonality_composite"),
                       labels=c("Threat", "Commonality"))

long <- long %>% 
  filter(condition != "") %>% 
  unite(col = study_measure, study, measure, sep = "_", remove = F) %>% 
  unite(col = study_condition, study, condition, sep = "_", remove = F)

long$study_condition <- factor(long$study_condition,
                               levels = rev(c("USA Evangelical_Muslim", "USA Evangelical_Atheist","Israel_Muslim", "West Bank_Jewish")))

levels(long$study_condition)
addline_format <- function(x,...){
  gsub('\\s','/n',x)
}

####Build Plot####
both <- ggplot(long, aes(y=study_condition, x = score, fill = study)) +
  geom_density_ridges(panel_scaling = FALSE, scale = 1.5,
                      quantile_lines = TRUE,
                      quantile_fun=function(x,...)mean(x))+
  xlim(1,5) +
  scale_y_discrete(expand=c(0,.05),
                   labels = c("USA Evangelical_Muslim" = "U.S. Evangelicals \n (Study 6) \n Muslim Outgroup",
                              "USA Evangelical_Atheist" = "U.S. Evangelicals \n (Study 6) \n Atheist Outgroup",
                              "Israel_Muslim" = "Israeli Jews \n (Study 7) \n Muslim Outgroup",
                              "West Bank_Jewish" = "Palestinian Muslims \n (Study 8) \n Jewish Outroup")) +
  facet_wrap(~measure, strip.position = "bottom") +
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size=12,vjust=-4),
        axis.text.y = element_text(vjust=-0.7),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(family = "sans", size = 12),
        text = element_text(family = "sans")) +
  scale_fill_grey(start = .8, end = .8)
both