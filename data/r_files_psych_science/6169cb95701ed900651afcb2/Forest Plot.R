#Code for Forest Plot
library(tidyverse)
library(hrbrthemes)
library(cowplot)
####Updated Forest Plot####
d.forest <- read.csv("change2.csv") #Read data for forest ploy
d.forest$context <- factor(d.forest$context, levels = c("Overall","Fiji", "USA",  "Middle East")) 
d.forest$order <- factor(d.forest.bar$order) #Make sure the order is how we want it
d.forest.ingroup = subset(d.forest, Group =='Ingroup')
forest <- ggplot(d.forest, aes(y=order, x = difference_means, xmin = lower_diff, xmax = upper_diff, shape = Group, color = direction)) +
  geom_point(data = subset(d.forest, direction == "positive" & Group == 'Outgroup'),  position = position_nudge(y = -0.2), size = 3, color = "#595959") +
  geom_point(data = subset(d.forest, direction == "negative" & Group == 'Outgroup'),  position = position_nudge(y = -0.2), color = "black") +
  geom_errorbarh(data = subset(d.forest, Group == 'Ingroup'), position = position_nudge(y = 0.2), height = 0.1, linetype='dashed', color = "black") +
  geom_errorbarh(data = subset(d.forest, Group == 'Outgroup'), position = position_nudge(y = -0.2), height = 0.1, color = "black") +
  geom_point(data = subset(d.forest, direction == "positive" & Group == 'Ingroup'),  position = position_nudge(y = 0.2), size=3,shape = 21, fill = 'white', color = "black") +
  geom_point(data = subset(d.forest, direction == "negative" & Group == 'Ingroup'),  position = position_nudge(y = 0.2),size=3,shape = 21, fill = 'white', color = "black") +
  scale_x_continuous(limits = c(-5, 10), name = "Change in Giving\n(Percent of Allotment)", breaks = c(-5, -2.5, 0 , 2.5, 5, 7.5, 10), 
                     labels = c("-5%", "-2.5%", "0%", "+2.5%","+5%", "+7.5%", "+10%")) +
  scale_alpha_discrete(range = c(.4, 1)) +
  geom_vline(xintercept=0, color='#4D4D4D', linetype='dashed') +
  facet_grid(context~., scales= 'free', space='free') +
  coord_cartesian(clip="off") +
  theme_ipsum() +
  theme(text=element_text(family = "sans", color = "black"), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_blank(),
        strip.text = element_blank(), 
        axis.title.x = element_text(family = "sans", hjust=0.5, face="bold"), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        plot.margin = margin(10,10,10,10))
forest

####Updated Bar Portion####
d.forest.bar <- read.csv("forest.baseline2.csv")
d.forest.bar$context <- factor(d.forest.bar$context, levels = c("Overall","Fiji", "USA",  "Middle East"))
d.forest.bar$order <- factor(d.forest.bar$order)
forest.bar <- ggplot(d.forest.bar, aes(y=Means, x = order)) +
  geom_bar(data = subset(d.forest.bar, direction == "positive" & Group == 'Outgroup'),  position = position_nudge(x = -0.2), stat="identity", width = .4, color="black") +
  geom_bar(data = subset(d.forest.bar, direction == "negative" & Group == 'Outgroup'),  position = position_nudge(x = -0.2), stat="identity", width = .4, color = "black") +
  geom_bar(data = subset(d.forest.bar, direction == "positive" & Group == 'Ingroup'),  position = position_nudge(x = 0.2), stat="identity", width = .4, fill="white", color="black") +
  geom_bar(data = subset(d.forest.bar, direction == "negative" & Group == 'Ingroup'),  position = position_nudge(x = 0.2), stat="identity", width = .4, color = "black") +
  geom_errorbar(data = subset(d.forest.bar, Group == 'Ingroup'), aes(ymin = Lower, ymax = Upper), position = position_nudge(x = 0.2), width = 0.1, linetype='dashed') +
  geom_errorbar(data = subset(d.forest.bar, Group == 'Outgroup'), aes(ymin = Lower, ymax = Upper), position = position_nudge(x = -0.2), width = 0.1) +
  scale_y_continuous(limits = c(0, 1), name = "Baseline Giving\n(Percent of Allotment)", breaks = c(0, .25, .5,.75,1), 
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_x_discrete(name = "Study",
                   breaks = d.forest.bar$order, labels = d.forest.bar$Study) +
  facet_grid(context~., scales= 'free', space='free') +
  coord_cartesian(clip="off") +
  theme_ipsum() +
  theme(text=element_text(family = "sans", color="black"), legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_blank(),
        strip.text = element_blank(), 
        axis.title.x = element_text(family = "sans",hjust=0.5, face="bold"),
        axis.title.y = element_blank(),
        plot.margin = margin(10,10,10,10)) +
  coord_flip() 
forest.bar

####Join the Plots####
joint_plot <- plot_grid(forest.bar, forest, rel_widths = c(1,1.3))
joint_plot