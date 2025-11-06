# FIGURE 1
# This code creates Figure 1 from the main manuscript. 

# Load the combined data from all experiments 
alldata <- read.csv("alldata.csv", header = T)
glimpse(alldata)

# Packages
library(tidyverse)
library(scales)
library(cowplot)
library(gridExtra)
library(grid)

# Create the correlation variables of the signed errors
alldata$guess_1_cor <- alldata$guess_1 - alldata$answer
alldata$guess_2_cor <- alldata$guess_2 - alldata$answer

# Ordering the perspective levels
names(alldata)[names(alldata) == "condition"] <- "Perspective"
order <- c("Disagree","Self","Agree")#Re-order factors so that they are shown correctly on the graph
alldata$Perspective <- ordered(alldata$Perspective, order)

# Experiment factor 
alldata$Ex <- factor(alldata$Ex,
                     levels = c("Experiment 1a","Experiment 1b","Experiment 2", 
                                "Experiemnt 3", "Experiment 4"),
                     labels = c("Experiment 1a","Experiment 1b","Experiment 2", 
                                "Experiment 3", "Experiment 4"))
order1 <- c("Experiment 1a","Experiment 1b","Experiment 2", 
            "Experiment 3", "Experiment 4")
alldata$Ex <- ordered(alldata$Ex, order1)

# Set colors
fillcolors <- c("#00AFBB", "#E7B800", "#FC4E07")

# Experiment 1a -----------------------------------------------------------

f1 <- ggplot(subset(alldata,alldata$Ex == "Experiment 1a"), aes(x = guess_1_cor,
                          y = guess_2_cor,
                          color = Perspective))+
  geom_point(size = .5, 
             alpha = 0.3, 
             show.legend = T)+ 
  geom_smooth(method = "lm", se = FALSE, show.legend = F)+
  labs(x = "First estimate",
       y = "Second estimate")+
  theme_classic()+ 
  scale_color_manual(values=fillcolors)+
  scale_y_continuous(label = comma)+
  scale_x_continuous(label = comma)+
  theme(legend.position = "top")
f1

# Experiment 1b -----------------------------------------------------------
f2 <- ggplot(subset(alldata,alldata$Ex == "Experiment 1b"), aes(x = guess_1_cor,
                                                                y = guess_2_cor,
                                                                color = Perspective))+
  geom_point(size = .5, 
             alpha = 0.3, 
             show.legend = T)+
  geom_smooth(method = "lm", se = FALSE, show.legend = F)+
  labs(x = "First estimate",
       y = "Second estimate")+
  theme_classic()+ 
  scale_color_manual(values=fillcolors)+
  scale_y_continuous(label = comma)+
  scale_x_continuous(label = comma)+
  theme(legend.position = "top")
f2

# Experiment 2 ------------------------------------------------------------
f3 <- ggplot(subset(alldata,alldata$Ex == "Experiment 2"), aes(x = guess_1_cor,
                                                                y = guess_2_cor,
                                                                color = Perspective))+
  geom_point(size = .5, 
             alpha = 0.3, 
             show.legend = T)+
  geom_smooth(method = "lm", se = FALSE, show.legend = F)+
  labs(x = "First estimate",
       y = "Second estimate")+
  theme_classic()+ 
  scale_color_manual(values=fillcolors)+
  scale_y_continuous(label = comma)+
  scale_x_continuous(label = comma)+
  theme(legend.position = "top")
f3

# Experiment 3 ------------------------------------------------------------
f4 <- ggplot(subset(alldata,alldata$Ex == "Experiment 3"), aes(x = guess_1_cor,
                                                               y = guess_2_cor,
                                                               color = Perspective))+
  geom_point(size = .5, 
             alpha = 0.3, 
             show.legend = T)+
  geom_smooth(method = "lm", se = FALSE, show.legend = F)+
  labs(x = "First estimate",
       y = "Second estimate")+
  theme_classic()+ 
  scale_color_manual(values=fillcolors)+
  scale_y_continuous(label = comma)+
  scale_x_continuous(label = comma)+
  theme(legend.position = "top")
f4

# Experiment 4 ------------------------------------------------------------
f5 <- ggplot(subset(alldata,alldata$Ex == "Experiment 4"), aes(x = guess_1_cor,
                                                               y = guess_2_cor,
                                                               color = Perspective))+
  geom_point(size = .5, 
             alpha = 0.3, 
             show.legend = T)+
  geom_smooth(method = "lm", se = FALSE, show.legend = F)+
  labs(x = "First estimate",
       y = "Second estimate")+
  theme_classic()+ 
  scale_color_manual(values=fillcolors)+
  scale_y_continuous(label = comma)+
  scale_x_continuous(label = comma)+
  theme(legend.position = "top")
f5

# Combining the plots together 
top_row <- cowplot::plot_grid(f1,f2,ncol = 2, labels = c("a","b"))
middle_row <- cowplot::plot_grid(f3,f4,ncol = 2, labels = c("c","d"))
bottom_row <- cowplot::plot_grid(NULL,f5,NULL,ncol = 3, rel_widths = c(0.25,0.5,0.25), labels = c("e"))

cowplot::plot_grid(top_row, middle_row, bottom_row, ncol=1, rel_heights=c(1,1,1))

cowplot::ggsave2("fig1.png",
                 width = 7,
                 height = 8,
                 units = "in",
                 dpi = 300)

