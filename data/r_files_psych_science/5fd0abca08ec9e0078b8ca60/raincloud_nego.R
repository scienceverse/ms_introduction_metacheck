packages <- c("ggplot2", "plyr", "dplyr", "lavaan", "cowplot", "rmarkdown", 
              "readr", "caTools", "bitops")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(cowplot)
library(dplyr)
library(readr)

source("R_rainclouds.R")
source("summarySE.R")

# width and height variables for saved plots
w = 6
h = 3



### Figure 2 main manuscript (upper panel)

best_only <- read.csv(
  file = "best_only.csv",
  header = TRUE,
  sep = ";",
  dec = ","
)



best_only$ball<-as.factor(best_only$ball)
best_only$ball<- factor(best_only$ball,
                        levels = c(0, 1),
                        labels = c("Bad Signal", "Good Signal"))

best_only$posterior<-as.factor(best_only$posterior)
best_only$posterior<- factor(best_only$posterior,
                             levels = c(0, 1),
                             labels = c("Prior beliefs", "Posterior beliefs"))


str(best_only)


sumrepdat <- summarySE(best_only, measurevar = "beliefs", groupvars=c("ball", "posterior"))

head(sumrepdat)
str(sumrepdat)

#best=posterior

p1 <- ggplot(best_only, aes(x = posterior, y = beliefs, fill = ball)) +
  geom_flat_violin(aes(fill = ball),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(posterior)-.15, y = beliefs, colour = ball),position = position_jitter(width = .05), size = 2, shape = 20)+
  geom_boxplot(aes(x = posterior, y = beliefs, fill = ball),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat, aes(x = as.numeric(posterior)+.1, y = beliefs_mean, group = ball, colour = ball), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(posterior)+.1, y = beliefs_mean, group = ball, colour = ball), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(posterior)+.1, y = beliefs_mean, group = ball, colour = ball, ymin = beliefs_mean-se, ymax = beliefs_mean+se), width = .05)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  #scale_color_grey(name = "Signal", start = 0.6, end = 0.2) +
  #scale_fill_grey(name = "Signal", start = 0.6, end = 0.2)+
  ggtitle("")

p1 + ylab("Beliefs of best participant") +
  scale_y_continuous(breaks = seq(0, 100, 20))


ggsave('raincloud_best.png', width = w, height = h)



### Figure 2 main manuscript (lower panel)

nobest_only <- read.csv(
  file = "nobest_only.csv",
  header = TRUE,
  sep = ";",
  dec = ","
)


nobest_only$ball<-as.factor(nobest_only$ball)
nobest_only$ball<- factor(nobest_only$ball,
                          levels = c(0, 1),
                          labels = c("Bad Signal", "Good Signal"))

nobest_only$posterior<-as.factor(nobest_only$posterior)
nobest_only$posterior<- factor(nobest_only$posterior,
                               levels = c(0, 1),
                               labels = c("Prior beliefs", "Posterior beliefs"))


str(nobest_only)

sumrepdat <- summarySE(nobest_only, measurevar = "beliefs", groupvars=c("ball", "posterior"))

head(sumrepdat)
str(sumrepdat)


p2 <- ggplot(nobest_only, aes(x = posterior, y = beliefs, fill = ball)) +
  geom_flat_violin(aes(fill = ball),position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(posterior)-.15, y = beliefs, colour = ball),position = position_jitter(width = .05), size = 2, shape = 20)+
  geom_boxplot(aes(x = posterior, y = beliefs, fill = ball),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  geom_line(data = sumrepdat, aes(x = as.numeric(posterior)+.1, y = beliefs_mean, group = ball, colour = ball), linetype = 3)+
  geom_point(data = sumrepdat, aes(x = as.numeric(posterior)+.1, y = beliefs_mean, group = ball, colour = ball), shape = 18) +
  geom_errorbar(data = sumrepdat, aes(x = as.numeric(posterior)+.1, y = beliefs_mean, group = ball, colour = ball, ymin = beliefs_mean-se, ymax = beliefs_mean+se), width = .05)+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("")

p2 + ylab("Beliefs of worse participant") +
  scale_y_continuous(breaks = seq(0, 100, 20))


ggsave('raincloud_nobest.png', width = w, height = h)



### Figure S2.3 SOM-U
time_data <- read.csv(
  file = "time_data.csv",
  header = TRUE,
  sep = ";",
  dec = ","
)



time_data$treat<-as.factor(time_data$treat)
time_data$treat<- factor(time_data$treat,
                         levels = c(0,1,2),
                         labels = c("Two bad signals", "Two opposite signals", "Two good signals"))


str(time_data)

sumrepdat <- summarySE(time_data, measurevar = "time", groupvars=c("treat"))

head(sumrepdat)
str(sumrepdat)

p3 <- ggplot(time_data, aes(x = treat, y = time, fill = treat, colour=treat)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0), adjust = 1, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(treat)-.15, y = time),position = position_jitter(width = .05), size = 2, shape = 20)+
  geom_boxplot(aes(x = treat, y = time),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "none") +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("")

p2 + ylab("Time needed to reach an agreement")


ggsave('time.png', width = w, height = h)