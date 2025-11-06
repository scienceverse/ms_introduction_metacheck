# -----------Sep 13,2021-------------------------
# -----------Author: Rui Sun---------------------

# setwd("")

data <- read.csv("dat2019_2020_to_use.csv")

# -----------------------------------------------------------------
# -----------Plotting for Study 2; Figure 1 in main text-----------
# -----------------------------------------------------------------

data$covid <- as.factor(data$covid)
dataplot <- data[,c("covid","age","Neg","Pos")]
dataplot1 <- melt(dataplot,id.vars=c("covid","age"))
names(dataplot1)[3] <- "emotion"


neg_interaction <- ggplot(data = dataplot1[which(dataplot1$emotion=="Neg"),], aes(x = age,  y = value, color = covid)) +
  geom_smooth(method = "lm")+
  scale_colour_manual( values = c("0" = "red","1" = "blue"),name  ="Pandemic Condition",
                       breaks=c("0", "1"),
                       labels=c("Before", "During"))+
  theme_light() +
  ylab("Negative Emotions") + 
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.title = element_text(color = "black", face = "bold", size = 12,hjust=0.5),
        axis.line = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12),
        axis.title = element_text(size = 13)
  )+
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
neg_interaction


pos_interaction <- ggplot(data = dataplot1[which(dataplot1$emotion=="Pos"),], aes(x = age,  y = value, color = covid)) +
  geom_smooth(method = "lm")+
  scale_colour_manual( values = c("0" = "red","1" = "blue"),name  ="Pandemic Condition",
                       breaks=c("0", "1"),
                       labels=c("Before", "During"))+
  theme_light() +
  ylab("Positive Emotions") + 
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.title = element_text(color = "black", face = "bold", size = 12,hjust=0.5),
        axis.line = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12),
        axis.title = element_text(size = 13)
  )+
  theme(plot.margin = unit(c(2,2,2,2), "lines"))
pos_interaction


require(ggpubr)
plot_interaction <- ggarrange(neg_interaction,pos_interaction,nrow=1,common.legend = TRUE,legend="right")
plot_interaction1 <-  annotate_figure(plot_interaction,
                                      bottom = text_grob("Age(Years)", 
                                                         hjust = 1, x = 0.5, size = 13))

plot_interaction1


