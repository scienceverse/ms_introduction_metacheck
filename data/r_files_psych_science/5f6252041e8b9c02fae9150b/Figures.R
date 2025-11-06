#Figures 5A, 5B, 7D, Supplementary figures 2A, 2B

#Clear environment workspace
rm(list=ls(all=TRUE))


#Load ggplot package for figures
library(ggplot2)


#Load data: choose files in R folder one by one in this order
#1.NF_task1, 2.F_task1, 3.NF_task2, 4.F_task2. 5.supp_corr_task1
#No Feedback group Task 1
dataNFt1 = read.table(file.choose(), header=T, sep="\t")

#Feedback group Task 1
dataFt1 = read.table(file.choose(), header=T, sep="\t")

#No Feedback group Task 2
dataNFt2 = read.table(file.choose(), header=T, sep="\t")

#Feedback group Task 2
dataFt2 = read.table(file.choose(), header=T, sep="\t")

#Supplemental figure 2 Task 1
datasupp = read.table(file.choose(), header=T, sep="\t")


#d' confidence scatterplots 

#No Feedback group Task 1
g = ggplot(dataNFt1, aes(x=d, y=confidence)) + theme_bw()+ 
  geom_point(shape=21, color="black", fill="steelblue3")+
  geom_smooth(method=lm,
              color="gray31", fill="lightgray")  + ggtitle("No Feedback group") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0.2, 3.3) + ylim(0.7, 4.3) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=18,face="bold"))

g + theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())



#Feedback group Task 1
g = ggplot(dataFt1, aes(x=d, y=confidence)) + theme_bw()+ 
  geom_point(shape=21, color="black", fill="goldenrod1")+ 
  geom_smooth(method=lm,
              color="gray31", fill="lightgray") + ggtitle("Feedback group") +  theme(plot.title = element_text(hjust = 0.5)) + xlim(0.2, 3.3) + ylim(0.7, 4.3) + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=18,face="bold"))

g + theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())


#No Feedback group Task 2
g = ggplot(dataNFt2, aes(x=d, y=confidence)) + theme_bw()+ 
  geom_point(shape=21, color="black", fill="steelblue3")+
  geom_smooth(method=lm,
              color="gray31", fill="lightgray")  + ggtitle("No Feedback group") + theme(plot.title = element_text(hjust = 0.5)) + xlim(-0.7, 3.3) + ylim(0.7, 4.3) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=18,face="bold"))

g + theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())



#Feedback group Task 2
g = ggplot(dataFt2, aes(x=d, y=confidence)) + theme_bw()+ 
  geom_point(shape=21, color="black", fill="goldenrod1")+ 
  geom_smooth(method=lm,
              color="gray31", fill="lightgray") + ggtitle("Feedback group") + theme(plot.title = element_text(hjust = 0.5)) +  xlim(0.2, 3.5) + ylim(0.7, 4.3) + 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=18,face="bold"))

g + theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())



# Supplementary Figure 2: Scatterplot learning rate and d
g = ggplot(datasupp, aes(x=learningrate, y=d)) + theme_bw()+ 
  geom_point(shape=21, color="black", fill="gray")+
  geom_smooth(method=lm,
              color="gray31", fill="lightgray")  + ggtitle("d' (across groups)") + xlab("learning rate")+ ylab("d'") + theme(plot.title = element_text(hjust = 0.5)) + xlim(-0.0016, 0.0017) + ylim(0.24, 3.5) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=18,face="bold"))

g + theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())


# Supplementary Figure 2: Scatterplot learning rate meta-d'

g = ggplot(datasupp, aes(x=learningrate, y=metad)) + theme_bw()+ 
  geom_point(shape=21, color="black", fill="gray")+
  geom_smooth(method=lm,
              color="gray31", fill="lightgray")  + ggtitle("meta-d' (across groups)") + xlab("learning rate")+ ylab("meta-d'") + theme(plot.title = element_text(hjust = 0.5)) + xlim(-0.0016, 0.0017) + ylim(-0.8, 4) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=18,face="bold"))

g + theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank())
