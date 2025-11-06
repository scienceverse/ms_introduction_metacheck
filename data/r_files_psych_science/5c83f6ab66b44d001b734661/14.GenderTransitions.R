####################################
# File 14 of 15
# R code for: Predicting Early Childhood Gender Transitions
# Updated: October 24 2018
# Contact James Rae [jrrae@umass.edu] with any questions
####################################

###################################
# Set up
###################################

# Read in libraries
library(ggplot2)
library(reshape2)
library(plyr)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(cowplot)
library(viridis)
library(extrafont)
library(ggpubr)

# Set working directory
setwd("")

# Read in data
data <- read.table("complete.data.csv",sep=",",header=TRUE)

# Put on 0-1 interval
data$PeerGenderPOMP <- data$PeerGenderPOMP/100
data$ToyGenderPOMP <- data$ToyGenderPOMP/100
data$ClothingGenderPOMP <- data$ClothingGenderPOMP/100
data$SimilarityGenderPOMP <- data$SimilarityGenderPOMP/100
data$IdentityGenderPOMP <- data$IdentityGenderPOMP/100

# Subset data
myvars <- c("id","PeerGenderPOMP","ToyGenderPOMP","ClothingGenderPOMP","SimilarityGenderPOMP","IdentityGenderPOMP","PCTSI.p","group","transition")
data <- data[myvars]

# Give new Names
names(data) <- c("subNum","P","T","C","S","I","PCTSI","group","transition")

# Reformat
data.long <- melt(data, id = "subNum", measure.vars = c("P","T","C","S","I","PCTSI"))
data.wideForMerge <- data[,c("subNum","group","transition")]
data.long <- merge(data.long, data.wideForMerge, by = "subNum")
data.long <- ddply(data.long, .(variable), transform,
                   rescale = scale(value))

# Get ID number
data.long$subNum <- as.factor(data.long$subNum)

#######################
# GNC No Transition
#######################

GNCNoTransition <- data.long[data.long$group == "Gender Nonconforming" & data.long$transition==0, ]
plot1 <-  ggplot(GNCNoTransition, aes(variable, subNum,fill=value )) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "")) +
  geom_tile(color="black", size=0.1) +
  scale_fill_viridis(direction = -1,option="B",limits=c(0,1))+
  ylab(" ") +
  xlab(" ") + 
  labs(fill = "rescale") + 
  theme(axis.text.x = element_text(size = 11, family = "Gill Sans MT")) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.line = element_blank()) + 
  theme(legend.position = "none") + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Non-Transitioners",family="Gill Sans MT") + 
  theme(plot.title = element_text(hjust = 0.5,size=15,face="plain",family="Gill Sans MT")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plot1


#######################
# Trans No Transition
#######################

TransNoTransition <- data.long[data.long$group == "Transgender" & data.long$transition==0, ]
plot2 <-  ggplot(TransNoTransition, aes(variable, subNum,fill=value )) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "")) +
  geom_tile(color="black", size=0.1) +
  scale_fill_viridis(direction = -1,option="B",limits=c(0,1))+
  ylab(" ") +
  xlab(" ") + 
  labs(fill = "rescale") + 
  theme(axis.text.x = element_text(size = 11, family = "Gill Sans MT")) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.line = element_blank()) + 
  theme(legend.position = "none") + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Transgender",family="Gill Sans MT") + 
  theme(plot.title = element_text(hjust = 0.5,size=15,face="plain",family="Gill Sans MT")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plot2

#######################
# Control No Transition
#######################

ControlNoTransition <- data.long[data.long$group == "Control" & data.long$transition==0, ]
plot3 <-  ggplot(ControlNoTransition, aes(variable, subNum,fill=value )) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "")) +
  geom_tile(color="black", size=0.1) +
  scale_fill_viridis(direction = -1,option="B",limits=c(0,1))+
  ylab(" ") +
  xlab(" ") + 
  labs(fill = "rescale") + 
  theme(axis.text.x = element_text(size = 11, family = "Gill Sans MT")) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.line = element_blank()) + 
  theme(legend.position = "none") + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Opposite-Sex Controls",family="Gill Sans MT") + 
  theme(plot.title = element_text(hjust = 0.5,size=15,face="plain",family="Gill Sans MT")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plot3

#######################
# GNC Transition
#######################

GNCTransition <- data.long[data.long$group == "Gender Nonconforming" & data.long$transition==1, ]
plot4 <-  ggplot(GNCTransition, aes(variable, subNum,fill=value )) +  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "")) +
  geom_tile(color="black", size=0.1) +
  scale_fill_viridis(direction = -1,option="B",limits=c(0,1))+
  ylab(" ") +
  xlab(" ") + 
  labs(fill = "rescale") + 
  theme(plot.background = element_rect(fill = "gray88",colour="grey88")) + #theme_classic() + 
  theme(axis.text.x = element_text(size = 11, family = "Gill Sans MT")) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.line = element_blank()) + 
  theme(legend.position = "none") + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Future Transitioners",family="Gill Sans MT") + 
  theme(plot.title = element_text(hjust = 0.5,size=15,face="plain",family="Gill Sans MT")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plot4

#######################
# Trans No Transition
#######################

TransTransition <- data.long[data.long$group == "Transgender" & data.long$transition==1, ]
plot5 <-  ggplot(TransTransition, aes(variable, subNum,fill=value )) +  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "")) +
  geom_tile(color="black", size=0.1) +
  scale_fill_viridis(direction = -1,option="B",limits=c(0,1))+
  ylab(" ") +
  xlab(" ") + 
  labs(fill = "rescale") + 
  theme(plot.background = element_rect(fill = "gray88",colour="grey88")) + #theme_classic() + 
  theme(axis.text.x = element_text(size = 11, family = "Gill Sans MT")) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.line = element_blank()) + 
  theme(legend.position = "none") + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Transgender",family="Gill Sans MT") + 
  theme(plot.title = element_text(hjust = 0.5,size=15,face="plain",family="Gill Sans MT")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plot5

#######################
# Control No Transition
#######################

ControlTransition <- data.long[data.long$group == "Control" & data.long$transition==1, ]
plot6 <-  ggplot(ControlTransition, aes(variable, subNum,fill=value )) + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "")) +
  geom_tile(color="black", size=0.1) +
  scale_fill_viridis(direction = -1,option="B",limits=c(0,1))+
  ylab(" ") +
  xlab(" ") + 
  labs(fill = "rescale") + 
  theme(plot.background = element_rect(fill = "gray88",colour="grey88")) + #theme_classic() + 
  theme(axis.text.x = element_text(size = 11, family = "Gill Sans MT")) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.line = element_blank()) + 
  theme(legend.position = "none") + 
  theme(axis.text.y = element_blank()) + 
  labs(title = "Opposite-Sex Controls",family="Gill Sans MT") + 
  theme(plot.title = element_text(hjust = 0.5,size=15,face="plain",family="Gill Sans MT")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plot6

#legend

#function to grab legend from a plot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



#re-do plot1 but with legend to grab
plot1_legend <- ggplot(GNCNoTransition, aes(variable, subNum,fill=value )) + 
  geom_tile(color="black", size=0.1) +
  scale_fill_viridis(direction = -1,option="B",limits=c(0,1))+
  ylab(" ") +
  xlab(" ") + 
  labs(fill = "Score") + 
  theme_classic() +
  theme(axis.text.x = element_text(size = 6, family = "Gill Sans MT")) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.line = element_blank()) + 
  theme(legend.direction = "horizontal", legend.position = "bottom")+ 
  theme(legend.key.size =  unit(0.5, "in")) + 
  theme(legend.text = element_text(size=14,family="Gill Sans MT")) + 
  theme(legend.title = element_text(size=16,family="Gill Sans MT"))+
  theme(axis.text.y = element_blank()) + 
  labs(title = "GNC",family="Gill Sans MT") + 
  theme(plot.title = element_text(hjust = 0.5,size = 15,family="Gill Sans MT")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

legend <- get_legend(plot1_legend) #grab legend from plot1

blank <- grid.rect(gp=gpar(col="white")) #if you need blank plots in final grid

#arranging plots into one grid

gt <- arrangeGrob(blank,plot1, plot2, plot3, plot4,plot5,plot6, legend, ncol=3, nrow = 4, 
                  layout_matrix = rbind(c(1,1,1),c(2,3,4), c(5,6,7), c(8,8,8)),
                  widths = c(3.5, 3.5,3.5), heights = c(.1,3.2, 3.2, .82),  top = textGrob(
                    "Comparison of Future-Transitioners to\nNon-Transitioners, Transgender, and Opposite-Sex Controls",
                    gp = gpar(fontface = 2, fontsize = 18,fontfamily="Gill Sans MT",lineheight=.8)
                    #hjust = 1,
                    #x = 1
                  ))

# Add labels to the arranged plots
p <- as_ggplot(gt) +    # transform to a ggplot
  draw_plot_label(label = c("A", "B"),family="Gill Sans MT", size = 18,y=c(.914,.514)) # Add labels

# Make image
png("Figure1.png", family = "Gill Sans MT" ,width = 9, height = 9, units = 'in', res = 1200)
p
dev.off()

ggsave("Figure1.pdf", p, width=9, height=9, 
       device = cairo_pdf, units = "in")


