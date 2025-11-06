# Figures to support the manuscript titled: "Beyond the Shape of Things: Infants Can Be Taught to Generalise Nouns by Function"
# Author of this R script: Dr Suzanne Aussems
# This code creates the following three figures:
# One figure for main analysis (presented in the manuscript)
# One figure for vocabulary analysis (presented in the manuscript)
# One figure (SF4) for percentages of all three choices in a stacked bar chart (in the Supplemental Material)

# Load packages
library(ggplot2)
library(plyr)
library(reshape2)
library(extrafont)

# Load data
df<-read.csv("main_analysis.csv", as.is=TRUE, header=TRUE, fileEncoding = 'UTF-8-BOM')
str(df)

# Create factors
df$Participant<-factor(df$Participant)
df$Group<-factor(df$Group, labels=c("Function-training group", "Control group"))
df$Generalisation<-factor(df$Generalisation, labels=c("First-order generalisation", "Second-order generalisation"))
str(df)

# Summary function

## The following function summarizes the data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Summarize the data

summary1 = summarySE(df, measurevar="Function", groupvars=c("Group", "Generalisation"))
summary1

# Figure 4 - Average proportion function choices by group and generalization task

# Load Times New Foman font
windowsFonts(Times=windowsFont("TT Times New Roman"))

# Set the dodge position for the entire plot
pd <- position_dodge(0.83)

p1 <- ggplot(data = summary1, aes(x = Group, y = Function, fill = Group)) +
  facet_wrap(~Generalisation) +
  # Add the means
  geom_point(data = summary1, aes(x = Group, y = Function, colour = Group, fill = Group, shape = Group),
             position = pd, size = 4.5) +
  # Add the 95% error bars around the means 
  geom_errorbar(data = summary1, aes(ymin = Function - ci, ymax = Function + ci, col =
                                            Group), position = pd, width = 0.10, cex = 1.25) +
  # Individual dots for performances of each participant
  geom_point(data = df, aes(group = Participant, color = Group, fill = Group, shape = Group), size = 2.5, 
             alpha=0.50, position=pd) +
  # Set the limits of the y-axis
  scale_y_continuous("Average percentage (%) function choices in test trials", limits=c(0,100), 
                     breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  # Set the blue and red colours
  scale_color_manual(values=c("midnightblue", "tomato2")) +
  # Label the x-axis
  scale_x_discrete("Group") +
  geom_hline(col = "grey50", lty=2, yintercept=33.33, cex=1) +
  # Formatting the graph background (white panels, legend position)
  theme(legend.title = element_blank(), 
        legend.position="top", 
        legend.key = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        axis.line = element_line(colour = "black")) +
  # Font type and size
  theme(axis.text = element_text(family="Times", size = 16, colour="black"),
        axis.title = element_text(family="Times", size=20),
        text = element_text(family="Times", size = 20))

# Save plot as high resolution PNG
png(filename = "Main_analysis.png", 
   width = 10, height = 7, units = "in", res = 300)
p1
dev.off()

# Figure 5 - Productive vocabulary by group, time, and word type

# Load data
df2<-read.csv("vocabulary_analysis.csv", as.is=TRUE, header=TRUE, fileEncoding = 'UTF-8-BOM')
str(df2)

# Create factors
df2$Participant<-factor(df2$Participant)
df2$Group<-factor(df2$Group, labels=c("Function-training group", "Control group"))
df2$Time<-factor(df2$Time, labels=c("Start of the study", "End of the study"))
df2$Word_type<-factor(df2$Word_type)
str(df2)

# Summarize the data

summary2 = summarySE(df2, measurevar="Production", groupvars=c("Group", "Time", "Word_type"))
summary2

p2 <- ggplot(data = summary2, aes(x = Time, y = Production)) +
  facet_wrap(~Word_type) +
  # Add the means
  geom_point(data = summary2, aes(x = Time, y = Production, colour = Group, shape = Group),
             position = pd, size = 4.5) +
  # Connect the means with lines
  geom_line(data = summary2, aes(x = Time, y = Production, col = Group, group=Group),
            position = pd, cex = 1.15) +
  # Add the 95% error bars around the means 
  geom_errorbar(data = summary2, aes(ymin = Production - ci, ymax = Production + ci, col =
                                            Group), position = pd, width = 0.10, cex = 1.15) +
  # Individual dots for performances of each participant
  geom_point(data = df2, aes(group = Participant, color = Group, shape = Group), size = 2.5, 
             alpha=0.4, position=pd) +
  # Individual lines between total scores of each participant
  geom_line(data = df2, aes(group = Participant, color = Group), size = 0.5, alpha=0.4, 
             position=pd) +
  scale_y_continuous("Average number of words in expressive vocabulary") +
  # Set the blue and red colours
  scale_color_manual(values=c("midnightblue", "tomato2")) +
  # Formatting the graph background (white panels, legend position)
  theme(legend.title = element_blank(), 
        legend.position="top", 
        legend.key = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        axis.line = element_line(colour = "black")) +
  # Font type and size
  theme(axis.text = element_text(family="Times", size = 16, colour="black"),
        axis.title = element_text(family="Times", size=20),
        text = element_text(family="Times", size = 20))

# Save plot as high resolution PNG
png(filename = "Vocabulary_analysis.png", 
    width = 10, height = 7, units = "in", res = 300)
p2
dev.off()

# Stacked bar chart

# Figure in Supplementary Material - Stacked bar chart of function, shape, and colour choices by group and generalisation test

# Load data
df3<-read.csv("supplement_stacked_barchart.csv", as.is=TRUE, header=TRUE, fileEncoding = 'UTF-8-BOM')
str(df3)

# Create factors
df3$Participant<-factor(df3$Participant)
df3$Group<-factor(df3$Group, labels=c("Function-training group", "Control group"))
df3$Choice<-factor(df3$Choice, levels=c("Colour", "Shape", "Function"))
df3$Generalisation<-factor(df3$Generalisation, labels=c("First-order generalisation", "Second-order generalisation"))
str(df3)

# Summarize the data

summary3 = summarySE(df3, measurevar="Percentage", groupvars=c("Group", "Choice", "Generalisation"))
summary3

# Stacked bar chart
p3 <- ggplot(summary3, aes(fill=Choice, y=Percentage, x=Group)) + 
  # Create panels by generalisation task
  facet_wrap(~Generalisation) +
  # Create the stacked bar chart
  geom_bar(position="stack", stat="identity")+
  # Label the y-axis with the dependent variable
  scale_y_continuous("Percentage of choices") +
  # Set the colours of the bars
  scale_fill_manual(values=c("pink", "tomato2", "midnightblue"), name="Choice")+
  # Formatting the graph background (white panels, legend position)
  theme(legend.title = element_blank(), 
        legend.position="top", 
        legend.key = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        axis.line = element_line(colour = "black")) +
  # Font type and size
  theme(axis.text = element_text(family="Times", size = 16, colour="black"),
        axis.title = element_text(family="Times", size=20),
        text = element_text(family="Times", size = 20))
p3

# Save plot as high resolution PNG
png(filename = "Stacked_barchart_Supplemental_Material.png", 
    width = 10, height = 7, units = "in", res = 300)
p3
dev.off()
