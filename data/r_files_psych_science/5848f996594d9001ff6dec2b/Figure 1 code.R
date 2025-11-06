# This code generates Figure 1 from the paper.

data <- read.csv('PK extension data repository 12-7.csv', stringsAsFactors = FALSE)
require(reshape2); require(ggplot2); require(plyr)


# Creating a dataset of means and sds which makes plotting in ggplot a lot easier. 
means <- data[, c('condition', 'intent.other.second', 'intent.other.first')]
means <- melt(means, id.vars = 'condition')
means <- group_by(means, condition, variable) %>% 
  summarize_each(funs(mean, sd, length)) %>% 
  as.data.frame()

colnames(means)[5] <- 'N'

means$se <- means$sd / sqrt(means$N)

means$condition <- as.factor(means$condition); means$variable <- as.factor(means$variable)
colnames(means) <- c('Condition', 'Intent_Question', 'Score', 'sd', 'N', 'se')

levels(means$Condition) <- c('PK', 'Reverse', '3', '4')

# Sourcing for this APA theme code for ggplot
# source('https://raw.githubusercontent.com/trinker/plotflow/master/R/theme_apa.R')

# Extrafont may require a bit of work to install, see the documentation online
require(extrafont)

# creating theme_apa function which makes ggplot graphs a bit more apa friendly. 
theme_apa <- function() {
  RMN <- "Times New Roman"
  out <- theme(
    plot.title=element_text(family=RMN, size=12, face="bold", colour="black"),
    axis.title.x=element_text(family=RMN, size=13, colour="black"),
    axis.title.y=element_text(family=RMN, size=13, angle=90, colour="black"),
    axis.text.x=element_text(family=RMN, size=12, colour="black"),
    axis.text.y=element_text(family=RMN, size=10, colour="black"),
    legend.title = element_text(family = 'Times New Roman', size = 13, colour = "black"),
    legend.text = element_text(family = 'Times New Roman', size = 12, colour = "black"),
    axis.ticks=element_line(colour="black")) 
  out <- out + theme(panel.background = element_rect(fill = "white", colour = "white"),
                     panel.border = element_blank(),
                     axis.line.x = element_line(size = .35, colour = 'grey50'),
                     axis.line.y = element_line(size = .35, colour = 'grey50'))
  out
}

# The main plotting code for the pre-reg conditions. First creates a bar plot with error bars. 
ggplot(means[means$Condition %in% c('PK', 'Reverse'),], aes(x = Intent_Question, y = Score, group = Condition, fill = Condition)) +
  geom_bar(stat = 'identity', position = position_dodge(), width = .7, colour = 'black')  +
  geom_errorbar(aes(ymin = Score - se, ymax = Score + se), size = .5, width = .15, position = position_dodge(.7)) +
  # This changes the color scheme to color-blind and black and white friendly
  scale_fill_manual(values = c("#ea484b", "#abd9e9")) +
  # This keeps the graph square and adds labels
  coord_fixed() + theme_apa() + ylab('Sexual Intent') + xlab('Question Order') +
  scale_x_discrete(labels = c('Asked first', 'Asked second')) +
  # The below line changes the y scale to have labels according to PK's -3 to 3 scale, while not changing the underlying data
  scale_y_continuous(breaks = c(4, 4.5, 5.0, 5.5), labels = c('0', '0.5', '1.0', '1.5')) +
  coord_cartesian(ylim=c(4, 5.5)) +
  # This adds the 'Say' and 'Want' labels to overly on the actual bars. 
  annotate("text", x = c(1.17, 1.82), y = c(4.25, 4.25), label = "Want", family = 'Times New Roman', size = 5.2) +
  annotate("text", x = c(.83, 2.18), y = c(4.25, 4.25), label = "Say", family = 'Times New Roman',  size = 5.2) +
  # Just modifying the axis margin widths a little. 
  theme(axis.title.x = element_text(margin = margin(t = 10, b = 10)),
        axis.title.y = element_text(margin = margin(r = 10, l = 10)))


# Some code instead of ggsave that exports as an editable powerpoint. Very useful and works surprisingly
# well, keeping editable elements. 
install_github('tomwenseleers/export', local = F)
require(export)

# This code exports a powerpoint file. 
graph2ppt()

# Alternatively, you can use old-school ggsave and get a pdf, or a png. 
ggsave('Figure 1.pdf', width = 5.5, height = 4.5, units = 'in')
ggsave('Figure 1.png', width = 5.5, height = 4.5, units = 'in')