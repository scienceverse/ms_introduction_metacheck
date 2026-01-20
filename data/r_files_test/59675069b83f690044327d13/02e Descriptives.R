library(foreign)
library(psych)
library(reshape2)
library(ggplot2)

### create list of jobs and merge with ba2_c ratings

ratings <- read.delim("~/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data/ba2_ratings.csv")
valid <- read.delim("~/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data/valid.csv")

table (valid$female)
describe(valid$age)
describe(valid$income)
describe(valid[,20:24])
describe(valid[,c(9, 12, 16, 18)])
mardia(valid$income)

molten <- melt(valid[,c(1, 20:29)], id=c("persnr"))

ggplot(molten[grep("_e", molten$variable),], aes(x = value, fill = variable)) + geom_density(alpha = 0.5) + scale_x_continuous(breaks=seq(0,7,1)) + scale_fill_discrete(name = "Construct", labels=c("Actual Personality", "Demanded Personality")) + theme(panel.grid.major = element_blank() ) + ggtitle("Extraversion")

ggplot(molten[grep("_a", molten$variable),], aes(x = value, fill = variable)) + geom_density(alpha = 0.5) + scale_x_continuous(breaks=seq(0,7,1)) + scale_fill_discrete(name = "Construct", labels=c("Actual Personality", "Demanded Personality")) + theme(panel.grid.major = element_blank() ) + ggtitle("Agreeableness")

ggplot(molten[grep("_c", molten$variable),], aes(x = value, fill = variable)) + geom_density(alpha = 0.5) + scale_x_continuous(breaks=seq(0,7,1)) + scale_fill_discrete(name = "Construct", labels=c("Actual Personality", "Demanded Personality")) + theme(panel.grid.major = element_blank() ) + ggtitle("Conscientiousness")

ggplot(molten[grep("_s", molten$variable),], aes(x = value, fill = variable)) + geom_density(alpha = 0.5) + scale_x_continuous(breaks=seq(0,7,1)) + scale_fill_discrete(name = "Construct", labels=c("Actual Personality", "Demanded Personality")) + theme(panel.grid.major = element_blank() ) + ggtitle("Emotional Stability")

ggplot(molten[grep("_o", molten$variable),], aes(x = value, fill = variable)) + geom_density(alpha = 0.5) + scale_x_continuous(breaks=seq(0,7,1)) + scale_fill_discrete(name = "Construct", labels=c("Actual Personality", "Demanded Personality")) + theme(panel.grid.major = element_blank() ) + ggtitle("Openness to Experience")

#greyscale of Figure 1, to avoid additional costs
ggplot(molten[grep("_o", molten$variable),], aes(x = value, fill = variable)) + geom_density(alpha = 0.5) + scale_x_continuous(breaks=seq(0,7,1)) + scale_fill_discrete(name = "Construct", labels=c("Actual Personality", "Demanded Personality")) + theme(panel.grid.major = element_blank() ) + ggtitle("Openness to Experience") + theme_bw() + scale_fill_grey(start = 0, end = .9)


library(overlap)
overlapEst(valid$ba2_e, valid$per_e)
overlapEst(valid$ba2_a, valid$per_a)
overlapEst(valid$ba2_c, valid$per_c)
overlapEst(valid$ba2_s, valid$per_s)
overlapEst(valid$ba2_o, valid$per_o)

# scale Big Five variables (to make them more conductive to RSA logic)
names(valid)
big5_scale <- as.data.frame(scale(valid[,20:29]))
colnames(big5_scale) <- paste(colnames(big5_scale), ".c", sep = "")
valid <- cbind(valid, big5_scale)

#define what corresponds to axis levels of RSA plots
valid$income_axis <- NA
valid[which(valid$loginc.res > 1), "income_axis"] <- "level.plus10"
valid[which(valid$loginc.res<.41 & valid$loginc.res > .39), "income_axis"] <- "level.plus04"
valid[which(valid$loginc.res<.21 & valid$loginc.res > .19), "income_axis"] <- "level.plus02"
valid[which(valid$loginc.res<.01 & valid$loginc.res > -.01), "income_axis"] <- "level.zero"
valid[which(valid$loginc.res<(-.19) & valid$loginc.res > (-.21)), "income_axis"] <- "level.minus02"
valid[which(valid$loginc.res<(-.39) & valid$loginc.res > (-.41)), "income_axis"] <- "level.minus04"
valid[which(valid$loginc.res< (-1)), "income_axis"] <- "level.minus10"
table(valid$income_axis)

aggregate(valid$income, by=list(valid$income_axis), FUN=mean)