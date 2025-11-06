install.packages("afex")
install.packages("psych")
install.packages("ggplot2")
library(afex)
library(psych)
library(ggplot2)

PP_data <- read.table("Dataset_Replicaton_Study4b_PactonPerruchet_2008_JEPLMC.txt", header=TRUE, sep="\t")
PP_data$targets <- ifelse(PP_data$targets == 29, c("029"), c("158"))

# Prepare aggregated dataset for confirmatory analysis
attach(PP_data)
PP_data_aggr_dependency <- aggregate(PP_data, by=list(Subject, digit_combination, targets, dependency), FUN=mean)
detach(PP_data)
PP_data_aggr_dependency <- PP_data_aggr_dependency[c(1:4,20)]
names(PP_data_aggr_dependency) <- c("Subject","digit_combination", "targets", "dependency", "Recognition.ACC" )
PP_data_aggr_dependency$Recognition.ACC <- PP_data_aggr_dependency$Recognition.ACC * 100

# Confirmatory analysis
model.aov_dep <- aov.car(Recognition.ACC ~ digit_combination*targets + Error(Subject/dependency), data = PP_data_aggr_dependency, return = "Anova")
nice.anova(model.aov_dep, es = "pes")

describeBy(PP_data_aggr_dependency, PP_data_aggr_dependency$dependency)

with(PP_data_aggr_dependency, t.test(Recognition.ACC[dependency == "non_adj_dep_test"], mu = 50))
with(PP_data_aggr_dependency, t.test(Recognition.ACC[dependency == "adjacent_dep_test"], mu = 50))

# Prepare aggregated dataset for exploratory analyses
attach(PP_data)
PP_data_aggr_digitx <- aggregate(PP_data, by=list(Subject, digit_combination, targets, dependency, digitx), FUN=mean)
detach(PP_data)
PP_data_aggr_digitx <- PP_data_aggr_digitx[which(PP_data_aggr_digitx$Group.4 == "non_adj_dep_test"),]
PP_data_aggr_digitx <- PP_data_aggr_digitx[c(1:5,21)]
names(PP_data_aggr_digitx) <- c("Subject","digit_combination", "targets", "dependency", "digitx", "Recognition.ACC" )
PP_data_aggr_digitx$Recognition.ACC <- PP_data_aggr_digitx$Recognition.ACC * 100

# Exploratory analyses
model.aov_digitx <- aov.car(Recognition.ACC ~ digit_combination*targets + Error(Subject/digitx), data = PP_data_aggr_digitx, return = "Anova")
nice.anova(model.aov_digitx, es = "pes")

describeBy(PP_data_aggr_digitx, PP_data_aggr_digitx$digitx)

with(PP_data_aggr_digitx, t.test(Recognition.ACC[digitx == "familiar"], mu = 50))
with(PP_data_aggr_digitx, t.test(Recognition.ACC[digitx == "transfer"], mu = 50))

# Plot of adjacent vs. non-adjacent
attach(PP_data_aggr_dependency)
plot_data_dependency <- aggregate(PP_data_aggr_dependency, by = list(digit_combination, targets, dependency), FUN = mean)
detach(PP_data_aggr_dependency)
plot_data_dependency$barlabels <- paste(plot_data_dependency$Group.1, plot_data_dependency$Group.2)
ggplot(data=plot_data_dependency, aes(x=barlabels, y=Recognition.ACC)) + geom_bar(stat="identity", position=position_dodge()) + facet_grid(~Group.3)

# Plot of familiar vs. transfer for non-adjacent only
attach(PP_data_aggr_digitx)
plot_data_digitx <- aggregate(PP_data_aggr_digitx, by = list(digit_combination, targets, digitx), FUN = mean)
detach(PP_data_aggr_digitx)
plot_data_digitx$barlabels <- paste(plot_data_digitx$Group.1, plot_data_digitx$Group.2)
ggplot(data=plot_data_digitx, aes(x=barlabels, y=Recognition.ACC)) + geom_bar(stat="identity", position=position_dodge()) + facet_grid(~Group.3)

