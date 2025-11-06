library(Rmisc)
library(ggplot2)
library(reshape)
library(ggplot2)
library(lsmeans)
library(Hmisc)

### LOAD PACKAGES AND DATA 

#input.1 = "C:/Users/mluhmann/Dropbox/SOEP_fit"
input.1 = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data"

setwd(input.1)

#decide between: 2005 data, or combined dataset

valid <- read.delim("valid.csv")

# scale Big Five variables (to make them more conductive to RSA logic)
names(valid)
big5_scale <- as.data.frame(scale(valid[,20:29]))
colnames(big5_scale) <- paste(colnames(big5_scale), ".c", sep = "")
valid <- cbind(valid, big5_scale)

# check scaling
summary(valid[,31:40])

table(valid$east)

#####################################
## a1 and a2
#####################################

# create groups of three parts
# note: graph as sent to group created with cut-off = 1 SD

#remove incomes above 100.000 euro (outliers that bias the means too much)
valid[which(valid$income>100000),"income"] <- 100000

##Extraversion

valid$per_e_grp <- cut(valid$per_e.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$ba2_e_grp <- cut(valid$ba2_e.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$e_fit <- paste(valid$per_e_grp, valid$ba2_e_grp, sep = "")

e_fit <- valid[which(valid$e_fit=="LL" | valid$e_fit=="MM" | valid$e_fit=="HH"),]

table(e_fit$e_fit)

aggregate(e_fit$income, by=list(e_fit$e_fit), FUN=mean)
aggregate(e_fit$loginc.res, by=list(e_fit$e_fit), FUN=mean)

# regression that computes marginal means for groups
fit <- lm(income ~ e_fit + east + female + age.c + age2 + educat.c + educat2 + unmarried + experience + wrkhrs, data=e_fit)
summary(fit)
lsmeans(fit, "e_fit")

e_marmeans <- as.data.frame(cbind(summary(lsmeans (fit, "e_fit"))[1:3]))

##Agreeableness

valid$per_a_grp <- cut(valid$per_a.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$ba2_a_grp <- cut(valid$ba2_a.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$a_fit <- paste(valid$per_a_grp, valid$ba2_a_grp, sep = "")

a_fit <- valid[which(valid$a_fit=="LL" | valid$a_fit=="MM" | valid$a_fit=="HH"),]

table(a_fit$a_fit)

aggregate(a_fit$income, by=list(a_fit$a_fit), FUN=mean)
aggregate(a_fit$loginc.res, by=list(a_fit$a_fit), FUN=mean)

# regression that computes marginal means for groups
fit <- lm(income ~ a_fit + east + female + age.c + age2 + educat.c + educat2 + unmarried + experience + wrkhrs, data=a_fit)
summary(fit)
lsmeans (fit, "a_fit")

a_marmeans <- as.data.frame(cbind(summary(lsmeans (fit, "a_fit"))[1:3]))

##Conscientiousness

valid$per_c_grp <- cut(valid$per_c.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$ba2_c_grp <- cut(valid$ba2_c.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$c_fit <- paste(valid$per_c_grp, valid$ba2_c_grp, sep = "")

c_fit <- valid[which(valid$c_fit=="LL" | valid$c_fit=="MM" | valid$c_fit=="HH"),]

table(c_fit$c_fit)

aggregate(c_fit$income, by=list(c_fit$c_fit), FUN=mean)
aggregate(c_fit$loginc.res, by=list(c_fit$c_fit), FUN=mean)



# regression that computes marginal means for groups
fit <- lm(income ~ c_fit + east + female + age.c + age2 + educat.c + educat2 + unmarried + experience + wrkhrs, data=c_fit)
summary(fit)
lsmeans (fit, "c_fit")

c_marmeans <- as.data.frame(cbind(summary(lsmeans (fit, "c_fit"))[1:3]))


##Stability

valid$per_s_grp <- cut(valid$per_s.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$ba2_s_grp <- cut(valid$ba2_s.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$s_fit <- paste(valid$per_s_grp, valid$ba2_s_grp, sep = "")

s_fit <- valid[which(valid$s_fit=="LL" | valid$s_fit=="MM" | valid$s_fit=="HH"),]

table(s_fit$s_fit)

aggregate(s_fit$income, by=list(s_fit$s_fit), FUN=mean)
aggregate(s_fit$loginc.res, by=list(s_fit$s_fit), FUN=mean)

# regression that computes marginal means for groups
fit <- lm(income ~ s_fit + east + female + age.c + age2 + educat.c + educat2 + unmarried + experience + wrkhrs, data=s_fit)
summary(fit)
lsmeans (fit, "s_fit")

s_marmeans <- as.data.frame(cbind(summary(lsmeans (fit, "s_fit"))[1:3]))

##Openness

valid$per_o_grp <- cut(valid$per_o.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$ba2_o_grp <- cut(valid$ba2_o.c, 
                           breaks = c(-Inf, -1, +1, Inf), 
                           labels = c("L", "M", "H"), 
                           right = FALSE)

valid$o_fit <- paste(valid$per_o_grp, valid$ba2_o_grp, sep = "")

o_fit <- valid[which(valid$o_fit=="LL" | valid$o_fit=="MM" | valid$o_fit=="HH"),]

table(o_fit$o_fit)

aggregate(o_fit$income, by=list(o_fit$o_fit), FUN=mean)
aggregate(o_fit$loginc.res, by=list(o_fit$o_fit), FUN=mean)

# regression that computes marginal means for groups
fit <- lm(income ~ o_fit + east + female + age.c + age2 + educat.c + educat2 + unmarried + experience + wrkhrs, data=o_fit)
summary(fit)
lsmeans (fit, "o_fit")

o_marmeans <- as.data.frame(cbind(summary(lsmeans (fit, "o_fit"))[1:3]))

## merge results

names(e_marmeans)[2] <- "lsmean_e"
names(a_marmeans)[2] <- "lsmean_a"
names(c_marmeans)[2] <- "lsmean_c"
names(s_marmeans)[2] <- "lsmean_s"
names(o_marmeans)[2] <- "lsmean_o"

names(e_marmeans)[3] <- "SE_e"
names(a_marmeans)[3] <- "SE_a"
names(c_marmeans)[3] <- "SE_c"
names(s_marmeans)[3] <- "SE_s"
names(o_marmeans)[3] <- "SE_o"

marmeans <- cbind(e_marmeans, a_marmeans[,2:3], c_marmeans[,2:3], s_marmeans[,2:3], o_marmeans[,2:3])
names(marmeans)[1] <- "group"

molten <- melt(marmeans)
tmp <- strsplit(as.character(molten$variable), "_")
molten <- cbind(molten, do.call(rbind, lapply(tmp, rbind)))
names(molten)[4] <- "stat"
names(molten)[5] <- "trait"

cast <-   cast(molten, group+trait~stat, fun=mean)

cast$person <- substr(cast$group, 0,1)
cast$PE_combination <- 0
cast[which(cast$group=="MM"),"PE_combination"] <- +0.5
cast[which(cast$group=="HH"),"PE_combination"] <- +1.0
cast$trait.avg <- ave(cast$lsmean, cast$trait, FUN=mean)

names(cast)[2] <- "Trait"
names(cast)[3] <- "Income"

ymin <- quantile(valid$income)[[2]]
ymax <- quantile(valid$income)[[4]]

#note: ymax doesn't work here, because HH_c exceed this value

#smoothed
#ggplot(cast,aes(y = Income, x = PE_combination)) +
#  geom_point(aes(colour= Trait, shape= Trait), size = 3, alpha = .8) + 
#  geom_smooth(method="loess", se= F, size = 1, aes(colour = Trait, group = Trait)) +
#  scale_x_continuous(breaks=seq(-1, 1, .5)) + ylim(c(ymin, 50000))+
#  theme_bw()

#straight lines
ggplot(cast,aes(y = Income, x = PE_combination)) +
  geom_point(aes(colour= Trait, shape= Trait), size = 3, alpha = .8) + 
  geom_line(size = 1, aes(colour = Trait, group = Trait)) +
  scale_x_continuous(breaks=seq(-1, 1, .5)) + ylim(c(ymin, 50000))+
  theme_bw() 

cast$group = factor(cast$group,levels(cast$group)[c(2, 3, 1)])
cast$Trait = factor(cast$Trait,levels(cast$Trait)[c(3, 1, 2, 5, 4)])
levels(cast$Trait) <- c("Extraversion", "Agreeableness", "Conscientiousness", "Emot. Stability", "Openness")

ggplot(cast, aes(y=Income, x=group, group=Trait)) +
  geom_line(aes(linetype=Trait, color=Trait))+
  geom_point(aes(color=Trait))+ theme(legend.position="left")+ ylim(c(30000, 50000))+
  theme_bw() +labs(title="Line of Congruence",
        x ="Combination", y = "Income (adjusted)")

cast2 <-   cast(cast[,1:3], Trait~group, fun=mean)

write.csv(cast2, file = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Results/fitted_a1-2.csv")