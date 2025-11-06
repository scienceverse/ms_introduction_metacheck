## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Study 4: Analyses

## Set working directory
if (!require("rstudioapi")) {install.packages("rstudioapi"); require("rstudioapi")}
currentPath <- getActiveDocumentContext()$path
setwd(dir = dirname(path = currentPath))

## Packages needed
if (!require("plyr")) {install.packages("plyr"); require("plyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("scales")) {install.packages("scales"); require("scales")}
if (!require("grid")) {install.packages("grid"); require("grid")}
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("ggrepel")) {install.packages("ggrepel"); require("ggrepel")}
if (!require("nlme")) {install.packages("nlme"); require("nlme")}
if (!require("lme4")) {install.packages("lme4"); require("lme4")}
if (!require("effects")) {install.packages("effects"); require("effects")}
if (!require("multcomp")) {install.packages("multcomp"); require("multcomp")}
if (!require("lsr")) {install.packages("lsr"); require("lsr")}
if (!require("car")) {install.packages("car"); require("car")}
if (!require("lsmeans")) {install.packages("lsmeans"); require("lsmeans")}
if (!require("cowplot")) {install.packages("cowplot"); require("cowplot")}
if (!require("boot")) {install.packages("boot"); require("boot")}
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}

## Load data
load("dat.wide.Rda")

## Examine structure and summary
str(dat.wide)

## How many participants admitted to looking up information?
plyr::count(dat.wide$lookup)
  ## Only 2

## Identify participants whose prior cannot be updated
plyr::count(dat.wide$Prior.Woman.Doctor)
  ## 3 people's beliefs cannot be updated

dat.wide$able.to.update <- ifelse(test = dat.wide$Prior.Woman.Doctor == 0, 
                                  yes = "Can't", 
                                  no = "Can")

dat.wide$able.to.update <- factor(dat.wide$able.to.update, 
                                  levels = c("Can't", "Can"))

plyr::count(dat.wide$able.to.update)

## Identify participants to exclude
dat.wide$exclude <- ifelse(test = dat.wide$able.to.update == "Can't" | dat.wide$lookup == "Yes", 
                           yes = "Exclude", 
                           no = "Include")

dat.wide$exclude <- factor(x = dat.wide$exclude, 
                           levels = c("Exclude", "Include"))

plyr::count(dat.wide$exclude)

## Exclude participants (keep the two participants who looked stuff up)
dat.wide <- subset(dat.wide, able.to.update == "Can")

## Demographics
plyr::count(dat.wide$gender)
  ## 528 males
  ## 364 females
  ## 4 unspecified
round(x = mean(dat.wide$Age, na.rm = TRUE), digits = 2)
round(x = sd(dat.wide$Age, na.rm = TRUE), digits = 2)

## ======================================================================= ##
## Prior beliefs

## Percentage of participants whose priors were <50%, =50%, or >50%
dat.wide$Prior.Man.Doctor.Discrete <- ifelse(test = dat.wide$Prior.Man.Doctor < .50, 
                                             yes = "Favor woman as doctor", 
                                             no = ifelse(test = dat.wide$Prior.Man.Doctor == .50, 
                                                         yes = "Favor both individuals equally", 
                                                         no = "Favor man as doctor"))

dat.wide$Prior.Man.Doctor.Discrete <- factor(dat.wide$Prior.Man.Doctor.Discrete, 
                                             levels = c("Favor woman as doctor", "Favor both individuals equally", "Favor man as doctor"))

count(dat.wide$Prior.Man.Doctor.Discrete)

## Ensure that every participant's prior beliefs sum to 1
count(dat.wide$Prior.Woman.Doctor + dat.wide$Prior.Man.Doctor)

## Distribution of prior beliefs, binned by decile
  ## Include only those participants who can update their beliefs and those who didn't look up information
ggplot(data = subset(dat.wide, exclude == "Include"), 
       mapping = aes(x = Prior.Man.Doctor)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),
           binwidth = 0.05, breaks = seq(-0.025, 1.025, 0.05),
           col = "black" , fill = "#F8766D") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.2)) + 
  scale_x_continuous(breaks = seq(0, 1, .1), label = percent_format()) + 
  scale_y_continuous(breaks = seq(0, .2, .05), label = percent_format()) + 
  labs(x = "\nPerentage chance that Man = Doctor\n(Woman != Doctor)", 
       y = "Proportion of participants\n") + 
  theme_bw(base_size = 24) + 
  theme(aspect.ratio = 1) + theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black"))

## ================================================================================================== ##
## ======================================================== ##
## ======================================================== ##
## Log likelihood ratios

## For subsetting purposes
dat.wide$condition <- interaction(dat.wide$Target, dat.wide$Data)

## Visualize likelihoods
colnames(dat.wide)

## Variables with likelihoods
likelihood.variables <- c("Percentage.Male.Doctors.Surgery", "Percentage.Male.Nurses.Surgery", 
                          "Percentage.Male.Doctors.CPR", "Percentage.Male.Nurses.CPR", 
                          "Percentage.Male.Doctors.SpongeBath", "Percentage.Male.Nurses.SpongeBath", 
                          "Percentage.Female.Doctors.Surgery", "Percentage.Female.Nurses.Surgery", 
                          "Percentage.Female.Doctors.CPR", "Percentage.Female.Nurses.CPR", 
                          "Percentage.Female.Doctors.SpongeBath", "Percentage.Female.Nurses.SpongeBath")

## 12 likelihood questions; each participant answers two questions
colnames(dat.wide)
likelihood.dat.wide <- dat.wide[,c("QualtricsID", "Target", "Data", "Order", "condition", "exclude",
                                   likelihood.variables)]


colnames(likelihood.dat.wide)

## LR: Doctor in numerator, Nurse in denominator
likelihood.dat.wide$man.surgery.LR <- with(likelihood.dat.wide, Percentage.Male.Doctors.Surgery / Percentage.Male.Nurses.Surgery)
likelihood.dat.wide$man.CPR.LR <- with(likelihood.dat.wide, Percentage.Male.Doctors.CPR / Percentage.Male.Nurses.CPR)
likelihood.dat.wide$man.SpongeBath.LR <- with(likelihood.dat.wide, Percentage.Male.Doctors.SpongeBath / Percentage.Male.Nurses.SpongeBath)

likelihood.dat.wide$woman.surgery.LR <- with(likelihood.dat.wide, Percentage.Female.Doctors.Surgery / Percentage.Female.Nurses.Surgery)
likelihood.dat.wide$woman.CPR.LR <- with(likelihood.dat.wide, Percentage.Female.Doctors.CPR / Percentage.Female.Nurses.CPR)
likelihood.dat.wide$woman.SpongeBath.LR <- with(likelihood.dat.wide, Percentage.Female.Doctors.SpongeBath / Percentage.Female.Nurses.SpongeBath)

## LR.long
colnames(likelihood.dat.wide)

LR.long <- melt(data = likelihood.dat.wide[,c("QualtricsID", "exclude", "Target", "Data", "Order", "condition", 
                                              "man.surgery.LR", "man.CPR.LR", "man.SpongeBath.LR", 
                                              "woman.surgery.LR", "woman.CPR.LR", "woman.SpongeBath.LR")], 
                measure.vars = c("man.surgery.LR", "man.CPR.LR", "man.SpongeBath.LR", 
                                 "woman.surgery.LR", "woman.CPR.LR", "woman.SpongeBath.LR"))

LR.long <- subset(x = LR.long, is.na(LR.long$value) == FALSE)

## Do data overwhelm prior completely?
LR.long$overwhelm.prior <- ifelse(test = LR.long$value == 0, 
                                  yes = "Data entirely diagnostic of nurse", 
                                  no = ifelse(test = is.infinite(LR.long$value) == TRUE, 
                                              yes = "Data entirely diagnostic of doctor", 
                                              no = "Data not entirely diagnostic"))

LR.long$overwhelm.prior <- factor(LR.long$overwhelm.prior, 
                                  levels = c("Data entirely diagnostic of nurse", 
                                             "Data not entirely diagnostic", 
                                             "Data entirely diagnostic of doctor"))

tapply(X = LR.long$overwhelm.prior, 
       INDEX = LR.long$condition, 
       FUN = count)

## Create LR for plotting purposes
## Infinity = 10,000
## Zero = 1/10,000

LR.long$value.plotting <- ifelse(test = is.infinite(LR.long$value) == TRUE, 
                                 yes = 10000, 
                                 no = ifelse(test = LR.long$value == 0, 
                                             yes = 1/10000, 
                                             no = LR.long$value))

## Create condition for plotting purposes
LR.long$condition.plotting <- NA
levels(LR.long$condition)

LR.long[grepl("^Man.CPR$", LR.long$condition), "condition.plotting"] <- 1
LR.long[grepl("^Woman.CPR$", LR.long$condition), "condition.plotting"] <- 2
LR.long[grepl("^Man.SpongeBath$", LR.long$condition), "condition.plotting"] <- 3
LR.long[grepl("^Woman.SpongeBath$", LR.long$condition), "condition.plotting"] <- 4
LR.long[grepl("^Man.Surgery$", LR.long$condition), "condition.plotting"] <- 5
LR.long[grepl("^Woman.Surgery", LR.long$condition), "condition.plotting"] <- 6

count(LR.long$condition)

LR.long$condition.plotting <- factor(x = LR.long$condition.plotting, 
                                     levels = c(1:6), 
                                     labels = c("Man performed CPR\nn = 139", 
                                                "Woman performed CPR\nn = 161", 
                                                "Man gave a sponge bath\nn = 158", 
                                                "Woman gave a sponge bath\nn = 144", 
                                                "Man performed surgery\nn = 152", 
                                                "Woman performed surgery\nn = 142"))

## Confirm that participants in each condition sum to 896
139+161+158+144+152+142

## Relevel for plotting purposes
LR.long$condition.plotting <- factor(LR.long$condition.plotting, 
                                     levels = c("Man performed CPR\nn = 139",
                                                "Man gave a sponge bath\nn = 158", 
                                                "Man performed surgery\nn = 152", 
                                                "Woman performed CPR\nn = 161", 
                                                "Woman gave a sponge bath\nn = 144", 
                                                "Woman performed surgery\nn = 142"))


## Plot
ggplot(data = LR.long, 
       mapping = aes(x = log(value.plotting), fill = overwhelm.prior)) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 0.4)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.40, 0.10), 
                     label = percent_format()) + 
  scale_fill_manual(values = c("white", "darkgrey", "black")) + 
  labs(x = "\nLog Likelihood Ratio", y = "Proportion of participants (in each condition)\n") + 
  theme_bw(base_size = 14) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

tapply(X = LR.long$value, 
       INDEX = LR.long$condition.plotting, 
       FUN = count)

## What proportion said infinity?
count(LR.long$condition)
  58/142 ## Woman-surgery
  44/152 ## Man-surgery

  ## ================================================================================================== ##
## Inferential statistics for likeliood ratios

## Comparisons between genders

## Medians
round(x = tapply(X = log(LR.long$value), 
                 INDEX = LR.long$condition.plotting, 
                 FUN = median), 
      digits = 2)
  
  ## Man vs. Woman - surgery
wilcox.test(log(value) ~ variable, 
            data = droplevels(subset(LR.long, condition == "Man.Surgery" | condition == "Woman.Surgery")))
  ## Wilcoxon P = 0.19
  ## Effect size, Pearson's r = 0.08
qnorm(wilcox.test(log(value) ~ variable, 
                  data = droplevels(subset(LR.long, condition == "Man.Surgery" | condition == "Woman.Surgery")))$p.value/2) / sqrt(152+142)

  ## Man vs. Woman - sponge bath
wilcox.test(log(value) ~ variable, 
            data = droplevels(subset(LR.long, condition == "Man.SpongeBath" | condition == "Woman.SpongeBath"))) 
  ## Wilcoxon P = 0.05
  ## Effect size, Pearson's r = 0.11
qnorm(wilcox.test(log(value) ~ variable, 
                  data = droplevels(subset(LR.long, condition == "Man.SpongeBath" | condition == "Woman.SpongeBath")))$p.value/2) / sqrt(158+144)

  ## Man vs. Woman - CPR
wilcox.test(log(value) ~ variable, 
            data = droplevels(subset(LR.long, condition == "Man.CPR" | condition == "Woman.CPR"))) 
  ## Wilcoxon P = 0.66
  ## Effect size, Pearson's r = 0.03
qnorm(wilcox.test(log(value) ~ variable, 
                  data = droplevels(subset(LR.long, condition == "Man.CPR" | condition == "Woman.CPR")))$p.value/2) / sqrt(139+161)

  
## ================================================================================================== ##
## Calculate model posterior; compare to reported posterior

## Create new dataframe
prior.model.reported <- dat.wide[,c("QualtricsID", "condition", "Target", "Data", "lookup", "able.to.update","exclude", 
                                    "Prior.Man.Doctor", "Prior.Woman.Doctor",
                                    "Posterior.Man.Doctor", "Posterior.Woman.Doctor")]

## Delete comments here and below
## Subset out only the participants to keep
## prior.model.reported <- subset(prior.model.reported, exclude == "Include")
## Delete comments here and above

## Create prior.odds.ratio: divide differently for when target is man vs. woman
prior.model.reported$prior.odds.ratio <- ifelse(test = prior.model.reported$Target == "Man", 
                                                yes = prior.model.reported$Prior.Man.Doctor / prior.model.reported$Prior.Woman.Doctor, 
                                                no = prior.model.reported$Prior.Woman.Doctor / prior.model.reported$Prior.Man.Doctor)


## Create posterior.odds.ratio: divide differently for when target is man vs. woman
prior.model.reported$posterior.odds.ratio <- ifelse(test = prior.model.reported$Target == "Man", 
                                                    yes = prior.model.reported$Posterior.Man.Doctor / prior.model.reported$Posterior.Woman.Doctor, 
                                                    no = prior.model.reported$Posterior.Woman.Doctor / prior.model.reported$Posterior.Man.Doctor)

## Create model.posterior.odds.ratio
## Merge with likelihood ratios
LR.merge <- LR.long[,c("QualtricsID", "value")]
colnames(LR.merge)[2] <- "LR"

prior.model.reported <- merge(x = prior.model.reported, 
                              y = LR.merge, 
                              by = "QualtricsID")

## Rearrange columns
colnames(prior.model.reported)

prior.model.reported <- prior.model.reported[,c("QualtricsID",
                                                "Target", "Data", "condition", 
                                                "Prior.Man.Doctor", "Prior.Woman.Doctor", 
                                                "Posterior.Man.Doctor", "Posterior.Woman.Doctor", 
                                                "prior.odds.ratio", "LR", "posterior.odds.ratio")]

## Calculate model.posterior.odds.ratio
prior.model.reported$model.posterior.odds.ratio <- with(prior.model.reported, 
                                                        prior.odds.ratio * LR)

## Convert the following to percent format:
## prior.odds.ratio
## posterior.odds.ratio
## model.posterior.odds.ratio

## prior.odds.ratio --> percent format
prior.model.reported$prior_percent <- with(prior.model.reported, prior.odds.ratio / (1 + prior.odds.ratio) )

tapply(X = prior.model.reported$prior_percent, 
       INDEX = prior.model.reported$condition, 
       FUN = mean)

## posterior.odds.ratio --> percent format
prior.model.reported$posterior_percent <- ifelse(test = is.infinite(prior.model.reported$posterior.odds.ratio) == TRUE, 
                                                 yes = 1, 
                                                 no = ( prior.model.reported$posterior.odds.ratio / (1 + prior.model.reported$posterior.odds.ratio) )
                                                )

tapply(X = prior.model.reported$posterior_percent, 
       INDEX = prior.model.reported$condition, 
       FUN = mean)

## model.posterior.odds.ratio --> percent format
prior.model.reported$model.posterior_percent <- ifelse(test = is.infinite(prior.model.reported$model.posterior.odds.ratio) == TRUE, 
                                                       yes = 1, 
                                                       no = ( prior.model.reported$model.posterior.odds.ratio / (1 + prior.model.reported$model.posterior.odds.ratio) )
                                                      )

tapply(X = prior.model.reported$model.posterior_percent, 
       INDEX = prior.model.reported$condition, 
       FUN = mean)

## Calculate difference
prior.model.reported$diff <- with(prior.model.reported, posterior_percent - model.posterior_percent)

## ================================================================================================== ##
## Create long form version for plotting purposes
prior.model.reported_long <- melt(prior.model.reported, measure.vars = c("prior_percent", "posterior_percent", "model.posterior_percent"))
  896*3
  ## Each subject gives three responses: prior, model posterior, and reported posterior

prior.model.reported_long$variable <- factor(prior.model.reported_long$variable, 
                                             levels = c("prior_percent", "model.posterior_percent", "posterior_percent"), 
                                             labels = c("Prior", "Model Posterior", "Reported Posterior"))

prior.model.reported_long$Data <- factor(prior.model.reported$Data, 
                                         levels = c("CPR", "SpongeBath", "Surgery"), 
                                         labels = c("Performed CPR", "Gave a Sponge Bath", "Performed Surgery"))

## Plot means
ggplot(data = prior.model.reported_long, 
       aes(x = Target, y = value, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  facet_wrap(~Data, nrow = 1) + 
  theme_bw() + 
  labs(y = "Probability that target is doctor\n") + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(2, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(aspect.ratio = 1) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## ================================================================================================== ##
## Descriptive statistics
tapply(X = subset(prior.model.reported_long, variable == "Reported Posterior")$value, 
       INDEX = list(subset(prior.model.reported_long, variable == "Reported Posterior")$Target, 
                    subset(prior.model.reported_long, variable == "Reported Posterior")$Data), 
       FUN = mean)

## Inferential statistics

## All subjects
fit.p_vs_mp <- lme(fixed = value ~ variable * Target * Data, 
                   random = ~1|QualtricsID, 
                   data = prior.model.reported_long, 
                   method = "ML")

## Dataframe of fitted values
names(allEffects(fit.p_vs_mp))
p_vs_mp.plot <- as.data.frame(allEffects(fit.p_vs_mp)[["variable:Target:Data"]])
p_vs_mp.plot <- droplevels(p_vs_mp.plot)


## Relevel for plotting purposes
levels(p_vs_mp.plot$Data)

p_vs_mp.plot$Data <- factor(p_vs_mp.plot$Data, 
                            levels = c("Performed CPR", "Gave a Sponge Bath", "Performed Surgery"))


levels(p_vs_mp.plot$variable)

p_vs_mp.plot$variable <- factor(p_vs_mp.plot$variable, 
                                levels = c("Prior", "Model Posterior", "Reported Posterior"))

## Plot fitted values
ggplot(data = p_vs_mp.plot, 
       aes(x = Target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  facet_wrap(~Data, nrow = 1) + 
  theme_bw(base_size = 14) + 
  labs(x = "Target",
       y = expression(paste(italic(P), "(Target = Doctor)"))) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold", size = 14)) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## Obtain contrasts
contrasts <- contrast(lsmeans(object = fit.p_vs_mp, specs = ~variable * Target | Data), 
                      method = "pairwise", 
                      adjust = "none")

## ========================================================================================== ##
## Pull out surgery condition
surgery.contrasts <- as.data.frame(subset(summary(contrasts), Data == "Performed Surgery"))

## Subset out only relevant contrasts
surgery.contrasts <- subset(surgery.contrasts,
                            contrast == "Prior,Man - Prior,Woman" |
                              contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                              contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                              contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                              contrast == "Model Posterior,Woman - Reported Posterior,Woman")

surgery.contrasts

## Calculate effect size
surgery.contrasts$effect.size <- with(surgery.contrasts, 
                                      sqrt((t.ratio^2)/(t.ratio^2 + df)))

## =========== #
## =========== #
## =========== #
## =========== #
## =========== #
## Bootstrapping effect sizes to get 95% CIs
colnames(prior.model.reported_long)
dat.boot <- prior.model.reported_long[,c("QualtricsID", "condition", "Target", "Data", "variable", "value")]
colnames(dat.boot)


es.r <- function(data, indices) {
  
  d <- data[indices,]
  
  fit <- lme(fixed = value ~ variable * Target * Data, 
             random = ~1|QualtricsID, 
             data = d)
  
  all.contrasts <- contrast(lsmeans(object = fit, specs = ~variable * Target | Data), 
                            method = "pairwise", 
                            adjust = "none")
  
  only.surgery.contrasts <- subset(as.data.frame(subset(summary(all.contrasts), Data == "Performed Surgery")),
                                   contrast == "Prior,Man - Prior,Woman" |
                                     contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                                     contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                                     contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                                     contrast == "Model Posterior,Woman - Reported Posterior,Woman")
  
  only.surgery.contrasts$r <- with(only.surgery.contrasts, 
                                   sqrt((t.ratio^2)/(t.ratio^2 + df)))
  
  priorMan.vs.priorWoman.es <- only.surgery.contrasts[1,8]       ## Prior Man vs. Prior Woman
  modelMan.vs.reportedMan.es <- only.surgery.contrasts[2,8]      ## Model Man vs. Reported Man
  modelMan.vs.modelWoman.es <- only.surgery.contrasts[3,8]       ## Model Man vs. Model Woman
  reportedMan.vs.reportedWoman.es <- only.surgery.contrasts[4,8] ## Reported Man vs. Reported Woman
  modelWoman.vs.reportedWoman.es <- only.surgery.contrasts[5,8]  ## Model Woman vs. Reported Woman
  
  return(cbind(priorMan.vs.priorWoman.es,          ## index = 1
               modelMan.vs.reportedMan.es,         ## index = 2
               modelMan.vs.modelWoman.es,          ## index = 3
               reportedMan.vs.reportedWoman.es,    ## index = 4
               modelWoman.vs.reportedWoman.es      ## index = 5
              )
        )  
}

set.seed(123)

es.boot.results <- boot(data = dat.boot, 
                        statistic = es.r, 
                        R = 10000, 
                        strata = dat.boot$condition)

es.boot.results

## View distributions
melt(as.data.frame(es.boot.results$t), 
     measure.vars = c("V1", "V2", "V3", "V4", "V5")) %>%
  ggplot(mapping = aes(x = value)) +
  geom_histogram() + 
  facet_wrap(~variable)

## 95% CI
  ## Prior Man vs. Prior Woman
quantile(x = es.boot.results$t[,1], 
         probs = c(0.025, 0.975))

  ## Model Man vs. Reported Man
quantile(x = es.boot.results$t[,2], 
         probs = c(0.025, 0.975))

  ## Model Man vs. Model Woman
quantile(x = es.boot.results$t[,3], 
         probs = c(0.025, 0.975))

  ## Reported Man vs. Reported Woman
quantile(x = es.boot.results$t[,4], 
         probs = c(0.025, 0.975))

  ## Model Woman vs. Reported Woman
quantile(x = es.boot.results$t[,5], 
         probs = c(0.025, 0.975))

## Round P values
round(surgery.contrasts$p.value, digits = 4)

## Means
subset(p_vs_mp.plot, Data == "Performed Surgery")[,c("variable", "Target", "Data", "fit")]

## Percent saying >50% for priors
table(prior.model.reported$Prior.Man.Doctor[prior.model.reported$Data == "Surgery"] > 0.50)
238/(238+56)

## ========================================================================================== ##
## Subset out relevant spongebath contrasts
spongeBath.contrasts <- as.data.frame(subset(summary(contrasts), Data == "Gave a Sponge Bath"))

spongeBath.contrasts <- subset(spongeBath.contrasts,
                               contrast == "Prior,Man - Prior,Woman" |
                                 contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                                 contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                                 contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                                 contrast == "Model Posterior,Woman - Reported Posterior,Woman")

## Calculate effect size
spongeBath.contrasts$effect.size <- with(spongeBath.contrasts, 
                                         sqrt((t.ratio^2)/(t.ratio^2 + df)))

spongeBath.contrasts

## Round P values
round(spongeBath.contrasts$p.value, digits = 4)

## Means
subset(p_vs_mp.plot, Data == "Gave a Sponge Bath")[,c("variable", "Target", "Data", "fit")]


## ========================================================================================== ##
## Subset out relevant CPR contrasts
CPR.contrasts <- as.data.frame(subset(summary(contrasts), Data == "Performed CPR"))

CPR.contrasts <- subset(CPR.contrasts,
                        contrast == "Prior,Man - Prior,Woman" |
                          contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                          contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                          contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                          contrast == "Model Posterior,Woman - Reported Posterior,Woman")

## Calculate effect size
CPR.contrasts$effect.size <- with(CPR.contrasts, 
                                  sqrt((t.ratio^2)/(t.ratio^2 + df)))

CPR.contrasts

## Round P values
round(CPR.contrasts$p.value, digits = 4)

## ============================================================================================================== ##
## Surgery: likelihoods
surgery.likelihoods <- ggplot(data = subset(LR.long, Data == "Surgery"), 
                              mapping = aes(x = log(value.plotting))) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black", fill = "#00BFC4") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 0.42)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.40, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  labs(x = "Log Likelihood Ratio", y = "Proportion of participants (in each condition)") + 
  theme_bw(base_size = 16) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black"))

## Surgery: fitted values
surgery.plot <- ggplot(data = subset(p_vs_mp.plot, Data == "Performed Surgery"), 
                       mapping = aes(x = Target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 16) + 
  labs(x = "\nTarget",
       y = expression(paste(italic(P), "(Target = Doctor)"))
       #y = "\n\n\n\n\nP(Target = Doctor)"
      ) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man performed surgery\nn = 152", "Woman performed surgery\nn = 142")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1) + 
  theme(plot.margin = unit(c(t = 1, 0, 0, 1), "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

plot_grid(surgery.likelihoods, surgery.plot, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 2),
          rel_heights = c(0.75, 1))

## ============================================================================================================== ##
## At the individual subject level

diff.histogram <- ggplot(data = subset(prior.model.reported, Data == "Surgery"), 
                         mapping = aes(x = diff)) + 
  geom_histogram(mapping = aes(y = (..count..)/sum(..count..)), 
                 color = "black", fill = "lightgrey") + 
  coord_cartesian(ylim = c(0, 0.50)) + 
  scale_y_continuous(breaks = seq(0, 0.50, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  theme(aspect.ratio = 5/4) + 
  labs(x = "\nReported Posterior - Model Posterior", 
       y = "Proportion of participants\n") + 
  theme(axis.title = element_text(size = 13)) + 
  theme(axis.text = element_text(size = 13))

t.test(prior.model.reported$diff[prior.model.reported$Data == "Surgery"])
cohensD(prior.model.reported$diff[prior.model.reported$Data == "Surgery"])

## Sensitivity to likelihoods
summary(log(prior.model.reported$LR[prior.model.reported$Data == "Surgery"]))

summary(lm(formula = posterior_percent ~ LR + prior_percent, 
           data = subset(prior.model.reported, Data == "Surgery" & is.infinite(LR) == FALSE)))

sqrt( (3.686^2) / ( (3.686^2) + 189) )

lm.beta::lm.beta(lm(formula = posterior_percent ~ LR + prior_percent, 
                    data = subset(prior.model.reported, Data == "Surgery" & is.infinite(LR) == FALSE)))


surgery.LR.reported <- subset(prior.model.reported, Data == "Surgery")


surgery.LR.reported$LR.plotting <- surgery.LR.reported$LR

table(surgery.LR.reported$LR.plotting)

surgery.LR.reported$LR.plotting <- ifelse(test = is.infinite(surgery.LR.reported$LR.plotting) == TRUE, 
                                          yes = exp(10), 
                                          no = surgery.LR.reported$LR.plotting)


surgery.LR.reported$LR.type <- ifelse(test = is.infinite(surgery.LR.reported$LR) == TRUE, 
                                      yes = "Inf", 
                                      no = "Not Inf")

table(surgery.LR.reported$LR.type)

LR.reportedPosterior.scatter <- ggplot(data = surgery.LR.reported, 
                                       mapping = aes(x = log(LR.plotting), y = posterior_percent, color = LR.type)) + 
  geom_jitter() + 
  geom_smooth(method = "lm") + 
  coord_cartesian(xlim = c(-3, 10), ylim = c(0, 1)) + 
  scale_x_continuous(breaks = seq(-2, 10, 2), 
                     labels = c("-2", "0", "2", "4", "6", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     label = percent_format()) + 
  labs(x = "\nLog Likelihood Ratio", 
       y = "Reported Posterior\n") + 
  theme(legend.position = "none") + 
  theme(aspect.ratio = 10/6)

## Stitch two plots together
plot_grid(diff.histogram, LR.reportedPosterior.scatter, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 1),
          rel_heights = c(1, 1))

cor.test(x = surgery.LR.reported$LR[surgery.LR.reported$LR.type == "Not Inf"], 
         y = surgery.LR.reported$posterior_percent[surgery.LR.reported$LR.type == "Not Inf"])

## ============================================================================================================== ##
## Spongebath: likelihoods
spongeBath.likelihoods <- ggplot(data = subset(LR.long, Data == "SpongeBath"), 
                                 mapping = aes(x = log(value.plotting))) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black", fill = "#00BFC4") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 0.4)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.40, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  labs(x = "Log Likelihood Ratio", y = "Proportion of participants (in each condition)") + 
  theme_bw(base_size = 14.5) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 14.5)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black"))

spongeBath.plot <- ggplot(data = subset(p_vs_mp.plot, Data == "Gave a Sponge Bath"), 
                       mapping = aes(x = Target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 16) + 
  labs(x = "\nTarget",
       ##y = "\n\n\n\n\nP(Target = Doctor)"
       y = expression(paste(italic(P), "(Target = Doctor)"))
      ) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man gave a sponge bath\nn = 158", "Woman gave a sponge bath\nn = 144")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1) + 
  theme(plot.margin = unit(c(0, 0, 0, 3.5), "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

plot_grid(spongeBath.likelihoods, spongeBath.plot, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 2),
          rel_heights = c(0.75, 1))

## ============================================================================================================== ##
## CPR: likelihoods
CPR.likelihoods <- ggplot(data = subset(LR.long, Data == "CPR"), 
                          mapping = aes(x = log(value.plotting))) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black", fill = "#00BFC4") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 0.4)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.40, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  labs(x = "Log Likelihood Ratio", y = "Proportion of participants (in each condition)") + 
  theme_bw(base_size = 16) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 16)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black"))

CPR.plot <- ggplot(data = subset(p_vs_mp.plot, Data == "Performed CPR"), 
                          mapping = aes(x = Target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 16) + 
  labs(x = "\nTarget",
       ##y = "\n\n\n\n\nP(Target = Doctor)"
       y = expression(paste(italic(P), "(Target = Doctor)"))       
       ) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man performed CPR\nn = 139", "Woman performed CPR\nn = 161")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1) + 
  theme(plot.margin = unit(c(0, 0, 0, 3.5), "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

plot_grid(CPR.likelihoods, CPR.plot, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 2),
          rel_heights = c(0.75, 1))

## ================================================================================================================================== ##
## ================================================================================================================================== ##
## ================================================================================================================================== ##

## Robustness check with various adjustment factors
  ## Do this with same dataset but different name
doc <- prior.model.reported_long
doc$logit.transformed.value <- NULL

  ## Vector of adjustment factors
adjustment.factor <- seq(from = 0.005, to = 0.10, by = 0.005)

  ## Empty output list; to be filled with dataframe, one for each adjustment factor
output <- list()

  ## Loop through and obtain dataframe for each adjustment factor

for (i in 1:length(adjustment.factor)) {

    ## Print which adjustment factor is being processed; should go from 0.005 to 0.10 in increments of 0.005
  print(adjustment.factor[i])
  
  ## Create transformed DV
  doc$logit.transformed.value <- logit(p = doc$value, 
                                       adjust = adjustment.factor[i])
  
  ## Fit model
  fit.transformed <- lme(fixed = logit.transformed.value ~ variable * Target * Data,
                         random = ~1|QualtricsID/variable, 
                         data = doc, 
                         method = "ML")
  
  ## Obtain contrasts
  contrasts <- contrast(lsmeans(object = fit.transformed, specs = ~variable * Target | Data), 
                        method = "pairwise", 
                        adjust = "none")
  
  ## Pull out surgery condition
  surgery.contrasts <- as.data.frame(subset(summary(contrasts), Data == "Performed Surgery"))
  
  ## Subset out only relevant contrasts
  surgery.contrasts <- subset(surgery.contrasts,
                              contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                                contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                                contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                                contrast == "Model Posterior,Woman - Reported Posterior,Woman")
  
  ## Calculate effect size
  surgery.contrasts$effect.size <- with(surgery.contrasts, 
                                        sqrt((t.ratio^2)/(t.ratio^2 + df)))
  
  ## Add variable to indicate which adjustment factor
  surgery.contrasts$adjustment.factor <- adjustment.factor[i]
  
  ## Save dataframe in output
  output[[i]] <- surgery.contrasts
  
}

## Combine all dataframes in output into single dataframe
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
adjust.df <- bind_rows(output)

## Clean up adjust.df
adjust.df <- droplevels(adjust.df)

## Relevel contrast
levels(adjust.df$contrast)

adjust.df$contrast <- factor(adjust.df$contrast, 
                             levels = c("Model Posterior,Man - Model Posterior,Woman", 
                                        "Reported Posterior,Man - Reported Posterior,Woman", 
                                        "Model Posterior,Man - Reported Posterior,Man", 
                                        "Model Posterior,Woman - Reported Posterior,Woman"))

## Plot P values
ggplot(data = adjust.df, 
       mapping = aes(x = adjustment.factor, y = p.value)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~contrast, nrow = 2, ncol = 2) + 
  geom_hline(yintercept = 0.05, color = "red") + 
  labs(x = "Adjustment factor", 
       y =  expression(paste(italic(P), " value"))) + 
  coord_cartesian(xlim = c(0, 0.10), ylim = c(0, 0.80)) + 
  scale_x_continuous(breaks = seq(0, 0.10, 0.01)) + 
  scale_y_continuous(breaks = seq(0, 0.80, 0.10)) + 
  theme_bw(base_size = 13) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.background = element_rect(fill = "white")) + 
  theme(strip.background =element_rect(color = "white")) + 
  theme(strip.text.x = element_text(face = "bold", size = 12)) + 
  theme(aspect.ratio = 9/11) + 
  theme(panel.spacing = unit(4, "lines"))

