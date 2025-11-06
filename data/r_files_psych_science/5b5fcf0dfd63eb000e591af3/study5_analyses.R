## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Study 5: Analyses

## Set working directory
if (!require("rstudioapi")) {install.packages("rstudioapi"); require("rstudioapi")}
currentPath <- getActiveDocumentContext()$path
setwd(dir = dirname(path = currentPath))

## Packages needed
if (!require("car")) {install.packages("car"); require("car")}
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
if (!require("lsmeans")) {install.packages("lsmeans"); require("lsmeans")}
if (!require("GGally")) {install.packages("GGally"); require("GGally")}
if (!require("smacof")) {install.packages("smacof"); require("smacof")}
if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("cowplot")) {install.packages("cowplot"); require("cowplot")}
if (!require("boot")) {install.packages("boot"); require("boot")}
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("MBESS")) {install.packages("MBESS"); require("MBESS")}

## Load data
load("study5_dat.Rda")

## Examine structure and summary
str(dat)

plyr::count(dat$able.to.update)
  ## 5 people not able to update
plyr::count(dat$Prior.Man.Pilot)
  ## 4 people gave 100% probability to man being the pilot; 1 person gave 0% probability to man being the pilot

## Exclude 5 participants for not being able to update
dat <- subset(dat, able.to.update == "yes")

## Demogrpahics
plyr::count(dat$gender)
round(x = mean(dat$age, na.rm = TRUE), digits = 2)
round(x = sd(dat$age, na.rm = TRUE), digits = 2)
  ## N = 348, 177 males, 169 females, 2 unspecified
  ## Mean age = 36.28
  ## SD = 12.27

## ======================================================================= ##
## Prior beliefs

## Percentage of participants whose priors were <50%, =50%, or >50%
dat$Prior.Man.Pilot.Discrete <- ifelse(test = dat$Prior.Man.Pilot < .50, 
                                       yes = "Favor woman as pilot", 
                                       no = ifelse(test = dat$Prior.Man.Pilot == .50, 
                                                   yes = "Favor both individuals equally", 
                                                   no = "Favor man as pilot"))

dat$Prior.Man.Pilot.Discrete <- factor(dat$Prior.Man.Pilot.Discrete, 
                                       levels = c("Favor woman as pilot", 
                                                  "Favor both individuals equally", 
                                                  "Favor man as pilot"))

plyr::count(dat$Prior.Man.Pilot.Discrete)

## Ensure that every participant's prior beliefs sum to 1
plyr::count(dat$Prior.Man.Pilot + dat$Prior.Woman.Pilot)

## Distribution of prior beliefs, binned by decile
ggplot(data = dat, 
       mapping = aes(x = Prior.Man.Pilot)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),
           binwidth = 0.05, breaks = seq(-0.025, 1.025, 0.05),
           col = "black" , fill = "#F8766D") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.2)) + 
  scale_x_continuous(breaks = seq(0, 1, .1), label = percent_format()) + 
  scale_y_continuous(breaks = seq(0, .2, .05), label = percent_format()) + 
  labs(x = "\nPerentage chance that Man = Pilot\n(Woman != Pilot)", 
       y = "Proportion of participants\n") + 
  theme_bw(base_size = 24) + 
  theme(aspect.ratio = 1) + theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black"))

## ================================================================================================== ##
## ======================================================== ##
## ======================================================== ##
## Log likelihood ratios

## Create wide version of dataset containing only:
## ResponseID, target, data, and likelihood responses
colnames(dat)

## Isolate variables that contain likelihood data
grep(pattern = "lk", x = colnames(dat), value = TRUE)

dat.lr <- dat[,c("ResponseID", "target", "condition",
                 grep(pattern = "lk", x = colnames(dat), value = TRUE))]

## ------------------------------------------------------------------------- ##
## percent.male.pilots.comm.ATC
dat.lr$percent.male.pilots.comm.ATC <- NA

dat.lr$percent.male.pilots.comm.ATC[dat.lr$condition == "1"] <- 
  dat.lr$cond1.lk_percent.male.pilots.comm.ATC[dat.lr$condition == "1"]

dat.lr$percent.male.pilots.comm.ATC[dat.lr$condition == "2"] <- 
  dat.lr$cond2.lk_percent.male.pilots.comm.ATC[dat.lr$condition == "2"]

dat.lr$percent.male.pilots.comm.ATC[dat.lr$condition == "13"] <- 
  dat.lr$cond13.lk_percent.male.pilots.comm.ATC[dat.lr$condition == "13"]

dat.lr$percent.male.pilots.comm.ATC[dat.lr$condition == "14"] <- 
  dat.lr$cond14.lk_percent.male.pilots.comm.ATC[dat.lr$condition == "14"]

plyr::count(dat.lr$percent.male.pilots.comm.ATC)
## nrow(dat.lr)-154
nrow(dat.lr)-170
  ## 178
plyr::count(dat.lr$target)
  ## 178

hist(dat.lr$percent.male.pilots.comm.ATC)
  ## looks right; nearly all male pilots communicate with ATC

## ------------------------------------------------------------------------- ##
## percent.male.flightattendants.comm.ATC
dat.lr$percent.male.flightattendants.comm.ATC <- NA

dat.lr$percent.male.flightattendants.comm.ATC[dat.lr$condition == "1"] <- 
  dat.lr$cond1.lk_percent.male.flightAttendants.comm.ATC[dat.lr$condition == "1"]

dat.lr$percent.male.flightattendants.comm.ATC[dat.lr$condition == "2"] <- 
  dat.lr$cond2.lk_percent.male.flightAttendants.comm.ATC[dat.lr$condition == "2"]

dat.lr$percent.male.flightattendants.comm.ATC[dat.lr$condition == "13"] <- 
  dat.lr$cond13.lk_percent.male.flightAttendants.comm.ATC[dat.lr$condition == "13"]

dat.lr$percent.male.flightattendants.comm.ATC[dat.lr$condition == "14"] <- 
  dat.lr$cond14.lk_percent.male.flightAttendants.comm.ATC[dat.lr$condition == "14"]

plyr::count(dat.lr$percent.male.flightattendants.comm.ATC)
nrow(dat.lr)-170
  ## 178
plyr::count(dat.lr$target)
  ## 178

hist(dat.lr$percent.male.flightattendants.comm.ATC)
  ## looks right; virtually no flight attendants communicate with ATC

## ------------------------------------------------------------------------- ##
## percent.female.pilots.comm.ATC
dat.lr$percent.female.pilots.comm.ATC <- NA

dat.lr$percent.female.pilots.comm.ATC[dat.lr$condition == "7"] <- 
  dat.lr$cond7.lk_percent.female.pilots.comm.ATC[dat.lr$condition == "7"]

dat.lr$percent.female.pilots.comm.ATC[dat.lr$condition == "8"] <- 
  dat.lr$cond8.lk_percent.female.pilots.comm.ATC[dat.lr$condition == "8"]

dat.lr$percent.female.pilots.comm.ATC[dat.lr$condition == "19"] <- 
  dat.lr$cond19.lk_percent.female.pilots.comm.ATC[dat.lr$condition == "19"]

dat.lr$percent.female.pilots.comm.ATC[dat.lr$condition == "20"] <- 
  dat.lr$cond20.lk_percent.female.pilots.comm.ATC[dat.lr$condition == "20"]

plyr::count(dat.lr$percent.female.pilots.comm.ATC)
nrow(dat.lr)-178
  ## 170
plyr::count(dat.lr$target)
  ## 170

hist(dat.lr$percent.female.pilots.comm.ATC)
  ## looks right; virtually all female pilots communicate with ATC

## ------------------------------------------------------------------------- ##
## percent.female.flightattendants.comm.ATC
dat.lr$percent.female.flightattendants.comm.ATC <- NA

dat.lr$percent.female.flightattendants.comm.ATC[dat.lr$condition == "7"] <- 
  dat.lr$cond7.lk_percent.female.flightAttendants.comm.ATC[dat.lr$condition == "7"]

dat.lr$percent.female.flightattendants.comm.ATC[dat.lr$condition == "8"] <- 
  dat.lr$cond8.lk_percent.female.flightAttendants.comm.ATC[dat.lr$condition == "8"]

dat.lr$percent.female.flightattendants.comm.ATC[dat.lr$condition == "19"] <- 
  dat.lr$cond19.lk_percent.female.flightAttendants.comm.ATC[dat.lr$condition == "19"]

dat.lr$percent.female.flightattendants.comm.ATC[dat.lr$condition == "20"] <- 
  dat.lr$cond20.lk_percent.female.flightAttendants.comm.ATC[dat.lr$condition == "20"]

plyr::count(dat.lr$percent.female.flightattendants.comm.ATC)
nrow(dat.lr)-178
  ## 170
plyr::count(dat.lr$target)
  ## 170

hist(dat.lr$percent.female.flightattendants.comm.ATC)
## looks right; virtually no female flight attendants communicate with ATC during a flight

## ======== ##
## ======== ##
## ======== ##
## Reorganize dataset
colnames(dat.lr)
LR.dat <- dat.lr[,c(1:3, 20:23, 4:19)]
colnames(LR.dat)

LR.dat$likelihood.pilot <- NA

LR.dat$likelihood.pilot[LR.dat$target == "man"] <- 
  LR.dat$percent.male.pilots.comm.ATC[LR.dat$target == "man"]

LR.dat$likelihood.pilot[LR.dat$target == "woman"] <- 
  LR.dat$percent.female.pilots.comm.ATC[LR.dat$target == "woman"]

## --------------- ##
## --------------- ##
## --------------- ##
LR.dat$likelihood.flightattendant <- NA

LR.dat$likelihood.flightattendant[LR.dat$target == "man"] <- 
  LR.dat$percent.male.flightattendants.comm.ATC[LR.dat$target == "man"]

LR.dat$likelihood.flightattendant[LR.dat$target == "woman"] <- 
  LR.dat$percent.female.flightattendants.comm.ATC[LR.dat$target == "woman"]

## ======== ##
## ======== ##
## ======== ##
LR.dat$LR <- with(LR.dat, likelihood.pilot / likelihood.flightattendant)
plyr::count(LR.dat$LR)
  ## 0 NaNs
  ## 85 Infs

## Do data overwhelm prior completely?
LR.dat$overwhelm.prior <- ifelse(test = LR.dat$LR == 0, 
                                 yes = "Data entirely diagnostic of flight attendant", 
                                 no = ifelse(test = is.infinite(LR.dat$LR) == TRUE, 
                                             yes = "Data entirely diagnostic of pilot", 
                                             no = "Data not entirely diagnostic"))

LR.dat$overwhelm.prior <- factor(LR.dat$overwhelm.prior, 
                                 levels = c("Data entirely diagnostic of flight attendant", 
                                            "Data not entirely diagnostic", 
                                            "Data entirely diagnostic of pilot"))


levels(LR.dat$overwhelm.prior)
plyr::count(LR.dat$overwhelm.prior)

## Create LR for plotting purposes
## Infinity = 10,000
## Zero = 1/10,000
LR.dat$value.plotting <- ifelse(test = is.infinite(LR.dat$LR) == TRUE, 
                                yes = 10000, 
                                no = ifelse(test = LR.dat$LR == 0, 
                                            yes = 1/10000, 
                                            no = LR.dat$LR))

## condition for plotting purposes
plyr::count(LR.dat$target)
LR.dat$condition.plotting <- NA

LR.dat$condition.plotting <- ifelse(test = LR.dat$target == "man", 
                                    yes = "Man Communicated w/ATC\nn = 178", 
                                    no =  "Woman Communicated w/ATC\nn = 170")

LR.dat$condition.plotting <- factor(LR.dat$condition.plotting, 
                                    levels = c("Man Communicated w/ATC\nn = 178", 
                                               "Woman Communicated w/ATC\nn = 170"))

## Make sure n's sum up to 348
178+170

## Visualize
ggplot(data = LR.dat, 
       mapping = aes(x = log(value.plotting), fill = overwhelm.prior)) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 0.80)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                "-5", "-4", "-3", "-2", "-1",
                                "0", 
                                "1", "2", "3", "4", "5", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(label = percent_format()) + 
  scale_fill_manual(values = c("white", "darkgrey", "black")) + 
  labs(x = "\nLog Likelihood Ratio", y = "Proportion of participants (in each condition)\n") + 
  theme_bw() + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(face = "bold")) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## ==================================================================================== ##
## Inferential statistics on likelihood ratios
## Comparisons between genders

## Man vs. Woman - comm w/ATC
wilcox.test(log(LR) ~ target, data = LR.dat)
  ## Wilcoxon P = 0.0001
  ## Effect size, Pearson's r = 0.18
qnorm(wilcox.test(log(LR) ~ condition.plotting, data = LR.dat)$p.value/2) / sqrt(178+170)

tapply(log(LR.dat$LR), 
       LR.dat$condition.plotting, 
       FUN = median)

## ================================================================================================== ##
## Calculate model posterior; compare to reported posterior

## Create new dataframe
prior.model.reported <- merge(x = dat[,c("ResponseID", "target",
                                         "gender", "age",
                                         "Prior.Man.Pilot", "Prior.Woman.Pilot", "Prior.Target.Pilot", 
                                         "Posterior.Man.Pilot", "Posterior.Woman.Pilot", "Posterior.Target.Pilot")], 
                              y = LR.dat[,c("ResponseID", "LR")], 
                              by = "ResponseID",
                              all.x = FALSE, 
                              all.y = TRUE)

## Create prior.odds.ratio: divide differently for when target is man vs. woman
prior.model.reported$prior.odds.ratio <- ifelse(test = prior.model.reported$target == "man", 
                                                yes = prior.model.reported$Prior.Man.Pilot / prior.model.reported$Prior.Woman.Pilot, 
                                                no = prior.model.reported$Prior.Woman.Pilot / prior.model.reported$Prior.Man.Pilot)

## Create posterior.odds.ratio: divide differently for when target is man vs. woman
prior.model.reported$posterior.odds.ratio <- ifelse(test = prior.model.reported$target == "man", 
                                                    yes = prior.model.reported$Posterior.Man.Pilot / prior.model.reported$Posterior.Woman.Pilot, 
                                                    no = prior.model.reported$Posterior.Woman.Pilot / prior.model.reported$Posterior.Man.Pilot)


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
       INDEX = prior.model.reported$target, 
       FUN = mean)

## posterior.odds.ratio --> percent format
prior.model.reported$posterior_percent <- ifelse(test = is.infinite(prior.model.reported$posterior.odds.ratio) == TRUE, 
                                                 yes = 1, 
                                                 no = ( prior.model.reported$posterior.odds.ratio / (1 + prior.model.reported$posterior.odds.ratio) )
                                                )

tapply(X = prior.model.reported$posterior_percent, 
       INDEX = list(prior.model.reported$target), 
       FUN = mean)

## model.posterior.odds.ratio --> percent format
prior.model.reported$model.posterior_percent <- ifelse(test = is.infinite(prior.model.reported$model.posterior.odds.ratio) == TRUE, 
                                                       yes = 1, 
                                                       no = ( prior.model.reported$model.posterior.odds.ratio / (1 + prior.model.reported$model.posterior.odds.ratio) )
                                                      )

tapply(X = prior.model.reported$model.posterior_percent, 
       INDEX = list(prior.model.reported$target), 
       FUN = mean)

## Calculate difference
prior.model.reported$diff <- with(prior.model.reported, posterior_percent - model.posterior_percent)

## ================================================================================================== ##
## Create long form version for plotting purposes
prior.model.reported_long <- melt(prior.model.reported, measure.vars = c("prior_percent", "posterior_percent", "model.posterior_percent"))
348*3
  ## Each subject gives three responses: prior, model posterior, and reported posterior

prior.model.reported_long$variable <- factor(prior.model.reported_long$variable, 
                                             levels = c("prior_percent", "model.posterior_percent", "posterior_percent"), 
                                             labels = c("Prior", "Model Posterior", "Reported Posterior"))

prior.model.reported_long$target <- factor(prior.model.reported_long$target, 
                                           levels = c("man", "woman"), 
                                           labels = c("Man", "Woman"))

## Plot means
ggplot(data = prior.model.reported_long, 
       aes(x = target, y = value, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  theme_bw() + 
  labs(y = "Probability that target is pilot\n") + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format()) + 
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
## Inferential statistics

## All subjects
fit.p_vs_mp <- lme(fixed = value ~ variable * target, 
                   random = ~1|ResponseID, 
                   data = prior.model.reported_long, 
                   method = "ML")

## Dataframe of fitted values
names(allEffects(fit.p_vs_mp))
p_vs_mp.plot <- as.data.frame(allEffects(fit.p_vs_mp)[["variable:target"]])
p_vs_mp.plot <- droplevels(p_vs_mp.plot)

## Relevel for plotting purposes
levels(p_vs_mp.plot$variable)

p_vs_mp.plot$variable <- factor(p_vs_mp.plot$variable, 
                                levels = c("Prior", "Model Posterior", "Reported Posterior"))

## Plot fitted values
ggplot(data = p_vs_mp.plot, 
       aes(x = target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 14) + 
  labs(x = "Target",
       y = expression(paste(italic(P), "(Target = Pilot)"))) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man communicated/ATC\nn = 178", "Woman communicated w/ATC\nn = 170")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(),
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1)

# Obtain contrasts
contrasts <- contrast(lsmeans(object = fit.p_vs_mp, specs = ~variable * target), 
                              method = "pairwise", 
                              adjust = "none")

## Save as dataframe
contrasts <- as.data.frame(subset(summary(contrasts)))

## Subset out only relevant contrasts
contrasts <- subset(contrasts,
                    contrast == "Prior,Man - Prior,Woman" |
                      contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                      contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                      contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                      contrast == "Model Posterior,Woman - Reported Posterior,Woman")

## Calculate effect size
contrasts$effect.size <- with(contrasts, 
                              sqrt((t.ratio^2)/(t.ratio^2 + df)))

contrasts

## =========== #
## =========== #
## =========== #
## =========== #
## Bootstrapping effect sizes to get 95% CIs
colnames(prior.model.reported_long)
dat.boot <- prior.model.reported_long[,c("ResponseID", "target", "variable", "value")]
colnames(dat.boot)

es.r <- function(data, indices) {
  
  d <- data[indices,]
  
  fit <- lme(fixed = value ~ variable * target, 
             random = ~1|ResponseID, 
             data = d)
  
  all.contrasts <- contrast(lsmeans(object = fit, specs = ~ variable * target), 
                            method = "pairwise", 
                            adjust = "none")
  
  main.contrasts <- subset(as.data.frame(subset(summary(all.contrasts))),
                           contrast == "Prior,Man - Prior,Woman" |
                             contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                             contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                             contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                             contrast == "Model Posterior,Woman - Reported Posterior,Woman")
  
  main.contrasts$r <- with(main.contrasts, 
                           sqrt((t.ratio^2)/(t.ratio^2 + df)))
  
  priorMan.vs.priorWoman.es <- main.contrasts[1,7]       ## Prior Man vs. Prior Woman
  modelMan.vs.reportedMan.es <- main.contrasts[2,7]      ## Model Man vs. Reported Man
  modelMan.vs.modelWoman.es <- main.contrasts[3,7]       ## Model Man vs. Model Woman
  reportedMan.vs.reportedWoman.es <- main.contrasts[4,7] ## Reported Man vs. Reported Woman
  modelWoman.vs.reportedWoman.es <- main.contrasts[5,7]  ## Model Woman vs. Reported Woman
  
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
                        strata = dat.boot$target)

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
round(contrasts$p.value, digits = 4)

## Means
p_vs_mp.plot[,c("variable", "target", "fit")]

## Percent saying >50% for priors
table(prior.model.reported$Prior.Man.Pilot > 0.50)
315/(315+33)


## ========================================================================================================================== ##
## comm w/ATC likelihoods
commATC.likelihoods <- ggplot(data = LR.dat, 
                              mapping = aes(x = log(value.plotting))) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black", fill = "#00BFC4") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(0, 0.30)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.30, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  labs(x = "Log Likelihood Ratio", y = "Proportion of participants (in each condition)") + 
  theme_bw(base_size = 14.4) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 14.4)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black"))

commATC.plot <- ggplot(data = p_vs_mp.plot, 
                       mapping = aes(x = target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 14.4) + 
  labs(x = "Target",
       y = expression(paste(italic(P), "(Target = Pilot)"))) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man communicated/ATC\nn = 178", "Woman communicated w/ATC\nn = 170")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(),
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(plot.margin = unit(c(0, 0, 0, 3.5), "lines")) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1)

plot_grid(commATC.likelihoods, commATC.plot, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 2),
          rel_heights = c(0.75, 1))

## ========================================================================================================================== ##
## ========================================================================================================================== ##
## ========================================================================================================================== ##

## Evaluations of Person X data
str(dat)

## Collapse responses
levels(dat$attr.cond)

dat$query <- ifelse(test = dat$attr.cond == "1", 
                    yes = dat$c1.self, 
                    no = dat$c2.self)

dat$intelligent <- ifelse(test = dat$attr.cond == "1", 
                          yes = dat$c1.intel, 
                          no = dat$c2.intel)

dat$accurate <- ifelse(test = dat$attr.cond == "1", 
                       yes = dat$c1.acc, 
                       no = dat$c2.acc)

dat$fair <- ifelse(test = dat$attr.cond == "1", 
                   yes = dat$c1.fair, 
                   no = dat$c2.fair)

dat$just <- ifelse(test = dat$attr.cond == "1", 
                   yes = dat$c1.just, 
                   no = dat$c2.just)

## Collapse comments
dat$c1.free <- as.character(dat$c1.free)
dat$c2.free <- as.character(dat$c2.free)

dat$comment <- ifelse(test = dat$attr.cond == "1", 
                      yes = dat$c1.free, 
                      no = dat$c2.free)

## Rename rows
rownames(dat) <- c(1:nrow(dat))

## =============================================================================================================== ##
## query: which statement do participants agree with?
plyr::count(dat$query)
275/348 ## 79% agree that both man and woman are equally likely to be a doctor
68/348  ## 20% agree that man is more likely to be a doctor
5/348   ## <1% agree that woman is more likely to be a doctor

## =============================================================================================================== ##
## Four main items of interest: intelligent, accurate, fair, and just

## Create dataset with just composite items
dat.composite <- dat[,c("intelligent", "accurate", "fair", "just")]

dat.composite <- melt(dat.composite, measure.vars = c("intelligent", "accurate", "fair", "just"))

levels(dat.composite$variable)
dat.composite$variable <- factor(x = dat.composite$variable, 
                                 levels = c("fair", "intelligent", "just", "accurate"), 
                                 labels = c("1 = Unfair ... 7 = Fair", 
                                            "1 = Unintelligent ... 7 = Intelligent", 
                                            "1 = Unjust ... 7 = Just",
                                            "1 = Inaccurate ... 7 = Accurate"))

## Visualize
ggplot(data = dat.composite, 
       aes(x = value)) + 
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  facet_wrap(~variable, nrow = 2) + 
  coord_cartesian(xlim = c(0:8)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), labels = percent_format()) + 
  labs(y = "Proprotion of participants") + 
  theme_bw(base_size = 14) + 
  theme(aspect.ratio = 1) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.background = element_blank()) + 
  theme(panel.spacing = unit(2, "lines")) + 
  theme(strip.text.x = element_text(size = 14, face = "bold")) + 
  theme(axis.title.y = element_text(vjust = 2, size = 16)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title.x = element_blank())

## Correlations among items
cor(dat[,c("intelligent", "accurate", "fair", "just")])
ggpairs(dat[,c("intelligent", "accurate", "fair", "just")])

## Cronbach's alpha
psych::alpha(x = var(dat[,c("intelligent", "accurate", "fair", "just")]))
  ## alpha = 0.91

## Compute composite average
dat$composite.avg <- with(dat, (intelligent + accurate + fair + just) / 4)

## Descriptives of composite average
round(x = mean(dat$composite.avg), digits = 2)
round(x = std.error(dat$composite.avg), digits = 2)

## Inferential statistics on composite average
  ## One sample t-test against 4
t.test(x = dat$composite.avg, 
       mu = 4)

  ## Cohen's d
round(x = cohensD(x = dat$composite.avg, 
                  mu = 4), 
      digits = 2)

  ## 95% CI
one.sample.t.cohensD <- function(data, indices) {
  d <- data[indices,]
  return(cohensD(x = d$composite.avg, mu = 4))
}

one.sample.t.cohensD.results <- boot(data = dat, 
                                     statistic = one.sample.t.cohensD, 
                                     R = 10000)

hist(one.sample.t.cohensD.results$t)

round(x = quantile(x = one.sample.t.cohensD.results$t, 
                   probs = c(0.025, 0.975)), 
      digits = 2)

## Item means and standard errors
round(apply(X = dat[,c("fair", "just", "accurate", "intelligent")], 
            MARGIN = 2, 
            FUN = mean), 
      digits = 2)

round(apply(X = dat[,c("fair", "just", "accurate", "intelligent")], 
            MARGIN = 2, 
            FUN = std.error), 
      digits = 2)

## ================================================================================================ ##

## Reported probability as a function of Person X evaluation

## Merge prior, model posterior, and reported posterior with dat
colnames(dat)
colnames(prior.model.reported)

dat <- merge(x = dat, 
             y = prior.model.reported[,c("ResponseID", "posterior_percent", "model.posterior_percent")], 
             by = "ResponseID")

dat %>%
  ggplot(mapping = aes(x = composite.avg, y = posterior_percent, color = target)) + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "<-Negative    Positive->\n\nEvaluation of Person X",
       y = expression(paste(italic(P), "(Target = Pilot)"))) + 
  coord_cartesian(xlim = c(0.5, 7.5),
                  ylim = c(0, 1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.10),
                     label = percent_format()) + 
  scale_color_discrete(labels = c("Man communicated w/ATC",
                                  "Woman communicated w/ATC")) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.25, 0.2)) + 
  theme(legend.justification = c(0.25, 0.2)) + 
  theme(legend.direction = "vertical") + 
  theme(aspect.ratio = 10/7)

## Inferential statistcs
dat$composite.avg.shifted <- dat$composite.avg - 1

summary(lm(formula = posterior_percent ~ composite.avg.shifted * target, 
           data = dat))

summary(lm(formula = posterior_percent ~ composite.avg.shifted * target, 
           data = dat))$coefficients[3,3]

sqrt((summary(lm(formula = posterior_percent ~ composite.avg.shifted * target, 
                 data = dat))$coefficients[3,3])^2/((summary(lm(formula = posterior_percent ~ composite.avg.shifted * target, 
                                                                data = dat))$coefficients[3,3])^2 + 344))

fitted.es.function <- function(data, indices) {
  d <- data[indices,]
  d$composite.avg.shifted <- d$composite.avg - 1
  fit <- lm(formula = posterior_percent ~ composite.avg.shifted * target, 
            data = d)
  t.stat <- summary(fit)$coefficients[3,3]
  r <- sqrt(t.stat^2 / (t.stat^2 + 344))
  return(r)
}

fitted.es.results <- boot(data = dat, 
                          R = 10000, 
                          statistic = fitted.es.function, 
                          strata = dat$target)

hist(fitted.es.results$t)

quantile(x = fitted.es.results$t, 
         probs = c(0.025, 0.975))

## Main effect of target
anova(lm(formula = posterior_percent ~ composite.avg.shifted * target, 
         data = dat))

## Effect size
etaSquared(lm(formula = posterior_percent ~ composite.avg.shifted * target, 
              data = dat))

## 95% CI around effect sizes
  ## Main effect of target
ci.pvaf(F.value = 84.579, 
        df.1 = 1,
        df.2 = 344,
        N = 348, 
        conf.level = 0.95)

  ## Interaction
ci.pvaf(F.value = 10.714, 
        df.1 = 1,
        df.2 = 344,
        N = 348, 
        conf.level = 0.95)

## ================================================================================================ ##
## Correlation between diff and composite.avg

prior.model.reported <- merge(x = prior.model.reported, 
                              y = dat[,c("ResponseID", "accurate", "intelligent", "fair", "just", "composite.avg")], 
                              by = "ResponseID", 
                              all.x = TRUE)

## Correlation
cor.test(x = prior.model.reported$composite.avg, 
         y = prior.model.reported$diff, 
         method = "pearson")

## Slope
summary(lm(diff ~ composite.avg, prior.model.reported))

## Histograms on sides of scatterplot
library("ggExtra")

p <- ggplot(data = prior.model.reported,
            mapping = aes(x = composite.avg, y = diff)) +
  geom_point()
ggExtra::ggMarginal(p, type = "histogram")

p <- ggplot(data = prior.model.reported, 
            mapping = aes(x = composite.avg, y = diff)) + 
  geom_jitter(color = "darkgrey", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", lwd = 1.25) + 
  labs(x = "<-Negative             Positive->\n\nAverage Evaluation", 
       y = "Judgment\n\n<-Underestimate      Overestimate->") + 
  coord_cartesian(xlim = c(0:8), ylim = c(-1.1, 1.1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) + 
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(aspect.ratio = 1)

p

ggExtra::ggMarginal(p, type = "histogram")

ggplot(data = prior.model.reported, 
       mapping = aes(x = just, y = diff)) + 
  geom_jitter(color = "darkgrey", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", lwd = 1.25) + 
  labs(x = "<-Negative               Positive->\n\nAverage Evaluation", 
       y = "Judgment\n\n<-Underestimate        Overestimate->") + 
  coord_cartesian(xlim = c(0:8), ylim = c(-1.1, 1.1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) + 
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(aspect.ratio = 1)


## For each of the two conditions: man vs. woman comm w/ATC
table(prior.model.reported$target)

man.scatter <- ggplot(data = subset(prior.model.reported, target == "man"), 
                      mapping = aes(x = just, y = diff)) + 
  geom_jitter(color = "darkgrey", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", lwd = 1.25) + 
  labs(x = "<-Negative              Positive->\n\nAverage Evaluation", 
       y = "Judgment\n\n<-Underestimate        Overestimate->", 
       title = "Man communicated w/ATC\nn = 178") + 
  coord_cartesian(xlim = c(0:8), ylim = c(-1.1, 1.1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) + 
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(aspect.ratio = 1) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title = element_text(size = 12))



woman.scatter <- ggplot(data = subset(prior.model.reported, target == "woman"), 
                        mapping = aes(x = just, y = diff)) + 
  geom_jitter(color = "darkgrey", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", lwd = 1.25) + 
  labs(x = "<-Negative               Positive->\n\nAverage Evaluation", 
       y = "\n\n\n\n\n\nJudgment\n\n<-Underestimate         Overestimate->", 
       title = "Woman communicated w/ATC\nn = 170") + 
  coord_cartesian(xlim = c(0:8), ylim = c(-1.1, 1.1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) + 
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(aspect.ratio = 1) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title = element_text(size = 12))

## Stitch two plots together side by side
grid.newpage()
grid.draw(cbind(ggplotGrob(man.scatter), 
                ggplotGrob(woman.scatter), 
                size = "first"))

## N.S. interaction
summary(lm(diff ~ composite.avg * target, data = prior.model.reported))

## Dataframe with:
## subject ID, prior.man.pilot, and query
colnames(prior.model.reported)
colnames(dat)

dat.prior.query <- merge(x = dat[,c("ResponseID", "query")], 
                         y = prior.model.reported[,c("ResponseID", "Prior.Man.Pilot")], 
                         by = "ResponseID")


## Factor
dat.prior.query$query <- factor(x = dat.prior.query$query, 
                                levels = c(1:3), 
                                labels = c("q.Woman more likely", "q.Equally likely", "q.Man more likely"))


dat.prior.query$Prior.Man.Pilot <- ifelse(test = dat.prior.query$Prior.Man.Pilot > 0.50, 
                                          yes = "p.Man more likely", 
                                          no = ifelse(test = dat.prior.query$Prior.Man.Pilot == 0.50, 
                                                      yes = "p.Equally likely",
                                                      no = "p.Woman more likely"))

dat.prior.query$Prior.Man.Pilot <- factor(x = dat.prior.query$Prior.Man.Pilot, 
                                          levels = c("p.Woman more likely", "p.Equally likely", "p.Man more likely"))


## Table S5
round(table(dat.prior.query$query, 
            dat.prior.query$Prior.Man.Pilot)/348*100, 
      digits = 2)

round(table(dat.prior.query$query)/348 * 100, digits = 2)
round(table(dat.prior.query$Prior.Man.Pilot)/348 * 100, digits = 2)

