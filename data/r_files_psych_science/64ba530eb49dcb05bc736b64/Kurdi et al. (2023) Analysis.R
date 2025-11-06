# Kurdi et al. (2023): Implicit evaluation of oppressed social groups
# Analysis code

if (!require(BayesFactor)) {install.packages("BayesFactor"); require(BayesFactor)}
if (!require(bayestestR)) {install.packages("bayestestR"); require(bayestestR)}
if (!require(beanplot)) {install.packages("beanplot"); require(beanplot)}
if (!require(brms)) install.packages("brms"); require(brms)
if (!require(emmeans)) {install.packages("emmeans"); require(emmeans)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(psych)) {install.packages("psych"); require(psych)}
if (!require(reshape2)) {install.packages("reshape2"); require(reshape2)}
if (!require(rstanarm)) {install.packages("rstanarm"); require(rstanarm)}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

citation <- "Kurdi et al. (2023)"
palette <- adjustcolor(c("#009e73", "#0072b2", "#f0e442"), alpha.f = 0.5)

# Study 1 -----------------------------------------------------------------

study1 <- read.csv(paste(citation, "Study 1.csv"))

study1$iatCond <- factor(study1$iatCond)
study1$iatCond <- factor(study1$iatCond, levels = levels(study1$iatCond)[c(2, 1, 3)])

tapply(study1$dScore, study1$iatCond, mean, na.rm = TRUE)
tapply(study1$dScore, study1$iatCond, sd, na.rm = TRUE)

psych::alpha(study1[, c("victimAwful", "victimBad", "victimFantastic", "victimGood", "victimGreat",
                        "victimHorrible", "victimPleasant", "victimTerrible", "victimUnpleasant", "victimWonderful")],
             check.keys = TRUE)
psych::alpha(study1[, c("aggressorAwful", "aggressorBad", "aggressorFantastic", "aggressorGood", "aggressorGreat",
                        "aggressorHorrible", "aggressorPleasant", "aggressorTerrible", "aggressorUnpleasant", "aggressorWonderful")],
             check.keys = TRUE)
psych::alpha(study1[, c("controlAwful", "controlBad", "controlFantastic", "controlGood", "controlGreat",
                        "controlHorrible", "controlPleasant", "controlTerrible", "controlUnpleasant", "controlWonderful")],
             check.keys = TRUE)

mean(study1$victimAggressorEval, na.rm = TRUE)
sd(study1$victimAggressorEval, na.rm = TRUE)

mean(study1$aggressorControlEval, na.rm = TRUE)
sd(study1$aggressorControlEval, na.rm = TRUE)

mean(study1$victimControlEval, na.rm = TRUE)
sd(study1$victimControlEval, na.rm = TRUE)

# Explicit evaluations

study1Exp <- melt(study1[, c("subjSessionID", "study", "victimAggressorEval",
                             "aggressorControlEval", "victimControlEval", "names")],
                  id.vars = c("subjSessionID", "study", "names"))

study1Exp$valueS <- NA
study1Exp$valueS[study1Exp$variable == "victimAggressorEval" & study1Exp$study == "Study 1A"] <-
     scale(study1Exp$value[study1Exp$variable == "victimAggressorEval"  & study1Exp$study == "Study 1A"], center = FALSE)
study1Exp$valueS[study1Exp$variable == "aggressorControlEval" & study1Exp$study == "Study 1A"] <-
     scale(study1Exp$value[study1Exp$variable == "aggressorControlEval" & study1Exp$study == "Study 1A"], center = FALSE)
study1Exp$valueS[study1Exp$variable == "victimControlEval" & study1Exp$study == "Study 1A"] <-
     scale(study1Exp$value[study1Exp$variable == "victimControlEval" & study1Exp$study == "Study 1A"], center = FALSE)
study1Exp$valueS[study1Exp$variable == "victimAggressorEval" & study1Exp$study == "Study 1B"] <-
     scale(study1Exp$value[study1Exp$variable == "victimAggressorEval"  & study1Exp$study == "Study 1B"], center = FALSE)
study1Exp$valueS[study1Exp$variable == "aggressorControlEval" & study1Exp$study == "Study 1B"] <-
     scale(study1Exp$value[study1Exp$variable == "aggressorControlEval" & study1Exp$study == "Study 1B"], center = FALSE)
study1Exp$valueS[study1Exp$variable == "victimControlEval" & study1Exp$study == "Study 1B"] <-
     scale(study1Exp$value[study1Exp$variable == "victimControlEval" & study1Exp$study == "Study 1B"], center = FALSE)

bayesMixedFitExpStudy1A <- stan_glmer(valueS ~ variable + (1 | subjSessionID) + (1 | names),
                                 study1Exp[study1Exp$study == "Study 1A", ])
bayesMixedFitExpStudy1A

interceptPriorExpStudy1 <- normal(location = bayesMixedFitExpStudy1A$coefficients["(Intercept)"],
                         scale = (posterior_interval(bayesMixedFitExpStudy1A)["(Intercept)", ][2]-posterior_interval(bayesMixedFitExpStudy1A)["(Intercept)", ][1])/2,
                         autoscale = FALSE)
slopePriorsExpStudy1 <- normal(location = c(bayesMixedFitExpStudy1A$coefficients["variableaggressorControlEval"],
                                   bayesMixedFitExpStudy1A$coefficients["variablevictimControlEval"]),
                      scale = c((posterior_interval(bayesMixedFitExpStudy1A)["variableaggressorControlEval", ][2]-posterior_interval(bayesMixedFitExpStudy1A)["variableaggressorControlEval", ][1])/2,
                                (posterior_interval(bayesMixedFitExpStudy1A)["variablevictimControlEval", ][2]-posterior_interval(bayesMixedFitExpStudy1A)["variablevictimControlEval", ][1])/2),
                      autoscale = FALSE)

bayesMixedFitExpStudy1B <- stan_glmer(valueS ~ variable + (1 | subjSessionID) + (1 | names),
                                      study1Exp[study1Exp$study == "Study 1B", ],
                                      prior = slopePriorsExpStudy1, prior_intercept = interceptPriorExpStudy1)
bayesMixedFitExpStudy1B

emmeans(bayesMixedFitExpStudy1B, ~ variable)

emmeansExpStudy1 <- data.frame(emmeans(bayesMixedFitExpStudy1B, ~ variable))

# Implicit evaluations

study1$dScoreS <- NA
study1$dScoreS[study1$study == "Study 1A"] <- scale(study1$dScore[study1$study == "Study 1A"], center = FALSE)
study1$dScoreS[study1$study == "Study 1B"] <- scale(study1$dScore[study1$study == "Study 1B"], center = FALSE)

bayesMixedFitImpStudy1A <- stan_glmer(dScoreS ~ iatCond + (1 | names),
                                      study1[study1$study == "Study 1A", ])
bayesMixedFitImpStudy1A

interceptPriorImpStudy1 <- normal(location = bayesMixedFitImpStudy1A$coefficients["(Intercept)"],
                                  scale = (posterior_interval(bayesMixedFitImpStudy1A)["(Intercept)", ][2]-posterior_interval(bayesMixedFitImpStudy1A)["(Intercept)", ][1])/2,
                                  autoscale = FALSE)
slopePriorsImpStudy1 <- normal(location = c(bayesMixedFitImpStudy1A$coefficients["iatCondAggressor/Control"],
                                            bayesMixedFitImpStudy1A$coefficients["iatCondVictim/Control"]),
                               scale = c((posterior_interval(bayesMixedFitImpStudy1A)["iatCondAggressor/Control", ][2]-posterior_interval(bayesMixedFitImpStudy1A)["iatCondAggressor/Control", ][1])/2,
                                         (posterior_interval(bayesMixedFitImpStudy1A)["iatCondVictim/Control", ][2]-posterior_interval(bayesMixedFitImpStudy1A)["iatCondVictim/Control", ][1])/2),
                               autoscale = FALSE)

bayesMixedFitImpStudy1B <- stan_glmer(dScoreS ~ iatCond + (1 | names),
                                      study1[study1$study == "Study 1B", ],
                                      prior = slopePriorsImpStudy1, prior_intercept = interceptPriorImpStudy1)
bayesMixedFitImpStudy1B

emmeans(bayesMixedFitImpStudy1B, ~ iatCond)

emmeansImpStudy1 <- data.frame(emmeans(bayesMixedFitImpStudy1B, ~ iatCond))

pdf(paste(citation, "Study 1.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(study1$victimAggressorEval, center = FALSE),
         scale(study1$aggressorControlEval, center = FALSE),
         scale(study1$victimControlEval, center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[2], palette[3]),
         border = "black",
         main = "Explicit evaluations\n(Study 1)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = 1:3, labels = levels(study1$iatCond))
axis(2)
for (i in 1:3) {
  points(i, emmeansExpStudy1$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansExpStudy1$lower.HPD[i], i, emmeansExpStudy1$upper.HPD[i])
}
beanplot(scale(study1$dScore[study1$iatCond == levels(study1$iatCond)[1]], center = FALSE),
         scale(study1$dScore[study1$iatCond == levels(study1$iatCond)[2]], center = FALSE),
         scale(study1$dScore[study1$iatCond == levels(study1$iatCond)[3]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[2], palette[3]),
         border = "black",
         main = "Implicit evaluations\n(Study 1)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = 1:3, labels = levels(study1$iatCond))
axis(2)
for (i in 1:3) {
  points(i, emmeansImpStudy1$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansImpStudy1$lower.HPD[i], i, emmeansImpStudy1$upper.HPD[i])
}
par(op)
dev.off()


# Study 2 -----------------------------------------------------------------

study2 <- read.csv(paste(citation, "Study 2.csv"))

study2$learnCond <- factor(study2$learnCond)

study2$iatCond <- factor(study2$iatCond)
study2$iatCond <- factor(study2$iatCond, levels = levels(study2$iatCond)[c(2, 1, 3)])

study2$names <- NA

for (i in 1:nrow(study2)) {
     if (study2$iatCond[i] == "Victim/Aggressor") study2$names[i] <- paste0(study2$victimName[i], "/", study2$aggressorName[i])
     else if (study2$iatCond[i] == "Aggressor/Control") study2$names[i] <- paste0(study2$aggressorName[i], "/", study2$controlName[i])
     else if (study2$iatCond[i] == "Victim/Control") study2$names[i] <- paste0(study2$victimName[i], "/", study2$controlName[i])
}

tapply(study2$dScore, list(study2$learnCond, study2$iatCond), mean, na.rm = TRUE)
tapply(study2$dScore, list(study2$learnCond, study2$iatCond), sd, na.rm = TRUE)

tapply(study2$expEval, list(study2$learnCond, study2$iatCond), mean, na.rm = TRUE)
tapply(study2$expEval, list(study2$learnCond, study2$iatCond), sd, na.rm = TRUE)

psych::alpha(study2[, c(paste0("victimGood", 1:5), paste0("victimBad", 1:5))],
             check.keys = TRUE)
psych::alpha(study2[, c(paste0("aggressorGood", 1:5), paste0("aggressorBad", 1:5))],
             check.keys = TRUE)
psych::alpha(study2[, c(paste0("controlGood", 1:5), paste0("controlBad", 1:5))],
             check.keys = TRUE)

# Explicit evaluations

study2$expEvalS <- scale(study2$expEval, center = FALSE)

bayesMixedFitExpStudy2 <- stan_glmer(expEvalS ~ learnCond * iatCond + (1 | names),
                                study2)
bayesMixedFitExpStudy2

summary(bayesMixedFitExpStudy2)

emmeans(bayesMixedFitExpStudy2, ~ learnCond | iatCond)
pairs(emmeans(bayesMixedFitExpStudy2, ~ learnCond | iatCond))

emmeansExpStudy2 <- data.frame(emmeans(bayesMixedFitExpStudy2, ~ learnCond | iatCond))

lmFitExpStudy2 <- lm(expEvalS ~ learnCond * iatCond, study2)

emmeans(lmFitExpStudy2, ~ learnCond | iatCond)
pairs(emmeans(lmFitExpStudy2, ~ learnCond | iatCond))

# Implicit evaluations

study2$dScoreS <- scale(study2$dScore, center = FALSE)

bayesMixedFitImpStudy2 <- stan_glmer(dScoreS ~ learnCond * iatCond + (1 | names),
                                     study2)
bayesMixedFitImpStudy2

summary(bayesMixedFitImpStudy2)

emmeans(bayesMixedFitImpStudy2, ~ learnCond | iatCond)
pairs(emmeans(bayesMixedFitImpStudy2, ~ learnCond | iatCond))

emmeansImpStudy2 <- data.frame(emmeans(bayesMixedFitImpStudy2, ~ learnCond | iatCond))

lmFitImpStudy2 <- lm(dScoreS ~ learnCond * iatCond, study2)

emmeans(lmFitImpStudy2, ~ learnCond | iatCond)
pairs(emmeans(lmFitImpStudy2, ~ learnCond | iatCond))

# Explicit knowledge

prop.table(table(grepl("Armenia|armenia|Turk|turk", study2$guess[!is.na(study2$guess) & study2$learnCond == "Experimental"])))
prop.table(table(grepl("Nazi|nazi|Jew|jew|german|German", study2$guess[!is.na(study2$guess) & study2$learnCond == "Experimental"])))

mean(study2$knowledge, na.rm = TRUE)
sd(study2$knowledge, na.rm = TRUE)
quantile(ttestBF(study2$knowledge[!is.na(study2$knowledge)], posterior = TRUE, iterations = 1000)[, "mu"], c(0.025, 0.975))

pdf(paste(citation, "Study 2.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(study2$expEval[study2$learnCond == levels(study2$learnCond)[1] & study2$iatCond == levels(study2$iatCond)[1]], center = FALSE),
         scale(study2$expEval[study2$learnCond == levels(study2$learnCond)[2] & study2$iatCond == levels(study2$iatCond)[1]], center = FALSE),
         scale(study2$expEval[study2$learnCond == levels(study2$learnCond)[1] & study2$iatCond == levels(study2$iatCond)[2]], center = FALSE),
         scale(study2$expEval[study2$learnCond == levels(study2$learnCond)[2] & study2$iatCond == levels(study2$iatCond)[2]], center = FALSE),
         scale(study2$expEval[study2$learnCond == levels(study2$learnCond)[1] & study2$iatCond == levels(study2$iatCond)[3]], center = FALSE),
         scale(study2$expEval[study2$learnCond == levels(study2$learnCond)[2] & study2$iatCond == levels(study2$iatCond)[3]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1], palette[2], palette[2], palette[3], palette[3]),
         border = "black",
         main = "Explicit evaluations\n(Study 2)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1.5, 3.5, 5.5), labels = levels(study2$iatCond))
axis(2)
legend("topleft", pch = c(21, 23), pt.bg = c("black", "transparent"),
       legend = c("Control condition", "Experimental condition"), bty = "n")
for (i in 1:6) {
  if (i %% 2 != 0) {
    points(i, emmeansExpStudy2$emmean[i], pch = 21, col = "black", bg = "black")
  }
  else if (i %% 2 == 0) {
    points(i, emmeansExpStudy2$emmean[i], pch = 23, col = "black", bg = "transparent")
  }
  segments(i, emmeansExpStudy2$lower.HPD[i], i, emmeansExpStudy2$upper.HPD[i])
}
beanplot(scale(study2$dScore[study2$learnCond == levels(study2$learnCond)[1] & study2$iatCond == levels(study2$iatCond)[1]], center = FALSE),
         scale(study2$dScore[study2$learnCond == levels(study2$learnCond)[2] & study2$iatCond == levels(study2$iatCond)[1]], center = FALSE),
         scale(study2$dScore[study2$learnCond == levels(study2$learnCond)[1] & study2$iatCond == levels(study2$iatCond)[2]], center = FALSE),
         scale(study2$dScore[study2$learnCond == levels(study2$learnCond)[2] & study2$iatCond == levels(study2$iatCond)[2]], center = FALSE),
         scale(study2$dScore[study2$learnCond == levels(study2$learnCond)[1] & study2$iatCond == levels(study2$iatCond)[3]], center = FALSE),
         scale(study2$dScore[study2$learnCond == levels(study2$learnCond)[2] & study2$iatCond == levels(study2$iatCond)[3]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1], palette[2], palette[2], palette[3], palette[3]),
         border = "black",
         main = "Implicit evaluations\n(Study 2)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1.5, 3.5, 5.5), labels = levels(study2$iatCond))
axis(2)
legend("topleft", pch = c(21, 23), pt.bg = c("black", "transparent"),
       legend = c("Control condition", "Experimental condition"), bty = "n")
for (i in 1:6) {
  if (i %% 2 != 0) {
    points(i, emmeansImpStudy2$emmean[i], pch = 21, col = "black", bg = "black")
  }
  else if (i %% 2 == 0) {
    points(i, emmeansImpStudy2$emmean[i], pch = 23, col = "black", bg = "transparent")
  }
  segments(i, emmeansImpStudy2$lower.HPD[i], i, emmeansImpStudy2$upper.HPD[i])
}
par(op)
dev.off()

# Study 3 -----------------------------------------------------------------

study3 <- read.csv(paste(citation, "Study 3.csv"))

study3$learnCond <- factor(study3$learnCond)

study3$iatCond <- factor(study3$iatCond)
study3$iatCond <- factor(study3$iatCond, levels = levels(study3$iatCond)[c(2, 1, 3)])

tapply(study3$dScore, list(study3$learnCond, study3$iatCond), mean, na.rm = TRUE)
tapply(study3$dScore, list(study3$learnCond, study3$iatCond), sd, na.rm = TRUE)

tapply(study3$expEval, list(study3$learnCond, study3$iatCond), mean, na.rm = TRUE)
tapply(study3$expEval, list(study3$learnCond, study3$iatCond), sd, na.rm = TRUE)

psych::alpha(study3[, c(paste0("victimGood", 1:5), paste0("victimBad", 1:5))],
             check.keys = TRUE)
psych::alpha(study3[, c(paste0("aggressorGood", 1:5), paste0("aggressorBad", 1:5))],
             check.keys = TRUE)
psych::alpha(study3[, c(paste0("controlGood", 1:5), paste0("controlBad", 1:5))],
             check.keys = TRUE)

# Explicit evaluations

study3$expEvalS <- scale(study3$expEval, center = FALSE)

bayesMixedFitExpStudy3 <- stan_glm(expEvalS ~ learnCond * iatCond, family = gaussian(),
                                   study3)
bayesMixedFitExpStudy3

emmeans(bayesMixedFitExpStudy3, ~ learnCond | iatCond)
pairs(emmeans(bayesMixedFitExpStudy3, ~ learnCond | iatCond))

emmeansExpStudy3 <- data.frame(emmeans(bayesMixedFitExpStudy3, ~ learnCond | iatCond))

lmFitExpStudy3 <- lm(expEvalS ~ learnCond * iatCond, study3)

emmeans(lmFitExpStudy3, ~ learnCond | iatCond)
pairs(emmeans(lmFitExpStudy3, ~ learnCond | iatCond))

# Implicit evaluations

study3$dScoreS <- scale(study3$dScore, center = FALSE)

bayesMixedFitImpStudy3 <- stan_glm(dScoreS ~ learnCond * iatCond, family = gaussian(),
                                   study3)
bayesMixedFitImpStudy3

emmeans(bayesMixedFitImpStudy3, ~ learnCond | iatCond)
pairs(emmeans(bayesMixedFitImpStudy3, ~ learnCond | iatCond))

emmeansImpStudy3 <- data.frame(emmeans(bayesMixedFitImpStudy3, ~ learnCond | iatCond))

lmFitImpStudy3 <- lm(dScoreS ~ learnCond * iatCond, study3)

emmeans(lmFitImpStudy3, ~ learnCond | iatCond)
pairs(emmeans(lmFitImpStudy3, ~ learnCond | iatCond))

# Explicit knowledge

table(study3$guess)
prop.table(table(study3$guess))

mean(study3$knowledge, na.rm = TRUE)
sd(study3$knowledge, na.rm = TRUE)
quantile(ttestBF(study3$knowledge[!is.na(study3$knowledge)], posterior = TRUE, iterations = 1000)[, "mu"], c(0.025, 0.975))

pdf(paste(citation, "Study 3.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(study3$expEval[study3$learnCond == levels(study3$learnCond)[1] & study3$iatCond == levels(study3$iatCond)[1]], center = FALSE),
         scale(study3$expEval[study3$learnCond == levels(study3$learnCond)[2] & study3$iatCond == levels(study3$iatCond)[1]], center = FALSE),
         scale(study3$expEval[study3$learnCond == levels(study3$learnCond)[1] & study3$iatCond == levels(study3$iatCond)[2]], center = FALSE),
         scale(study3$expEval[study3$learnCond == levels(study3$learnCond)[2] & study3$iatCond == levels(study3$iatCond)[2]], center = FALSE),
         scale(study3$expEval[study3$learnCond == levels(study3$learnCond)[1] & study3$iatCond == levels(study3$iatCond)[3]], center = FALSE),
         scale(study3$expEval[study3$learnCond == levels(study3$learnCond)[2] & study3$iatCond == levels(study3$iatCond)[3]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1], palette[2], palette[2], palette[3], palette[3]),
         border = "black",
         main = "Explicit evaluations\n(Study 3)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1.5, 3.5, 5.5), labels = levels(study3$iatCond))
axis(2)
legend("topleft", pch = c(21, 23), pt.bg = c("black", "transparent"),
       legend = c("Control condition", "Experimental condition"), bty = "n")
for (i in 1:6) {
  if (i %% 2 != 0) {
    points(i, emmeansExpStudy3$emmean[i], pch = 21, col = "black", bg = "black")
  }
  else if (i %% 2 == 0) {
    points(i, emmeansExpStudy3$emmean[i], pch = 23, col = "black", bg = "transparent")
  }
  segments(i, emmeansExpStudy3$lower.HPD[i], i, emmeansExpStudy3$upper.HPD[i])
}
beanplot(scale(study3$dScore[study3$learnCond == levels(study3$learnCond)[1] & study3$iatCond == levels(study3$iatCond)[1]], center = FALSE),
         scale(study3$dScore[study3$learnCond == levels(study3$learnCond)[2] & study3$iatCond == levels(study3$iatCond)[1]], center = FALSE),
         scale(study3$dScore[study3$learnCond == levels(study3$learnCond)[1] & study3$iatCond == levels(study3$iatCond)[2]], center = FALSE),
         scale(study3$dScore[study3$learnCond == levels(study3$learnCond)[2] & study3$iatCond == levels(study3$iatCond)[2]], center = FALSE),
         scale(study3$dScore[study3$learnCond == levels(study3$learnCond)[1] & study3$iatCond == levels(study3$iatCond)[3]], center = FALSE),
         scale(study3$dScore[study3$learnCond == levels(study3$learnCond)[2] & study3$iatCond == levels(study3$iatCond)[3]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1], palette[2], palette[2], palette[3], palette[3]),
         border = "black",
         main = "Implicit evaluations\n(Study 3)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1.5, 3.5, 5.5), labels = levels(study3$iatCond))
axis(2)
legend("topleft", pch = c(21, 23), pt.bg = c("black", "transparent"),
       legend = c("Control condition", "Experimental condition"), bty = "n")
for (i in 1:6) {
  if (i %% 2 != 0) {
    points(i, emmeansImpStudy3$emmean[i], pch = 21, col = "black", bg = "black")
  }
  else if (i %% 2 == 0) {
    points(i, emmeansImpStudy3$emmean[i], pch = 23, col = "black", bg = "transparent")
  }
  segments(i, emmeansImpStudy3$lower.HPD[i], i, emmeansImpStudy3$upper.HPD[i])
}
par(op)
dev.off()

# Study 4 -----------------------------------------------------------------

study4 <- read.csv(paste(citation, "Study 4.csv"))

study4$learnCond <- factor(study4$learnCond)

study4$dScore <- study4$dScore*(-1)
study4$expEval <- study4$expEval*(-1)

tapply(study4$dScore, study4$learnCond, mean, na.rm = TRUE)
tapply(study4$dScore, study4$learnCond, sd, na.rm = TRUE)

tapply(study4$expEval, study4$learnCond, mean, na.rm = TRUE)
tapply(study4$expEval, study4$learnCond, sd, na.rm = TRUE)

# Explicit evaluations

study4$expEvalS <- scale(study4$expEval, center = FALSE)

bayesMixedFitExpStudy4 <- stan_glm(expEvalS ~ learnCond, family = gaussian(), study4)
bayesMixedFitExpStudy4

emmeans(bayesMixedFitExpStudy4, ~ learnCond)
pairs(emmeans(bayesMixedFitExpStudy4, ~ learnCond))

emmeansExpStudy4 <- data.frame(emmeans(bayesMixedFitExpStudy4, ~ learnCond))

lmFitExpStudy4 <- lm(expEvalS ~ learnCond, study4)

emmeans(lmFitExpStudy4, ~ learnCond)
pairs(emmeans(lmFitExpStudy4, ~ learnCond))

# Implicit evaluations

study4$dScoreS <- scale(study4$dScore, center = FALSE)

bayesMixedFitImpStudy4 <- stan_glm(dScoreS ~ learnCond, family = gaussian(), study4)
bayesMixedFitImpStudy4

emmeans(bayesMixedFitImpStudy4, ~ learnCond)
pairs(emmeans(bayesMixedFitImpStudy4, ~ learnCond))

emmeansImpStudy4 <- data.frame(emmeans(bayesMixedFitImpStudy4, ~ learnCond))

lmFitImpStudy4 <- lm(dScoreS ~ learnCond, study4)

emmeans(lmFitImpStudy4, ~ learnCond)
pairs(emmeans(lmFitImpStudy4, ~ learnCond))

pdf(paste(citation, "Study 4.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(study4$expEval, center = FALSE)[study4$learnCond == levels(study4$learnCond)[1]],
         scale(study4$expEval, center = FALSE)[study4$learnCond == levels(study4$learnCond)[2]],
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1]),
         border = "black",
         main = "Explicit evaluations\n(Study 4)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1, 2), labels = levels(study4$learnCond))
axis(2)
for (i in 1:2) {
  points(i, emmeansExpStudy4$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansExpStudy4$lower.HPD[i], i, emmeansExpStudy4$upper.HPD[i])
}
beanplot(scale(study4$dScore, center = FALSE)[study4$learnCond == levels(study4$learnCond)[1]],
         scale(study4$dScore, center = FALSE)[study4$learnCond == levels(study4$learnCond)[2]],
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1]),
         border = "black",
         main = "Implicit evaluations\n(Study 4)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1, 2), labels = levels(study4$learnCond))
axis(2)
for (i in 1:2) {
  points(i, emmeansImpStudy4$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansImpStudy4$lower.HPD[i], i, emmeansImpStudy4$upper.HPD[i])
}
par(op)
dev.off()

# Explicit memory (exploratory)

mean(study4$memoryAccTotal, na.rm = TRUE)
sd(study4$memoryAccTotal, na.rm = TRUE)
quantile(ttestBF(study4$memoryAccTotal[!is.na(study4$memoryAccTotal)], posterior = TRUE, iterations = 1000)[, "mu"], c(0.025, 0.975))

cor(study4$memoryAccTotal, study4$expEval, use = "complete.obs")
cor(study4$memoryAccTotal, study4$dScore, use = "complete.obs")

1/correlationBF(study4$memoryAccTotal, study4$expEval)
1/correlationBF(study4$memoryAccTotal, study4$dScore)

# Study 5A -----------------------------------------------------------------

study5A <- read.csv(paste(citation, "Study 5A.csv"))

study5A$learnCond <- factor(study5A$learnCond)
study5A$dScore <- study5A$dScore*(-1)
study5A$expEval <- study5A$expEval*(-1)

tapply(study5A$dScore, study5A$learnCond, mean, na.rm = TRUE)
tapply(study5A$dScore, study5A$learnCond, sd, na.rm = TRUE)

tapply(study5A$expEval, study5A$learnCond, mean, na.rm = TRUE)
tapply(study5A$expEval, study5A$learnCond, sd, na.rm = TRUE)

psych::alpha(study5A[, c(paste0("target2Good", 1:5), paste0("target2Bad", 1:5))],
             check.keys = TRUE)
psych::alpha(study5A[, c(paste0("target1Good", 1:5), paste0("target1Bad", 1:5))],
             check.keys = TRUE)

# Explicit evaluations

study5A$expEvalS <- scale(study5A$expEval, center = FALSE)

bayesMixedFitExpStudy5A <- stan_glm(expEvalS ~ learnCond, family = gaussian(), study5A)
bayesMixedFitExpStudy5A

emmeans(bayesMixedFitExpStudy5A, ~ learnCond)
pairs(emmeans(bayesMixedFitExpStudy5A, ~ learnCond))
1/lmBF(expEvalS ~ learnCond, study5A[!is.na(study5A$expEvalS), ])

emmeansExpStudy5A <- data.frame(emmeans(bayesMixedFitExpStudy5A, ~ learnCond))

lmFitExpStudy5A <- lm(expEvalS ~ learnCond, study5A)

emmeans(lmFitExpStudy5A, ~ learnCond)
pairs(emmeans(lmFitExpStudy5A, ~ learnCond))

# Implicit evaluations

study5A$dScoreS <- scale(study5A$dScore, center = FALSE)

bayesMixedFitImpStudy5A <- stan_glm(dScoreS ~ learnCond, family = gaussian(), study5A)
bayesMixedFitImpStudy5A

emmeans(bayesMixedFitImpStudy5A, ~ learnCond)
pairs(emmeans(bayesMixedFitImpStudy5A, ~ learnCond))
1/lmBF(dScoreS ~ learnCond, study5A[!is.na(study5A$dScoreS), ])

emmeansImpStudy5A <- data.frame(emmeans(bayesMixedFitImpStudy5A, ~ learnCond))

lmFitImpStudy5A <- lm(dScoreS ~ learnCond, study5A)

emmeans(lmFitImpStudy5A, ~ learnCond)
pairs(emmeans(lmFitImpStudy5A, ~ learnCond))

pdf(paste(citation, "Study 5A.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(study5A$expEval, center = FALSE)[study5A$learnCond == levels(study5A$learnCond)[1]],
         scale(study5A$expEval, center = FALSE)[study5A$learnCond == levels(study5A$learnCond)[2]],
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1]),
         border = "black",
         main = "Explicit evaluations\n(Study 5A)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1, 2), labels = levels(study5A$learnCond))
axis(2)
for (i in 1:2) {
  points(i, emmeansExpStudy5A$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansExpStudy5A$lower.HPD[i], i, emmeansExpStudy5A$upper.HPD[i])
}
beanplot(scale(study5A$dScore, center = FALSE)[study5A$learnCond == levels(study5A$learnCond)[1]],
         scale(study5A$dScore, center = FALSE)[study5A$learnCond == levels(study5A$learnCond)[2]],
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1]),
         border = "black",
         main = "Implicit evaluations\n(Study 5A)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1, 2), labels = levels(study5A$learnCond))
axis(2)
for (i in 1:2) {
  points(i, emmeansImpStudy5A$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansImpStudy5A$lower.HPD[i], i, emmeansImpStudy5A$upper.HPD[i])
}
par(op)
dev.off()

# Study 5B -----------------------------------------------------------------

study5B <- read.csv(paste(citation, "Study 5B.csv"))

mean(study5B$dScore, na.rm = TRUE)
sd(study5B$dScore, na.rm = TRUE)

mean(study5B$expEval, na.rm = TRUE)
sd(study5B$expEval, na.rm = TRUE)

psych::alpha(study5B[, c(paste0("target2Good", 1:5), paste0("target2Bad", 1:5))],
             check.keys = TRUE)
psych::alpha(study5B[, c(paste0("target1Good", 1:5), paste0("target1Bad", 1:5))],
             check.keys = TRUE)

# Explicit evaluations

study5B$expEvalS <- scale(study5B$expEval, center = FALSE)
study5B$names <- paste0(study5B$target1Name, "/", study5B$target2Name)

bayesMixedFitExpStudy5B <- stan_glmer(expEvalS ~ 1 + (1 | names), family = gaussian(), study5B)
bayesMixedFitExpStudy5B

hdi(bayesMixedFitExpStudy5B, ci = 0.95)

emmeansExpStudy5B <- cbind(fixef(bayesMixedFitExpStudy5B), hdi(bayesMixedFitExpStudy5B, ci = 0.95)$CI_low,
                           hdi(bayesMixedFitExpStudy5B, ci = 0.95)$CI_high)
emmeansExpStudy5B <- data.frame(emmeansExpStudy5B)
colnames(emmeansExpStudy5B) <- c("emmean", "lower.HPD", "upper.HPD")

lmFitExpStudy5B <- lm(expEvalS ~ 1, study5B)
summary(lmFitExpStudy5B)

# Implicit evaluations

study5B$dScoreS <- scale(study5B$dScore, center = FALSE)

bayesMixedFitImpStudy5B <- stan_glmer(dScoreS ~ 1 + (1 | names), family = gaussian(), study5B)
bayesMixedFitImpStudy5B

hdi(bayesMixedFitImpStudy5B, ci = 0.95)

emmeansImpStudy5B <- cbind(fixef(bayesMixedFitImpStudy5B), hdi(bayesMixedFitImpStudy5B, ci = 0.95)$CI_low,
                           hdi(bayesMixedFitImpStudy5B, ci = 0.95)$CI_high)
emmeansImpStudy5B <- data.frame(emmeansImpStudy5B)
colnames(emmeansImpStudy5B) <- c("emmean", "lower.HPD", "upper.HPD")

lmFitImpStudy5B <- lm(dScoreS ~ 1, study5B)
summary(lmFitImpStudy5B)

pdf(paste(citation, "Study 5B.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(study5B$expEval, center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1]),
         border = "black",
         main = "Explicit evaluations\n(Study 5B)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(2)
for (i in 1:1) {
  points(i, emmeansExpStudy5B$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansExpStudy5B$lower.HPD[i], i, emmeansExpStudy5B$upper.HPD[i])
}
beanplot(scale(study5B$dScore, center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1]),
         border = "black",
         main = "Implicit evaluations\n(Study 5B)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(2)
for (i in 1:1) {
  points(i, emmeansImpStudy5B$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansImpStudy5B$lower.HPD[i], i, emmeansImpStudy5B$upper.HPD[i])
}
par(op)
dev.off()

# Explicit knowledge items (exploratory)

mean(study5B$knowledge, na.rm = TRUE)
sd(study5B$knowledge, na.rm = TRUE)
quantile(ttestBF(study5B$knowledge[!is.na(study5B$knowledge)], posterior = TRUE, iterations = 1000)[, "mu"], c(0.025, 0.975))

cor(study5B$knowledge, study5B$expEval, use = "complete.obs")
cor(study5B$knowledge, study5B$dScore, use = "complete.obs")

1/correlationBF(study5B$knowledge, study5B$expEval)
1/correlationBF(study5B$knowledge, study5B$dScore)

table(study5B$guessAcc, useNA = "always")

tapply(study5B$expEvalS, study5B$guessAcc, mean, na.rm = TRUE)
tapply(study5B$dScoreS, study5B$guessAcc, mean, na.rm = TRUE)

lmBF(expEval ~ guessAcc, study5B[!(is.na(study5B$expEval)), ])
1/lmBF(dScore ~ guessAcc, study5B)

# Study 5C -----------------------------------------------------------------

study5C <- read.csv(paste(citation, "Study 5C.csv"))

study5C$learnCond <- factor(study5C$learnCond)
study5C$dScore <- study5C$dScore*(-1)
study5C$expEval <- study5C$expEval*(-1)

tapply(study5C$dScore, study5C$learnCond, mean, na.rm = TRUE)
tapply(study5C$dScore, study5C$learnCond, sd, na.rm = TRUE)

tapply(study5C$expEval, study5C$learnCond, mean, na.rm = TRUE)
tapply(study5C$expEval, study5C$learnCond, sd, na.rm = TRUE)

psych::alpha(study5C[, c(paste0("target2Good", 1:5), paste0("target2Bad", 1:5))],
             check.keys = TRUE)
psych::alpha(study5C[, c(paste0("target1Good", 1:5), paste0("target1Bad", 1:5))],
             check.keys = TRUE)

# Explicit evaluations

study5C$expEvalS <- scale(study5C$expEval, center = FALSE)

bayesMixedFitExpStudy5C <- stan_glm(expEvalS ~ learnCond, family = gaussian(), study5C)
bayesMixedFitExpStudy5C

emmeans(bayesMixedFitExpStudy5C, ~ learnCond)
pairs(emmeans(bayesMixedFitExpStudy5C, ~ learnCond))

emmeansExpStudy5C <- data.frame(emmeans(bayesMixedFitExpStudy5C, ~ learnCond))

lmFitExpStudy5C <- lm(expEvalS ~ learnCond, study5C)

emmeans(lmFitExpStudy5C, ~ learnCond)
pairs(emmeans(lmFitExpStudy5C, ~ learnCond))

# Implicit evaluations

study5C$dScoreS <- scale(study5C$dScore, center = FALSE)

bayesMixedFitImpStudy5C <- stan_glm(dScoreS ~ learnCond, family = gaussian(), study5C)
bayesMixedFitImpStudy5C

emmeans(bayesMixedFitImpStudy5C, ~ learnCond)
pairs(emmeans(bayesMixedFitImpStudy5C, ~ learnCond))
1/lmBF(dScoreS ~ learnCond, study5C[!is.na(study5C$dScoreS), ])

emmeansImpStudy5C <- data.frame(emmeans(bayesMixedFitImpStudy5C, ~ learnCond))

lmFitImpStudy5C <- lm(dScoreS ~ learnCond, study5C)

emmeans(lmFitImpStudy5C, ~ learnCond)
pairs(emmeans(lmFitImpStudy5C, ~ learnCond))

pdf(paste(citation, "Study 5C.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(study5C$expEval, center = FALSE)[study5C$learnCond == levels(study5C$learnCond)[1]],
         scale(study5C$expEval, center = FALSE)[study5C$learnCond == levels(study5C$learnCond)[2]],
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1]),
         border = "black",
         main = "Explicit evaluations\n(Study 5C)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1, 2), labels = levels(study5C$learnCond))
axis(2)
for (i in 1:2) {
  points(i, emmeansExpStudy5C$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansExpStudy5C$lower.HPD[i], i, emmeansExpStudy5C$upper.HPD[i])
}
beanplot(scale(study5C$dScore, center = FALSE)[study5C$learnCond == levels(study5C$learnCond)[1]],
         scale(study5C$dScore, center = FALSE)[study5C$learnCond == levels(study5C$learnCond)[2]],
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[1]),
         border = "black",
         main = "Implicit evaluations\n(Study 5C)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = c(1, 2), labels = levels(study5C$learnCond))
axis(2)
for (i in 1:2) {
  points(i, emmeansImpStudy5C$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansImpStudy5C$lower.HPD[i], i, emmeansImpStudy5C$upper.HPD[i])
}
par(op)
dev.off()

# Supplementary Study 1 -----------------------------------------------------------------

suppStudy1 <- read.csv(paste(citation, "Supplementary Study 1.csv"))

suppStudy1$iatCond <- factor(suppStudy1$iatCond)
suppStudy1$iatCond <- factor(suppStudy1$iatCond, levels = levels(suppStudy1$iatCond)[c(2, 1, 3)],
                         labels = c("Victim/Privileged", "Privileged/Control", "Victim/Control"))

tapply(suppStudy1$dScore, suppStudy1$iatCond, mean, na.rm = TRUE)
tapply(suppStudy1$dScore, suppStudy1$iatCond, sd, na.rm = TRUE)

psych::alpha(suppStudy1[, c("victimAwful", "victimBad", "victimFantastic", "victimGood", "victimGreat",
                        "victimHorrible", "victimPleasant", "victimTerrible", "victimUnpleasant", "victimWonderful")],
             check.keys = TRUE)
psych::alpha(suppStudy1[, c("aggressorAwful", "aggressorBad", "aggressorFantastic", "aggressorGood", "aggressorGreat",
                        "aggressorHorrible", "aggressorPleasant", "aggressorTerrible", "aggressorUnpleasant", "aggressorWonderful")],
             check.keys = TRUE)
psych::alpha(suppStudy1[, c("controlAwful", "controlBad", "controlFantastic", "controlGood", "controlGreat",
                        "controlHorrible", "controlPleasant", "controlTerrible", "controlUnpleasant", "controlWonderful")],
             check.keys = TRUE)

mean(suppStudy1$victimAggressorEval, na.rm = TRUE)
sd(suppStudy1$victimAggressorEval, na.rm = TRUE)

mean(suppStudy1$aggressorControlEval, na.rm = TRUE)
sd(suppStudy1$aggressorControlEval, na.rm = TRUE)

mean(suppStudy1$victimControlEval, na.rm = TRUE)
sd(suppStudy1$victimControlEval, na.rm = TRUE)

# Explicit evaluations

suppStudy1Exp <- melt(suppStudy1[, c("subjSessionID", "victimAggressorEval", "aggressorControlEval", 
                             "victimControlEval", "names")], id.vars = c("subjSessionID", "names"))

head(suppStudy1Exp)

suppStudy1Exp$valueS <- NA
suppStudy1Exp$valueS[suppStudy1Exp$variable == "victimAggressorEval"] <- scale(suppStudy1Exp$value[suppStudy1Exp$variable == "victimAggressorEval"], center = FALSE)
suppStudy1Exp$valueS[suppStudy1Exp$variable == "aggressorControlEval"] <- scale(suppStudy1Exp$value[suppStudy1Exp$variable == "aggressorControlEval"], center = FALSE)
suppStudy1Exp$valueS[suppStudy1Exp$variable == "victimControlEval"] <- scale(suppStudy1Exp$value[suppStudy1Exp$variable == "victimControlEval"], center = FALSE)

bayesMixedFitExpSuppStudy1 <- stan_glmer(valueS ~ variable + (1 | subjSessionID) + (1 | names), suppStudy1Exp)
bayesMixedFitExpSuppStudy1

summary(bayesMixedFitExpSuppStudy1)
emmeans(bayesMixedFitExpSuppStudy1, ~ variable)

emmeansExpSuppStudy1 <- data.frame(emmeans(bayesMixedFitExpSuppStudy1, ~ variable))

# Implicit evaluations

suppStudy1$dScoreS <- scale(suppStudy1$dScore, center = FALSE)

bayesMixedFitImpSuppStudy1 <- stan_glmer(dScoreS ~ iatCond + (1 | names), suppStudy1)
bayesMixedFitImpSuppStudy1

summary(bayesMixedFitImpSuppStudy1)
emmeans(bayesMixedFitImpSuppStudy1, ~ iatCond)

emmeansImpSuppStudy1 <- data.frame(emmeans(bayesMixedFitImpSuppStudy1, ~ iatCond))

pdf(paste(citation, "Supplementary Study 1.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(suppStudy1$victimAggressorEval, center = FALSE),
         scale(suppStudy1$aggressorControlEval, center = FALSE),
         scale(suppStudy1$victimControlEval, center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[2], palette[3]),
         border = "black",
         main = "Explicit evaluations\n(Supplementary Study 1)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = 1:3, labels = levels(suppStudy1$iatCond))
axis(2)
for (i in 1:3) {
  points(i, emmeansExpSuppStudy1$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansExpSuppStudy1$lower.HPD[i], i, emmeansExpSuppStudy1$upper.HPD[i])
}
beanplot(scale(suppStudy1$dScore[suppStudy1$iatCond == levels(suppStudy1$iatCond)[1]], center = FALSE),
         scale(suppStudy1$dScore[suppStudy1$iatCond == levels(suppStudy1$iatCond)[2]], center = FALSE),
         scale(suppStudy1$dScore[suppStudy1$iatCond == levels(suppStudy1$iatCond)[3]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[1], palette[2], palette[3]),
         border = "black",
         main = "Implicit evaluations\n(Supplementary Study 1)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = 1:3, labels = levels(suppStudy1$iatCond))
axis(2)
for (i in 1:3) {
  points(i, emmeansImpSuppStudy1$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansImpSuppStudy1$lower.HPD[i], i, emmeansImpSuppStudy1$upper.HPD[i])
}
par(op)
dev.off()

# Trait judgments (exploratory)

# Oppressed group

ttestBF(suppStudy1$victimPrivileged[!is.na(suppStudy1$victimPrivileged)], mu = 4)
cohensD(suppStudy1$victimPrivileged, mu = 4)
mean(suppStudy1$victimPrivileged, na.rm = TRUE)

ttestBF(suppStudy1$victimOppressed[!is.na(suppStudy1$victimOppressed)], mu = 4)
cohensD(suppStudy1$victimOppressed, mu = 4)
mean(suppStudy1$victimOppressed, na.rm = TRUE)

ttestBF(suppStudy1$victimOppressive[!is.na(suppStudy1$victimOppressive)], mu = 4)
cohensD(suppStudy1$victimOppressive, mu = 4)
mean(suppStudy1$victimOppressive, na.rm = TRUE)

ttestBF(suppStudy1$victimWeak[!is.na(suppStudy1$victimWeak)], mu = 4)
cohensD(suppStudy1$victimWeak, mu = 4)
mean(suppStudy1$victimWeak, na.rm = TRUE)

ttestBF(suppStudy1$victimWealthy[!is.na(suppStudy1$victimWealthy)], mu = 4)
cohensD(suppStudy1$victimWealthy, mu = 4)
mean(suppStudy1$victimWealthy, na.rm = TRUE)

ttestBF(suppStudy1$victimPoor[!is.na(suppStudy1$victimPoor)], mu = 4)
cohensD(suppStudy1$victimPoor, mu = 4)
mean(suppStudy1$victimPoor, na.rm = TRUE)

# Privileged group

ttestBF(suppStudy1$aggressorPrivileged[!is.na(suppStudy1$aggressorPrivileged)], mu = 4)
cohensD(suppStudy1$aggressorPrivileged, mu = 4)
mean(suppStudy1$aggressorPrivileged, na.rm = TRUE)

ttestBF(suppStudy1$aggressorOppressed[!is.na(suppStudy1$aggressorOppressed)], mu = 4)
cohensD(suppStudy1$aggressorOppressed, mu = 4)
mean(suppStudy1$aggressorOppressed, na.rm = TRUE)

ttestBF(suppStudy1$aggressorOppressive[!is.na(suppStudy1$aggressorOppressive)], mu = 4)
cohensD(suppStudy1$aggressorOppressive, mu = 4)
mean(suppStudy1$aggressorOppressive, na.rm = TRUE)

ttestBF(suppStudy1$aggressorWeak[!is.na(suppStudy1$aggressorWeak)], mu = 4)
cohensD(suppStudy1$aggressorWeak, mu = 4)
mean(suppStudy1$aggressorWeak, na.rm = TRUE)

ttestBF(suppStudy1$aggressorWealthy[!is.na(suppStudy1$aggressorWealthy)], mu = 4)
cohensD(suppStudy1$aggressorWealthy, mu = 4)
mean(suppStudy1$aggressorWealthy, na.rm = TRUE)

ttestBF(suppStudy1$aggressorPoor[!is.na(suppStudy1$aggressorPoor)], mu = 4)
cohensD(suppStudy1$aggressorPoor, mu = 4)
mean(suppStudy1$aggressorPoor, na.rm = TRUE)

# Control group

ttestBF(suppStudy1$controlPrivileged[!is.na(suppStudy1$controlPrivileged)], mu = 4)
cohensD(suppStudy1$controlPrivileged, mu = 4)
mean(suppStudy1$controlPrivileged, na.rm = TRUE)

ttestBF(suppStudy1$controlOppressed[!is.na(suppStudy1$controlOppressed)], mu = 4)
cohensD(suppStudy1$controlOppressed, mu = 4)
mean(suppStudy1$controlOppressed, na.rm = TRUE)

ttestBF(suppStudy1$controlOppressive[!is.na(suppStudy1$controlOppressive)], mu = 4)
cohensD(suppStudy1$controlOppressive, mu = 4)
mean(suppStudy1$controlOppressive, na.rm = TRUE)

ttestBF(suppStudy1$controlWeak[!is.na(suppStudy1$controlWeak)], mu = 4)
cohensD(suppStudy1$controlWeak, mu = 4)
mean(suppStudy1$controlWeak, na.rm = TRUE)

ttestBF(suppStudy1$controlWealthy[!is.na(suppStudy1$controlWealthy)], mu = 4)
cohensD(suppStudy1$controlWealthy, mu = 4)
mean(suppStudy1$controlWealthy, na.rm = TRUE)

ttestBF(suppStudy1$controlPoor[!is.na(suppStudy1$controlPoor)], mu = 4)
cohensD(suppStudy1$controlPoor, mu = 4)
mean(suppStudy1$controlPoor, na.rm = TRUE)

# Supplementary Study 2 ---------------------------------------------------

suppStudy2 <- read.csv(paste(citation, "Supplementary Study 2.csv"))

suppStudy2$learnCond <- factor(suppStudy2$learnCond)
suppStudy2$learnCond <- factor(suppStudy2$learnCond, levels = levels(suppStudy2$learnCond)[c(3, 2, 1)])

tapply(suppStudy2$dScore, suppStudy2$learnCond, mean, na.rm = TRUE)
tapply(suppStudy2$dScore, suppStudy2$learnCond, sd, na.rm = TRUE)

tapply(suppStudy2$victimControlEval, suppStudy2$learnCond, mean, na.rm = TRUE)
tapply(suppStudy2$victimControlEval, suppStudy2$learnCond, sd, na.rm = TRUE)

# Explicit evaluations

suppStudy2$victimControlEvalS <- scale(suppStudy2$victimControlEval, center = FALSE)

bayesMixedFitExpSuppStudy2 <- stan_glmer(victimControlEvalS ~ learnCond + (1 | names), suppStudy2)
bayesMixedFitExpSuppStudy2

emmeans(bayesMixedFitExpSuppStudy2, ~ learnCond)
pairs(emmeans(bayesMixedFitExpSuppStudy2, ~ learnCond))

emmeansExpSuppStudy2 <- data.frame(emmeans(bayesMixedFitExpSuppStudy2, ~ learnCond))

# Implicit evaluations

suppStudy2$dScoreS <- scale(suppStudy2$dScore, center = FALSE)

bayesMixedFitImpSuppStudy2 <- stan_glmer(dScoreS ~ learnCond + (1 | names), suppStudy2)
bayesMixedFitImpSuppStudy2

emmeans(bayesMixedFitImpSuppStudy2, ~ learnCond)
pairs(emmeans(bayesMixedFitImpSuppStudy2, ~ learnCond))

emmeansImpSuppStudy2 <- data.frame(emmeans(bayesMixedFitImpSuppStudy2, ~ learnCond))

pdf(paste(citation, "Supplementary Study 2.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(suppStudy2$victimControlEval[suppStudy2$learnCond == levels(suppStudy2$learnCond)[1]], center = FALSE),
         scale(suppStudy2$victimControlEval[suppStudy2$learnCond == levels(suppStudy2$learnCond)[2]], center = FALSE),
         scale(suppStudy2$victimControlEval[suppStudy2$learnCond == levels(suppStudy2$learnCond)[3]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[3], palette[3], palette[3]),
         border = "black",
         main = "Explicit evaluations\n(Supplementary Study 2)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = 1:3, labels = levels(suppStudy2$learnCond))
axis(2)
for (i in 1:3) {
  points(i, emmeansExpSuppStudy2$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansExpSuppStudy2$lower.HPD[i], i, emmeansExpSuppStudy2$upper.HPD[i])
}
beanplot(scale(suppStudy2$dScore[suppStudy2$learnCond == levels(suppStudy2$learnCond)[1]], center = FALSE),
         scale(suppStudy2$dScore[suppStudy2$learnCond == levels(suppStudy2$learnCond)[2]], center = FALSE),
         scale(suppStudy2$dScore[suppStudy2$learnCond == levels(suppStudy2$learnCond)[3]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[3], palette[3], palette[3]),
         border = "black",
         main = "Implicit evaluations\n(Supplementary Study 2)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = 1:3, labels = levels(suppStudy2$learnCond))
axis(2)
for (i in 1:3) {
  points(i, emmeansImpSuppStudy2$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansImpSuppStudy2$lower.HPD[i], i, emmeansImpSuppStudy2$upper.HPD[i])
}
par(op)
dev.off()


# Supplementary Study 3 ---------------------------------------------------

suppStudy3 <- read.csv(paste(citation, "Supplementary Study 3.csv"))

suppStudy3$learnCond <- factor(suppStudy3$learnCond)

suppStudy3$names <- paste0(suppStudy3$victimName, "/", suppStudy3$controlName)

tapply(suppStudy3$dScore, suppStudy3$learnCond, mean, na.rm = TRUE)
tapply(suppStudy3$dScore, suppStudy3$learnCond, sd, na.rm = TRUE)

tapply(suppStudy3$expEval, suppStudy3$learnCond, mean, na.rm = TRUE)
tapply(suppStudy3$expEval, suppStudy3$learnCond, sd, na.rm = TRUE)

# Explicit evaluations

suppStudy3$expEvalS <- scale(suppStudy3$expEval, center = FALSE)

bayesMixedFitExpSuppStudy3 <- stan_glmer(expEvalS ~ learnCond + (1 | names), suppStudy3)
bayesMixedFitExpSuppStudy3

emmeans(bayesMixedFitExpSuppStudy3, ~ learnCond)
pairs(emmeans(bayesMixedFitExpSuppStudy3, ~ learnCond))

emmeansExpSuppStudy3 <- data.frame(emmeans(bayesMixedFitExpSuppStudy3, ~ learnCond))

lmFitExpSuppStudy3 <- lm(expEvalS ~ learnCond, suppStudy3)
lmFitExpSuppStudy3

emmeans(lmFitExpSuppStudy3, ~ learnCond)
pairs(emmeans(lmFitExpSuppStudy3, ~ learnCond))

# Implicit evaluations

suppStudy3$dScoreS <- scale(suppStudy3$dScore, center = FALSE)

bayesMixedFitImpSuppStudy3 <- stan_glmer(dScoreS ~ learnCond + (1 | names), suppStudy3)
bayesMixedFitImpSuppStudy3

emmeans(bayesMixedFitImpSuppStudy3, ~ learnCond)
pairs(emmeans(bayesMixedFitImpSuppStudy3, ~ learnCond))

emmeansImpSuppStudy3 <- data.frame(emmeans(bayesMixedFitImpSuppStudy3, ~ learnCond))

lmFitImpSuppStudy3 <- lm(dScoreS ~ learnCond, suppStudy3)
lmFitImpSuppStudy3

emmeans(lmFitImpSuppStudy3, ~ learnCond)
pairs(emmeans(lmFitImpSuppStudy3, ~ learnCond))

pdf(paste(citation, "Supplementary Study 3.pdf"), width = 12)
op <- par(mfrow = c(1, 2))
beanplot(scale(suppStudy3$expEval[suppStudy3$learnCond == levels(suppStudy3$learnCond)[1]], center = FALSE),
         scale(suppStudy3$expEval[suppStudy3$learnCond == levels(suppStudy3$learnCond)[2]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[3], palette[3]),
         border = "black",
         main = "Explicit evaluations\n(Supplementary Study 3)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = 1:2, labels = levels(suppStudy3$learnCond))
axis(2)
for (i in 1:2) {
  points(i, emmeansExpSuppStudy3$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansExpSuppStudy3$lower.HPD[i], i, emmeansExpSuppStudy3$upper.HPD[i])
}
beanplot(scale(suppStudy3$dScore[suppStudy3$learnCond == levels(suppStudy3$learnCond)[1]], center = FALSE),
         scale(suppStudy3$dScore[suppStudy3$learnCond == levels(suppStudy3$learnCond)[2]], center = FALSE),
         what = c(0, 1, 0, 0),
         col = list(palette[3], palette[3]),
         border = "black",
         main = "Implicit evaluations\n(Supplementary Study 3)",
         axes = FALSE, bw = 0.2, ylab = "Standardized mean difference",
         ylim = c(-4, 4))
abline(h = 0, lty = 3)
axis(1, at = 1:2, labels = levels(suppStudy3$learnCond))
axis(2)
for (i in 1:2) {
  points(i, emmeansImpSuppStudy3$emmean[i], pch = 21, col = "black", bg = "black")
  segments(i, emmeansImpSuppStudy3$lower.HPD[i], i, emmeansImpSuppStudy3$upper.HPD[i])
}
par(op)
dev.off()

# Internal consistency of Implicit Association Tests (Study 1) ----------------------

calcD <- function(block1, block2) {
     is.na(block1) <- block1 > 10000
     is.na(block2) <- block2 > 10000
     mean1 <- mean(as.numeric(block1), na.rm = TRUE)
     mean2 <- mean(as.numeric(block2), na.rm = TRUE)
     sd <- sd(c(as.numeric(block1), as.numeric(block2)), na.rm = TRUE)
     d <- (mean2 - mean1) / sd
     return(d)
}

study1long <- read.csv(paste(citation, "Study 1 Long-format IAT.csv"))
study1long <- study1long[!(study1long$session_id %in% names(table(study1long$session_id)[!(table(study1long$session_id) == 140)])), ]

study1longCrit <- study1long[grepl("Group", study1long$block_pairing_definition)
                             & grepl("Good", study1long$block_pairing_definition), ]

study1longCritAV <- study1longCrit[grepl("Group 1", study1longCrit$block_pairing_definition)
                                   & grepl("Group 2", study1longCrit$block_pairing_definition), ]
study1longCritAC <- study1longCrit[grepl("Group 1", study1longCrit$block_pairing_definition)
                                   & grepl("Group 3", study1longCrit$block_pairing_definition), ]
study1longCritVC <- study1longCrit[grepl("Group 2", study1longCrit$block_pairing_definition)
                                   & grepl("Group 3", study1longCrit$block_pairing_definition), ]

# Victim/Aggressor IAT

for (i in 1:length(study1longCritAV$trial_number)) {
     if (grepl("Group 1/Bad", study1longCritAV[study1longCritAV$block_number == 3, ]$block_pairing_definition[i])) {
          study1longCritAV[study1longCritAV$block_number == 3, ]$trial_number[i] <- study1longCritAV[study1longCritAV$block_number == 3, ]$trial_number[i] + 40
     }
     else if (grepl("Group 1/Bad", study1longCritAV[study1longCritAV$block_number == 5, ]$block_pairing_definition[i])) {
          study1longCritAV[study1longCritAV$block_number == 5, ]$trial_number[i] <- study1longCritAV[study1longCritAV$block_number == 5, ]$trial_number[i] + 40
     }
     print(i/length(study1longCritAV$trial_number))
}

table(table(study1longCritAV$trial_number))

study1AVIATWide <- reshape(study1longCritAV, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study1AVIATWide), gregexpr("[[:digit:]]+", colnames(study1AVIATWide)))
colnames(study1AVIATWide)[2:dim(study1AVIATWide)[2]] <- as.numeric(unlist(colNum))
study1AVIATWide <- study1AVIATWide[, order(as.numeric(colnames(study1AVIATWide)))]

relIATstudy1AV <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study1AVIATWide)) {
          firstHalf[j] <- calcD(study1AVIATWide[j, which(colnames(study1AVIATWide) == "1"):which(colnames(study1AVIATWide) == "40")][, ranVec],
                                study1AVIATWide[j, which(colnames(study1AVIATWide) == "41"):which(colnames(study1AVIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study1AVIATWide[j, which(colnames(study1AVIATWide) == "1"):which(colnames(study1AVIATWide) == "40")][, -ranVec],
                                 study1AVIATWide[j, which(colnames(study1AVIATWide) == "41"):which(colnames(study1AVIATWide) == "80")][, -ranVec])    
     }
     relIATstudy1AV[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy1AV)

# Aggressor/Control IAT

for (i in 1:length(study1longCritAC$trial_number)) {
     if (grepl("Group 1/Bad", study1longCritAC[study1longCritAC$block_number == 3, ]$block_pairing_definition[i])) {
          study1longCritAC[study1longCritAC$block_number == 3, ]$trial_number[i] <- study1longCritAC[study1longCritAC$block_number == 3, ]$trial_number[i] + 40
     }
     else if (grepl("Group 1/Bad", study1longCritAC[study1longCritAC$block_number == 5, ]$block_pairing_definition[i])) {
          study1longCritAC[study1longCritAC$block_number == 5, ]$trial_number[i] <- study1longCritAC[study1longCritAC$block_number == 5, ]$trial_number[i] + 40
     }
     print(i/length(study1longCritAC$trial_number))
}

table(table(study1longCritAC$trial_number))

study1ACIATWide <- reshape(study1longCritAC, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study1ACIATWide), gregexpr("[[:digit:]]+", colnames(study1ACIATWide)))
colnames(study1ACIATWide)[2:dim(study1ACIATWide)[2]] <- as.numeric(unlist(colNum))
study1ACIATWide <- study1ACIATWide[, order(as.numeric(colnames(study1ACIATWide)))]

relIATstudy1AC <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study1ACIATWide)) {
          firstHalf[j] <- calcD(study1ACIATWide[j, which(colnames(study1ACIATWide) == "1"):which(colnames(study1ACIATWide) == "40")][, ranVec],
                                study1ACIATWide[j, which(colnames(study1ACIATWide) == "41"):which(colnames(study1ACIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study1ACIATWide[j, which(colnames(study1ACIATWide) == "1"):which(colnames(study1ACIATWide) == "40")][, -ranVec],
                                 study1ACIATWide[j, which(colnames(study1ACIATWide) == "41"):which(colnames(study1ACIATWide) == "80")][, -ranVec])    
     }
     relIATstudy1AC[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy1AC)

# Victim/Control IAT

for (i in 1:length(study1longCritVC$trial_number)) {
     if (grepl("Group 2/Bad", study1longCritVC[study1longCritVC$block_number == 3, ]$block_pairing_definition[i])) {
          study1longCritVC[study1longCritVC$block_number == 3, ]$trial_number[i] <- study1longCritVC[study1longCritVC$block_number == 3, ]$trial_number[i] + 40
     }
     else if (grepl("Group 2/Bad", study1longCritVC[study1longCritVC$block_number == 5, ]$block_pairing_definition[i])) {
          study1longCritVC[study1longCritVC$block_number == 5, ]$trial_number[i] <- study1longCritVC[study1longCritVC$block_number == 5, ]$trial_number[i] + 40
     }
     print(i/length(study1longCritVC$trial_number))
}

table(table(study1longCritVC$trial_number))

study1VCIATWide <- reshape(study1longCritVC, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study1VCIATWide), gregexpr("[[:digit:]]+", colnames(study1VCIATWide)))
colnames(study1VCIATWide)[2:dim(study1VCIATWide)[2]] <- as.numeric(unlist(colNum))
study1VCIATWide <- study1VCIATWide[, order(as.numeric(colnames(study1VCIATWide)))]

relIATstudy1VC <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study1VCIATWide)) {
          firstHalf[j] <- calcD(study1VCIATWide[j, which(colnames(study1VCIATWide) == "1"):which(colnames(study1VCIATWide) == "40")][, ranVec],
                                study1VCIATWide[j, which(colnames(study1VCIATWide) == "41"):which(colnames(study1VCIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study1VCIATWide[j, which(colnames(study1VCIATWide) == "1"):which(colnames(study1VCIATWide) == "40")][, -ranVec],
                                 study1VCIATWide[j, which(colnames(study1VCIATWide) == "41"):which(colnames(study1VCIATWide) == "80")][, -ranVec])    
     }
     relIATstudy1VC[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy1VC)

# Internal consistency of Implicit Association Tests (Study 2) ----------------------

study2long <- read.csv(paste(citation, "Study 2 Long-format IAT.csv"))
study2long <- study2long[!(study2long$session_id %in% names(table(study2long$session_id)[!(table(study2long$session_id) == 140)])), ]

study2longCrit <- study2long[grepl("Bonnians|Jebbians|Laapians|Niffians", study2long$block_pairing_definition)
                             & grepl("Good", study2long$block_pairing_definition), ]

study2longCritAV <- study2longCrit[study2longCrit$session_id %in% study2$subjSessionID[study2$iatCond == "Victim/Aggressor"], ]
study2longCritAC <- study2longCrit[study2longCrit$session_id %in% study2$subjSessionID[study2$iatCond == "Aggressor/Control"], ]
study2longCritVC <- study2longCrit[study2longCrit$session_id %in% study2$subjSessionID[study2$iatCond == "Victim/Control"], ]

# Victim/Aggressor IAT

for (i in 1:nrow(study2longCritAV)) {
     if (study2$order[study2$subjSessionID == study2longCritAV$session_id[i]] == "Incongruent" & study2longCritAV$block_number[i] == 3) {
          study2longCritAV$trial_number[i] <- study2longCritAV$trial_number[i] + 40
     }
     else if (study2$order[study2$subjSessionID == study2longCritAV$session_id[i]] == "Congruent"  & study2longCritAV$block_number[i] == 5) {
          study2longCritAV$trial_number[i] <- study2longCritAV$trial_number[i] + 40
     }
     print(i/length(study2longCritAV$trial_number))
}

table(table(study2longCritAV$trial_number))

study2AVIATWide <- reshape(study2longCritAV, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study2AVIATWide), gregexpr("[[:digit:]]+", colnames(study2AVIATWide)))
colnames(study2AVIATWide)[2:dim(study2AVIATWide)[2]] <- as.numeric(unlist(colNum))
study2AVIATWide <- study2AVIATWide[, order(as.numeric(colnames(study2AVIATWide)))]

relIATstudy2AV <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study2AVIATWide)) {
          firstHalf[j] <- calcD(study2AVIATWide[j, which(colnames(study2AVIATWide) == "1"):which(colnames(study2AVIATWide) == "40")][, ranVec],
                                study2AVIATWide[j, which(colnames(study2AVIATWide) == "41"):which(colnames(study2AVIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study2AVIATWide[j, which(colnames(study2AVIATWide) == "1"):which(colnames(study2AVIATWide) == "40")][, -ranVec],
                                 study2AVIATWide[j, which(colnames(study2AVIATWide) == "41"):which(colnames(study2AVIATWide) == "80")][, -ranVec])    
     }
     relIATstudy2AV[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy2AV)

# Aggressor/Control IAT

for (i in 1:nrow(study2longCritAC)) {
     if (study2$order[study2$subjSessionID == study2longCritAC$session_id[i]] == "Incongruent" & study2longCritAC$block_number[i] == 3) {
          study2longCritAC$trial_number[i] <- study2longCritAC$trial_number[i] + 40
     }
     else if (study2$order[study2$subjSessionID == study2longCritAC$session_id[i]] == "Congruent"  & study2longCritAC$block_number[i] == 5) {
          study2longCritAC$trial_number[i] <- study2longCritAC$trial_number[i] + 40
     }
     print(i/length(study2longCritAC$trial_number))
}

table(table(study2longCritAC$trial_number))

study2ACIATWide <- reshape(study2longCritAC, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study2ACIATWide), gregexpr("[[:digit:]]+", colnames(study2ACIATWide)))
colnames(study2ACIATWide)[2:dim(study2ACIATWide)[2]] <- as.numeric(unlist(colNum))
study2ACIATWide <- study2ACIATWide[, order(as.numeric(colnames(study2ACIATWide)))]

relIATstudy2AC <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study2ACIATWide)) {
          firstHalf[j] <- calcD(study2ACIATWide[j, which(colnames(study2ACIATWide) == "1"):which(colnames(study2ACIATWide) == "40")][, ranVec],
                                study2ACIATWide[j, which(colnames(study2ACIATWide) == "41"):which(colnames(study2ACIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study2ACIATWide[j, which(colnames(study2ACIATWide) == "1"):which(colnames(study2ACIATWide) == "40")][, -ranVec],
                                 study2ACIATWide[j, which(colnames(study2ACIATWide) == "41"):which(colnames(study2ACIATWide) == "80")][, -ranVec])    
     }
     relIATstudy2AC[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy2AC)

# Victim/Control IAT

for (i in 1:nrow(study2longCritVC)) {
     if (study2$order[study2$subjSessionID == study2longCritVC$session_id[i]] == "Incongruent" & study2longCritVC$block_number[i] == 3) {
          study2longCritVC$trial_number[i] <- study2longCritVC$trial_number[i] + 40
     }
     else if (study2$order[study2$subjSessionID == study2longCritVC$session_id[i]] == "Congruent"  & study2longCritVC$block_number[i] == 5) {
          study2longCritVC$trial_number[i] <- study2longCritVC$trial_number[i] + 40
     }
     print(i/length(study2longCritVC$trial_number))
}

table(table(study2longCritVC$trial_number))

study2VCIATWide <- reshape(study2longCritVC, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study2VCIATWide), gregexpr("[[:digit:]]+", colnames(study2VCIATWide)))
colnames(study2VCIATWide)[2:dim(study2VCIATWide)[2]] <- as.numeric(unlist(colNum))
study2VCIATWide <- study2VCIATWide[, order(as.numeric(colnames(study2VCIATWide)))]

relIATstudy2VC <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study2VCIATWide)) {
          firstHalf[j] <- calcD(study2VCIATWide[j, which(colnames(study2VCIATWide) == "1"):which(colnames(study2VCIATWide) == "40")][, ranVec],
                                study2VCIATWide[j, which(colnames(study2VCIATWide) == "41"):which(colnames(study2VCIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study2VCIATWide[j, which(colnames(study2VCIATWide) == "1"):which(colnames(study2VCIATWide) == "40")][, -ranVec],
                                 study2VCIATWide[j, which(colnames(study2VCIATWide) == "41"):which(colnames(study2VCIATWide) == "80")][, -ranVec])    
     }
     relIATstudy2VC[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy2VC)


# Internal consistency of Implicit Association Tests (Study 3) ----------------------

study3long <- read.csv(paste(citation, "Study 3 Long-format IAT.csv"))
study3long <- study3long[!(study3long$session_id %in% names(table(study3long$session_id)[!(table(study3long$session_id) == 140)])), ]

study3longCrit <- study3long[grepl("Armenian|Turkish|Portuguese", study3long$block_pairing_definition)
                             & grepl("Good", study3long$block_pairing_definition), ]

study3longCritAV <- study3longCrit[study3longCrit$session_id %in% study3$subjSessionID[study3$iatCond == "Victim/Aggressor"], ]
study3longCritAC <- study3longCrit[study3longCrit$session_id %in% study3$subjSessionID[study3$iatCond == "Aggressor/Control"], ]
study3longCritVC <- study3longCrit[study3longCrit$session_id %in% study3$subjSessionID[study3$iatCond == "Victim/Control"], ]

# Victim/Aggressor IAT

for (i in 1:nrow(study3longCritAV)) {
     if (study3$order[study3$subjSessionID == study3longCritAV$session_id[i]] == "Incongruent" & study3longCritAV$block_number[i] == 3) {
          study3longCritAV$trial_number[i] <- study3longCritAV$trial_number[i] + 40
     }
     else if (study3$order[study3$subjSessionID == study3longCritAV$session_id[i]] == "Congruent"  & study3longCritAV$block_number[i] == 5) {
          study3longCritAV$trial_number[i] <- study3longCritAV$trial_number[i] + 40
     }
     print(i/length(study3longCritAV$trial_number))
}

table(table(study3longCritAV$trial_number))

study3AVIATWide <- reshape(study3longCritAV, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study3AVIATWide), gregexpr("[[:digit:]]+", colnames(study3AVIATWide)))
colnames(study3AVIATWide)[2:dim(study3AVIATWide)[2]] <- as.numeric(unlist(colNum))
study3AVIATWide <- study3AVIATWide[, order(as.numeric(colnames(study3AVIATWide)))]

relIATstudy3AV <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study3AVIATWide)) {
          firstHalf[j] <- calcD(study3AVIATWide[j, which(colnames(study3AVIATWide) == "1"):which(colnames(study3AVIATWide) == "40")][, ranVec],
                                study3AVIATWide[j, which(colnames(study3AVIATWide) == "41"):which(colnames(study3AVIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study3AVIATWide[j, which(colnames(study3AVIATWide) == "1"):which(colnames(study3AVIATWide) == "40")][, -ranVec],
                                 study3AVIATWide[j, which(colnames(study3AVIATWide) == "41"):which(colnames(study3AVIATWide) == "80")][, -ranVec])    
     }
     relIATstudy3AV[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy3AV)

# Aggressor/Control IAT

for (i in 1:nrow(study3longCritAC)) {
     if (study3$order[study3$subjSessionID == study3longCritAC$session_id[i]] == "Incongruent" & study3longCritAC$block_number[i] == 3) {
          study3longCritAC$trial_number[i] <- study3longCritAC$trial_number[i] + 40
     }
     else if (study3$order[study3$subjSessionID == study3longCritAC$session_id[i]] == "Congruent"  & study3longCritAC$block_number[i] == 5) {
          study3longCritAC$trial_number[i] <- study3longCritAC$trial_number[i] + 40
     }
     print(i/length(study3longCritAC$trial_number))
}

table(table(study3longCritAC$trial_number))

study3ACIATWide <- reshape(study3longCritAC, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study3ACIATWide), gregexpr("[[:digit:]]+", colnames(study3ACIATWide)))
colnames(study3ACIATWide)[2:dim(study3ACIATWide)[2]] <- as.numeric(unlist(colNum))
study3ACIATWide <- study3ACIATWide[, order(as.numeric(colnames(study3ACIATWide)))]

relIATstudy3AC <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study3ACIATWide)) {
          firstHalf[j] <- calcD(study3ACIATWide[j, which(colnames(study3ACIATWide) == "1"):which(colnames(study3ACIATWide) == "40")][, ranVec],
                                study3ACIATWide[j, which(colnames(study3ACIATWide) == "41"):which(colnames(study3ACIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study3ACIATWide[j, which(colnames(study3ACIATWide) == "1"):which(colnames(study3ACIATWide) == "40")][, -ranVec],
                                 study3ACIATWide[j, which(colnames(study3ACIATWide) == "41"):which(colnames(study3ACIATWide) == "80")][, -ranVec])    
     }
     relIATstudy3AC[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy3AC)

# Victim/Control IAT

for (i in 1:nrow(study3longCritVC)) {
     if (study3$order[study3$subjSessionID == study3longCritVC$session_id[i]] == "Incongruent" & study3longCritVC$block_number[i] == 3) {
          study3longCritVC$trial_number[i] <- study3longCritVC$trial_number[i] + 40
     }
     else if (study3$order[study3$subjSessionID == study3longCritVC$session_id[i]] == "Congruent"  & study3longCritVC$block_number[i] == 5) {
          study3longCritVC$trial_number[i] <- study3longCritVC$trial_number[i] + 40
     }
     print(i/length(study3longCritVC$trial_number))
}

table(table(study3longCritVC$trial_number))

study3VCIATWide <- reshape(study3longCritVC, timevar = "trial_number",
                           idvar = c("session_id"),
                           drop = c("block_pairing_definition", "block_number",
                                    "block_trial_count", "trial_name", "trial_response",
                                    "trial_error"),
                           direction = "wide")

colNum <- regmatches(colnames(study3VCIATWide), gregexpr("[[:digit:]]+", colnames(study3VCIATWide)))
colnames(study3VCIATWide)[2:dim(study3VCIATWide)[2]] <- as.numeric(unlist(colNum))
study3VCIATWide <- study3VCIATWide[, order(as.numeric(colnames(study3VCIATWide)))]

relIATstudy3VC <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study3VCIATWide)) {
          firstHalf[j] <- calcD(study3VCIATWide[j, which(colnames(study3VCIATWide) == "1"):which(colnames(study3VCIATWide) == "40")][, ranVec],
                                study3VCIATWide[j, which(colnames(study3VCIATWide) == "41"):which(colnames(study3VCIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study3VCIATWide[j, which(colnames(study3VCIATWide) == "1"):which(colnames(study3VCIATWide) == "40")][, -ranVec],
                                 study3VCIATWide[j, which(colnames(study3VCIATWide) == "41"):which(colnames(study3VCIATWide) == "80")][, -ranVec])    
     }
     relIATstudy3VC[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy3VC)


# Internal consistency of Implicit Association Tests (Study 4) ----------------------

study4long <- read.csv(paste(citation, "Study 4 Long-format IAT.csv"))
study4long <- study4long[!(study4long$session_id %in% names(table(study4long$session_id)[!(table(study4long$session_id) == 140)])), ]

study4longCrit <- study4long[grepl("Native", study4long$block_pairing_definition)
                             & grepl("Good", study4long$block_pairing_definition), ]

for (i in 1:nrow(study4longCrit)) {
  if (study4$order[study4$subjSessionID == study4longCrit$session_id[i]] == "Incongruent" & study4longCrit$block_number[i] == 3) {
    study4longCrit$trial_number[i] <- study4longCrit$trial_number[i] + 40
  }
  else if (study4$order[study4$subjSessionID == study4longCrit$session_id[i]] == "Congruent"  & study4longCrit$block_number[i] == 5) {
    study4longCrit$trial_number[i] <- study4longCrit$trial_number[i] + 40
  }
  print(i/length(study4longCrit$trial_number))
}

table(table(study4longCrit$trial_number))

study4IATWide <- reshape(study4longCrit, timevar = "trial_number",
                         idvar = c("session_id"),
                         drop = c("block_pairing_definition", "block_number",
                                  "block_trial_count", "trial_name", "trial_response",
                                  "trial_error"),
                         direction = "wide")

colNum <- regmatches(colnames(study4IATWide), gregexpr("[[:digit:]]+", colnames(study4IATWide)))
colnames(study4IATWide)[2:dim(study4IATWide)[2]] <- as.numeric(unlist(colNum))
study4IATWide <- study4IATWide[, order(as.numeric(colnames(study4IATWide)))]

relIATstudy4 <- vector()
set.seed(2345)
for (i in 1:500) {
  ranVec <- sample(1:40, 20)
  firstHalf <- vector()
  secondHalf <- vector()
  for (j in 1:nrow(study4IATWide)) {
    firstHalf[j] <- calcD(study4IATWide[j, which(colnames(study4IATWide) == "1"):which(colnames(study4IATWide) == "40")][, ranVec],
                          study4IATWide[j, which(colnames(study4IATWide) == "41"):which(colnames(study4IATWide) == "80")][, ranVec])
    secondHalf[j] <- calcD(study4IATWide[j, which(colnames(study4IATWide) == "1"):which(colnames(study4IATWide) == "40")][, -ranVec],
                           study4IATWide[j, which(colnames(study4IATWide) == "41"):which(colnames(study4IATWide) == "80")][, -ranVec])    
  }
  relIATstudy4[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
  print(i)
}

mean(relIATstudy4)

# Internal consistency of Implicit Association Tests (Study 5A) ----------------------

study5Along <- read.csv(paste(citation, "Study 5A Long-format IAT.csv"))
study5Along <- study5Along[!(study5Along$session_id %in% names(table(study5Along$session_id)[!(table(study5Along$session_id) == 140)])), ]

study5AlongCrit <- study5Along[grepl("Black", study5Along$block_pairing_definition)
                             & grepl("Good", study5Along$block_pairing_definition), ]

for (i in 1:nrow(study5AlongCrit)) {
     if (study5A$order[study5A$subjSessionID == study5AlongCrit$session_id[i]] == "Incongruent" & study5AlongCrit$block_number[i] == 3) {
          study5AlongCrit$trial_number[i] <- study5AlongCrit$trial_number[i] + 40
     }
     else if (study5A$order[study5A$subjSessionID == study5AlongCrit$session_id[i]] == "Congruent"  & study5AlongCrit$block_number[i] == 5) {
          study5AlongCrit$trial_number[i] <- study5AlongCrit$trial_number[i] + 40
     }
     print(i/length(study5AlongCrit$trial_number))
}

table(table(study5AlongCrit$trial_number))

study5AIATWide <- reshape(study5AlongCrit, timevar = "trial_number",
                         idvar = c("session_id"),
                         drop = c("block_pairing_definition", "block_number",
                                  "block_trial_count", "trial_name", "trial_response",
                                  "trial_error"),
                         direction = "wide")

colNum <- regmatches(colnames(study5AIATWide), gregexpr("[[:digit:]]+", colnames(study5AIATWide)))
colnames(study5AIATWide)[2:dim(study5AIATWide)[2]] <- as.numeric(unlist(colNum))
study5AIATWide <- study5AIATWide[, order(as.numeric(colnames(study5AIATWide)))]

relIATstudy5A <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study5AIATWide)) {
          firstHalf[j] <- calcD(study5AIATWide[j, which(colnames(study5AIATWide) == "1"):which(colnames(study5AIATWide) == "40")][, ranVec],
                                study5AIATWide[j, which(colnames(study5AIATWide) == "41"):which(colnames(study5AIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study5AIATWide[j, which(colnames(study5AIATWide) == "1"):which(colnames(study5AIATWide) == "40")][, -ranVec],
                                 study5AIATWide[j, which(colnames(study5AIATWide) == "41"):which(colnames(study5AIATWide) == "80")][, -ranVec])    
     }
     relIATstudy5A[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy5A)

# Internal consistency of Implicit Association Tests (Study 5B) ----------------------

study5Blong <- read.csv(paste(citation, "Study 5B Long-format IAT.csv"))
study5Blong <- study5Blong[!(study5Blong$session_id %in% names(table(study5Blong$session_id)[!(table(study5Blong$session_id) == 140)])), ]

study5BlongCrit <- study5Blong[grepl("Target", study5Blong$block_pairing_definition)
                               & grepl("Good", study5Blong$block_pairing_definition), ]

for (i in 1:nrow(study5BlongCrit)) {
     if (study5B$order[study5B$subjSessionID == study5BlongCrit$session_id[i]] == "Incongruent" & study5BlongCrit$block_number[i] == 3) {
          study5BlongCrit$trial_number[i] <- study5BlongCrit$trial_number[i] + 40
     }
     else if (study5B$order[study5B$subjSessionID == study5BlongCrit$session_id[i]] == "Congruent"  & study5BlongCrit$block_number[i] == 5) {
          study5BlongCrit$trial_number[i] <- study5BlongCrit$trial_number[i] + 40
     }
     print(i/length(study5BlongCrit$trial_number))
}

table(table(study5BlongCrit$trial_number))

study5BIATWide <- reshape(study5BlongCrit, timevar = "trial_number",
                          idvar = c("session_id"),
                          drop = c("block_pairing_definition", "block_number",
                                   "block_trial_count", "trial_name", "trial_response",
                                   "trial_error"),
                          direction = "wide")

colNum <- regmatches(colnames(study5BIATWide), gregexpr("[[:digit:]]+", colnames(study5BIATWide)))
colnames(study5BIATWide)[2:dim(study5BIATWide)[2]] <- as.numeric(unlist(colNum))
study5BIATWide <- study5BIATWide[, order(as.numeric(colnames(study5BIATWide)))]

relIATstudy5B <- vector()
set.seed(2345)
for (i in 1:500) {
     ranVec <- sample(1:40, 20)
     firstHalf <- vector()
     secondHalf <- vector()
     for (j in 1:nrow(study5BIATWide)) {
          firstHalf[j] <- calcD(study5BIATWide[j, which(colnames(study5BIATWide) == "1"):which(colnames(study5BIATWide) == "40")][, ranVec],
                                study5BIATWide[j, which(colnames(study5BIATWide) == "41"):which(colnames(study5BIATWide) == "80")][, ranVec])
          secondHalf[j] <- calcD(study5BIATWide[j, which(colnames(study5BIATWide) == "1"):which(colnames(study5BIATWide) == "40")][, -ranVec],
                                 study5BIATWide[j, which(colnames(study5BIATWide) == "41"):which(colnames(study5BIATWide) == "80")][, -ranVec])    
     }
     relIATstudy5B[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
     print(i)
}

mean(relIATstudy5B)

# Internal consistency of Implicit Association Tests (Study 5C) ----------------------

study5Clong <- read.csv(paste(citation, "Study 5C Long-format IAT.csv"))
study5Clong <- study5Clong[!(study5Clong$session_id %in% names(table(study5Clong$session_id)[!(table(study5Clong$session_id) == 140)])), ]

study5ClongCrit <- study5Clong[grepl("Black", study5Clong$block_pairing_definition)
                               & grepl("Good", study5Clong$block_pairing_definition), ]

for (i in 1:nrow(study5ClongCrit)) {
  if (study5C$order[study5C$subjSessionID == study5ClongCrit$session_id[i]] == "Incongruent" & study5ClongCrit$block_number[i] == 3) {
    study5ClongCrit$trial_number[i] <- study5ClongCrit$trial_number[i] + 40
  }
  else if (study5C$order[study5C$subjSessionID == study5ClongCrit$session_id[i]] == "Congruent"  & study5ClongCrit$block_number[i] == 5) {
    study5ClongCrit$trial_number[i] <- study5ClongCrit$trial_number[i] + 40
  }
  print(i/length(study5ClongCrit$trial_number))
}

table(table(study5ClongCrit$trial_number))

study5CIATWide <- reshape(study5ClongCrit, timevar = "trial_number",
                          idvar = c("session_id"),
                          drop = c("block_pairing_definition", "block_number",
                                   "block_trial_count", "trial_name", "trial_response",
                                   "trial_error"),
                          direction = "wide")

colNum <- regmatches(colnames(study5CIATWide), gregexpr("[[:digit:]]+", colnames(study5CIATWide)))
colnames(study5CIATWide)[2:dim(study5CIATWide)[2]] <- as.numeric(unlist(colNum))
study5CIATWide <- study5CIATWide[, order(as.numeric(colnames(study5CIATWide)))]

relIATstudy5C <- vector()
set.seed(2345)
for (i in 1:500) {
  ranVec <- sample(1:40, 20)
  firstHalf <- vector()
  secondHalf <- vector()
  for (j in 1:nrow(study5CIATWide)) {
    firstHalf[j] <- calcD(study5CIATWide[j, which(colnames(study5CIATWide) == "1"):which(colnames(study5CIATWide) == "40")][, ranVec],
                          study5CIATWide[j, which(colnames(study5CIATWide) == "41"):which(colnames(study5CIATWide) == "80")][, ranVec])
    secondHalf[j] <- calcD(study5CIATWide[j, which(colnames(study5CIATWide) == "1"):which(colnames(study5CIATWide) == "40")][, -ranVec],
                           study5CIATWide[j, which(colnames(study5CIATWide) == "41"):which(colnames(study5CIATWide) == "80")][, -ranVec])    
  }
  relIATstudy5C[i] <- cor(firstHalf, secondHalf, use = "pairwise.complete.obs")
  print(i)
}

mean(relIATstudy5C)
