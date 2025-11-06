#———————————————————————————————————————————————————————————————————————————————
# Does Pro-Environmental Behavior Increase Subjective Well-Being? -- Study 1
#———————————————————————————————————————————————————————————————————————————————
library(effectsize)
library(lmerTest)
library(tidyverse)
library(psych)
library(interactions)

# Read in the data
s1 <- read_csv("Data/For OSF/s1_data.csv")

# Compute variables for analyses
#————————————————————————————————————————————————
hist(s1$response) 
table(s1$response) 
# Most of the time, people reported either engaging in PEB or in a 
# non-environmentally relevant behavior. Few anti-environmental behaviors and
# few "observed" behaviors
# 1 = Committed positive; 2 = Committed negative
# 3 = witnessed positive; 4 = witnessed negative
# 5 = Non-ERB
# Re-code as anti-environmental (-1), neutral (0), or pro-environmental (1)
s1$peb <- ifelse(s1$response == 1, 1, 
          ifelse(s1$response == 2, -1, 0)) 
table(s1$peb)

# Compute person means
s1 <- s1 %>% 
  group_by(pid) %>% 
  mutate(peb.perAvg = mean(peb, na.rm=T),
         mood.perAvg = mean(mood, na.rm=T)) %>% 
  ungroup()

# Grand mean-center the person means
s1$peb.between <- s1$peb.perAvg - mean(s1$peb, na.rm=T)
# Compute deviations from person-means
s1$peb.within <- s1$peb - s1$peb.perAvg


# Demographics
#————————————————————————————————————————————————
s1.dem <- s1 %>% group_by(pid) %>% slice(1) %>% ungroup()
table(s1.dem$country)
describe(s1.dem$age)
table(s1.dem$gender) / nrow(s1.dem)

# 2 = middle school, 3 = high school, 4 = some college, 
# 5 = college degree, 6 = PhD/MD
table(s1.dem$education, useNA = "ifany") / nrow(s1.dem)

describe(s1.dem$politics)
table(s1.dem$politics, useNA = "ifany") / nrow(s1.dem)
barplot(table(s1.dem$politics))


# Descriptive statistics
#————————————————————————————————————————————————
s1.dem %>% 
  select(peb.perAvg, mood.perAvg, sdb,
         value_bio, value_alt, value_ego, value_hed) %>% 
  describe()

length(which(s1.dem$value_bio < 0)) / nrow(s1.dem)

describe(s1$peb)
describe(s1$mood)

#———————————————————————————————————————————————————————————————————————————————
# Pre-registered analyses
#———————————————————————————————————————————————————————————————————————————————
# Model 1: Within- and between-person links
s1.m1 <- lmer(mood ~ peb.between + peb.within + (peb.within | pid/day), s1)
s1.m1.betas <- standardize_parameters(s1.m1)
summary(s1.m1)
round(confint(s1.m1, method="Wald"), 2)
s1.m1.betas

# Figure
s1 %>% 
  select(peb, mood, pid) %>% 
  na.omit() %>% 
  ggplot(aes(x=peb, y=mood)) + 
  geom_line(aes(y=predict(lmer(mood ~ peb + (peb|pid))), 
                group = pid), linewidth=.5, alpha=.3) + 
  labs(x="Pro-Environmental Behavior", y = "Subjective Well-Being") +
  theme_minimal() +
  theme(text=element_text(size=12, color="black"),
        axis.text=element_text(size=11, color="black"))


# Model 2: Control for social desirability bias
s1.m2 <- lmer(mood ~ peb.between + peb.within + sdb + 
                    (peb.within | pid/day), s1)
s1.m2.betas <- standardize_parameters(s1.m2)
summary(s1.m2)
round(confint(s1.m2, method="Wald"), 2)
s1.m2.betas


# Model 3: Test for moderation by environmental values
s1.m3 <- lmer(mood ~ peb.between*value_bio + peb.within*value_bio + sdb +
                     (peb.within | pid/day), s1)
s1.m3.betas <- standardize_parameters(s1.m3)
summary(s1.m3)
round(confint(s1.m3, method="Wald"), 2)
s1.m3.betas

sim_slopes(s1.m3, pred=peb.within, modx=value_bio, digits = 3,
           modx.values = c(-6, -4, -2, 0))
# Significant until values_bio < -3.65

# That's 2.7 SDs < Mean
(mean(s1.dem$value_bio) + 3.645) / sd(s1.dem$value_bio)

# Only n = 1 below that threshold
s1.dem %>% filter(value_bio < -3.645) %>% nrow()

# The coefficient hits 0 at about -7.55
sim_slopes(s1.m3, pred=peb.within, modx=value_bio, digits = 3,
           modx.values = c(-7.5, -7.55, -7.6))


interact_plot(s1.m3, pred=peb.within, modx=value_bio, 
              plot.points = T, modx.values = c(-2, -1, 0 ,1, 2),
              legend.main = "Environmental Values", 
              point.size = .5, line.thickness = .75) + 
  labs(x="Pro-Environmental Behavior\n(deviations from person-means)", 
       y = "Subjective Well-Being",
       title= "Interaction Plot") +
  theme_bw() +
  theme(legend.position = "bottom")

johnson_neyman(s1.m3, pred=peb.within, modx=value_bio)$plot +
  geom_density(data=s1, aes(value_bio), fill="grey", color=NA,
               adjust=2, trim=T, position=position_nudge(y=.015)) +
  labs(x = "Environmental Values", 
       y = "Slope of the Within-Person Association",
       title="Johnson-Neyman Plot") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


#———————————————————————————————————————————————————————————————————————————————
# Exploratory analyses (not pre-registered)
#———————————————————————————————————————————————————————————————————————————————
# Model 4: Test for moderation by political orientation
s1$politics.mc <- as.numeric(scale(s1$politics, scale=F))
s1.m4 <- lmer(mood ~ peb.between*politics.mc + peb.within*politics.mc + 
                    (peb.within | pid/day) + sdb, s1)
summary(s1.m4)
round(confint(s1.m4, method="Wald"), 2)
standardize_parameters(s1.m4)


# Model 5: Test for moderation by social desirability bias
s1$sdb.mc <- as.numeric(scale(s1$sdb, scale=F))
s1.m5 <- lmer(mood ~ peb.between*sdb.mc + peb.within*sdb.mc + 
                     (peb.within | pid/day), 
               s1, control=lmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun= 2e5)))
summary(s1.m5)
round(confint(s1.m5, method="Wald"), 2)
standardize_parameters(s1.m5)


# Model 6: Test for moderation by altruistic values
s1.m6 <- lmer(mood ~ peb.between*value_alt + peb.within*value_alt + sdb +
                (peb.within | pid/day), s1)
summary(s1.m6)
round(confint(s1.m6, method="Wald"), 2)
standardize_parameters(s1.m6)


# Model 7: Test for moderation by US vs. non-US
s1$usa <- ifelse(s1$country == "United States of America", 1, 0)
s1.dem$usa <- ifelse(s1.dem$country == "United States of America", 1, 0)

s1.m7 <- lmer(mood ~ peb.between*value_bio + peb.within*value_bio +
                     peb.between*usa + peb.within*usa + sdb +
                     (peb.within | pid/day), s1)
summary(s1.m7)
round(confint(s1.m7, method="Wald"), 2)
standardize_parameters(s1.m7)

# Are there significant differences in any variables across countries?
summary(lmer(peb ~ usa + (1 | pid/day), s1))
summary(lmer(mood ~ usa + (1 | pid/day), s1))
summary(lm(sdb ~ usa, s1.dem))
summary(lm(value_bio ~ usa, s1.dem))
summary(lm(politics ~ usa, s1.dem))
anova(lmer(peb ~ country + (1 | pid/day), s1))
anova(lmer(mood ~ country + (1 | pid/day), s1))
anova(lm(sdb ~ country, s1.dem))
anova(lm(value_bio ~ country, s1.dem))
anova(lm(politics ~ country, s1.dem))
# No


# Model 8: Test for association between social desirability and PEB
summary(s1.m8 <- lm(peb.between ~ sdb, s1.dem))
round(confint(s1.m8, method="Wald"), 2)
standardize_parameters(s1.m8)

ggplot(s1.dem, aes(sdb, peb.between)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal() +
  labs(x="Social Desirability Bias", 
       y="Pro-Environmental Behavior (Person Means)")
