#———————————————————————————————————————————————————————————————————————————————
# Does Pro-Environmental Behavior Increase Subjective Well-Being? -- Study 2
#———————————————————————————————————————————————————————————————————————————————
library(car)
library(lavaan)
library(emmeans)
library(effectsize)
library(interactions)
library(cowplot)
library(tidyverse)
library(psych)

# Read in the data
s2 <- read_csv("Data/For OSF/s2_data.csv")
s2$Condition <- factor(s2$Condition, levels=c("Control", "PEB", "Fun"))

# Demographics 
#————————————————————————————————————————————————
describe(s2$age)
table(s2$gender, useNA="ifany") / nrow(s2)
table(s2$race, useNA="ifany") / nrow(s2)

# Descriptive statistics for moderators
#————————————————————————————————————————————————
describe(s2 %>% select(politics, envSelfImage, envConc))


#———————————————————————————————————————————————————————————————————————————————
# What did the participants in the Fun and PEB conditions actually do?
#———————————————————————————————————————————————————————————————————————————————
s2_text <- read_csv("Data/S2 text.csv")

# How many PEBs did each participant in the PEB condition report?
s2_text$pebCount <- rowSums(s2_text %>% select(litter:activism), na.rm=T)
describe(s2_text %>% filter(Condition=="PEB") %>% pull(pebCount))

# How many fun activities did each participant in the Fun condition report?
s2_text$funCount <- rowSums(s2_text %>% select(treat:self_care), na.rm=T)
describe(s2_text %>% filter(Condition=="Fun") %>% pull(funCount))

# Create frequency table of PEBs
s2_text %>% 
  filter(Condition == "PEB") %>% 
  summarise(across(litter:activism, 
                   function(x) {sum(x, na.rm = T)})) %>% 
  pivot_longer(cols = everything(), 
               names_to = "Behavior", 
               values_to = "Count") %>% 
  ggplot(aes(reorder(Behavior, Count), Count)) + 
  geom_col() + 
  geom_text(aes(label=Count), hjust=-.5) +
  scale_x_discrete(labels=c("Contact representative",
                            "Repair or repurpose item",
                            "Carpool", 
                            "Conserve water", 
                            "Buy used vs. new",
                            "Eat plant-based meals",
                            "Recycle or compost",
                            "Public transit vs. drive",
                            "Reduce waste",
                            "Reduce energy use",
                            "Clean up litter",
                            "Walk or bike vs. drive")) +
  coord_flip(ylim=c(0,105)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size=12, color="black"),
        text = element_text(size=12, color="black")) 

# Create frequency table of fun activities
s2_text %>% 
  filter(Condition == "Fun") %>% 
  summarise(across(treat:self_care, 
                   function(x) {sum(x, na.rm = T)})) %>% 
  pivot_longer(cols = everything(), 
               names_to = "Behavior", 
               values_to = "Count") %>% 
  ggplot(aes(reorder(Behavior, Count), Count)) + 
  geom_col() + 
  geom_text(aes(label=Count), hjust=-.5) +
  scale_x_discrete(labels=rev(c("Physical activity",
                                "Rest or take breaks",
                                "Spend time with family/friends",
                                "TV/movies/video games",
                                "Have a treat",
                                "Have a special meal",
                                "Mindfulness",
                                "Sleep",
                                "Self care",
                                "Read for pleasure",
                                "Call family/friends",
                                "Arts or crafts",
                                "Write for pleasure",
                                "Play music"))) +
  coord_flip(ylim=c(0,105)) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(size=12, color="black"),
        text = element_text(size=12, color="black")) 



#———————————————————————————————————————————————————————————————————————————————
# Compute subjective well-being variable
#———————————————————————————————————————————————————————————————————————————————
# Internal reliability for SWB measures
omega(s2 %>% select(matches("P\\d.t1")), nfactor=1) # .89
omega(s2 %>% select(matches("P\\d.t2")), nfactor=1) # .91

omega(s2 %>% select(matches("N\\d.t1")), nfactor=1) # .84
omega(s2 %>% select(matches("N\\d.t2")), nfactor=1) # .87

omega(s2 %>% select(meaning1.t1:meaning6.t1), nfactor=1) # .90
omega(s2 %>% select(meaning1.t2:meaning6.t2), nfactor=1) # .91

# Score each measure
s2$pa.t1 <- rowMeans(s2 %>% select(matches("P\\d.t1")), na.rm=T)
s2$pa.t2 <- rowMeans(s2 %>% select(matches("P\\d.t2")), na.rm=T)

s2$na.t1 <- rowMeans(s2 %>% select(matches("N\\d.t1")), na.rm=T) 
s2$na.t2 <- rowMeans(s2 %>% select(matches("N\\d.t2")), na.rm=T) 

s2$meaning.t1 <- rowMeans(s2 %>% select(meaning1.t1:meaning6.t1), na.rm=T)
s2$meaning.t2 <- rowMeans(s2 %>% select(meaning1.t2:meaning6.t2), na.rm=T)

# SWB measurement model
s2.measMod <- cfa('swb_t1 =~ a*pa.t1 + b*na.t1 + c*meaning.t1
                   swb_t2 =~ a*pa.t2 + b*na.t2 + c*meaning.t2
    
                   pa.t1 ~~ pa.t2
                   na.t1 ~~ na.t2
                   meaning.t1 ~~ meaning.t2',
                  s2, estimator="MLR", missing="ML")
summary(s2.measMod, fit.measures=T, standardized=T)

# Extract factor scores 
s2$swb.t1 <- as.numeric(lavPredict(s2.measMod)[,1])
s2$swb.t2 <- as.numeric(lavPredict(s2.measMod)[,2])


#———————————————————————————————————————————————————————————————————————————————
# Main effects of experimental condition
#———————————————————————————————————————————————————————————————————————————————
m1 <- lm(swb.t2 ~ Condition + swb.t1, s2)
Anova(m1, type=3)
m1.means <- emmeans(m1, revpairwise ~ Condition)
m1.means
eff_size(m1.means, sigma = sigma(m1), edf=df.residual(m1))

ggplot(as_tibble(m1.means$emmeans),
       aes(x=Condition, y=emmean)) +
  geom_point(size=2) + 
  geom_line(aes(group=1)) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.1) +
  theme_minimal() +
  theme(axis.text = element_text(size=11, color="black"),
        text = element_text(size=12, color="black")) + 
  labs(x="\nCondition", y= "Subjective Well-Being") +
  coord_cartesian(ylim=c(-.1,.1))


# Sensitivity analysis: Do the results differ across aspects of SWB?
#———————————————————————————————————————————————————————————————————————————————
# Main effects of condition for each
Anova(lm(pa.t2 ~ Condition + pa.t1, s2), type=3)
Anova(lm(na.t2 ~ Condition + na.t1, s2), type=3)
Anova(lm(meaning.t2 ~ Condition + meaning.t1, s2), type=3)

# PEB > Control contrast is significant for all 3. Curiously, the Fun > Control
# contrast is only significant for meaning. Though PEB never differs from Fun.
emmeans(lm(pa.t2 ~ Condition + pa.t1, s2), revpairwise ~ Condition)
emmeans(lm(na.t2 ~ Condition + na.t1, s2), revpairwise ~ Condition)
emmeans(lm(meaning.t2 ~ Condition + meaning.t1, s2), revpairwise ~ Condition)


#———————————————————————————————————————————————————————————————————————————————
# Tests for moderation (pre-registered as exploratory)
#———————————————————————————————————————————————————————————————————————————————
# Mean-center the candidate moderators
s2$politics.mc <- scale(s2$politics, scale=F)
s2$envConc.mc <- scale(s2$envConc, scale=F)
s2$envSelfImage.mc <- scale(s2$envSelfImage, scale=F)
c <- list(Condition=contr.sum)

m2 <- lm(swb.t2 ~ swb.t1 + Condition * envConc.mc, s2, contrasts = c)
m3 <- lm(swb.t2 ~ swb.t1 + Condition * envSelfImage.mc, s2, contrasts = c)
m4 <- lm(swb.t2 ~ swb.t1 + Condition * politics.mc, s2, contrasts = c)
Anova(m2, type=3)
Anova(m3, type=3)
Anova(m4, type=3)


#———————————————————————————————————————————————————————————————————————————————
# Additional tests for moderation (not pre-registered)
#———————————————————————————————————————————————————————————————————————————————
# Create a latent variable from the three candidate moderators
summary(mm <- cfa('mod =~ envConc + envSelfImage + politics',
                   s2, missing="ML", estimator="MLR"),
        standardized=T) 
s2$lv <- as.numeric(scale(lavPredict(mm)))

# Does this latent variable moderate the effect of condition?
Anova(em1 <- lm(swb.t2 ~ swb.t1 + Condition * lv, s2, contrasts = c),
      type=3) # No. Though, there is a small main effect


# Interactions with each specific intervention using a regression model
s2$PEB <- ifelse(s2$Condition == "PEB", 1, 0)
s2$Fun <- ifelse(s2$Condition == "Fun", 1, 0)
em2 <- lm(swb.t2 ~ swb.t1 + PEB + Fun + lv + PEB:lv + Fun:lv, s2)

summary(em2)
round(confint(em2, method="Wald"), 2)
standardize_parameters(em2)

intFiga <- interact_plot(em2, pred = PEB, modx=lv, 
                         y.label="Subjective Well-Being",
                         x.label = "PEB Condition",
                         legend.main="Environmentalism") + 
  coord_cartesian(ylim=c(-.2, .2)) +
  theme(legend.position = "none")
intFigb <- interact_plot(em2, pred = Fun, modx=lv, 
                         y.label=element_blank(),
                         x.label = "Fun Condition",
              legend.main="Environmentalism") +
  coord_cartesian(ylim=c(-.2, .2)) +
  theme(legend.position="none",
        axis.text.y = element_blank())
plot_grid(intFiga, intFigb, rel_widths = c(1.2, 1))


#———————————————————————————————————————————————————————————————————————————————
# Investigating demand characteristics (not pre-registered)
#———————————————————————————————————————————————————————————————————————————————
s2_dc <- read_csv("Data/s2_gpt_scored.csv") %>% 
  transmute(id, belief=score) %>% 
  right_join(s2, by="id") %>% 
  mutate(Belief = factor(ifelse(belief == 0, "No", "Yes")))

# How many participants in each condition said something indicating that they 
# believed that they were receiving a well-being intervention?
s2_dc %>% 
  group_by(Condition) %>% 
  summarise(n = n(),
            count = sum(belief),
            prop = count/n)

# What did the Yes-Controls think their instructions were?
s2_dc %>% 
  filter(Condition=="Control" & 
         belief == 1) %>% 
  mutate(text=paste(qualCheck, actions)) %>% 
  pull(text)
# What did the Yes-PEBs write?
s2_dc %>% 
  filter(Condition=="PEB" & 
           belief == 1) %>% 
  mutate(text=paste(qualCheck, actions)) %>% 
  pull(text)

# How did this affect the results?
em3 <- lm(swb.t2 ~ swb.t1 + Condition * Belief, 
          s2_dc, contrasts=list(Condition=contr.sum, 
                             Belief=contr.sum))

# Main effects of condition and beliefs, no interaction
Anova(em3, type=3) 

# PEB > Control contrast is significant; though Fun > Control is not
emmeans(em3, trt.vs.ctrl ~ Condition)
emmeans(em3, trt.vs.ctrl ~ Condition) %>% 
  eff_size(edf=df.residual(em3), sigma=sigma(em3))


emmeans(em3, c("Condition", "Belief")) %>% 
  as_tibble() %>% 
  left_join(s2_dc %>% group_by(Condition, Belief) %>% summarise(n = n()),
            by=c("Condition", "Belief")) %>% 
  ggplot(aes(Condition, emmean, color=Belief, group=Belief)) +
  geom_point(position = position_dodge(.1)) + 
  geom_line(position = position_dodge(.1)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), 
                width=.1, position = position_dodge(.1)) +
  geom_text(aes(label=ifelse(Condition=="Control", paste("n = ", n), "")), 
             show.legend=F,
             nudge_x = -.25) +
  geom_text(aes(label=ifelse(Condition=="Fun", paste("n = ", n), "")), 
             show.legend=F,
             nudge_x = .25) +
  geom_text(aes(label=ifelse(Condition=="PEB", paste("n = ", n), "")), 
             show.legend=F,
             nudge_x = .25, nudge_y = .01) +
  labs(x="Condition", y="Subjective Well-Being", 
       color="Belief") +
  theme_minimal() +
  theme(axis.text = element_text(size=11, color="black"),
        text = element_text(size=12, color="black"))
