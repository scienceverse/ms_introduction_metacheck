
# First run the "SvDvA_Analysis_4.R" script
# Make sure that the working directory is correctly assigned

source("Analysis/SvDvA Analysis_4.R", echo  = TRUE)

dat <- d1a
# Age and gender statistics
psych::describe(as.numeric(dat$Age))
IQR(dat$Age)
prop.table(table(dat$Gender))

# Separate the right long data 
dlongw1 <- dlong %>% filter(wave == "1")

# Analysis section --------------------------------------------------------
dlong_mse_1vm <- dlong %>% 
  select(id,condition,question,range,wave,mse.guess1,mse.mean) %>%
  gather(mse,estimate,mse.guess1:mse.mean) %>% 
  mutate(c = ifelse(mse == "mse.guess1",-1,1)) %>% 
  filter(wave == "1")
dlong_mse_2vm <- dlong %>% 
  select(id,condition,question,range,wave,mse.guess2,mse.mean) %>%
  gather(mse,estimate,mse.guess2:mse.mean) %>% 
  mutate(c = ifelse(mse == "mse.guess2",-1,1)) %>% 
  filter(wave == "1")
dlong_mae_1vm <- dlong %>% 
  select(id,condition,question,range,wave,mae.guess1,mae.mean) %>%
  gather(mae,estimate,mae.guess1:mae.mean) %>% 
  mutate(c = ifelse(mae == "mae.guess1",-1,1)) %>% 
  filter(wave == "1")
dlong_mae_2vm <- dlong %>% 
  select(id,condition,question,range,wave,mae.guess2,mae.mean) %>%
  gather(mae,estimate,mae.guess2:mae.mean) %>% 
  mutate(c = ifelse(mae == "mae.guess2",-1,1)) %>% 
  filter(wave == "1")


# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_1vm)
tt <- getME(ic1,"theta")
ll <- getME(ic1,"lower")
derivs1 <- ic1@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic1,c("theta","fixef"))
ic1_update <- update(ic1,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic1_update)
0.98/sqrt(964)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_2vm)
summary(ic2)
16.52/sqrt(964)

# WITHIN CROWD SELF
ic3 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Self"))
summary(ic3)
1.107/sqrt(323)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self"))
summary(ic4)
6.54/sqrt(323)


# WITHIN CROWD AGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Agree"))
summary(ic5)
1.54/sqrt(323)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Agree"))
summary(ic6)
4.98/sqrt(323)

# WITHIN CROWD DISAGREE
ic7 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree"))
summary(ic7)
0.81/sqrt(318)

ic8 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree"))
summary(ic8)
15.57/sqrt(318)


# LOOKING AT EACH QUESTION RANGE SEPARATELY #
# Mid-Range questions -----------------------------------------------------
dlongmid <- dlongw1 %>% 
  dplyr::filter(range == "mid")

# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = subset(dlong_mse_1vm,dlong_mse_1vm$range == "mid"))
summary(ic1)
5.76/sqrt(964)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = subset(dlong_mse_2vm, dlong_mse_2vm$range == "mid"))
summary(ic2)
13.48/sqrt(964)

# WITHIN CROWD SELF
ic3 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Self" & dlong_mse_1vm$range=="mid"))
summary(ic3)
1.85/sqrt(323)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self" & dlong_mse_2vm$range == "mid"))
summary(ic4)
6.34/sqrt(323)

# WITHIN CROWD AGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Agree" & dlong_mse_1vm$range == "mid"))
summary(ic5)
2.66/sqrt(323)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Agree" & dlong_mse_2vm$range == "mid"))
summary(ic6)
4.59/sqrt(323)


# WITHIN CROWD DISAGREE
ic7 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree" & dlong_mse_1vm$range == "mid"))
summary(ic7)
5.36/sqrt(318)

ic8 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree" & dlong_mse_2vm$range == "mid"))

summary(ic8)
11.79/sqrt(318)

# LOOKING AT THE DIFFERENCE SCORES (Benefit of averaging)

dlongmid$diffmse <- dlongmid$mse.guess1 - dlongmid$mse.mean
dlongmid$diffmae <- dlongmid$mae.guess1 - dlongmid$mae.mean
# Create conditions datasets for comparisons
d_a_s <- dlongmid %>% 
  filter(condition == "Agree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_s <- dlongmid %>% 
  filter(condition == "Disagree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_a <- dlongmid %>% 
  filter(condition == "Disagree" | condition == "Agree") %>% 
  mutate(condition_c = ifelse(condition == "Agree",0.5,-0.5))


ic9 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = d_a_s)
summary(ic9)
1.41*sqrt((1/323)+(1/323))

ic10 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic10)
4.80*sqrt((1/318)+(1/323))

ic11 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic11)
3.89*sqrt((1/318)+(1/323))


# Extreme-range questions -------------------------------------------------
dlongext <- dlongw1 %>% 
  dplyr::filter(range == "ext")

# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
             (1|id) +
             (1|question), data = subset(dlong_mse_1vm,dlong_mse_1vm$range == "ext"))
tt <- getME(ic1,"theta")
ll <- getME(ic1,"lower")
derivs1 <- ic1@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic1,c("theta","fixef"))
ic1_update <- update(ic1,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic1_update)
2.85/sqrt(964)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = subset(dlong_mse_2vm, dlong_mse_2vm$range == "ext"))
summary(ic2)
11.80/sqrt(964)

# WITHIN CROWD SELF
ic3 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Self" & dlong_mse_1vm$range=="ext"))
summary(ic3)
0.12/sqrt(323)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self" & dlong_mse_2vm$range == "ext"))
tt <- getME(ic4,"theta")
ll <- getME(ic4,"lower")
derivs1 <- ic4@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic4,c("theta","fixef"))
ic4_update <- update(ic4,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic4_update)
3.90/sqrt(323)

# WITHIN CROWD AGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Agree" & dlong_mse_1vm$range == "ext"))
summary(ic5)
0.22/sqrt(323)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Agree" & dlong_mse_2vm$range == "ext"))
summary(ic6)
3.05/sqrt(323)

# WITHIN CROWD DISAGREE
ic7 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree" & dlong_mse_1vm$range == "ext"))
summary(ic7)
4.70/sqrt(318)

ic8 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree" & dlong_mse_2vm$range == "ext"))

summary(ic8)
11.91/sqrt(318)

# LOOKING AT THE DIFFERENCE SCORES (Benefit of averaging)

dlongext$diffmse <- dlongext$mse.guess1 - dlongext$mse.mean
dlongext$diffmae <- dlongext$mae.guess1 - dlongext$mae.mean
# Create conditions datasets for comparisons
d_a_s <- dlongext %>% 
  filter(condition == "Agree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_s <- dlongext %>% 
  filter(condition == "Disagree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_a <- dlongext %>% 
  filter(condition == "Disagree" | condition == "Agree") %>% 
  mutate(condition_c = ifelse(condition == "Agree",0.5,-0.5))


ic9 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = d_a_s)
summary(ic9)
0.25*sqrt((1/323)+(1/323))

ic10 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic10)
5.38*sqrt((1/318)+(1/323))

ic11 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic11)
5.65*sqrt((1/318)+(1/323))


# Bracketing --------------------------------------------------------------
# Create a function to find bracketing 
b.find <- function(x,q1,q2) {
    ifelse(q1<x & q2>x | q1>x & q2<x,1,0)
  }

dbrack <- dlongw1

dbrack <- dbrack %>% 
  dplyr::mutate(brate = b.find(answer, guess_1, guess_2))

names(dbrack)[names(dbrack) == "condition"] <- "Perspective"

# Bracketing analysis -----------------------------------------------------
# Mid-range
d_a_s <- dbrack %>%
  filter(range == "mid") %>% 
  filter(Perspective == "Agree" | Perspective == "Self") %>% 
  mutate(condition_c = ifelse(Perspective == "Self",0.5,-0.5))


d_d_s <- dbrack %>%
  filter(range == "mid") %>% 
  filter(Perspective == "Disagree" | Perspective == "Self") %>% 
  mutate(condition_c = ifelse(Perspective == "Self",0.5,-0.5))

d_d_a <- dbrack %>%
  filter(range == "mid") %>% 
  filter(Perspective == "Disagree" | Perspective == "Agree") %>% 
  mutate(condition_c = ifelse(Perspective == "Agree",0.5,-0.5))


ic12 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
             data = d_a_s)
summary(ic12)
0.98*sqrt((1/323)+(1/323))


ic13 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic13)
8.62*sqrt((1/318)+(1/323))


ic14 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic14)
9.94*sqrt((1/318)+(1/323))


# Percentage bracketed
dbrack %>% 
  group_by(Perspective,brate) %>% 
  filter(range == "mid") %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))


# Extreme-range -----------------------------------------------------------

d_a_s <- dbrack %>%
  filter(range == "ext") %>% 
  filter(Perspective == "Agree" | Perspective == "Self") %>% 
  mutate(condition_c = ifelse(Perspective == "Self",0.5,-0.5))

d_d_s <- dbrack %>%
  filter(range == "ext") %>% 
  filter(Perspective == "Disagree" | Perspective == "Self") %>% 
  mutate(condition_c = ifelse(Perspective == "Self",0.5,-0.5))

d_d_a <- dbrack %>%
  filter(range == "ext") %>% 
  filter(Perspective == "Disagree" | Perspective == "Agree") %>% 
  mutate(condition_c = ifelse(Perspective == "Agree",0.5,-0.5))


ic12 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
             data = d_a_s)
summary(ic12)
1.58*sqrt((1/323)+(1/323))

ic13 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic13)
4.71*sqrt((1/318)+(1/323))


ic14 <- lmer(brate ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic14)
6.24*sqrt((1/318)+(1/323))

# Percentage bracketed
dbrack %>% 
  group_by(Perspective,brate) %>% 
  filter(range == "ext") %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# To the answer -----------------------------------------------------
# Create a function for second guesses moving away from true
b.away <- function(t,g1,g2)
{
  ifelse(g2>g1 & g2<t | g2>t & g2<g1,1,0)
}

dbrackaway <- dbrack %>% 
  filter(brate == 0) %>% 
  mutate(away = b.away(answer, guess_1, guess_2))

dbrackaway %>% 
  group_by(Perspective,away) %>% 
  filter(range == "mid") %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

dbrackaway %>% 
  group_by(Perspective,away) %>% 
  filter(range == "ext") %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))