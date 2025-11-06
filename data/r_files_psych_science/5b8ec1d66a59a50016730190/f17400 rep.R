# load packages ----

library(tidyverse)
library(compute.es)
library(BayesFactor)
library(pwr)
library(metafor)
library(ggpubr)

# study 1a ----
# get data in shape, code dospert

# get our power to detect reported effects, half their effects -

power.t.test(n = (556/2), d = .57, power=NULL)
power.t.test(n = (556/2), d = .57/2, power=NULL)

# our data

r1a <- read.csv('s1a.csv')
summary(r1a)
nrow(r1a)

dos <- select(r1a, DOS1:DOS40)
alpha(dos)

r1a$dospert <- apply(dos, 1, mean)
r1a <- subset(r1a, AttnCheck == 5)
nrow(r1a)

r1a <- r1a[complete.cases(r1a$dospert),]
nrow(r1a)

summary(r1a)

write.csv(r1a, "clean1a.csv")

# werre our participants just out to lunch? check positive controls

amc1 <- glm(conjError ~ conjCond, data = r1a, family = 'binomial')
summary(amc1)
exp(coef(amc1))
exp(confint(amc1))

tab1 <- table(r1a$conjCond, r1a$conjError)
prop.table(tab1, 1)

# amc checked out bigly

# replication stats----
# basic t-test
ns1a <- table(r1a$Condition)
t1a <- t.test(dospert ~ Condition, r1a, na.rm=T)
es1a <- tes(t1a$statistic, ns1a[1], ns1a[2], verbose=F) 


t1a
es1a$d
es1a$l.d
es1a$u.d

# okay, now try some Bayes..go to JASP



ttestBF(formula = dospert ~ Condition, data=r1a, nullInterval = c(-Inf, 0))

# evidence slightly favors null, even with fairly generous prior

#small telescopes
# figure out point ES and 90th% bound
es1a90 <- tes(t1a$statistic, ns1a[1], ns1a[2], verbose=F, level=90)

# initial study n was 61. what power to detect rep poiont
power.t.test(delta=es1a90$d, n = 31, power = NULL)

# and at 90th% bound
power.t.test(delta=es1a90$l.d, n = 31, power = NULL)

# so not significant, evidence slightly favors null, and it fails small telescopes approach.



# study 1b ----
# get our power to detect reported effects, half their effects -

power.t.test(n = (548/2), d = .32, power=NULL)
power.t.test(n = (548/2), d = .32/2, power=NULL)


# get data in shape, 

r1b <- read.csv('s1b.csv')
summary(r1b)
nrow(r1b)




r1b <- r1b[complete.cases(r1b$RiskLikelihood),]
nrow(r1b)
table(r1b$Gender)
write.csv(r1b, "clean1b.csv")
# were our participants just out to lunch? check positive controls

amc1b <- glm(conjError ~ conjCond, data = r1b, family = 'binomial')
summary(amc1b)
exp(coef(amc1b))
exp(confint(amc1b))


tab2 <- table(r1b$conjCond, r1b$conjError)
prop.table(tab2, 1)
# amc checked out bigly

# replication stats----
# basic t-test
ns1b <- table(r1b$Condition)
t1b <- t.test(RiskLikelihood ~ Condition, r1b, na.rm=T)
es1b <- tes(t1b$statistic, ns1b[1], ns1b[2], verbose=F) 


t1b
es1b$d
es1b$l.d
es1b$u.d

# okay, now try some Bayes. informed priors in JASP. defaults here

ttestBF(formula = RiskLikelihood ~ Condition, data=r1b, nullInterval = c(-Inf, 0))

# evidence slightly favors null, even with fairly generous prior

#small telescopes
# figure out point ES and 90th% bound
es1b90 <- tes(t1b$statistic, ns1b[1], ns1b[2], verbose=F, level=90)

# initial study n was 200. what power to detect rep poiont
power.t.test(delta=es1b90$d, n = 100, power = NULL)

# and at 90th% bound
power.t.test(delta=es1b90$u.d, n = 100, power = NULL)

# so not significant, evidence favors null, and it fails small telescopes approach.




# just god believers? ----
# get data in shape, code RiskLikelihood

r1bGod <- subset(r1b, God == 1)
summary(r1bGod)


# replication stats----
# basic t-test
ns1bGod <- table(r1bGod$Condition)
t1bGod <- t.test(RiskLikelihood ~ Condition, r1bGod, na.rm=T)
es1bGod <- tes(t1bGod$statistic, ns1bGod[1], ns1bGod[2], verbose=F) 


t1bGod
es1bGod$d
es1bGod$l.d
es1bGod$u.d

# okay, now try some Bayes

ttestBF(formula = RiskLikelihood ~ Condition, data=r1bGod, nullInterval = c(-Inf, 0))

# evidence favors null, even with fairly generous prior

#small telescopes
# figure out point ES and 90th% bound
es1bGod90 <- tes(t1bGod$statistic, ns1bGod[1], ns1bGod[2], verbose=F, level=90)

# initial study n was 61. what power to detect rep poiont
power.t.test(delta=es1bGod90$d, n = 30, power = NULL)

# and at 90th% bound
power.t.test(delta=es1bGod90$l.d, n = 30, power = NULL)

# so not significant, evidence favors null, and it fails small telescopes approach.



# bottom line: 1a came closish if just using significance. failed on others. both believers and full sample
# 1b failed in all senses




# meta-analytic stuff ----

# get replication d and vard

r1a.d <- es1a$d * -1
r1a.vard <- es1a$var.d

# estimate originals t (59) = 2.21 reported

o1a.es <- tes(2.21, 30, 31, verbose = F)

o1a.d <-o1a.es$d
o1a.vard <- o1a.es$var.d

# meta-analyze the two

m1a <- rma(c(r1a.d, o1a.d), c(r1a.vard, o1a.vard)) # oops, meta not significant




#second study
r1b.d <- es1b$d * -1
r1b.vard <- es1b$var.d


# estimate originals t(200) = 2.27 reported

o1b.es <- tes(2.27, 101, 101, verbose = F)

o1b.d <-o1b.es$d
o1b.vard <- o1b.es$var.d

# meta-analyze the two

m1b <- rma(c(r1b.d, o1b.d), c(r1b.vard, o1b.vard)) # meta not significant

m1b


# now an overall estimate with all 4 studies

m1ab <- rma(c(o1a.d, r1a.d, o1b.d, r1b.d ), c(o1a.vard, r1a.vard, o1b.vard, r1b.vard), slab = c('original 1a','replication 1a',  'original 1b', 'replication 1b')) # oops, meta not significant

m1ab

forest(m1ab)



# moderation by belief in God ----

summary(r1a)

fit1a <- aov(dospert ~ Condition * God, data = r1a)
summary(fit1a)



summary(r1b)

fit1b <- aov(RiskLikelihood ~ Condition * God, data = r1b)
summary(fit1b)



# now do analyses filtering for just people who nailed the scrambles ----
# 1a ----
r1a <- read.csv('clean1a.csv')


s1a <- read.csv('code1a_Strict.csv') # grab steph's coding
s1a <- s1a %>% select(id, Wrong)
summary(s1a)
nrow(s1a)

hist(s1a$Wrong) # most everyone gets < 2 wrong, cutoff there?


clean1a <- merge(r1a, s1a, by = 'id') %>% 
  select(Condition, dospert, Wrong) %>% 
  filter(Wrong < 1)  # keep people who had perfect score on sentences

summary(clean1a)
nrow(clean1a)


# replication stats----
# basic t-test
ns1a.c <- table(clean1a$Condition)
t1a.c <- t.test(dospert ~ Condition, clean1a, na.rm=T)
es1a.c <- tes(t1a.c$statistic, ns1a.c[1], ns1a.c[2], verbose=F) 


t1a.c
es1a.c$d
es1a.c$l.d
es1a.c$u.d

# okay, now try some Bayes..go to JASP



ttestBF(formula = dospert ~ Condition, data=clean1a, nullInterval = c(-Inf, 0))

# evidence slightly favors null, even with fairly generous prior

#small telescopes
# figure out point ES and 90th% bound
es1a90.c <- tes(t1a.c$statistic, ns1a.c[1], ns1a.c[2], verbose=F, level=90)

# initial study n was 61. what power to detect rep poiont
power.t.test(delta=es1a90.c$d, n = 31, power = NULL)

# and at 90th% bound
power.t.test(delta=es1a90.c$l.d, n = 31, power = NULL)

# so not significant, evidence slightly favors null, and it fails small telescopes approach.




# 1b ----
r1b <- read.csv('clean1b.csv')


s1b <- read.csv('code1b_Strict.csv') # grab steph's coding
s1b <- s1b %>% select(tag, Wrong)
summary(s1b)
nrow(s1b)

hist(s1b$Wrong) # most everyone gets < 2 wrong, cutoff there?


clean1b <- merge(r1b, s1b, by = 'tag') %>% 
  select(Condition, RiskLikelihood, Wrong) %>% 
  filter(Wrong < 1)  # keep people who had perfect score on sentences

summary(clean1b)
nrow(clean1b)


# replication stats----
# basic t-test
ns1b.c <- table(clean1b$Condition)
t1b.c <- t.test(RiskLikelihood ~ Condition, clean1b, na.rm=T)
es1b.c <- tes(t1b.c$statistic, ns1b.c[1], ns1b.c[2], verbose=F) 


t1b.c
es1b.c$d
es1b.c$l.d
es1b.c$u.d

# okay, now try some Bayes..go to JASP



ttestBF(formula = RiskLikelihood ~ Condition, data=clean1b, nullInterval = c(-Inf, 0))

# evidence slightly favors null, even with fairly generous prior

#small telescopes
# figure out point ES and 90th% bound
es1b90.c <- tes(t1b.c$statistic, ns1b.c[1], ns1b.c[2], verbose=F, level=90)

# initial study n was 61. what power to detect rep poiont
power.t.test(delta=es1b90.c$d, n = 31, power = NULL)

# and at 90th% bound
power.t.test(delta=es1a90.c$l.d, n = 31, power = NULL)

# so not significant, evidence slightly favors null, and it fails small telescopes approach.



# raw data plots ----

# study 1a

a.plot <- ggplot(r1a, aes(x=Condition, y=dospert)) +
  geom_boxplot(outlier.shape = NA, width = .5) +
  geom_point(position="jitter", alpha=.3) +
  labs(x = NULL, y = 'risk') +
  coord_cartesian(ylim=c(1, 4.5)) +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ,
        panel.grid.minor.y = element_blank() ,
        panel.border = element_rect(color="darkgrey")) 
  

# 1b


b.plot <- ggplot(r1b, aes(x=Condition, y=RiskLikelihood)) +
  geom_boxplot(outlier.shape = NA, width = .5) +
  geom_point(position="jitter", alpha=.3) +
  labs(x = NULL, y = 'risk') +
  coord_cartesian(ylim=c(.5, 5.5)) +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ,
        panel.grid.minor.y = element_blank() ,
        panel.border = element_rect(color="darkgrey")) 


ggarrange(a.plot, b.plot, 
          labels = c("1a", "1b"), nrow = 2)




# demographic table ----



