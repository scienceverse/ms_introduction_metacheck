####Set-up (need brms package installed, plus rstan)
#The following containts analysis code for the logistic regression analyses
#Reported in Greene & Naveh-Benjamin "A Specificity Principle of Memory: Evidence from Aging and Associative Memory"
#The data is in tall format, with a single observation per row

library(brms) #See Burkner (2017) for details
library(car) #For recoding variables (see below)

methods(class="brmsfit") #what all you can do to a fitted BRMS model

####EXPERIMENT 1

d1 <- read.csv("countsExp1.csv") #full data, all probes
d1$ID = factor(d1$ID)
d1$Age = factor(d1$Age) #0 = young, 1 = old
d1$Delay = factor(d1$Delay) #0 = short, 1 = long
d1$Probe = factor(d1$Probe) #1 = Intact, 2 = Related, 3 = Unrelated

###Intact Probes only
d2 <- subset(d1, Probe==1, select=c(ID, Age, Delay, Response))
head(d2)

#For the full range of responses, use categorical logistic regression
get_prior(Response ~ Age*Delay + (1|ID), data=d2,
          family=categorical(link="logit"))

#Interaction Model
int1 <- brm(Response ~ Age*Delay + (1|ID), data=d2,
            family=categorical(link="logit"),
            prior=c(set_prior("cauchy(0,2.5)", class="b"),
                    set_prior("cauchy(0,10)", class="Intercept")),
            chains=3, cores=2, control=list(adapt_delta=0.95),
            save_model = "intact_stan.txt", file = "intact_exp1.txt")
summary(int1)

#Drop the interaction, main effects only model
int1.me <- brm(Response ~ Age + Delay + (1|ID), data=d2,
               family=categorical(link="logit"),
               prior=c(set_prior("cauchy(0,2.5)", class="b"),
                       set_prior("cauchy(0,10)", class="Intercept")), 
               sample_prior = TRUE,
               chains=3, cores=2, control=list(adapt_delta=0.95),
               file = "intactexp1.nointSP")
summary(int1.me)

#Model checks
plot(int1.me)
marginal_effects(int1.me, categorical=TRUE, points=TRUE)
pp_check(int1.me) #posterior predictive check
int1.loo <- loo(int1.me) #leave-one-out cross-validation
plot(int1.loo, diagnostic=c("k", "n_eff"),
     label_points=FALSE, main="PSIS diagnostic plot")

#Savage-Dickey Hypothesis Tests for Estimates overlapping with 0
hypothesis(int1.me, "mu2_Delay = 0")
hypothesis(int1.me, "mu3_Age2=0")
hypothesis(int1.me, "mu2_Age2=0")

#compare WAIC (Widely Applicable Information Criteria)
w1 <- WAIC(int1)
w2 <- WAIC(int1.me)
compare_ic(w1, w2, ic='waic')

#Reverse logit
param <- fixef(int1.me)[,'Estimate']
plogis(param)

##Binomial logistic regression model
#Accuracy for intact probes; 1 if response is 'intact', 0 otherwise
d2.binary <- subset(d1, Probe==1, select=c(ID, Age, Delay, Response))
d2.binary$Response <-recode(d2.binary$Response, "1=1;2=0;3=0")
head(d2.binary)

d2.bin <- brm(Response ~ Age + Delay + (1|ID), data=d2.binary,
              family=bernoulli(link="logit"),
              prior=c(set_prior("cauchy(0,2.5)", class="b"),
                      set_prior("cauchy(0,10)", class="Intercept")),
              sample_prior = TRUE,
              chains=3, cores=3, control=list(adapt_delta=0.95),
              file="intexp1.binarySP")
summary(d2.bin)
params1 <- fixef(d2.bin)[,'Estimate'] #Reverse logit
plogis(params1) 
hypothesis(d2.bin, "Delay2=0") #Hypothesis test on estimates that overlap with 0
hypothesis(d2.bin, "Age2=0")



###Related Probes only
d3 <- subset(d1, Probe==2, select=c(ID, Age, Delay, Response))
d3$Response <-recode(d3$Response,"1=2;2=1") #set baseline to be related responses
head(d3)

#Interaction model
rel1 <- brm(Response ~ Age*Delay + (1|ID), data=d3,
            family=categorical(link="logit"),
            prior=c(set_prior("cauchy(0,2.5)", class="b"),
                    set_prior("cauchy(0,10)", class="Intercept")),
            chains=3, cores=2, control=list(adapt_delta=0.95),
            file="related_exp1")
summary(rel1)

#Main effects only
rel1.me <- brm(Response ~ Age + Delay + (1|ID), data=d3,
               family=categorical(link="logit"),
               prior=c(set_prior("cauchy(0,2.5)", class="b"),
                       set_prior("cauchy(0,10)", class="Intercept")),
               chains=3, cores=2, control=list(adapt_delta=0.95),
               file="related_exp1.noint")
summary(rel1.me)

#Model checks
plot(rel1.me)
marginal_effects(rel1.me, categorical=TRUE, points=TRUE)
pp_check(rel1.me) #posterior predictive check
rel1.loo <- loo(rel1.me) #leave-one-out cross-validation
plot(rel1.loo, diagnostic=c("k", "n_eff"),
     label_points=FALSE, main="PSIS diagnostic plot")

#WAIC comparison
w.rel1 <- WAIC(rel1)
w.rel2 <- WAIC(rel1.me)
compare_ic(w.rel1, w.rel2, ic='waic')

#Reverse logit
pars2 <- fixef(rel1.me)[,'Estimate']
plogis(pars2)

#Same ME only model, but recode back to orginal (for plotting all probes on same response scale)
d1.r <- subset(d1, Probe==2, select=c(ID, Age, Delay, Response))
r.me <- brm(Response ~ Age + Delay + (1|ID), data=d1.r,
            family=categorical(link="logit"),
            prior=c(set_prior("cauchy(0,2.5)", class="b"),
                    set_prior("cauchy(0,10)", class="Intercept")),
            chains=3, cores=2, control=list(adapt_delta=0.95),
            file="r.me")
summary(r.me)

##Binomial logistic regression
#Accuracy for related probes; 1 if response given was 'related', 0 otherwise
d3.binary <- subset(d1, Probe==2, select=c(ID, Age, Delay, Response))
d3.binary$Response <-recode(d3.binary$Response, "1=0;2=1;3=0")

d3.bin <- brm(Response ~ Age + Delay + (1|ID), data=d3.binary,
              family=bernoulli(link="logit"),
              prior=c(set_prior("cauchy(0,2.5)", class="b"),
                      set_prior("cauchy(0,10)", class="Intercept")),
              chains=3, cores=3, control=list(adapt_delta=0.95),
              file="relexp1.binary")
summary(d3.bin)
params2 <- fixef(d3.bin)[,'Estimate'] #Reverse logit
plogis(params2)


##Unrelated Probes only
d4 <- subset(d1, Probe==3, select=c(ID, Age, Delay, Response))
d4$Response <-recode(d4$Response,"1=3;3=1") #set baseline to be unrelated responses
head(d4)

#Interaction model 
unr1 <- brm(Response ~ Age*Delay + (1|ID), data=d4,
            family=categorical(link="logit"),
            prior=c(set_prior("cauchy(0,2.5)", class="b"),
                    set_prior("cauchy(0,10)", class="Intercept")),
            chains=3, cores=2, control=list(adapt_delta=0.95),
            file="unrelated_exp1")
summary(unr1)

#Main Effects only
unr1.me <- brm(Response ~ Age + Delay + (1|ID), data=d4,
               family=categorical(link="logit"),
               prior=c(set_prior("cauchy(0,2.5)", class="b"),
                       set_prior("cauchy(0,10)", class="Intercept")),
               chains=3, cores=2, control=list(adapt_delta=0.95),
               file="unrelated_exp1.noint")
summary(unr1.me)
pp_check(unr1.me)

#Model checks
plot(unr1.me)
marginal_effects(unr1.me, categorical=TRUE, points=TRUE)
pp_check(unr1.me) #posterior predictive check
unr1.loo <- loo(unr1.me) #leave-one-out cross-validation
plot(unr1.loo, diagnostic=c("k", "n_eff"),
     label_points=FALSE, main="PSIS diagnostic plot")

#WAIC comparison
w.unr1 <- WAIC(unr1)
w.unr2 <- WAIC(unr1.me)
compare_ic(w.unr1, w.unr2, ic='waic')

#Reverse logit
pars3 <- fixef(unr1.me)[,'Estimate']
plogis(pars3)

#Reset probe types (Intact = 1, Related = 2, Unrelated = 3)
d1.u <- subset(d1, Probe==3, select=c(ID, Age, Delay, Response))
u.me <- brm(Response ~ Age + Delay + (1|ID), data=d1.u,
            family=categorical(link="logit"),
            prior=c(set_prior("cauchy(0,2.5)", class="b"),
                    set_prior("cauchy(0,10)", class="Intercept")),
            chains=3, cores=2, control=list(adapt_delta=0.95),
            file="u.me")
summary(u.me)


##Binomial logistic regression
#Accuracy for unrelated probes; 1 if response is 'unrelated', 0 otherwise
d4.binary <- subset(d1, Probe==3, select=c(ID, Age, Delay, Response))
d4.binary$Response <-recode(d4.binary$Response, "1=0;2=0;3=1")
head(d4.binary)

d4.bin <- brm(Response ~ Age + Delay + (1|ID), data=d4.binary,
              family=bernoulli(link="logit"),
              prior=c(set_prior("cauchy(0,2.5)", class="b"),
                      set_prior("cauchy(0,10)", class="Intercept")),
              chains=3, cores=3, control=list(adapt_delta=0.95), 
              sample_prior = TRUE,
              file="unrelexp1.binarySP")
summary(d4.bin)
params3 <- fixef(d4.bin)[,'Estimate'] #Reverse logit
plogis(params3)



###EXPERIMENT 2 

d5 <- read.csv("countsExp2.csv")
ID = factor(d5$ID)
Age = factor(d5$Age)
Delay = factor(d5$Delay)
Probe = factor(d5$Probe)

###Intact Probes Only
d6 <- subset(d5, Probe==1, select=c(ID, Age, Delay, Response))
head(d6)

#Interaction model
int2 <- brm(Response ~ Age*Delay + (1|ID), data=d6,
            family=categorical(link="logit"),
            prior=c(set_prior("cauchy(0,2.5)", class="b"),
                    set_prior("cauchy(0,10)", class="Intercept")),
            chains=3, cores=3, control=list(adapt_delta=0.95, max_treedepth=15),
            file = "intactexp2")
summary(int2)

#Main Effects only
int2.me <- brm(Response ~ Age + Delay + (1|ID), data=d6,
               family=categorical(link="logit"),
               prior=c(set_prior("cauchy(0,2.5)", class="b"),
                       set_prior("cauchy(0,10)", class="Intercept")),
               chains=3, cores=3, control=list(adapt_delta=0.95),
               file="intactexp2.noint")
summary(int2.me)

#Model checks
plot(int2.me)
marginal_effects(int2.me, categorical=TRUE, points=TRUE)
pp_check(int2.me)  #posterior predictive check
int2.loo <- loo(int2.me) #leave-one-out cross-validation
plot(int2.loo, diagnostic=c("k", "n_eff"),
     label_points=FALSE, main="PSIS diagnostic plot")

#WAIC comparison
w.int2 <- waic(int2)
w.int2.me <- waic(int2.me)
compare_ic(w.int2, w.in2.me, ic='waic')

#Reverse logit
pars4 <- fixef(int2.me)[,'Estimate']
plogis(pars4)


##Binomial Logistic Regression model
#Accuracy for intact probes; 1 if response is 'intact', 0 otherwise
d6.binary <- subset(d5, Probe==1, select=c(ID, Age, Delay, Response))
d6.binary$Response <-recode(d6.binary$Response, "1=1;2=0;3=0")
head(d6.binary)

d6.bin <- brm(Response ~ Age + Delay + (1|ID), data=d6.binary,
              family=bernoulli(link="logit"),
              prior=c(set_prior("cauchy(0,2.5)", class="b"),
                      set_prior("cauchy(0,10)", class="Intercept")),
              sample_prior = TRUE,
              chains=3, cores=3, control=list(adapt_delta=0.95),
              file="intexp2.binarySP")
summary(d6.bin)
params4 <- fixef(d6.bin)[,'Estimate'] #Reverse logit
plogis(params4)

#Test hypothesis using Savage-Dickey method
hypothesis(d6.bin, "Delay=0")


###Related Probes Only
d7 <- subset(d5, Probe==2, select=c(ID, Age, Delay, Response))
d7$Response <-recode(d7$Response,"1=2;2=1") #set baseline to be related responses
head(d7)

#Interaction model
rel2 <- brm(Response ~ Age*Delay + (1|ID), data=d7,
            family=categorical(link="logit"),
            prior=c(set_prior("cauchy(0,2.5)", class="b"),
                    set_prior("cauchy(0,10)", class="Intercept")),
            chains=3, cores=3, control=list(adapt_delta=0.95),
            file="relatedexp2")
summary(rel2)

#Main effects only
rel2.me <- brm(Response ~ Age + Delay + (1|ID), data=d7,
               family=categorical(link="logit"),
               prior=c(set_prior("cauchy(0,2.5)", class="b"),
                       set_prior("cauchy(0,10)", class="Intercept")),
               chains=3, cores=3, control=list(adapt_delta=0.95),
               file="relatedexp2.noint")
summary(rel2.me)
pp_check(rel2.me)

#Model checks
plot(rel2.me)
marginal_effects(rel2.me, categorical=TRUE, points=TRUE)
pp_check(rel2.me)  #posterior predictive check
rel2.loo <- loo(rel2.me) #leave-one-out cross-validation
plot(rel2.loo, diagnostic=c("k", "n_eff"),
     label_points=FALSE, main="PSIS diagnostic plot")

#WAIC comparison
w.rel2int <- waic(rel2)
w.rel2noint <- waic(rel2.me)
compare_ic(w.rel2int, w.rel2noint, ic='waic')

#Reverse logit
pars5 <- fixef(rel2.me)[,'Estimate']
plogis(pars5)

#Recode probe types (Intact = 1, Related = 2, Unrelated =3)
d2.r <- subset(d5, Probe==2, select=c(ID, Age, Delay, Response))
r.me2 <- brm(Response ~ Age + Delay + (1|ID), data=d2.r,
             family=categorical(link="logit"),
             prior=c(set_prior("cauchy(0,2.5)", class="b"),
                     set_prior("cauchy(0,10)", class="Intercept")),
             chains=3, cores=2, control=list(adapt_delta=0.95),
             file="r.me2")


##Binomial logistic regression
#Accuracy for related probes; 1 if response given was 'related', 0 otherwise
d7.binary <- subset(d5, Probe==2, select=c(ID, Age, Delay, Response))
d7.binary$Response <-recode(d7.binary$Response, "1=0;2=1;3=0")

d7.bin <- brm(Response ~ Age + Delay + (1|ID), data=d7.binary,
              family=bernoulli(link="logit"),
              prior=c(set_prior("cauchy(0,2.5)", class="b"),
                      set_prior("cauchy(0,10)", class="Intercept")),
              chains=3, cores=3, control=list(adapt_delta=0.95),
              sample_prior = TRUE,
              file="relexp2.binarySP")
summary(d7.bin)
params5 <- fixef(d7.bin)[,'Estimate']
plogis(params5)

#Test of null effect (point estimate 0) using Savage-Dickey method
hypothesis(d7.bin, "Delay=0")


###Unrelated Probes Only
d5.u1 <- subset(d5, Probe==3, select=c(ID, Age, Delay, Probe, Response))
d5.u2 <- subset(d5, Probe==4, select=c(ID, Age, Delay, Probe, Response))
d5.u <- rbind(d5.u1, d5.u2)
d5.u$Response <-recode(d5.u$Response, "1=3;3=1")

#Test for effect of probe type (Unr-Opposite vs Unrelated-Within)
unr.exp2 <- brm(Response ~ Probe + (1|ID), data=d5.u,
                family=bernoulli(link="logit"),
                prior=c(set_prior("cauchy(0,2.5)", class="b"),
                        set_prior("cauchy(0,10)", class="Intercept")),
                chains=3, cores=3, control=list(adapt_delta=0.95),
                file="unrexp2.accuracy")
summary(unr.exp2) #Accuracy worse for within probes

#Age by Probe Type Interaction?
unr.exp2.2 <- brm(Response ~ Age*Probe + (1|ID), data=d5.u,
                  family=bernoulli(link="logit"),
                  prior=c(set_prior("cauchy(0,2.5)", class="b"),
                          set_prior("cauchy(0,10)", class="Intercept")),
                  chains=3, cores=3, control=list(adapt_delta=0.95),
                  file="unrexp2.accuracy.2")
summary(unr.exp2.2) #No interaction

#Main effect only model
unr.exp2.3 <- brm(Response ~ Age + Probe + Delay + (1|ID), data=d5.u,
                  family=bernoulli(link="logit"),
                  prior=c(set_prior("cauchy(0,2.5)", class="b"),
                          set_prior("cauchy(0,10)", class="Intercept")),
                  chains=3, cores=3, control=list(adapt_delta=0.95),
                  file="unrexp2.accuracy.3")
summary(unr.exp2.3) #ME of age and probe

#Model checks
plot(unr.exp2.3)
marginal_effects(unr.exp2.3, categorical=TRUE, points=TRUE)
pp_check(unr.exp2.3)  #posterior predictive check
unr.exp2.3.loo <- loo(unr.exp2.3) #leave-one-out cross-validation
plot(unr.exp2.3.loo, diagnostic=c("k", "n_eff"),
     label_points=FALSE, main="PSIS diagnostic plot")

#WAIC comparison
w.unr2 <- waic(unr.exp2.2)
w.unr2.me <- waic(unr2.exp2.3)
compare_ic(w.unr2, w.unr2.me, ic='waic')

#Reverse logit
pars6 <- fixef(unr2.exp2.3)[,'Estimate']
plogis(pars6)

#For posterior plots, collapse the 2 types of unrelated probes
#Recode probe types (Intact = 1, Related = 2, Unrelated =3)
d5.u <- subset(d5, Probe==2, select=c(ID, Age, Delay, Response))

unr.exp2.4 <- brm(Response ~ (1|ID), data=d5.u,
                  family=categorical(link="logit"),
                  prior=c(set_prior("cauchy(0,10)", class="Intercept")),
                  chains=3, cores=2, control=list(adapt_delta=0.95),
                  file="unrexp2.intvsrelated")
summary(unr.exp2.4)

#Interaction model without Probe
unr2 <- brm(Response ~ Age*Delay + (1|ID), data=d5.u,
            family=categorical(link="logit"),
            prior=c(set_prior("cauchy(0,2.5)", class="b"),
                    set_prior("cauchy(0,10)", class="Intercept")),
            chains=3, cores=3, control=list(adapt_delta=0.95),
            file="unrelatedexp2")
summary(unr2)

#Main effects only without Probe
unr2.me <- brm(Response ~ Age + Delay + (1|ID), data=d9,
               family=categorical(link="logit"),
               prior=c(set_prior("cauchy(0,2.5)", class="b"),
                       set_prior("cauchy(0,10)", class="Intercept")),
               chains=3, cores=3, control=list(adapt_delta=0.95),
               file="unrelexp2.noint")
summary(unr2.me)
pp_check(unr2.me)


#Accuracy for unrelated probes; 1 if response is 'unrelated', 0 otherwise
d5.binary <- subset(d5, Probe==3, select=c(ID, Age, Delay, Response))
d5.binary$Response <-recode(d5.binary$Response, "1=0;2=0;3=1")
head(d5.binary)

d5.bin <- brm(Response ~ Age + Delay + (1|ID), data=d5.binary,
              family=bernoulli(link="logit"),
              prior=c(set_prior("cauchy(0,2.5)", class="b"),
                      set_prior("cauchy(0,10)", class="Intercept")),
              chains=3, cores=3, control=list(adapt_delta=0.95),
              sample_prior = TRUE,
              file="unrexp2.binarySP")
summary(d5.bin)
params6 <- fixef(d5.bin)[,'Estimate']
plogis(params6)
hypothesis(d9.bin, "Delay=0") #Savage-Dickey Hypothesis test






#Posterior Predictive Check Plots for SOM
library(ggplot2)
library(ggpubr)


plot1 <- pp_check(int1.me, type="bars", nsamples=100, freq=FALSE) +
  scale_x_discrete(name="Response",
                   labels=c("Intact", "Related", "Unrelated"),
                   limits=c("1.0", "2.0", "3.0")) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Intact Probes", tag="a") +
  theme(text = element_text(family="serif", size=14, color="black"))
plot1

annotate_figure(plot1,
                top=text_grob("Experiment 1 Logistic Fits",
                              face="bold", size=14))

plot2 <- pp_check(r.me, type="bars", nsamples=100, freq=FALSE) +
  scale_x_discrete(name="Response",
                   labels=c("Intact", "Related", "Unrelated"),
                   limits=c("1.0", "2.0", "3.0")) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Related Probes", tag="b") +
  theme(text = element_text(family="serif", size=14, color="black"))
plot2

plot3 <- pp_check(u.me, type="bars", nsamples=100, freq=FALSE) +
  scale_x_discrete(name="Response",
                   labels=c("Intact", "Related", "Unrelated"),
                   limits=c("1.0", "2.0", "3.0")) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Unrelated Probes", tag="c") +
  theme(text = element_text(family="serif", size=14, color="black"))
plot3

figure <- ggarrange(plot1, plot2, plot3,
                    ncol=1, nrow=3) 
annotate_figure(figure,
                top=text_grob("Experiment 1 Logistic Fits",
                              face="bold", size=14))
figure

plot4 <- pp_check(int2.me, type="bars", nsamples=100, freq=FALSE) +
  scale_x_discrete(name="Response",
                   labels=c("Intact", "Related", "Unrelated"),
                   limits=c("1.0", "2.0", "3.0")) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Intact Probes", tag="a") +
  theme(text = element_text(family="serif", size=14, color="black"))
annotate_figure(plot4,
                top=text_grob("Experiment 2 Logistic Fits",
                              face="bold", size=14))

plot5 <- pp_check(r.me2, type="bars", nsamples=100, freq=FALSE) +
  scale_x_discrete(name="Response",
                   labels=c("Intact", "Related", "Unrelated"),
                   limits=c("1.0", "2.0", "3.0")) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Related Probes", tag="b") +
  theme(text = element_text(family="serif", size=14, color="black"))
plot5

plot6 <- pp_check(unr.exp2.4, type="bars", nsamples=100, freq=FALSE) +
  scale_x_discrete(name="Response",
                   labels=c("Intact", "Related", "Unrelated"),
                   limits=c("1.0", "2.0", "3.0")) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title="Unrelated Probes", tag="c") +
  theme(text = element_text(family="serif", size=14, color="black"))
plot6

figure2 <- ggarrange(plot4, plot5, plot6,
                     ncol=1, nrow=3) 
annotate_figure(figure2,
                top=text_grob("Experiment 2 Logistic Fits",
                              face="bold", size=14))
figure2
