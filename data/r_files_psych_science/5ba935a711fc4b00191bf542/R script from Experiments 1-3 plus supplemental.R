############################Study 1A (with baselines)##############
onea<-read.csv("/Users/kelle/Downloads/Study1a.csv")
onea<-read.csv("/Users/kelle/Documents/Experiment1(new_search).csv")


library(lme4)
library(lmerTest)

#maximum random effects where the model still converges (see Barr, Levy, Tily, Sheepers)
m1 <- lmer(intensitycombined ~ target*NeutralContrast+target*ValenceContrast +
             (target+NeutralContrast+ValenceContrast|id) +
             (target|stim),
           data=onea, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1)

#target vs. baselines (intercept is the difference from baseline among target images); failed to converge until only the two random slopes were remaining.
m1a <- lmer(intensitybasediff ~ controldummy*NeutralContrast+controldummy*ValenceContrast +
              (controldummy|id) +
              (controldummy|stim),
            data=onea, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1a)
#non-target vs. baselines (intercept is the difference among control/non-target images)
m1b <- lmer(intensitybasediff ~ targetdummy*NeutralContrast+targetdummy*ValenceContrast +
              (targetdummy+NeutralContrast+ValenceContrast|id) +
              (targetdummy|stim),
            data=onea, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1b)

#simple for negative stimuli
m1c <- lmer(intensitycombined ~ target*positivedummy+target*neutraldummy +
              (target+positivedummy+neutraldummy|id) +
              (target|stim),
            data=onea, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1c)
#simple for positive stimuli; unlike the previous models, this one converged additional random slopes (the interactions)
m1d <- lmer(intensitycombined ~ target*negativedummy+target*neutraldummy +
              (target*negativedummy+target*neutraldummy|id) +
              (target|stim),
            data=onea, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1d)
#simple for neutral stimuli
m1e <- lmer(intensitycombined ~ target*positivedummy+target*negativedummy +
              (target*negativedummy+target*neutraldummy|id) +
              (target|stim),
            data=onea, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1e)

#liking not affected by the manipulation (nor is there a target*valence interaction on liking)
m2 <- lmer(liking ~ target*NeutralContrast+target*ValenceContrast +
             (target*NeutralContrast+target*ValenceContrast|id) +
             (target|stim),
           data=onea, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2)

#effect on intensity when controlling for liking
m1n <- lmer(intensitycombined ~ target*NeutralContrast+target*ValenceContrast+liking +
             (target+NeutralContrast+ValenceContrast|id) +
             (target|stim),
           data=onea, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1n)

##distinctiveness
m3 <- lmer(dist ~ target*NeutralContrast+target*ValenceContrast +
             (target*NeutralContrast+target*ValenceContrast|id) +
             (target|stim),
           data=onea, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m3)



##robustness tests (Supplemental Material)

#robustness, excluding people who guessed RQ correctly
m1f <- lmer(intensitycombined_excludeRQ ~ target*NeutralContrast+target*ValenceContrast +
              (target*NeutralContrast+target*ValenceContrast|id) +
              (target|stim),
            data=onea, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1f)

#excluding people who answered 1 or 2 to the demand intensity question
m1g <- lmer(intensitycombined_excludedemand ~ target*NeutralContrast+target*ValenceContrast +
              (target*NeutralContrast+target*ValenceContrast|id) +
              (target|stim),
            data=onea, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1g)


#robustness, excluding both groups (research question correct; demand)
m1i <- lmer(intensitycombined_excludeboth ~ target*NeutralContrast+target*ValenceContrast +
              (target*NeutralContrast+target*ValenceContrast|id) +
              (target|stim),
            data=onea, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1i)


#robustness, simple effect among the people who answered "0" to the experimenter demand emotional intensity item
m1g2 <- lmer(intensitycombined ~ target*demand_intensity +
               (target+demand_intensity|id) +
               (target|stim),
             data=onea, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1g2)

#simple effect among the people who didn't answer correctly to the research question question
onea$rqwrong<-abs(onea$rqcorrect-1)
m1f2 <- lmer(intensitycombined ~ target*rqwrong +
               (target*rqwrong|id) +
               (target|stim),
             data=onea, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1f2)


#Supp Mat: effect of target on each emotional intensity item individually; one more objective (how intense is this image) the 2nd more subjective (your reaction)
m1l <- lmer(intensity ~ target*NeutralContrast+target*ValenceContrast +
             (target+NeutralContrast+ValenceContrast|id) +
             (target|stim),
           data=onea, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1l)

m1m <- lmer(reaction ~ target*NeutralContrast+target*ValenceContrast +
             (target+NeutralContrast+ValenceContrast|id) +
             (target|stim),
           data=onea, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1m)


#See separate file for mediation model R script in all experiments.



##########################Spatial Cueing Study (Experiment 2)###################

cf<-read.csv("/Users/kelle/Documents/Experiment2(first_carrasco_cueing).csv")

#use lme4 for forced-choice measures, because it also has generalized models which are needed for binomial DVs.
library("lme4")
#use lmerTest for the ratings, because it also outputs DFs and p-values
library("lmerTest")

#intensity choice; wouldn't converge with the targetcode id random effect, so it was removed (Barr, Levy, Tily, & Sheepers, 2013)
m1 <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, family=binomial,verbose=T)
summary(m1)

#DV choice (distinctiveness + vividness); wouldn't converge with either target random slope
m3 <- glmer(cbind(DVMORE,2) ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (arousalcontrast+valencecontrast|id) +
             (1|stim),
           data=cf, family=binomial, verbose=T)
summary(m3)

##rest will probably go in SOM-R, if there is room there.

#liking
m2 <- lmer(liking ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (arousalcontrast*targetcode+valencecontrast*targetcode|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2)

#intensity rating DV (intensitycombined)
m4 <- lmer(intensitycombined ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (targetcode*arousalcontrast+targetcode*valencecontrast|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4)

#recognition
m5 <- glmer(correct ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (1|id) +
             (1|stim),
           data=cf, verbose=T,family=binomial)
summary(m5)

#manip check; wouldn't converge with any more random slopes
m6 <- glmer(visualMC ~ targetcode*arousalcontrast+targetcode*valencecontrast +
              (targetcode|id) +
              (1|stim),
            data=cf, family=binomial,verbose=T)
summary(m6)

#simples (positive, neutral, negative) on intensity choice
m1a <- glmer(IMORE ~ targetcode*dummynegative+targetcode*dummyneutral +
              (dummynegative+dummyneutral|id) +
              (targetcode|stim),
            data=cf, family=binomial, verbose=T)
summary(m1a)

#simple on neutral; wouldn't converge with any of the random slopes, so just used random intercepts (Barr, Levy, Sheepers, Tily)
m1b <- glmer(IMORE ~ targetcode*dummynegative+targetcode*dummypositive +
              (1|id) +
              (1|stim),
            data=cf, family=binomial, verbose=T)
summary(m1b)

#simple on negative; wouldn't converge with either target random slope (by id or by stim)
m1c <- glmer(IMORE ~ targetcode*dummypositive+targetcode*dummyneutral +
              (dummypositive+dummyneutral|id) +
              (1|stim),
            data=cf, family=binomial, verbose=T)
summary(m1c)

#control for liking and recognition; wouldn't converge until I had only the one random intercept remaining.
m1d <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast+liking +
             (1|id),
           data=cf, family=binomial,verbose=T)
summary(m1d)

m1e <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast+correct +
             (1|id) +
             (1|stim),
           data=cf, family=binomial,verbose=T)
summary(m1e)

#just the no motor response trials
m1f <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast+targetcode*xtrial +
             (arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, family=binomial,verbose=T)
summary(m1f)

#just the no manip check trials
m1g <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast+targetcode*finalfour +
             (arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, family=binomial,verbose=T)
summary(m1g)


#simples on liking
m2a <- lmer(liking ~ targetcode*dummynegative+targetcode*dummyneutral +
             (dummynegative*targetcode+dummyneutral*targetcode|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2a)

m2b <- lmer(liking ~ targetcode*dummynegative+targetcode*dummypositive +
              (dummynegative*targetcode+dummypositive*targetcode|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2b)

m2c <- lmer(liking ~ targetcode*dummypositive+targetcode*dummyneutral +
              (dummypositive*targetcode+dummyneutral*targetcode|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2c)

#liking model controlling for emotional intensity
m2d <- lmer(liking ~ targetcode*arousalcontrast+targetcode*valencecontrast+IMORE +
             (arousalcontrast*targetcode+valencecontrast*targetcode|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2d)

#simples on distinctiveness choice
m3a <- glmer(cbind(DVMORE,2) ~ targetcode*dummynegative+targetcode*dummyneutral +
               (dummynegative+dummyneutral|id) +
               (targetcode|stim),
             data=cf, family=binomial, verbose=T)
summary(m3a)

m3b <- glmer(cbind(DVMORE,2) ~ targetcode*dummynegative+targetcode*dummypositive +
               (dummynegative+dummypositive|id) +
               (targetcode|stim),
             data=cf, family=binomial, verbose=T)
summary(m3b)

m3c <- glmer(cbind(DVMORE,2) ~ targetcode*dummypositive+targetcode*dummyneutral +
               (dummypositive+dummyneutral|id) +
               (targetcode|stim),
             data=cf, family=binomial, verbose=T)
summary(m3c)

m4a <- lmer(intensitycombined ~ targetcode*dummyneutral+targetcode*dummynegative +
             (targetcode*dummyneutral+targetcode*dummynegative|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4a)

m4b <- lmer(intensitycombined ~ targetcode*dummypositive+targetcode*dummynegative +
              (targetcode*dummypositive+targetcode*dummynegative|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4b)

m4c <- lmer(intensitycombined ~ targetcode*dummyneutral+targetcode*dummypositive +
              (targetcode*dummyneutral+targetcode*dummypositive|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4c)

#controlling for liking
newm4d <- lmer(intensitycombined ~ targetcode*arousalcontrast+targetcode*valencecontrast+liking +
              (targetcode*arousalcontrast+targetcode*valencecontrast|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(newm4d)

#controlling for recognition
m4e <- lmer(intensitycombined ~ targetcode*arousalcontrast+targetcode*valencecontrast+correct +
             (targetcode+arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4e)


#See separate R script file for the mediation models in all experiments.



######################## Cueing Mental x Visual (New; E3?)###############

#long form data file
att<-read.csv("/Users/Kelle/Downloads/ATTN-EMO-LONG.csv")

att<-read.csv("/Users/kelle/Documents/Experiment3(mentalXvisual).csv")

#short form data file
at<-read.csv("/Users/LaurenMrkva/Documents/ATTENTION-EMO-LONG.csv")

library(lme4)
library(lmerTest)


###primary model (big; visual and mental, includes all participants); failed to converge with the valence and arousal random slopes
m1<-glmer(IMORE ~ target*arousalcontrast*mentaltarget+target*valencecontrast*mentaltarget+target*arousalcontrast*mentalx2+target*valencecontrast*mentalx2+
            (target|id)+(target|stim),
          data=att, verbose=T,family=binomial)
summary(m1)
confint(m1,method='Wald')


##recognition difference (cued minus noncued)
#probability of getting cued correct
mean(att$reccorrect[att$cuedirection==.5],na.rm=TRUE)
#probability of getting noncued correct
mean(att$reccorrect[att$cuedirection==-.5],na.rm=TRUE)

#effect of cue on recognition
m8<-glmer(reccorrect ~ cuedirection +
            (cuedirection|id)+(cuedirection|stim),
          data=att, verbose=T,family=binomial)
summary(m8)


#effect on intensity when removing those who guessed RQ correctly (on multiple-choice question)
m4b<-glmer(IMORE_suspicious ~ target +
             (target|id)+(target|stim),
           data=att, verbose=T,family=binomial)
summary(m4b)

#manipulation check
mmc<-glmer(MC ~ target*arousalcontrast+target*valencecontrast+
             (target|id)+(1|stim),
           data=att, verbose=T,family=binomial)
summary(mmc)

###distinctiveness section
library(lmerTest)
#distinctiveness, full model; wouldn't converge with the target|stim random effect, so I included only the random intercept for stimulus
m4<-glmer(dist ~ target*arousalcontrast*mentaltarget+target*valencecontrast*mentaltarget+target*arousalcontrast*mentalx2+target*valencecontrast*mentalx2+
            (target|id)+(1|stim),
          data=att, verbose=T,family=binomial)
summary(m4)
confint(m4,method='Wald')


#preacher and hayes mediation; See the other R script file for mediation models.





######################## Attention Repetition Study (Supp Exp 2d)###############

dose<-read.csv("/Users/kelle/Downloads/iapsdose.csv")

#use lmerTest, because it also outputs DFs and p-values
install.packages("lmerTest")
library("lmerTest")

m1 <- lmer(intensitycombined ~ target*linear+target*quadratic+target*cubic +
             (target*linear+target*quadratic+target*cubic|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1)

#distinctiveness
m4 <- lmer(distinctiveness ~ target*linear+target*quadratic+target*cubic +
             (target*linear+target*quadratic+target*cubic|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4)



#time point 1
m6 <- lmer(intensitycombined ~ target*dummy30+target*dummy60+target*dummy100 +
             (target*dummy30+target*dummy60+target*dummy100|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m6)

#time point 2
m7 <- lmer(intensitycombined ~ target*dummy0+target*dummy60+target*dummy100 +
             (target*dummy0+target*dummy60+target*dummy100|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m7)

#time point 3
m8 <- lmer(intensitycombined ~ target*dummy30+target*dummy0+target*dummy100 +
             (target*dummy30+target*dummy0+target*dummy100|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m8)

#time point 4
m9 <- lmer(intensitycombined ~ target*dummy30+target*dummy60+target*dummy0 +
             (target*dummy30+target*dummy60+target*dummy0|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m9)

dose<-read.csv("/Users/Owner/Desktop/iapsdose.csv")


#means or target effects larger in one anchoring condition compared to another?
m12 <- lmer(intensitycombined ~ target +
              (target|id) +
              (target|stim),
            data=dose, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m12)





####################Supp Exp 2a####

sequential<-read.csv("/Users/kelle/Documents/Experiment Supp2a(seq).csv")

#Sequential; overall model
seq11 <- lmer(intensity ~ Target*ArousalContrast+Target*ValenceContrast +
                (ArousalContrast+ValenceContrast|sub) +
                (Target|IAPSImage),
              data=sequential, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11)

#liking (for meta)
seq11l <- lmer(liking ~ Target*ArousalContrast+Target*ValenceContrast +
                (Target*ArousalContrast+Target*ValenceContrast|sub) +
                (Target|IAPSImage),
              data=sequential, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11l)

#simple on neg
seq11n <- lmer(intensity ~ Target*dummypos+Target*dummyneut +
                (Target*dummypos+Target*dummyneut|sub) +
                (Target|IAPSImage),
              data=sequential, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11n)

#simple on pos
seq11p <- lmer(intensity ~ Target*dummyneg+Target*dummyneut +
                 (Target*dummyneg+Target*dummyneut|sub) +
                 (Target|IAPSImage),
               data=sequential, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11p)

#simple on neut
seq11a <- lmer(intensity ~ Target*dummypos+Target*dummyneg +
                (Target*dummypos+Target*dummyneg|sub) +
                (Target|IAPSImage),
              data=sequential, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11a)



#####################delay study (Supp Exp 2b)#############

delay<-read.csv("/Users/kelle/Documents/Experiment 1b.csv")
delay<-read.csv("/Users/kelle/Documents/delay with time 2(Supp2b).csv")
#final individual model (meta, i.e., without time 2)
delay2f <- lmer(EmoIntensity ~ NeutralContrast*Target + ValenceContrast*Target +
                  (NeutralContrast*Target+ ValenceContrast*Target|ID) +
                  (Target|stim),
                data=delay, verbose=T,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=100000)))
summary(delay2f)

#meta; liking
delay2l <- lmer(Liking ~ NeutralContrast*Target + ValenceContrast*Target +
                  (NeutralContrast*Target+ ValenceContrast*Target|ID) +
                  (Target|stim),
                data=delay, verbose=T,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=100000)))
summary(delay2l)



#simples at session1
delay2 <- lmer(EmoIntensity ~ NeutralContrast*Target*dummytime2 + ValenceContrast*Target*dummytime2 +
                 (NeutralContrast*Target*dummytime2 + ValenceContrast*Target|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay2)

#simple at delay session (supplemental material)
delay3 <- lmer(EmoIntensity ~ NeutralContrast*Target*dummytime1 + ValenceContrast*Target*dummytime1 +
                 (NeutralContrast*Target*dummytime1 + ValenceContrast*Target*dummytime1|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay3)
confint(delay3,method='Wald')
confint(delay2,method='Wald')

#across both sessions (supplemental material)
delay1 <- lmer(EmoIntensity ~ NeutralContrast*Target*Time1or2 + ValenceContrast*Target*Time1or2 +
                 (NeutralContrast*Target*Time1or2 + ValenceContrast*Target|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay1)
confint(delay1,method='Wald')



##########################Experiment 2C (also referred to in main text as first follow-up experiment, IV to M)##
#analyses involving exp 1c data only
ivm<-read.csv("/Users/kelle/Downloads/IVtoM(SuppE2CakaFirstFollowup).csv")

#emotional intensity
model1 <- lmer(intensitycombined ~ target*neutralcontrast+target*valencecontrast +
                 (target*neutralcontrast+target*valencecontrast|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model1)

#distinctiveness Exp 1a
model2 <- lmer(rateddistinctiveness ~ target*neutralcontrast+target*valencecontrast +
                 (target*neutralcontrast+target*valencecontrast|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model2)

#recall distinctiveness measure (saturation and contrast)
model3 <- lmer(recalleddistinctiveness ~ target*neutralcontrast+target*valencecontrast +
                 (target*neutralcontrast+target*valencecontrast|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model3)

#I believe these are the final models for the simple effects, but I need to double-check this.
#neutral simple effect, emotional intensity
model1a <- lmer(intensitycombined ~ target*dummynegative+target*dummypositive +
                  (target*dummynegative+target*dummypositive|ID) +
                  (target|stim),
                data=ivm, verbose=T,
                control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model1a)
#positive simple effect, emotional intensity
model1b <- lmer(intensitycombined ~ target*dummynegative+target*dummyneutral +
                  (target*dummynegative+target*dummyneutral|ID) +
                  (target|stim),
                data=ivm, verbose=T,
                control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model1b)
#negative simple effect, emotional intensity
model1c <- lmer(intensitycombined ~ target*dummyneutral+target*dummypositive +
                  (target*dummyneutral+target*dummypositive|ID) +
                  (target|stim),
                data=ivm, verbose=T,
                control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model1c)

#neutral simple effect on rated dist
model4 <- lmer(distinctive ~ target*dummypositive+target*dummynegative +
                 (target*dummypositive+target*dummynegative|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model4)

#positive simple effect on self-report
model5 <- lmer(distinctive ~ target*dummyneutral+target*dummynegative +
                 (target*dummyneutral+target*dummynegative|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model5)

#negative simple effect on self-report
model6 <- lmer(distinctive ~ target*dummypositive+target*dummyneutral +
                 (target*dummypositive+target*dummyneutral|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(model6)




#######M to DV Distinctiveness manipulation (saturation); referred to in text as 2nd follow-up experiment in the Discussion of Experiment 1###
md<-read.csv("/Users/Kelle/Documents/MtoDV(2ndfollowup).csv")


mod1 <- lmer(intensityaverage ~ distinctive*neutralcontrast+distinctive*valencecontrast +
               (distinctive*neutralcontrast+distinctive*valencecontrast|ID)+(distinctive|stim),
             data=md, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(mod1)




####Let me know, at km3386@columbia.edu, if you need the meta-analysis script or data file. I used Borenstein et al. software from Meta-Analysis.com which is point and click,
#so there is no code here. I used some of the models above to get estimates of effects or means for each individual study and then used the Borenstein software to run the actual meta-analysis.

