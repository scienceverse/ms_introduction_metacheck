


d<-read.csv('data.csv')

##########################
# Main text analysis
#########################

# Follow-back main analysis
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)
      ,data=d)

summary(r)
confint(r)

# Blocking main analysis
r<-lm(blocked~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)
      ,data=d)

summary(r)
confint(r)

######### effects when sub-setting on extreme users
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)
      ,data=d[d$extreme,])

summary(r)
confint(r)

######### Check if the treatment is moderated by any of the user characteristics

# bot score
ps=c()
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)*scale(bot_score)
      ,data=d)

summary(r)
confint(r)
ps <- c(0.7684)

# user ethnicity
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)*scale(Ethnicity!="White")
      ,data=d)

summary(r)
confint(r)
ps <- c(ps,0.128)

# political extremity
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)*scale(extreme)
        
      ,data=d)

summary(r)
confint(r)
ps <- c(ps, 0.036454 )

# age
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)*scale(age)
      
      ,data=d)

summary(r)
confint(r)
ps <- c(ps,0.05882)

# gender
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)*scale(m3_gender_female)
      
      ,data=d)

summary(r)
confint(r)
ps <- c(ps,0.5189  )

# toxcity
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)*scale(TOXICITY)
      
      ,data=d)

summary(r)
confint(r)
ps <- c(ps, 0.959  )

# racial slur
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy)*(black_dummy)*scale(racial_slur>0)
      
      ,data=d)

summary(r)
confint(r)
ps <- c(ps,0.2188 )

# BH adjustment of p values
p.adjust(ps,method='BH')




###########
# Supplement analysis
##########

# Robustness of results using continuous measure of political orientation

# Follow-back
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(leaning)*(black_dummy)
      ,data=d)
summary(r)
confint(r)

# blocking

r<-lm(blocked~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(leaning)*(black_dummy)
      ,data=d)
summary(r)

# Robustness of results using different measures of political orientation

## follow-back using grinberg method
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy_grinberg)*(black_dummy)
      ,data=d)
summary(r)

## follow-back using barbera method
r<-lm(follower~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy_barbera)*(black_dummy)
      ,data=d)
summary(r)

## blocking using grinberg method
r<-lm(blocked~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy_grinberg)*(black_dummy)
      ,data=d)
summary(r)


r<-lm(blocked~((co_partisan_dummy)
                +(counter_partisan_dummy))*
        scale(rep_dummy_barbera)*(black_dummy)
      ,data=d)
summary(r)
