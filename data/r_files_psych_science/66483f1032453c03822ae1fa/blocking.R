require('quickblock')

#df_<-read.csv('data.csv')
d<-read.csv('OSF/data.csv')

##########################
# Blocking procedure 
# * blocking and random assignment was done before conducting the experiment.
# 
#########################

d$female <- 0

d[d$m3_gender_female>.5,]$female <- 1

cols<-c('days_w_tweets_2wk', 'Lfollowers', 'rep_dummy', 'reciprocity_rate','extremity','female')

# first block on covirates
d$strata_race <- quickblock(d[, cols], size_constraint = 12)
# assign the social condition
set.seed(5)
d$condition_race<-assign_treatment(d$strata_race,c('black','white'))
#combine the social condition and blocks
d$strata_copartisan <- qb_blocking(paste(d$strata_race,d$condition_race,sep='_'))
#assign to the ideo condition, whether msg w/ the condocradn bot or disconcordant
set.seed(5)
d$condition_copartisan<-assign_treatment(d$strata_copartisan,c('concord','discord','neutral'))
