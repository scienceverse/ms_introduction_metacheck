############## questions or comments? please email me: engelhardtc@health.missouri.edu ########

setwd("")
dat = read.delim(file="Bayesian_models_data.txt",header=T, stringsAsFactors = F)# data brought in as character, not factor
#id = participant number; game = game condition (nonviolent, violent);
#group = diagnosis (autism, typical); aggcog = proportion of words completed
#aggressively on the 98-item aggressive cognition measure; affect = aggressive
#affect composite scores; t1 = standardized and summed intensity and duration 
#on trial 1; t2 = standardized and summed intensity and duration on trial 2 (reactive
#aggression); #avg = standardized and summed intensity and duration on trials 3 to 9
#(average aggressiveness); game_exp = previous game experience (see variable codebook)

# rename columns
colnames(dat)=c('id','game','group','aggcog','affect','t1','t2','avg','game_exp') 

#you will need to install the BayesFactor package to run this code: 'install.packages("BayesFactor")'
library('BayesFactor')

# check classes
for (i in names(dat)) {
  print(i)
  print(class(dat[,i]))
}
# clone dataset and replace missing values with NA
dat2 = dat
dat2[dat == "."] = NA
# ensure columns 4-9 are numeric
for (i in 4:9) {
  print(names(dat2)[i])
  dat2[,i] = as.numeric(dat2[,i])
}
#ensure columns 2-3 are factors
for (i in 2:3) {
  print(names(dat2)[i])
  dat2[,i] = as.factor(dat2[,i])
}
#check column types
for (i in names(dat2)) {
  print(i)
  print(class(dat2[,i]))
}

#note that Bayes factors might differ slightly from those reported 
#in the published paper due to margin of error

#model including unprovoked aggression: trial 1
good=!is.na(dat2$t1)
temp=dat2[good,]
t1=anovaBF(t1 ~ game*group,data = temp,whichModels='withmain',rscaleFixed=.43)

#compare model with group to model with 2 main effects
t1[2]/t1[3]

#compare model with group to full model
t1[2]/t1[4]

#follow-up contrast for ASD
asdonly=dat2[which(dat2$group=='autism'),]

good=!is.na(asdonly$t1)
temp=asdonly[good,]
t1=anovaBF(t1 ~ game,data = temp,whichModels='withmain',rscaleFixed=.43)
1/t1[1]


#follow-up contrast for TD
tdonly=dat2[which(dat2$group=='typical'),]

good=!is.na(tdonly$t1)
temp=tdonly[good,]
t1=anovaBF(t1 ~ game,data = temp,whichModels='withmain',rscaleFixed=.43)
1/t1[1]


#model including reactive aggression: trial 2
good=!is.na(dat2$t2)
temp=dat2[good,]
t2=anovaBF(t2 ~ game*group,data = temp,whichModels='withmain',rscaleFixed=.43)

#compare null model to model with violent content, group diagnosis, 
#additive model, and then full model
1/t2[1]
1/t2[2]
1/t2[3]
1/t2[4]

#follow-up contrast for ASD
good=!is.na(asdonly$t2)
temp=asdonly[good,]
t2=anovaBF(t2 ~ game,data = temp,whichModels='withmain',rscaleFixed=.43)
1/t2[1]

#follow-up contrast for TD
good=!is.na(tdonly$t2)
temp=tdonly[good,]
t2=anovaBF(t2 ~ game,data = temp,whichModels='withmain',rscaleFixed=.43)
1/t2[1]

#model including average aggressiveness: trials 3 to 9
good=!is.na(dat2$avg)
temp=dat2[good,]
avg=anovaBF(avg ~ game*group,data = temp,whichModels='withmain',rscaleFixed=.43)

#compare null model to model with violetn content, group diagnosis, 
#additive model, and then full model
1/avg[1]
1/avg[2]
1/avg[3]
1/avg[4]

#follow-up contrast for ASD
good=!is.na(asdonly$avg)
temp=asdonly[good,]
avg=anovaBF(avg ~ game,data = temp,whichModels='withmain',rscaleFixed=.43)
1/avg[1]

#follow-up contrast for TD
good=!is.na(tdonly$avg)
temp=tdonly[good,]
avg=anovaBF(avg ~ game,data = temp,whichModels='withmain',rscaleFixed=.43)
1/avg[1]

#model including aggressive cognition
good=!is.na(dat2$aggcog)
temp=dat2[good,]
aggcog=anovaBF(aggcog ~ game*group,data = temp,whichModels='withmain',rscaleFixed=.45)

#compare null model to model with violent content, group diagnosis, 
#additive model, and then full model
1/aggcog[1]
1/aggcog[2]
1/aggcog[3]
1/aggcog[4]

#follow-up contrast for ASD
good=!is.na(asdonly$aggcog)
temp=asdonly[good,]
aggcog=anovaBF(aggcog ~ game,data = temp,whichModels='withmain',rscaleFixed=.45)
1/aggcog[1]

#follow-up contrast for TD
good=!is.na(tdonly$aggcog)
temp=tdonly[good,]
aggcog=anovaBF(aggcog ~ game,data = temp,whichModels='withmain',rscaleFixed=.45)
1/aggcog[1]

#model including aggressive affect
good=!is.na(dat2$affect)
temp=dat2[good,]
affect=anovaBF(affect ~ game*group,data = temp,whichModels='withmain',rscaleFixed=.61)

#compare null model to model with violent content, group diagnosis, 
#additive model, and then full model
1/affect[1]
1/affect[2]
1/affect[3]
1/affect[4]

#follow-up contrast for ASD
good=!is.na(asdonly$affect)
temp=asdonly[good,]
affect=anovaBF(affect ~ game,data = temp,whichModels='withmain',rscaleFixed=.61)
1/affect[1]

#follow-up contrast for TD
good=!is.na(tdonly$affect)
temp=tdonly[good,]
affect=anovaBF(affect ~ game,data = temp,whichModels='withmain',rscaleFixed=.61)
affect


#############supplementary analyses with previous game experience##############
#generalTestBF used for models with continuous predictors

#first shorten column names
colnames(dat2)=c('id','gm','grp','aggcog','affect','t1','t2','avg','exp')

#plot for unprovoked aggression: trial 1
good=!is.na(dat2$t1)
temp=dat2[good,]
t1=generalTestBF(t1 ~ gm*grp*exp,data = temp,whichModels='withmain',rscaleFixed=.43)
sort=sort(t1, decreasing=T)
plot(sort,cex.axis=1.4,cex.names=1.4,marginExpand=.5)

#plot for reactive aggression: trial 2
good=!is.na(dat2$t2)
temp=dat2[good,]
t2=generalTestBF(t2 ~ gm*grp*exp,data = temp,whichModels='withmain',rscaleFixed=.43)
sort=sort(t2, decreasing=T)
plot(sort)
plot(sort,cex.axis=1.4,cex.names=1.4,marginExpand=.5)

#plot for average aggressiveness: trials 3-9
good=!is.na(dat2$avg)
temp=dat2[good,]
avg=generalTestBF(avg ~ gm*grp*exp,data = temp,whichModels='withmain',rscaleFixed=.43)
sort=sort(avg, decreasing=T)
plot(sort)
plot(sort,cex.axis=1.4,cex.names=1.4,marginExpand=.5)

#plot for aggressive cognition
good=!is.na(dat2$aggcog)
temp=dat2[good,]
aggcog=generalTestBF(aggcog ~ gm*grp*exp,data = temp,whichModels='withmain',rscaleFixed=.45)
sort=sort(aggcog, decreasing=T)
plot(sort)
plot(sort,cex.axis=1.4,cex.names=1.4,marginExpand=.5)

#plot for aggressive affect
good=!is.na(dat2$affect)
temp=dat2[good,]
affect=generalTestBF(affect ~ gm*grp*exp,data = temp,whichModels='withmain',rscaleFixed=.61)
sort=sort(affect, decreasing=T)
plot(sort)
plot(sort,cex.axis=1.4,cex.names=1.4,marginExpand=.5)
