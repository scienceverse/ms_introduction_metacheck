## Charlesworth & Banaji Patterns of Change I.
## Script version date: April 30th, 2018

####################################################################################################################################

## Weighting and raking for full sample

####################################################################################################################################

## Load the data object
load("fullsample_alldata_043018.RData")

# Install packages
install.packages("anesrake", dependencies = TRUE)
install.packages("car", dependencies = TRUE)

# Load packages
require(anesrake)
require(car)

## Initial data cleaning -------------------------
# Centering att.center1 and demographics in .iat files
# center1 is centered with  0 = neutral, center is mean-centered
table(age.iat$att.new) # 1 = prefer old to young
age.iat$att.center1 <- as.numeric(age.iat$att.new)
age.iat$att.center1 <- age.iat$att.center1 - 4
age.iat$att.center <- scale(age.iat$att.center1, scale = FALSE)
table(age.iat$att.center1)

table(dis.iat$att.new) # 1 = prefer dis to abled
dis.iat$att.center1 <- as.numeric(dis.iat$att.new)
dis.iat$att.center1 <- dis.iat$att.center1 - 4
dis.iat$att.center <- scale(dis.iat$att.center1, scale = FALSE)
table(dis.iat$att.center1)

table(race.iat$att.new) # 1 = prefer Black to White
race.iat$att.center1 <- as.numeric(race.iat$att.new)
race.iat$att.center1 <- race.iat$att.center1 - 4
race.iat$att.center <- scale(race.iat$att.center1, scale = FALSE)
table(race.iat$att.center1)

table(skin.iat$att.new) # 1 = prefer Dark to Light
skin.iat$att.center1 <- as.numeric(skin.iat$att.new)
skin.iat$att.center1 <- skin.iat$att.center1 - 4
skin.iat$att.center <- scale(skin.iat$att.center1, scale = FALSE)
table(skin.iat$att.center1)

table(s.iat$att.new) # 1 = prefer Gay to Straight
s.iat$att.center1 <- as.numeric(s.iat$att.new)
s.iat$att.center1 <- s.iat$att.center1 - 4
s.iat$att.center <- scale(s.iat$att.center1, scale = FALSE)
table(s.iat$att.center1)

table(weight.iat$att.new)  # 1 = prefer fat to thin
weight.iat$att.center1 <- as.numeric(weight.iat$att.new)
weight.iat$att.center1 <- weight.iat$att.center1 - 4
weight.iat$att.center <- scale(weight.iat$att.center1, scale = FALSE)
table(weight.iat$att.center1)


## Demographic variables: 
# Age
age.iat$age.center <- scale(age.iat$age.new, center = TRUE, scale = FALSE)
dis.iat$age.center <- scale(dis.iat$age.new, center = TRUE, scale = FALSE)
race.iat$age.center <- scale(race.iat$age.new, center = TRUE, scale = FALSE)
s.iat$age.center <- scale(s.iat$age.new, center = TRUE, scale = FALSE)
skin.iat$age.center <- scale(skin.iat$age.new, center = TRUE, scale = FALSE)
weight.iat$age.center <- scale(weight.iat$age.new, center = TRUE, scale = FALSE)

# order
age.iat$order.new <- relevel(age.iat$Order, "Young+Good first")
dis.iat$order.new <- relevel(dis.iat$Order, "Abled+Good first")
race.iat$order.new <- relevel(race.iat$Order, "White+Good first")
s.iat$order.new <- relevel(s.iat$Order, "Straight+Good first")
skin.iat$order.new <- relevel(skin.iat$Order, "Light Skin+Good first")
weight.iat$order.new <- relevel(weight.iat$Order, "Thin+Good first")

# ethnic.race.new
race.iat$ethnic.race.new <- relevel(race.iat$ethnic.race.new, "White")
dis.iat$ethnic.race.new <- relevel(dis.iat$ethnic.race.new, "White")
s.iat$ethnic.race.new <- relevel(s.iat$ethnic.race.new, "White")
skin.iat$ethnic.race.new <- relevel(skin.iat$ethnic.race.new, "White")
weight.iat$ethnic.race.new <- relevel(weight.iat$ethnic.race.new, "White")
age.iat$ethnic.race.new <- relevel(age.iat$ethnic.race.new, "White")

race.iat$ethnicrace.center <- as.numeric(race.iat$ethnic.race.new)
race.iat$ethnicrace.center <- scale(race.iat$ethnicrace.center, center = TRUE, scale = FALSE)
dis.iat$ethnicrace.center <- as.numeric(dis.iat$ethnic.race.new)
dis.iat$ethnicrace.center <- scale(dis.iat$ethnicrace.center, center = TRUE, scale = FALSE)
s.iat$ethnicrace.center <- as.numeric(s.iat$ethnic.race.new)
s.iat$ethnicrace.center <- scale(s.iat$ethnicrace.center, center = TRUE, scale = FALSE)
skin.iat$ethnicrace.center <- as.numeric(skin.iat$ethnic.race.new)
skin.iat$ethnicrace.center <- scale(skin.iat$ethnicrace.center, center = TRUE, scale = FALSE)
weight.iat$ethnicrace.center <- as.numeric(weight.iat$ethnic.race.new)
weight.iat$ethnicrace.center <- scale(weight.iat$ethnicrace.center, center = TRUE, scale = FALSE)
age.iat$ethnicrace.center <- as.numeric(age.iat$ethnic.race.new)
age.iat$ethnicrace.center <- scale(age.iat$ethnicrace.center, center = TRUE, scale = FALSE)

# sex
table(age.iat$sex.new)
table(dis.iat$sex.new)
table(race.iat$sex.new)
table(s.iat$sex.new)
table(skin.iat$sex.new)
table(weight.iat$sex.new)
race.iat$sex.center <- as.numeric(race.iat$sex.new)
race.iat$sex.center <- scale(race.iat$sex.center, center = TRUE, scale = FALSE)
dis.iat$sex.center <- as.numeric(dis.iat$sex.new)
dis.iat$sex.center <- scale(dis.iat$sex.center, center = TRUE, scale = FALSE)
s.iat$sex.center <- as.numeric(s.iat$sex.new)
s.iat$sex.center <- scale(s.iat$sex.center, center = TRUE, scale = FALSE)
skin.iat$sex.center <- as.numeric(skin.iat$sex.new)
skin.iat$sex.center <- scale(skin.iat$sex.center, center = TRUE, scale = FALSE)
weight.iat$sex.center <- as.numeric(weight.iat$sex.new)
weight.iat$sex.center <- scale(weight.iat$sex.center, center = TRUE, scale = FALSE)
age.iat$sex.center <- as.numeric(age.iat$sex.new)
age.iat$sex.center <- scale(age.iat$sex.center, center = TRUE, scale = FALSE)

# political id
table(age.iat$politicalid)
table(race.iat$politicalid)
table(dis.iat$politicalid)
table(s.iat$politicalid)
table(skin.iat$politicalid)
table(weight.iat$politicalid)

age.iat$politicalid.releveled <- as.numeric(age.iat$politicalid)
age.iat$politicalid.releveled <- age.iat$politicalid.releveled - 4
age.iat$politicalid.center <- scale(age.iat$politicalid.releveled, center = TRUE, scale = FALSE)
dis.iat$politicalid.releveled <- as.numeric(dis.iat$politicalid)
dis.iat$politicalid.releveled <- dis.iat$politicalid.releveled - 4
dis.iat$politicalid.center <- scale(dis.iat$politicalid.releveled, center = TRUE, scale = FALSE)
race.iat$politicalid.releveled <- as.numeric(race.iat$politicalid)
race.iat$politicalid.releveled <- race.iat$politicalid.releveled - 4
race.iat$politicalid.center <- scale(race.iat$politicalid.releveled, center = TRUE, scale = FALSE)
s.iat$politicalid.releveled <- as.numeric(s.iat$politicalid)
s.iat$politicalid.releveled <- s.iat$politicalid.releveled - 4
s.iat$politicalid.center <- scale(s.iat$politicalid.releveled, center = TRUE, scale = FALSE)
skin.iat$politicalid.releveled <- as.numeric(skin.iat$politicalid)
skin.iat$politicalid.releveled <- skin.iat$politicalid.releveled - 4
skin.iat$politicalid.center <- scale(skin.iat$politicalid.releveled, center = TRUE, scale = FALSE)
weight.iat$politicalid.releveled <- as.numeric(weight.iat$politicalid)
weight.iat$politicalid.releveled <- weight.iat$politicalid.releveled - 4
weight.iat$politicalid.center <- scale(weight.iat$politicalid.releveled, center = TRUE, scale = FALSE)


# education, with "some college" (factor level 5) as 0.
table(age.iat$edu.new)
age.iat$edu.releveled <- as.numeric(age.iat$edu.new)
age.iat$edu.releveled <- age.iat$edu.releveled - 5
age.iat$edu.center <- scale(age.iat$edu.releveled, center = TRUE, scale = FALSE)

table(dis.iat$edu.new)
dis.iat$edu.releveled <- as.numeric(dis.iat$edu.new)
dis.iat$edu.releveled <- dis.iat$edu.releveled - 5
dis.iat$edu.center <- scale(dis.iat$edu.releveled, center = TRUE, scale = FALSE)

table(race.iat$edu.new)
race.iat$edu.releveled <- as.numeric(race.iat$edu.new)
race.iat$edu.releveled <- race.iat$edu.releveled - 5
race.iat$edu.center <- scale(race.iat$edu.releveled, center = TRUE, scale = FALSE)

table(s.iat$edu.new)
s.iat$edu.releveled <- as.numeric(s.iat$edu.new)
s.iat$edu.releveled <- s.iat$edu.releveled - 5
s.iat$edu.center <- scale(s.iat$edu.releveled, center = TRUE, scale = FALSE)

table(skin.iat$edu.new)
skin.iat$edu.releveled <- as.numeric(skin.iat$edu.new)
skin.iat$edu.releveled <- skin.iat$edu.releveled - 5
skin.iat$edu.center <- scale(skin.iat$edu.releveled, center = TRUE, scale = FALSE)

table(weight.iat$edu.new)
weight.iat$edu.releveled <- as.numeric(weight.iat$edu.new)
weight.iat$edu.releveled <- weight.iat$edu.releveled - 5
weight.iat$edu.center <- scale(weight.iat$edu.releveled, center = TRUE, scale = FALSE)


# and attitude-specific variables
# disability
table(dis.iat$dis.new) # 1 = no
dis.iat$disabled.center <- as.numeric(dis.iat$dis.new)
dis.iat$disabled.center <- scale(dis.iat$disabled.center, center = TRUE, scale = FALSE)

# sexuality
table(s.iat$sexuality.new) # 1 = straight
s.iat$sexuality.center <- relevel(s.iat$sexuality.new, "Straight")
table(s.iat$sexuality.center)
s.iat$sexuality.center <- as.numeric(s.iat$sexuality.new)
s.iat$sexuality.center <- scale(s.iat$sexuality.center, center = TRUE, scale = FALSE)

# weight
table(weight.iat$weight.COMB)
weight.iat$weight.center <- as.numeric(weight.iat$weight.COMB)
weight.iat$weight.center <- scale(weight.iat$weight.center, center = TRUE, scale = FALSE)

table(weight.iat$iam.new)
weight.iat$iam.center <- as.numeric(weight.iat$iam.new)
weight.iat$iam.center <- scale(weight.iat$iam.center, center = TRUE, scale = FALSE)



# Subsetting weight data 
weight.iat$stim.collapse <- recode(weight.iat$Stimuli.new, "'body figure v1' = 'body'; 'body figure v2' = 'body'")
weightbody <- weight.iat[weight.iat$stim.collapse=="body",]
table(weightbody$month)
weightface <- weight.iat[weight.iat$stim.collapse=="face",]
table(weightface$month)




## AGE DATA ------------------------------------------------
## keep only 2007 - 2016 data
table(age.iat$month.cent)
age2007 <- age.iat[age.iat$month.cent>=37,]
age2007 <- age2007[age2007$year!=2006,]
table(age2007$month)
  
## First we need to clean the factor levels
### sex
table(age2007$sex.new, age2007$year)
### race
table(age2007$ethnic.race.new, age2007$year)
### education
table(age2007$edu.new, age2007$year)
### age
table(age2007$age.new, age2007$year)
age2007$agecats <- cut(age2007$age.new, c(0,15,25,35,45,55,63,99)) # cut offs to match the minimal distributions (disability attitude)
levels(age2007$agecats) <- c("age0014", "age1524", "age2534", "age3544",
                                     "age4554", "age5562","age6399")
table(age2007$agecats, age2007$year) # now all age categories for each year
### political
table(age2007$politicalid, age2007$year)


#### get the weights of the full population (2004 - 2015)
sex.weights <- as.data.frame(table(age2007$sex.new)/sum(table(age2007$sex.new)))
race.weights <- as.data.frame(table(age2007$ethnic.race.new)/sum(table(age2007$ethnic.race.new)))
edu.weights <- as.data.frame(table(age2007$edu.new)/sum(table(age2007$edu.new)))
age.weights <- as.data.frame(table(age2007$agecats)/sum(table(age2007$agecats)))
pol.weights <- as.data.frame(table(age2007$politicalid)/sum(table(age2007$politicalid)))

#### create the weights (frequency tables)
sex.weights <- sex.weights$Freq
race.weights <- race.weights$Freq
edu.weights <- edu.weights$Freq
age.weights <- age.weights$Freq
pol.weights <- pol.weights$Freq

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights, pol.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage", "Wpol")

# new matched variables in the data set
age2007$Wsex <- as.numeric(age2007$sex.new)
age2007$Wrace <- as.numeric(age2007$ethnic.race.new)
age2007$Wedu <- as.numeric(age2007$edu.new)
age2007$Wage <- as.numeric(age2007$agecats)
age2007$Wpol <- as.numeric(age2007$politicalid)

# now get sub-data for each year, to equate to full population
age.2007 <- age2007[age2007$year==2007,]
age.2008 <- age2007[age2007$year==2008,]
age.2009 <- age2007[age2007$year==2009,]
age.2010 <- age2007[age2007$year==2010,]
age.2011 <- age2007[age2007$year==2011,]
age.2012 <- age2007[age2007$year==2012,]
age.2013 <- age2007[age2007$year==2013,]
age.2014 <- age2007[age2007$year==2014,]
age.2015 <- age2007[age2007$year==2015,]
age.2016 <- age2007[age2007$year==2016,]

## create the weights per year...
weights2007 <- anesrake(targets, age.2007, caseid=age.2007$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2008 <- anesrake(targets, age.2008, caseid=age.2008$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2009 <- anesrake(targets, age.2009, caseid=age.2009$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2010 <- anesrake(targets, age.2010, caseid=age.2010$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2011 <- anesrake(targets, age.2011, caseid=age.2011$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2012 <- anesrake(targets, age.2012, caseid=age.2012$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2013 <- anesrake(targets, age.2013, caseid=age.2013$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2014 <- anesrake(targets, age.2014, caseid=age.2014$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2015 <- anesrake(targets, age.2015, caseid=age.2015$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2016 <- anesrake(targets, age.2016, caseid=age.2016$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007 <- as.data.frame(weights2007[c("caseid", "weightvec")])
weights2008 <- as.data.frame(weights2008[c("caseid", "weightvec")])
weights2009 <- as.data.frame(weights2009[c("caseid", "weightvec")])
weights2010 <- as.data.frame(weights2010[c("caseid", "weightvec")])
weights2011 <- as.data.frame(weights2011[c("caseid", "weightvec")])
weights2012 <- as.data.frame(weights2012[c("caseid", "weightvec")])
weights2013 <- as.data.frame(weights2013[c("caseid", "weightvec")])
weights2014 <- as.data.frame(weights2014[c("caseid", "weightvec")])
weights2015 <- as.data.frame(weights2015[c("caseid", "weightvec")])
weights2016 <- as.data.frame(weights2016[c("caseid", "weightvec")])

weights.all <- rbind(weights2007, weights2008, weights2009, weights2010,
                     weights2011, weights2012, weights2013, weights2014, weights2015, weights2016)

## now create the vector of dscores and weights and month variables
colnames(weights.all) <- c("session_id", "weightvec")
weights.all$session_id <- as.character(weights.all$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
# note: for these data frames, we are not going to include zip code information, since this will be shared and publicly accessible
timeseriesAge2 <- age2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                                   "date", "politicalid", "sex.new", "ethnic.race.new",
                                   "edu.new", "num.new", "age.new", "order.new")]
timeseriesAge2 <- merge(timeseriesAge2, weights.all, by = "session_id")


## DISABILITY DATA ------------------------------------------------
table(dis.full.demogs$month.cent)
dis2007 <- dis.full.demogs[dis.full.demogs$month.cent>=37,]
dis2007 <- dis2007[dis2007$year!=2006,]
table(dis2007$month)

## First we need to clean the factor levels
### sex
table(dis2007$sex.new, dis2007$year)
### race
table(dis2007$ethnic.race.new, dis2007$year)
### education
table(dis2007$edu.new, dis2007$year)
### age
table(dis2007$age.new, dis2007$year) # note: no one over 65 in 2004, will have to adjust, include 64 and up
dis2007$agecats <- cut(dis2007$age.new, c(0,15, 25,35,45,55,63,99))
levels(dis2007$agecats) <- c("age0014", "age1524", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
table(dis2007$agecats, dis2007$year) # now all age categories for each year
## disability
table(dis2007$dis.new, dis2007$year)
### political
table(dis2007$politicalid, dis2007$year)


#### get the weights of the full population (2004 - 2015)
sex.weights <- as.data.frame(table(dis2007$sex.new)/sum(table(dis2007$sex.new)))
race.weights <- as.data.frame(table(dis2007$ethnic.race.new)/sum(table(dis2007$ethnic.race.new)))
edu.weights <- as.data.frame(table(dis2007$edu.new)/sum(table(dis2007$edu.new)))
age.weights <- as.data.frame(table(dis2007$agecats)/sum(table(dis2007$agecats)))
dis.weights <- as.data.frame(table(dis2007$dis.new)/sum(table(dis2007$dis.new)))
pol.weights <- as.data.frame(table(dis2007$politicalid)/sum(table(dis2007$politicalid)))


#### create the weights (frequency tables)
sex.weights <- sex.weights$Freq
race.weights <- race.weights$Freq
edu.weights <- edu.weights$Freq
age.weights <- age.weights$Freq
dis.weights <- dis.weights$Freq
pol.weights <- pol.weights$Freq

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights, dis.weights, pol.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage", "Wdis", "Wpol")

# new matched variables in the data set
dis2007$Wsex <- as.numeric(dis2007$sex.new)
dis2007$Wrace <- as.numeric(dis2007$ethnic.race.new)
dis2007$Wedu <- as.numeric(dis2007$edu.new)
dis2007$Wage <- as.numeric(dis2007$agecats)
dis2007$Wdis <- as.numeric(dis2007$dis.new)
dis2007$Wpol <- as.numeric(dis2007$politicalid)

# now get sub-data for each year, to equate to full population
dis.2007 <- dis2007[dis2007$year==2007,]
dis.2008 <- dis2007[dis2007$year==2008,]
dis.2009 <- dis2007[dis2007$year==2009,]
dis.2010 <- dis2007[dis2007$year==2010,]
dis.2011 <- dis2007[dis2007$year==2011,]
dis.2012 <- dis2007[dis2007$year==2012,]
dis.2013 <- dis2007[dis2007$year==2013,]
dis.2014 <- dis2007[dis2007$year==2014,]
dis.2015 <- dis2007[dis2007$year==2015,]
dis.2016 <- dis2007[dis2007$year==2016,]


## create the weights per year...
weights2007 <- anesrake(targets, dis.2007, caseid=dis.2007$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2008 <- anesrake(targets, dis.2008, caseid=dis.2008$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2009 <- anesrake(targets, dis.2009, caseid=dis.2009$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2010 <- anesrake(targets, dis.2010, caseid=dis.2010$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2011 <- anesrake(targets, dis.2011, caseid=dis.2011$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2012 <- anesrake(targets, dis.2012, caseid=dis.2012$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2013 <- anesrake(targets, dis.2013, caseid=dis.2013$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2014 <- anesrake(targets, dis.2014, caseid=dis.2014$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2015 <- anesrake(targets, dis.2015, caseid=dis.2015$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2016 <- anesrake(targets, dis.2016, caseid=dis.2016$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007 <- as.data.frame(weights2007[c("caseid", "weightvec")])
weights2008 <- as.data.frame(weights2008[c("caseid", "weightvec")])
weights2009 <- as.data.frame(weights2009[c("caseid", "weightvec")])
weights2010 <- as.data.frame(weights2010[c("caseid", "weightvec")])
weights2011 <- as.data.frame(weights2011[c("caseid", "weightvec")])
weights2012 <- as.data.frame(weights2012[c("caseid", "weightvec")])
weights2013 <- as.data.frame(weights2013[c("caseid", "weightvec")])
weights2014 <- as.data.frame(weights2014[c("caseid", "weightvec")])
weights2015 <- as.data.frame(weights2015[c("caseid", "weightvec")])
weights2016 <- as.data.frame(weights2016[c("caseid", "weightvec")])

weights.all <- rbind(weights2007, weights2008, weights2009, weights2010,
                     weights2011, weights2012, weights2013, weights2014, weights2015, weights2016)

## now create the vector of dscores and weights and month variables
colnames(weights.all) <- c("session_id", "weightvec")
weights.all$session_id <- as.character(weights.all$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesDis2 <- dis2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                                   "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                                   "edu.new", "num.new", "age.new", "order.new", "dis.new", "distypes.new")]
timeseriesDis2 <- merge(timeseriesDis2, weights.all, by = "session_id")


## RACE DATA ------------------------------------------------
table(race.full.demogs$month.cent)
race2007 <- race.full.demogs[race.full.demogs$month.cent>=37,]
race2007 <- race2007[race2007$year!=2006,]
table(race2007$month)

## First we need to clean the factor levels
### sex
table(race2007$sex.new, race2007$year)
### race
table(race2007$ethnic.race.new, race2007$year)
### education
table(race2007$edu.new, race2007$year)
### age
table(race2007$age.new, race2007$year)
race2007$agecats <- cut(race2007$age.new, c(0, 25,35,45,55,63,99))
levels(race2007$agecats) <- c("age0024", "age2534", "age3544",
                                      "age4554", "age5562", "age6399")
table(race2007$agecats, race2007$year)
# now all age categories for each year
### political
table(race2007$politicalid, race2007$year)


#### get the weights of the full population (2004 - 2015)
sex.weights <- as.data.frame(table(race2007$sex.new)/sum(table(race2007$sex.new)))
race.weights <- as.data.frame(table(race2007$ethnic.race.new)/sum(table(race2007$ethnic.race.new)))
edu.weights <- as.data.frame(table(race2007$edu.new)/sum(table(race2007$edu.new)))
age.weights <- as.data.frame(table(race2007$agecats)/sum(table(race2007$agecats)))
pol.weights <- as.data.frame(table(race2007$politicalid)/sum(table(race2007$politicalid)))

#### create the weights (frequency tables)
sex.weights <- sex.weights$Freq
race.weights <- race.weights$Freq
edu.weights <- edu.weights$Freq
age.weights <- age.weights$Freq
pol.weights <- pol.weights$Freq

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights, pol.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage", "Wpol")

# new matched variables in the data set
race2007$Wsex <- as.numeric(race2007$sex.new)
race2007$Wrace <- as.numeric(race2007$ethnic.race.new)
race2007$Wedu <- as.numeric(race2007$edu.new)
race2007$Wage <- as.numeric(race2007$agecats)
race2007$Wpol <- as.numeric(race2007$politicalid)

# now get sub-data for each year, to equate to full population
race.2007 <- race2007[race2007$year==2007,]
race.2008 <- race2007[race2007$year==2008,]
race.2009 <- race2007[race2007$year==2009,]
race.2010 <- race2007[race2007$year==2010,]
race.2011 <- race2007[race2007$year==2011,]
race.2012 <- race2007[race2007$year==2012,]
race.2013 <- race2007[race2007$year==2013,]
race.2014 <- race2007[race2007$year==2014,]
race.2015 <- race2007[race2007$year==2015,]
race.2016 <- race2007[race2007$year==2016,]

## create the weights per year...
weights2007 <- anesrake(targets, race.2007, caseid=race.2007$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2008 <- anesrake(targets, race.2008, caseid=race.2008$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2009 <- anesrake(targets, race.2009, caseid=race.2009$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2010 <- anesrake(targets, race.2010, caseid=race.2010$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2011 <- anesrake(targets, race.2011, caseid=race.2011$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2012 <- anesrake(targets, race.2012, caseid=race.2012$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2013 <- anesrake(targets, race.2013, caseid=race.2013$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2014 <- anesrake(targets, race.2014, caseid=race.2014$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2015 <- anesrake(targets, race.2015, caseid=race.2015$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2016 <- anesrake(targets, race.2016, caseid=race.2016$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007 <- as.data.frame(weights2007[c("caseid", "weightvec")])
weights2008 <- as.data.frame(weights2008[c("caseid", "weightvec")])
weights2009 <- as.data.frame(weights2009[c("caseid", "weightvec")])
weights2010 <- as.data.frame(weights2010[c("caseid", "weightvec")])
weights2011 <- as.data.frame(weights2011[c("caseid", "weightvec")])
weights2012 <- as.data.frame(weights2012[c("caseid", "weightvec")])
weights2013 <- as.data.frame(weights2013[c("caseid", "weightvec")])
weights2014 <- as.data.frame(weights2014[c("caseid", "weightvec")])
weights2015 <- as.data.frame(weights2015[c("caseid", "weightvec")])
weights2016 <- as.data.frame(weights2016[c("caseid", "weightvec")])

weights.all <- rbind(weights2007, weights2008, weights2009, weights2010,
                     weights2011, weights2012, weights2013, weights2014, weights2015, weights2016)

## now create the vector of dscores and weights and month variables
colnames(weights.all) <- c("session_id", "weightvec")
weights.all$session_id <- as.character(weights.all$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesRace2 <- race2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                                     "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                                     "edu.new", "num.new", "age.new", "order.new")]
timeseriesRace2 <- merge(timeseriesRace2, weights.all, by = "session_id")



## SEXUALITY DATA ------------------------------------------------
table(sex.full.demogs$month.cent)
sex2007 <- sex.full.demogs[sex.full.demogs$month.cent>=37,]
sex2007 <- sex2007[sex2007$year!=2006,]
table(sex2007$month)

## First we need to clean the factor levels
### sex
table(sex2007$sex.new, sex2007$year)
### race
table(sex2007$ethnic.race.new, sex2007$year)
### education
table(sex2007$edu.new, sex2007$year)
### age
table(sex2007$age.new, sex2007$year)
sex2007$agecats <- cut(sex2007$age.new, c(0, 25,35,45,55,63,99))
levels(sex2007$agecats) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
table(sex2007$agecats, sex2007$year)
# now all age categories for each year
## sexuality
table(sex2007$sexuality.new, sex2007$year)
### political
table(sex2007$politicalid, sex2007$year)

#### get the weights of the full population (2004 - 2015)
sex.weights <- as.data.frame(table(sex2007$sex.new)/sum(table(sex2007$sex.new)))
race.weights <- as.data.frame(table(sex2007$ethnic.race.new)/sum(table(sex2007$ethnic.race.new)))
edu.weights <- as.data.frame(table(sex2007$edu.new)/sum(table(sex2007$edu.new)))
age.weights <- as.data.frame(table(sex2007$agecats)/sum(table(sex2007$agecats)))
sexuality.weights <- as.data.frame(table(sex2007$sexuality.new)/sum(table(sex2007$sexuality.new)))
pol.weights <- as.data.frame(table(sex2007$politicalid)/sum(table(sex2007$politicalid)))

#### create the weights (frequency tables)
sex.weights <- sex.weights$Freq
race.weights <- race.weights$Freq
edu.weights <- edu.weights$Freq
age.weights <- age.weights$Freq
sexuality.weights <- sexuality.weights$Freq
pol.weights <- pol.weights$Freq

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights, sexuality.weights, pol.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage", "Wsexuality", "Wpol")

# new matched variables in the data set
sex2007$Wsex <- as.numeric(sex2007$sex.new)
sex2007$Wrace <- as.numeric(sex2007$ethnic.race.new)
sex2007$Wedu <- as.numeric(sex2007$edu.new)
sex2007$Wage <- as.numeric(sex2007$agecats)
sex2007$Wsexuality <- as.numeric(sex2007$sexuality.new)
sex2007$Wpol <- as.numeric(sex2007$politicalid)

# now get sub-data for each year, to equate to full population
sex.2007 <- sex2007[sex2007$year==2007,]
sex.2008 <- sex2007[sex2007$year==2008,]
sex.2009 <- sex2007[sex2007$year==2009,]
sex.2010 <- sex2007[sex2007$year==2010,]
sex.2011 <- sex2007[sex2007$year==2011,]
sex.2012 <- sex2007[sex2007$year==2012,]
sex.2013 <- sex2007[sex2007$year==2013,]
sex.2014 <- sex2007[sex2007$year==2014,]
sex.2015 <- sex2007[sex2007$year==2015,]
sex.2016 <- sex2007[sex2007$year==2016,]

## create the weights per year...
weights2007 <- anesrake(targets, sex.2007, caseid=sex.2007$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2008 <- anesrake(targets, sex.2008, caseid=sex.2008$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2009 <- anesrake(targets, sex.2009, caseid=sex.2009$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2010 <- anesrake(targets, sex.2010, caseid=sex.2010$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2011 <- anesrake(targets, sex.2011, caseid=sex.2011$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2012 <- anesrake(targets, sex.2012, caseid=sex.2012$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2013 <- anesrake(targets, sex.2013, caseid=sex.2013$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2014 <- anesrake(targets, sex.2014, caseid=sex.2014$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2015 <- anesrake(targets, sex.2015, caseid=sex.2015$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2016 <- anesrake(targets, sex.2016, caseid=sex.2016$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007 <- as.data.frame(weights2007[c("caseid", "weightvec")])
weights2008 <- as.data.frame(weights2008[c("caseid", "weightvec")])
weights2009 <- as.data.frame(weights2009[c("caseid", "weightvec")])
weights2010 <- as.data.frame(weights2010[c("caseid", "weightvec")])
weights2011 <- as.data.frame(weights2011[c("caseid", "weightvec")])
weights2012 <- as.data.frame(weights2012[c("caseid", "weightvec")])
weights2013 <- as.data.frame(weights2013[c("caseid", "weightvec")])
weights2014 <- as.data.frame(weights2014[c("caseid", "weightvec")])
weights2015 <- as.data.frame(weights2015[c("caseid", "weightvec")])
weights2016 <- as.data.frame(weights2016[c("caseid", "weightvec")])

weights.all <- rbind(weights2007, weights2008, weights2009, weights2010,
                     weights2011, weights2012, weights2013, weights2014, weights2015, weights2016)

## now create the vector of dscores and weights and month variables
colnames(weights.all) <- c("session_id", "weightvec")
weights.all$session_id <- as.character(weights.all$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesSex2 <- sex2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                                   "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                                   "edu.new", "num.new", "age.new", "order.new", "sexuality.new", "Stimuli")]
timeseriesSex2 <- merge(timeseriesSex2, weights.all, by = "session_id")




## SKIN-TONE DATA ------------------------------------------------
table(skin.full.demogs$month.cent)
skin2007 <- skin.full.demogs[skin.full.demogs$month.cent>=37,]
skin2007 <- skin2007[skin2007$year!=2006,]
table(skin2007$month)

## First we need to clean the factor levels
### sex
table(skin2007$sex.new, skin2007$year)
### race
table(skin2007$ethnic.race.new, skin2007$year) # one extra racial group was not cleaned from this full data.
skin2007$ethnic.race.new <- recode(skin2007$ethnic.race.new, "'White South Asian White' = 'Other or Unknown'")
table(skin2007$ethnic.race.new, skin2007$year)
### education
table(skin2007$edu.new, skin2007$year)
### age
table(skin2007$age.new, skin2007$year)
skin2007$agecats <- cut(skin2007$age.new, c(0, 25,35,45,55,63,99))
levels(skin2007$agecats) <- c("age0024", "age2534", "age3544",
                                      "age4554", "age5562", "age6399")
table(skin2007$agecats, skin2007$year)
# now all age categories for each year
### political
table(skin2007$politicalid, skin2007$year)

#### get the weights of the full population (2004 - 2015)
sex.weights <- as.data.frame(table(skin2007$sex.new)/sum(table(skin2007$sex.new)))
race.weights <- as.data.frame(table(skin2007$ethnic.race.new)/sum(table(skin2007$ethnic.race.new)))
edu.weights <- as.data.frame(table(skin2007$edu.new)/sum(table(skin2007$edu.new)))
age.weights <- as.data.frame(table(skin2007$agecats)/sum(table(skin2007$agecats)))
pol.weights <- as.data.frame(table(skin2007$politicalid)/sum(table(skin2007$politicalid)))

#### create the weights (frequency tables)
sex.weights <- sex.weights$Freq
race.weights <- race.weights$Freq
edu.weights <- edu.weights$Freq
age.weights <- age.weights$Freq
pol.weights <- pol.weights$Freq

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights, pol.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage", "Wpol")

# new matched variables in the data set
skin2007$Wsex <- as.numeric(skin2007$sex.new)
skin2007$Wrace <- as.numeric(skin2007$ethnic.race.new)
skin2007$Wedu <- as.numeric(skin2007$edu.new)
skin2007$Wage <- as.numeric(skin2007$agecats)
skin2007$Wpol <- as.numeric(skin2007$politicalid)

# now get sub-data for each year, to equate to full population
skin.2007 <- skin2007[skin2007$year==2007,]
skin.2008 <- skin2007[skin2007$year==2008,]
skin.2009 <- skin2007[skin2007$year==2009,]
skin.2010 <- skin2007[skin2007$year==2010,]
skin.2011 <- skin2007[skin2007$year==2011,]
skin.2012 <- skin2007[skin2007$year==2012,]
skin.2013 <- skin2007[skin2007$year==2013,]
skin.2014 <- skin2007[skin2007$year==2014,]
skin.2015 <- skin2007[skin2007$year==2015,]
skin.2016 <- skin2007[skin2007$year==2016,]

## create the weights per year...
weights2007 <- anesrake(targets, skin.2007, caseid=skin.2007$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2008 <- anesrake(targets, skin.2008, caseid=skin.2008$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2009 <- anesrake(targets, skin.2009, caseid=skin.2009$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2010 <- anesrake(targets, skin.2010, caseid=skin.2010$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2011 <- anesrake(targets, skin.2011, caseid=skin.2011$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2012 <- anesrake(targets, skin.2012, caseid=skin.2012$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2013 <- anesrake(targets, skin.2013, caseid=skin.2013$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2014 <- anesrake(targets, skin.2014, caseid=skin.2014$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2015 <- anesrake(targets, skin.2015, caseid=skin.2015$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2016 <- anesrake(targets, skin.2016, caseid=skin.2016$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007 <- as.data.frame(weights2007[c("caseid", "weightvec")])
weights2008 <- as.data.frame(weights2008[c("caseid", "weightvec")])
weights2009 <- as.data.frame(weights2009[c("caseid", "weightvec")])
weights2010 <- as.data.frame(weights2010[c("caseid", "weightvec")])
weights2011 <- as.data.frame(weights2011[c("caseid", "weightvec")])
weights2012 <- as.data.frame(weights2012[c("caseid", "weightvec")])
weights2013 <- as.data.frame(weights2013[c("caseid", "weightvec")])
weights2014 <- as.data.frame(weights2014[c("caseid", "weightvec")])
weights2015 <- as.data.frame(weights2015[c("caseid", "weightvec")])
weights2016 <- as.data.frame(weights2016[c("caseid", "weightvec")])

weights.all <- rbind(weights2007, weights2008, weights2009, weights2010,
                     weights2011, weights2012, weights2013, weights2014, weights2015, weights2016)

## now create the vector of dscores and weights and month variables
colnames(weights.all) <- c("session_id", "weightvec")
weights.all$session_id <- as.character(weights.all$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesSkin2 <- skin2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                                    "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                                    "edu.new", "num.new", "age.new", "order.new", "Stimuli")]
timeseriesSkin2 <- merge(timeseriesSkin2, weights.all, by = "session_id")



## WEIGHT DATA ------------------------------------------------
## (1) Body figure ------------
# Only those who have taken the body figure test
table(weight.full.demogs$Stimuli.new)
weight.full.demogs$stim.collapse <- recode(weight.full.demogs$Stimuli.new, "'body figure v1' = 'body'; 'body figure v2' = 'body'")
weightbody <- weight.full.demogs[weight.full.demogs$stim.collapse=="body",]
table(weightbody$month)

## First we need to clean the factor levels
### sex
table(weightbody$sex.new, weightbody$year)
### race
table(weightbody$ethnic.race.new, weightbody$year)
### education
table(weightbody$edu.new, weightbody$year)
### age
table(weightbody$age.new, weightbody$year)
weightbody$agecats <- cut(weightbody$age.new, c(0, 25,35,45,55,63,99))
levels(weightbody$agecats) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
table(weightbody$agecats, weightbody$year)
# now all age categories for each year
## weight (iam.new)
table(weightbody$iam.new, weightbody$year)
### political
table(weightbody$politicalid, weightbody$year)

#### get the weights of the full population (2004 - 2015)
sex.weights <- as.data.frame(table(weightbody$sex.new)/sum(table(weightbody$sex.new)))
race.weights <- as.data.frame(table(weightbody$ethnic.race.new)/sum(table(weightbody$ethnic.race.new)))
edu.weights <- as.data.frame(table(weightbody$edu.new)/sum(table(weightbody$edu.new)))
age.weights <- as.data.frame(table(weightbody$agecats)/sum(table(weightbody$agecats)))
weight.weights <- as.data.frame(table(weightbody$iam.new)/sum(table(weightbody$iam.new)))
pol.weights <- as.data.frame(table(weightbody$politicalid)/sum(table(weightbody$politicalid)))

#### create the weights (frequency tables)
sex.weights <- sex.weights$Freq
race.weights <- race.weights$Freq
edu.weights <- edu.weights$Freq
age.weights <- age.weights$Freq
weight.weights <- weight.weights$Freq
pol.weights <- pol.weights$Freq

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights, weight.weights, pol.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage", "Wweight", "Wpol")

# new matched variables in the data set
weightbody$Wsex <- as.numeric(weightbody$sex.new)
weightbody$Wrace <- as.numeric(weightbody$ethnic.race.new)
weightbody$Wedu <- as.numeric(weightbody$edu.new)
weightbody$Wage <- as.numeric(weightbody$agecats)
weightbody$Wweight <- as.numeric(weightbody$iam.new)
weightbody$Wpol <- as.numeric(weightbody$politicalid)

# now get sub-data for each year, to equate to full population (only from 2010 onwards)
weight.2010 <- weightbody[weightbody$year==2010,]
weight.2011 <- weightbody[weightbody$year==2011,]
weight.2012 <- weightbody[weightbody$year==2012,]
weight.2013 <- weightbody[weightbody$year==2013,]
weight.2014 <- weightbody[weightbody$year==2014,]
weight.2015 <- weightbody[weightbody$year==2015,]
weight.2016 <- weightbody[weightbody$year==2016,]


## create the weights per year...
weights2010 <- anesrake(targets, weight.2010, caseid=weight.2010$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2011 <- anesrake(targets, weight.2011, caseid=weight.2011$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2012 <- anesrake(targets, weight.2012, caseid=weight.2012$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2013 <- anesrake(targets, weight.2013, caseid=weight.2013$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2014 <- anesrake(targets, weight.2014, caseid=weight.2014$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2015 <- anesrake(targets, weight.2015, caseid=weight.2015$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2016 <- anesrake(targets, weight.2016, caseid=weight.2016$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2010 <- as.data.frame(weights2010[c("caseid", "weightvec")])
weights2011 <- as.data.frame(weights2011[c("caseid", "weightvec")])
weights2012 <- as.data.frame(weights2012[c("caseid", "weightvec")])
weights2013 <- as.data.frame(weights2013[c("caseid", "weightvec")])
weights2014 <- as.data.frame(weights2014[c("caseid", "weightvec")])
weights2015 <- as.data.frame(weights2015[c("caseid", "weightvec")])
weights2016 <- as.data.frame(weights2016[c("caseid", "weightvec")])

weights.all <- rbind(weights2010,weights2011, weights2012, weights2013, weights2014, weights2015, weights2016)

## now create the vector of dscores and weights and month variables
colnames(weights.all) <- c("session_id", "weightvec")
weights.all$session_id <- as.character(weights.all$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons 
timeseriesWeightBody2 <- weightbody[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                                   "date", "politicalid", "sex.new", "ethnic.race.new",
                                   "edu.new", "num.new", "age.new", "order.new", "iam.new", "weight.COMB", "Stimuli.new",
                                   "State", "County")]
timeseriesWeightBody2 <- merge(timeseriesWeightBody2, weights.all, by = "session_id")

## (2) Face ------
# Only those who have taken the face test
weightface <- weight.full.demogs[weight.full.demogs$stim.collapse=="face",]
table(weightface$month)

## First we need to clean the factor levels
### sex
table(weightface$sex.new, weightface$year)
### race
table(weightface$ethnic.race.new, weightface$year)
### education
table(weightface$edu.new, weightface$year) ## changes in 2007, collapse MBA to MBA/JD
weightface$educats <- recode(weightface$edu.new, "'M.B.A.' = 'MBA/JD'; 'J.D.' = 'MBA/JD'")
### age
table(weightface$age.new, weightface$year)
weightface$agecats <- cut(weightface$age.new, c(0, 25,35,45,55,63,99))
levels(weightface$agecats) <- c("age0024", "age2534", "age3544",
                                "age4554", "age5562", "age6399")
table(weightface$agecats, weightface$year)
# now all age categories for each year
## weight (weight.COMB)
table(weightface$weight.COMB, weightface$year)
### political
table(weightface$politicalid, weightface$year)
# cannot include, because Neutral is not available...

#### get the weights of the full population (2004 - 2011)
sex.weights <- as.data.frame(table(weightface$sex.new)/sum(table(weightface$sex.new)))
race.weights <- as.data.frame(table(weightface$ethnic.race.new)/sum(table(weightface$ethnic.race.new)))
edu.weights <- as.data.frame(table(weightface$educats)/sum(table(weightface$educats)))
age.weights <- as.data.frame(table(weightface$agecats)/sum(table(weightface$agecats)))
weight.weights <- as.data.frame(table(weightface$weight.COMB)/sum(table(weightface$weight.COMB)))

#### create the weights (frequency tables)
sex.weights <- sex.weights$Freq
race.weights <- race.weights$Freq
edu.weights <- edu.weights$Freq
age.weights <- age.weights$Freq
weight.weights <- weight.weights$Freq

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights, weight.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage", "Wweight")

# new matched variables in the data set
weightface$Wsex <- as.numeric(weightface$sex.new)
weightface$Wrace <- as.numeric(weightface$ethnic.race.new)
weightface$Wedu <- as.numeric(weightface$educats)
weightface$Wage <- as.numeric(weightface$agecats)
weightface$Wweight <- as.numeric(weightface$weight.COMB)

# now get sub-data for each year, to equate to full population
weight.2004 <- weightface[weightface$year==2004,]
weight.2005 <- weightface[weightface$year==2005,]
weight.2006 <- weightface[weightface$year==2006,]
weight.2007 <- weightface[weightface$year==2007,]
weight.2008 <- weightface[weightface$year==2008,]
weight.2009 <- weightface[weightface$year==2009,]
weight.2010 <- weightface[weightface$year==2010,]
weight.2011 <- weightface[weightface$year==2011,]
weight.2007 <- weightface[weightface$year==2007,]
weight.2008 <- weightface[weightface$year==2008,]
weight.2009 <- weightface[weightface$year==2009,]
weight.2010 <- weightface[weightface$year==2010,]
weight.2011 <- weightface[weightface$year==2011,]


## create the weights per year...
weights2004 <- anesrake(targets, weight.2004, caseid=weight.2004$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2005 <- anesrake(targets, weight.2005, caseid=weight.2005$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2006 <- anesrake(targets, weight.2006, caseid=weight.2006$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2007 <- anesrake(targets, weight.2007, caseid=weight.2007$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2008 <- anesrake(targets, weight.2008, caseid=weight.2008$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2009 <- anesrake(targets, weight.2009, caseid=weight.2009$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2010 <- anesrake(targets, weight.2010, caseid=weight.2010$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2011 <- anesrake(targets, weight.2011, caseid=weight.2011$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2004 <- as.data.frame(weights2004[c("caseid", "weightvec")])
weights2005 <- as.data.frame(weights2005[c("caseid", "weightvec")])
weights2006 <- as.data.frame(weights2006[c("caseid", "weightvec")])
weights2007 <- as.data.frame(weights2007[c("caseid", "weightvec")])
weights2008 <- as.data.frame(weights2008[c("caseid", "weightvec")])
weights2009 <- as.data.frame(weights2009[c("caseid", "weightvec")])
weights2010 <- as.data.frame(weights2010[c("caseid", "weightvec")])
weights2011 <- as.data.frame(weights2011[c("caseid", "weightvec")])

weights.all <- rbind(weights2004, weights2005, weights2006, weights2007,
                     weights2008, weights2009, weights2010, weights2011)

## now create the vector of dscores and weights and month variables
colnames(weights.all) <- c("session_id", "weightvec")
weights.all$session_id <- as.character(weights.all$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons 
timeseriesWeightFace2 <- weightface[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                                 "date", "politicalid", "sex.new", "ethnic.race.new",
                                 "edu.new", "num.new", "age.new", "order.new", "iam.new", "weight.COMB", "Stimuli.new",
                                 "State", "County")]
timeseriesWeightFace2 <- merge(timeseriesWeightFace2, weights.all, by = "session_id")

