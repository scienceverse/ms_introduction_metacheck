# Author Joseph Bulbulia
# Date 15 January 2020 
# Religious Residue: Cross-Cultural Evidence that Religious Psychology and Behavior Persist Following Deidentification
# Journal of Personality and Social Psychology:  Personality Processes and Individual Differences: Study 3


nundat <- NZAVSlongData%>%
  dplyr::mutate(Id = as.factor(Questionnaire.Num))%>% # easier to write as subject identifier
  dplyr::select(-Questionnaire.Num)%>%
  mutate(Hours.Charity.T2= as.numeric(rep(NA,nrow(joindat))))%>% # not asked in these waves 
  mutate(Hours.Charity.T3= as.numeric(rep(NA,nrow(joindat))))%>% # not asked in these waves
  dplyr::select(Id,
                T01,
                T02,
                T03,
                T04,
                T05,
                T06,
                T07,
                T08,
                T09,
                Age.T1,
                Age.T2,
                Age.T3,
                Age.T4,
                Age.T5,
                Age.T6,
                Age.T7,
                Age.T8,
                Age.T9,
                Religious.T1,
                Religious.T2,
                Religious.T3,
                Religious.T4,
                Religious.T5,
                Religious.T6,
                Religious.T7,
                Religious.T8,
                Religious.T9,
                Hours.Charity.T1,
                Hours.Charity.T2,
                Hours.Charity.T3,
                Hours.Charity.T4,
                Hours.Charity.T5,
                Hours.Charity.T6,
                Hours.Charity.T7,
                Hours.Charity.T8,
                Hours.Charity.T9)  


nrow(nundat) # presently we have 31464 participants
str(nundat)  # check variables


# get data into long form required for some packages. 

library(tidyr)
inner_join( 
  nundat %>%
    dplyr::select(c(Id,starts_with('Religious')))%>%      #~~~~ Left side = date component ~~~~~~~~
    gather(Wave,Religious,-Id) %>%  #~ long form = 1 row per prod per seq   ~
    dplyr::mutate(Wave = recode_factor(Wave,
                                       Religious.T1 = "2009",
                                       Religious.T2 = "2010",
                                       Religious.T3  = "2011",
                                       Religious.T4  = "2012",
                                       Religious.T5  = "2013",
                                       Religious.T6  = "2014",
                                       Religious.T7  = "2015",
                                       Religious.T8  = "2016",
                                       Religious.T9 = "2017"))%>%
    arrange(Id),
  
  nundat%>%
    dplyr::select(c(Id,starts_with('T0')))%>%      #~~~~ Left side = date component ~~~~~~~~
    gather(Wave,YearMeasured,-Id) %>%  #~ long form = 1 row per prod per seq   ~
    dplyr::mutate(Wave = recode_factor(Wave,
                                       T01 = "2009",
                                       T02 = "2010",
                                       T03 = "2011",
                                       T04 = "2012",
                                       T05 = "2013",
                                       T06 = "2014",
                                       T07=  "2015",
                                       T08 = "2016",
                                       T09 = "2017"))%>%
    arrange(Id),
)-> nundat.1


inner_join( 
  nundat.1,
  nundat%>%
    dplyr::select(c(Id,starts_with('Hours.Charity')))%>%
    tidyr::gather(Wave,HoursCharity,-Id) %>% 
    dplyr::mutate(Wave = recode_factor(Wave,
                                       Hours.Charity.T1 = "2009",
                                       Hours.Charity.T2 = "2010",
                                       Hours.Charity.T3 = "2011",
                                       Hours.Charity.T4 = "2012",
                                       Hours.Charity.T5 = "2013",
                                       Hours.Charity.T6 = "2014",
                                       Hours.Charity.T7=  "2015",
                                       Hours.Charity.T8 = "2016",
                                       Hours.Charity.T9 = "2017"))%>%
    arrange(Id),
  
)->nundat.2


inner_join( 
  nundat.2,
  nundat%>%
    dplyr::select(c(Id,starts_with('Age')))%>%      #~~~~ Left side = date component ~~~~~~~~
    gather(Wave,Age,-Id) %>%  #~ long form = 1 row per prod per seq   ~
    mutate(Wave = recode_factor(Wave,
                                Age.T1 = "2009",
                                Age.T2 = "2010",
                                Age.T3  = "2011",
                                Age.T4  = "2012",
                                Age.T5  = "2013",
                                Age.T6  = "2014",
                                Age.T7  = "2015",
                                Age.T8  = "2016",
                                Age.T9 = "2017"))%>%
    arrange(Id),
) ->nundat.2.3

## Get binary data for sequence data
nundat.3 <-nundat.2.3%>%
  mutate(HoursGreaterthanZero = (ifelse(HoursCharity !=0,1,0)))
# check
str(nundat.3)
head(nundat.3)


nundat.4<-  nundat.3 %>%
  dplyr::select(Id,HoursGreaterthanZero,Religious,Wave,Age)
str(nundat.4)
head(nundat.4)

## MAKE WIDEVERSION
hourswidelong<-nundat.4%>%
  select(Id,Wave,HoursGreaterthanZero)
head(hourswidelong)

# widedata
hourswide<- spread(hourswidelong,Wave,HoursGreaterthanZero)
head(hourswide)

## MAKE WIDEVERSION
relwidelong<-nundat.4%>%
  select(Id,Wave,Religious)
head(relwidelong)

# wide data
hourswide<- spread(hourswidelong,Wave,HoursGreaterthanZero)
head(hourswide)

relwide<- spread(relwidelong,Wave,Religious)
head(relwide)

# ANALYSIS 
library(seqHMM)

head(hourswide)
head(hourswide[,2:10])
volall <-seqdef(hourswide[,2:10],labels = c( "Vol-","Vol+"))


# print sequence to check
volall
head(relwide)


# create and print rel sequence
head(nundat[,20:28]) # make sure that these are the years for religion

religious_seq = seqdef(relwide[,2:10],labels = c("Rel-","Rel+"))
nrow(religious_seq)


##  seq model 
# to combine religious and binary volunteering observations
mc_obs <- list(religious_seq,volall)
mc_obs1 <- list(volall)

#  to pass a list of the two sequence states
mc_obs

# to initialise the emmissions and transition probabilities 
amc_initmod <- build_hmm(observations = mc_obs,
                         n_states = 4,
                         channel_names = c("R","V"))

# view initial model
amc_initmod

# run seqy  HMM model
mc_fit_s <- fit_model(amc_initmod)

# print to  decimal places
options(scipen=999)
mc_fit_s$model

# plot  model
require("igraph")
require(TraMineR)

# turn off graphics device
dev.off()


#Make sure labels are correct
plot(mc_fit$model,
     layout = layout_nicely,
     loops = TRUE,  # option
     label.max.length = 15,
     vertex.size = 40,
     legend.prop =.2,
     ncol.legend = 5,
     with.legend = "FALSE",
     mark.expand = "Initial probabilities")
