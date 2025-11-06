
#install.packages("tidyverse")
#install.packages("MatchIt")
library(dplyr)
library(MatchIt)
library(car)

#turn off scientific notation on pvalues
  options(scipen=999)

#run mplus file "NZAVS-Longitudinal-Syntax-Asexuality - R - Personality" first to generate dataset
#read in mplus dataset, flag missing values, label variables
  database <- read.table(file = "C:/NZAVS Mplus Data/Asexuality-Mplus-Data-Extract-Propensity.txt", header=FALSE)
  database <- na_if(database, 9999)
  colnames(database) <- c("sexori","drawnfrom","asexhet","asexlgb","ee","ea","ec","en","eo","eh","approach","avoid","gender","age",
                        "asian", "pacific", "maori", "employed", "education", "partner", "parent", "deprivation", "religion", "subnum")
  sexori <- factor(database$sexori, levels = c("0","1","2"), labels = c("hetero", "ace", "LGB"))


#get rid of cases with missing data for relevant variables 
#propensity score matching can't handle missing data
  hetdatabase <- na.omit(select(database, drawnfrom, asexhet, sexori, ee, ea, ec, en, eo, eh, gender, age, asian, pacific, maori, employed, education, partner, parent, deprivation, religion, subnum))
  lgbdatabase <- na.omit(select(database, drawnfrom, asexlgb, sexori, ee, ea, ec, en, eo, eh, gender, age, asian, pacific, maori, employed, education, partner, parent, deprivation, religion, subnum))

#possibly unnecessary due to added step above
#create dataframe with only asexual and heterosexual participants
  df.acehetero <- subset(hetdatabase, sexori == 0 | sexori == 1)
#create dataframe with only asexual and lgb participants
  df.acelgb <- subset(lgbdatabase, sexori == 1 | sexori == 2)

#function to create a matched sample, run anovas, and return p-values
#compare asexuals and heterosexuals
sampleANOVAhet <- function(df.data){
  #run matching based on gender and age
    matchinfo <- matchit(asexhet ~ gender + age + asian + pacific + maori + employed + education + partner + parent + deprivation + religion, data = df.data, method="nearest", ratio = 1)
  #save matched data in a dataframe
    df.matcheddata <- match.data(matchinfo)[1:ncol(df.data)]
  #run 6 anovas for each personality trait
    anovae <- aov(ee ~ asexhet+age+drawnfrom, data = df.matcheddata)
    anovaa <- aov(ea ~ asexhet+age+drawnfrom, data = df.matcheddata)
    anovac <- aov(ec ~ asexhet+age+drawnfrom, data = df.matcheddata)
    anovan <- aov(en ~ asexhet+age+drawnfrom, data = df.matcheddata)
    anovao <- aov(eo ~ asexhet+age+drawnfrom, data = df.matcheddata)
    anovah <- aov(eh ~ asexhet+age+drawnfrom, data = df.matcheddata)
  #extract only the p-value from each anova
    pe <- (Anova(anovae, type = 3))$'Pr(>F)'[2]
    pa <- (Anova(anovaa, type = 3))$'Pr(>F)'[2]
    pc <- (Anova(anovac, type = 3))$'Pr(>F)'[2]
    pn <- (Anova(anovan, type = 3))$'Pr(>F)'[2]
    po <- (Anova(anovao, type = 3))$'Pr(>F)'[2]
    ph <- (Anova(anovah, type = 3))$'Pr(>F)'[2]
  #collate p values in a data frame and return
    df.pvalues = data.frame(pe, pa, pc, pn, po, ph)
    return(df.pvalues)
}

#function to create a matched sample, run anovas, and return p-values
#compare asexuals and LGB
sampleANOVAlgb <- function(df.data){
  #run matching based on gender and age
    matchinfo <- matchit(asexlgb ~ gender + age + asian + pacific + maori + employed + education + partner + parent + deprivation + religion, data = df.data, method="nearest", ratio = 1)
  #save matched data in a dataframe
    df.matcheddata <- match.data(matchinfo)[1:ncol(df.data)]
  #run 6 anovas for each personality trait
    anovae <- aov(ee ~ asexlgb+age+drawnfrom, data = df.matcheddata)
    anovaa <- aov(ea ~ asexlgb+age+drawnfrom, data = df.matcheddata)
    anovac <- aov(ec ~ asexlgb+age+drawnfrom, data = df.matcheddata)
    anovan <- aov(en ~ asexlgb+age+drawnfrom, data = df.matcheddata)
    anovao <- aov(eo ~ asexlgb+age+drawnfrom, data = df.matcheddata)
    anovah <- aov(eh ~ asexlgb+age+drawnfrom, data = df.matcheddata)
  #extract only the p-value from each anova
    pe <- (Anova(anovae, type = 3))$'Pr(>F)'[2]
    pa <- (Anova(anovaa, type = 3))$'Pr(>F)'[2]
    pc <- (Anova(anovac, type = 3))$'Pr(>F)'[2]
    pn <- (Anova(anovan, type = 3))$'Pr(>F)'[2]
    po <- (Anova(anovao, type = 3))$'Pr(>F)'[2]
    ph <- (Anova(anovah, type = 3))$'Pr(>F)'[2]
  #collate p values in a data frame and return
    df.pvalues = data.frame(pe, pa, pc, pn, po, ph)
    return(df.pvalues)
}


#run ps matching and anovas 1000 times and write the results
#asexuals and heterosexuals
  resultsHet <- replicate(1000, sampleANOVAhet(df.acehetero), simplify = FALSE)
#turn this list of lists into a readable table
  resultsHet <- data.frame(matrix(unlist(resultsHet), nrow=length(resultsHet), byrow=T))
#write to file
  write.csv(resultsHet,"resultsHetPsLong")

#same but asexuals and lgb
  resultsLGB <- replicate(1000, sampleANOVAlgb(df.acelgb), simplify = FALSE)
#turn this list of lists into a readable table
  resultsLGB <- data.frame(matrix(unlist(resultsLGB), nrow=length(resultsLGB), byrow=T))
#write to file
  write.csv(resultsLGB,"resultsLGBPsLong")



#generate one final dataset to write up actual results
#make sure to check against result pattern so you are not using a 'fringe' dataset
#check demographic means before matching
  print(group_by(df.acehetero, asexhet) %>% summarise(mgender = mean(gender), mage = mean(age), masian = mean(asian), mpac = mean(pacific), 
    mmao = mean(maori), memp = mean(employed), medu = mean(education), mpart = mean(partner), 
    mpar = mean(parent), mdep = mean(deprivation), mrelig = mean(religion)))
  matchinfohet <- matchit(asexhet ~ gender + age + asian + pacific + maori + employed + education + partner + parent + deprivation + religion, data = df.acehetero, method="nearest", ratio = 1)
#save matching summary and check demographic means are equal
  sumhet <- summary(matchinfohet)
  print(sumhet$sum.matched[c(1,2,4)])
#save matched data in a dataframe
  df.matcheddatahet <- match.data(matchinfohet)[1:ncol(df.acehetero)]
  write.csv(df.matcheddatahet,"datasethet")

#same but lgb
  print(group_by(df.acelgb, asexlgb) %>% summarise(mgender = mean(gender), mage = mean(age), masian = mean(asian), mpac = mean(pacific), 
    mmao = mean(maori), memp = mean(employed), medu = mean(education), mpart = mean(partner), 
    mpar = mean(parent), mdep = mean(deprivation), mrelig = mean(religion)))
  matchinfolgb <- matchit(asexlgb ~ gender + age + asian + pacific + maori + employed + education + partner + parent + deprivation + religion, data = df.acelgb, method="nearest", ratio = 1)
#save matching summary and check demographic means are equal
  sumlgb <- summary(matchinfolgb)
  print(sumlgb$sum.matched[c(1,2,4)])
#save matched data in a dataframe
  df.matcheddatalgb <- match.data(matchinfolgb)[1:ncol(df.acelgb)]
  write.csv(df.matcheddatalgb,"datasetLGB")
