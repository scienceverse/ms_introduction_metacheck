#LINES 3-265 ARE CODE FROM THE ORIGINAL INVESTIGATION (Vishkin, 2022, Psychological Science)
#FOLLOWING THAT THE NEW LINES OF CODE FOR THE REPLY-TO-COMMENTARY

rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots

library(dplyr)
library(reshape)
library(tidyr)
library(psych)
library(ggplot2)
library(ggrepel)
library(sjPlot)

setwd() #set working directory. data is downloadable from https://ratings.fide.com/download.phtml  
s1 <- read.csv(file="players.csv", header=TRUE, sep = ",")
#View(s1)

#removing inactive platers
s2=subset(s1,flag!='i')
s2=subset(s2,flag!='wi')
#View(s2)

s2$countryname <- NA
s2$GGI <- NA 
s2$GII <- NA
for (i in 1:length(s2$country)){
  if(s2$country[i]=='AFG') {s2$countryname[i]='Afghanistan'; s2$GGGI[i]=NA; s2$GII[i]=0.655}
  else if(s2$country[i]=='ALB') {s2$countryname[i]='Albania'; s2$GGGI[i]=0.769; s2$GII[i]=0.181}
  else if(s2$country[i]=='ALG') {s2$countryname[i]='Algeria'; s2$GGGI[i]=0.634; s2$GII[i]=0.429}
  else if(s2$country[i]=='ANG') {s2$countryname[i]='Angola'; s2$GGGI[i]=0.66; s2$GII[i]=0.536}
  else if(s2$country[i]=='ARG') {s2$countryname[i]='Argentina'; s2$GGGI[i]=0.746; s2$GII[i]=0.328}
  else if(s2$country[i]=='ARM') {s2$countryname[i]='Armenia'; s2$GGGI[i]=0.684; s2$GII[i]=0.245}
  else if(s2$country[i]=='AUS') {s2$countryname[i]='Australia'; s2$GGGI[i]=0.731; s2$GII[i]=0.097}
  else if(s2$country[i]=='AUT') {s2$countryname[i]='Austria'; s2$GGGI[i]=0.744; s2$GII[i]=0.069}
  else if(s2$country[i]=='AZE') {s2$countryname[i]='Azerbaijan'; s2$GGGI[i]=0.687; s2$GII[i]=0.323}
  else if(s2$country[i]=='BAH') {s2$countryname[i]='Bahamas'; s2$GGGI[i]=0.72; s2$GII[i]=0.341}
  else if(s2$country[i]=='BAN') {s2$countryname[i]='Bangladesh'; s2$GGGI[i]=0.726; s2$GII[i]=0.537}
  else if(s2$country[i]=='BAR') {s2$countryname[i]='Barbados'; s2$GGGI[i]=0.749; s2$GII[i]=0.252}
  else if(s2$country[i]=='BDI') {s2$countryname[i]='Burundi'; s2$GGGI[i]=0.745; s2$GII[i]=0.504}
  else if(s2$country[i]=='BEL') {s2$countryname[i]='Belgium'; s2$GGGI[i]=0.75; s2$GII[i]=0.043}
  else if(s2$country[i]=='BHU') {s2$countryname[i]='Bhutan'; s2$GGGI[i]=0.635; s2$GII[i]=0.421}
  else if(s2$country[i]=='BIH') {s2$countryname[i]='Bosnia & Herzegovina'; s2$GGGI[i]=NA; s2$GII[i]=0.149}
  else if(s2$country[i]=='BIZ') {s2$countryname[i]='Belize'; s2$GGGI[i]=0.671; s2$GII[i]=0.415}
  else if(s2$country[i]=='BLR') {s2$countryname[i]='Belarus'; s2$GGGI[i]=0.746; s2$GII[i]=0.118}
  else if(s2$country[i]=='BOL') {s2$countryname[i]='Bolivia'; s2$GGGI[i]=0.734; s2$GII[i]=0.417}
  else if(s2$country[i]=='BOT') {s2$countryname[i]='Botswana'; s2$GGGI[i]=0.709; s2$GII[i]=0.465}
  else if(s2$country[i]=='BRA') {s2$countryname[i]='Brazil'; s2$GGGI[i]=0.691; s2$GII[i]=0.408}
  else if(s2$country[i]=='BRN') {s2$countryname[i]='Bahrain'; s2$GGGI[i]=0.629; s2$GII[i]=0.212}
  else if(s2$country[i]=='BRU') {s2$countryname[i]='Brunei'; s2$GGGI[i]=0.686; s2$GII[i]=0.255}
  else if(s2$country[i]=='BUL') {s2$countryname[i]='Bulgaria'; s2$GGGI[i]=0.727; s2$GII[i]=0.206}
  else if(s2$country[i]=='BUR') {s2$countryname[i]='Burkina Faso'; s2$GGGI[i]=0.635; s2$GII[i]=0.594}
  else if(s2$country[i]=='CAF') {s2$countryname[i]='Central African Republic'; s2$GGGI[i]=NA; s2$GII[i]=0.68}
  else if(s2$country[i]=='CAM') {s2$countryname[i]='Cambodia'; s2$GGGI[i]=0.694; s2$GII[i]=0.474}
  else if(s2$country[i]=='CAN') {s2$countryname[i]='Canada'; s2$GGGI[i]=0.772; s2$GII[i]=0.08}
  else if(s2$country[i]=='CGO') {s2$countryname[i]='Congo'; s2$GGGI[i]=NA; s2$GII[i]=0.57}
  else if(s2$country[i]=='CHI') {s2$countryname[i]='Chile'; s2$GGGI[i]=0.723; s2$GII[i]=0.247}
  else if(s2$country[i]=='CHN') {s2$countryname[i]='China'; s2$GGGI[i]=0.676; s2$GII[i]=0.168}
  else if(s2$country[i]=='CIV') {s2$countryname[i]='Ivory Coast'; s2$GGGI[i]=0.606; s2$GII[i]=0.638}
  else if(s2$country[i]=='CMR') {s2$countryname[i]='Cameroon'; s2$GGGI[i]=0.686; s2$GII[i]=0.56}
  else if(s2$country[i]=='COL') {s2$countryname[i]='Colombia'; s2$GGGI[i]=0.758; s2$GII[i]=0.428}
  else if(s2$country[i]=='CRC') {s2$countryname[i]='Costa Rica'; s2$GGGI[i]=0.782; s2$GII[i]=0.288}
  else if(s2$country[i]=='CRO') {s2$countryname[i]='Croatia'; s2$GGGI[i]=0.72; s2$GII[i]=0.116}
  else if(s2$country[i]=='CUB') {s2$countryname[i]='Cuba'; s2$GGGI[i]=0.746; s2$GII[i]=0.304}
  else if(s2$country[i]=='CYP') {s2$countryname[i]='Cyprus'; s2$GGGI[i]=0.692; s2$GII[i]=0.086}
  else if(s2$country[i]=='CZE') {s2$countryname[i]='Czech Republic'; s2$GGGI[i]=0.706; s2$GII[i]=0.136}
  else if(s2$country[i]=='DEN') {s2$countryname[i]='Denmark'; s2$GGGI[i]=0.782; s2$GII[i]=0.038}
  else if(s2$country[i]=='DOM') {s2$countryname[i]='Dominican Republic'; s2$GGGI[i]=0.7; s2$GII[i]=0.455}
  else if(s2$country[i]=='ECU') {s2$countryname[i]='Ecuador'; s2$GGGI[i]=0.729; s2$GII[i]=0.384}
  else if(s2$country[i]=='EGY') {s2$countryname[i]='Egypt'; s2$GGGI[i]=0.629; s2$GII[i]=0.449}
  else if(s2$country[i]=='ENG') {s2$countryname[i]='UK'; s2$GGGI[i]=0.767; s2$GII[i]=0.118}
  else if(s2$country[i]=='ESA') {s2$countryname[i]='El Salvador'; s2$GGGI[i]=0.706; s2$GII[i]=0.383}
  else if(s2$country[i]=='ESP') {s2$countryname[i]='Spain'; s2$GGGI[i]=0.795; s2$GII[i]=0.07}
  else if(s2$country[i]=='EST') {s2$countryname[i]='Estonia'; s2$GGGI[i]=0.751; s2$GII[i]=0.086}
  else if(s2$country[i]=='ETH') {s2$countryname[i]='Ethiopia'; s2$GGGI[i]=0.705; s2$GII[i]=0.517}
  else if(s2$country[i]=='FIJ') {s2$countryname[i]='Fiji'; s2$GGGI[i]=0.678; s2$GII[i]=0.37}
  else if(s2$country[i]=='FIN') {s2$countryname[i]='Finland'; s2$GGGI[i]=0.832; s2$GII[i]=0.047}
  else if(s2$country[i]=='FRA') {s2$countryname[i]='France'; s2$GGGI[i]=0.781; s2$GII[i]=0.049}
  else if(s2$country[i]=='GAB') {s2$countryname[i]='Gabon'; s2$GGGI[i]=NA; s2$GII[i]=0.525}
  else if(s2$country[i]=='GAM') {s2$countryname[i]='Gambia'; s2$GGGI[i]=0.628; s2$GII[i]=0.612}
  else if(s2$country[i]=='GEO') {s2$countryname[i]='Georgia'; s2$GGGI[i]=0.708; s2$GII[i]=0.331}
  else if(s2$country[i]=='GER') {s2$countryname[i]='Germany'; s2$GGGI[i]=0.787; s2$GII[i]=0.084}
  else if(s2$country[i]=='GHA') {s2$countryname[i]='Ghana'; s2$GGGI[i]=0.673; s2$GII[i]=0.538}
  else if(s2$country[i]=='GRE') {s2$countryname[i]='Greece'; s2$GGGI[i]=0.701; s2$GII[i]=0.116}
  else if(s2$country[i]=='GUA') {s2$countryname[i]='Guatemala'; s2$GGGI[i]=0.666; s2$GII[i]=0.479}
  else if(s2$country[i]=='GUY') {s2$countryname[i]='Guyana'; s2$GGGI[i]=NA; s2$GII[i]=0.462}
  else if(s2$country[i]=='HAI') {s2$countryname[i]='Haiti'; s2$GGGI[i]=NA; s2$GII[i]=0.636}
  else if(s2$country[i]=='HON') {s2$countryname[i]='Honduras'; s2$GGGI[i]=0.722; s2$GII[i]=0.423}
  else if(s2$country[i]=='HUN') {s2$countryname[i]='Hungary'; s2$GGGI[i]=0.677; s2$GII[i]=0.233}
  else if(s2$country[i]=='INA') {s2$countryname[i]='Indonesia'; s2$GGGI[i]=0.7; s2$GII[i]=0.48}
  else if(s2$country[i]=='IND') {s2$countryname[i]='India'; s2$GGGI[i]=0.668; s2$GII[i]=0.488}
  else if(s2$country[i]=='IRI') {s2$countryname[i]='Iran'; s2$GGGI[i]=0.584; s2$GII[i]=0.459}
  else if(s2$country[i]=='IRL') {s2$countryname[i]='Ireland'; s2$GGGI[i]=0.798; s2$GII[i]=0.093}
  else if(s2$country[i]=='IRQ') {s2$countryname[i]='Iraq'; s2$GGGI[i]=NA; s2$GII[i]=0.577}
  else if(s2$country[i]=='ISL') {s2$countryname[i]='Iceland'; s2$GGGI[i]=0.877; s2$GII[i]=0.058}
  else if(s2$country[i]=='ISR') {s2$countryname[i]='Israel'; s2$GGGI[i]=0.718; s2$GII[i]=0.109}
  else if(s2$country[i]=='ITA') {s2$countryname[i]='Italy'; s2$GGGI[i]=0.707; s2$GII[i]=0.069}
  else if(s2$country[i]=='Ind') {s2$countryname[i]='India'; s2$GGGI[i]=0.668; s2$GII[i]=0.488}
  else if(s2$country[i]=='JAM') {s2$countryname[i]='Jamaica'; s2$GGGI[i]=0.735; s2$GII[i]=0.396}
  else if(s2$country[i]=='JOR') {s2$countryname[i]='Jordan'; s2$GGGI[i]=0.623; s2$GII[i]=0.45}
  else if(s2$country[i]=='JPN') {s2$countryname[i]='Japan'; s2$GGGI[i]=0.652; s2$GII[i]=0.094}
  else if(s2$country[i]=='KAZ') {s2$countryname[i]='Kazakhstan'; s2$GGGI[i]=0.71; s2$GII[i]=0.19}
  else if(s2$country[i]=='KEN') {s2$countryname[i]='Kenya'; s2$GGGI[i]=0.671; s2$GII[i]=0.518}
  else if(s2$country[i]=='KGZ') {s2$countryname[i]='Kyrgyzstan'; s2$GGGI[i]=0.689; s2$GII[i]=0.369}
  else if(s2$country[i]=='KOR') {s2$countryname[i]='South Korea'; s2$GGGI[i]=0.672; s2$GII[i]=0.064}
  else if(s2$country[i]=='KSA') {s2$countryname[i]='Saudi Arabia'; s2$GGGI[i]=0.599; s2$GII[i]=0.252}
  else if(s2$country[i]=='KUW') {s2$countryname[i]='Kuwait'; s2$GGGI[i]=0.65; s2$GII[i]=0.242}
  else if(s2$country[i]=='LAO') {s2$countryname[i]='Laos'; s2$GGGI[i]=0.731; s2$GII[i]=0.459}
  else if(s2$country[i]=='LAT') {s2$countryname[i]='Latvia'; s2$GGGI[i]=0.785; s2$GII[i]=0.176}
  else if(s2$country[i]=='LBA') {s2$countryname[i]='Libya'; s2$GGGI[i]=NA; s2$GII[i]=0.252}
  else if(s2$country[i]=='LES') {s2$countryname[i]='Lesotho'; s2$GGGI[i]=0.695; s2$GII[i]=0.553}
  else if(s2$country[i]=='LIB') {s2$countryname[i]='Lebanon'; s2$GGGI[i]=0.599; s2$GII[i]=0.411}
  else if(s2$country[i]=='LTU') {s2$countryname[i]='Lithuania'; s2$GGGI[i]=0.745; s2$GII[i]=0.124}
  else if(s2$country[i]=='LUX') {s2$countryname[i]='Luxembourg'; s2$GGGI[i]=0.725; s2$GII[i]=0.065}
  else if(s2$country[i]=='MAD') {s2$countryname[i]='Madagascar'; s2$GGGI[i]=0.719; s2$GII[i]=NA}
  else if(s2$country[i]=='MAR') {s2$countryname[i]='Morocco'; s2$GGGI[i]=0.605; s2$GII[i]=0.454}
  else if(s2$country[i]=='MAS') {s2$countryname[i]='Malaysia'; s2$GGGI[i]=0.677; s2$GII[i]=0.253}
  else if(s2$country[i]=='MAW') {s2$countryname[i]='Malawi'; s2$GGGI[i]=0.664; s2$GII[i]=0.565}
  else if(s2$country[i]=='MDA') {s2$countryname[i]='Moldova'; s2$GGGI[i]=0.757; s2$GII[i]=0.204}
  else if(s2$country[i]=='MDV') {s2$countryname[i]='Maldives'; s2$GGGI[i]=0.646; s2$GII[i]=0.369}
  else if(s2$country[i]=='MEX') {s2$countryname[i]='Mexico'; s2$GGGI[i]=0.754; s2$GII[i]=0.322}
  else if(s2$country[i]=='MGL') {s2$countryname[i]='Mongolia'; s2$GGGI[i]=0.706; s2$GII[i]=0.322}
  else if(s2$country[i]=='MKD') {s2$countryname[i]='North Macedonia'; s2$GGGI[i]=0.711; s2$GII[i]=0.143}
  else if(s2$country[i]=='MLI') {s2$countryname[i]='Mali'; s2$GGGI[i]=0.621; s2$GII[i]=0.671}
  else if(s2$country[i]=='MLT') {s2$countryname[i]='Malta'; s2$GGGI[i]=0.693; s2$GII[i]=0.175}
  else if(s2$country[i]=='MNE') {s2$countryname[i]='Montenegro'; s2$GGGI[i]=0.71; s2$GII[i]=0.109}
  else if(s2$country[i]=='MOZ') {s2$countryname[i]='Mozambique'; s2$GGGI[i]=0.723; s2$GII[i]=0.523}
  else if(s2$country[i]=='MRI') {s2$countryname[i]='Mauritius'; s2$GGGI[i]=0.665; s2$GII[i]=0.347}
  else if(s2$country[i]=='MTN') {s2$countryname[i]='Mauritania'; s2$GGGI[i]=0.614; s2$GII[i]=0.634}
  else if(s2$country[i]=='MYA') {s2$countryname[i]='Myanmar'; s2$GGGI[i]=0.665; s2$GII[i]=0.478}
  else if(s2$country[i]=='NAM') {s2$countryname[i]='Namibia'; s2$GGGI[i]=0.784; s2$GII[i]=0.44}
  else if(s2$country[i]=='NCA') {s2$countryname[i]='Nicaragua'; s2$GGGI[i]=0.804; s2$GII[i]=0.428}
  else if(s2$country[i]=='NED') {s2$countryname[i]='Netherlands'; s2$GGGI[i]=0.736; s2$GII[i]=0.043}
  else if(s2$country[i]=='NEP') {s2$countryname[i]='Nepal'; s2$GGGI[i]=0.68; s2$GII[i]=0.452}
  else if(s2$country[i]=='NGR') {s2$countryname[i]='Nigeria'; s2$GGGI[i]=0.635; s2$GII[i]=NA}
  else if(s2$country[i]=='NOR') {s2$countryname[i]='Norway'; s2$GGGI[i]=0.842; s2$GII[i]=0.045}
  else if(s2$country[i]=='NZL') {s2$countryname[i]='New Zealand'; s2$GGGI[i]=0.799; s2$GII[i]=0.123}
  else if(s2$country[i]=='OMA') {s2$countryname[i]='Oman'; s2$GGGI[i]=0.602; s2$GII[i]=0.306}
  else if(s2$country[i]=='PAK') {s2$countryname[i]='Pakistan'; s2$GGGI[i]=0.564; s2$GII[i]=0.538}
  else if(s2$country[i]=='PAN') {s2$countryname[i]='Panama'; s2$GGGI[i]=0.73; s2$GII[i]=0.407}
  else if(s2$country[i]=='PAR') {s2$countryname[i]='Paraguay'; s2$GGGI[i]=0.683; s2$GII[i]=0.446}
  else if(s2$country[i]=='PER') {s2$countryname[i]='Peru'; s2$GGGI[i]=0.714; s2$GII[i]=0.395}
  else if(s2$country[i]=='PHI') {s2$countryname[i]='Philippines'; s2$GGGI[i]=0.781; s2$GII[i]=0.43}
  else if(s2$country[i]=='PNG') {s2$countryname[i]='Papua New Guinea'; s2$GGGI[i]=NA; s2$GII[i]=0.725}
  else if(s2$country[i]=='POL') {s2$countryname[i]='Poland'; s2$GGGI[i]=0.736; s2$GII[i]=0.115}
  else if(s2$country[i]=='POR') {s2$countryname[i]='Portugal'; s2$GGGI[i]=0.744; s2$GII[i]=0.075}
  else if(s2$country[i]=='QAT') {s2$countryname[i]='Qatar'; s2$GGGI[i]=0.629; s2$GII[i]=0.185}
  else if(s2$country[i]=='ROU') {s2$countryname[i]='Romania'; s2$GGGI[i]=0.724; s2$GII[i]=0.276}
  else if(s2$country[i]=='RSA') {s2$countryname[i]='South Africa'; s2$GGGI[i]=0.78; s2$GII[i]=0.406}
  else if(s2$country[i]=='RUS') {s2$countryname[i]='Russia'; s2$GGGI[i]=0.706; s2$GII[i]=0.225}
  else if(s2$country[i]=='RWA') {s2$countryname[i]='Rwanda'; s2$GGGI[i]=0.791; s2$GII[i]=0.402}
  else if(s2$country[i]=='SCO') {s2$countryname[i]='UK'; s2$GGGI[i]=0.767; s2$GII[i]=0.118}
  else if(s2$country[i]=='SEN') {s2$countryname[i]='Senegal'; s2$GGGI[i]=0.684; s2$GII[i]=0.533}
  else if(s2$country[i]=='SIN') {s2$countryname[i]='Singapore'; s2$GGGI[i]=0.724; s2$GII[i]=0.065}
  else if(s2$country[i]=='SLE') {s2$countryname[i]='Sierra Leone'; s2$GGGI[i]=NA; s2$GII[i]=0.644}
  else if(s2$country[i]=='SLO') {s2$countryname[i]='Slovenia'; s2$GGGI[i]=0.743; s2$GII[i]=0.063}
  else if(s2$country[i]=='SRB') {s2$countryname[i]='Serbia'; s2$GGGI[i]=0.736; s2$GII[i]=0.132}
  else if(s2$country[i]=='SRI') {s2$countryname[i]='Sri Lanka'; s2$GGGI[i]=0.68; s2$GII[i]=0.401}
  else if(s2$country[i]=='STP') {s2$countryname[i]='Sao Tome and Principe'; s2$GGGI[i]=NA; s2$GII[i]=0.537}
  else if(s2$country[i]=='SUD') {s2$countryname[i]='Sudan'; s2$GGGI[i]=NA; s2$GII[i]=0.545}
  else if(s2$country[i]=='SUI') {s2$countryname[i]='Switzerland'; s2$GGGI[i]=0.779; s2$GII[i]=0.025}
  else if(s2$country[i]=='SUR') {s2$countryname[i]='Suriname'; s2$GGGI[i]=0.707; s2$GII[i]=0.436}
  else if(s2$country[i]=='SVK') {s2$countryname[i]='Slovakia'; s2$GGGI[i]=0.718; s2$GII[i]=0.191}
  else if(s2$country[i]=='SWE') {s2$countryname[i]='Sweden'; s2$GGGI[i]=0.82; s2$GII[i]=0.039}
  else if(s2$country[i]=='SWZ') {s2$countryname[i]='Eswatini'; s2$GGGI[i]=NA; s2$GII[i]=0.567}
  else if(s2$country[i]=='SYR') {s2$countryname[i]='Syria'; s2$GGGI[i]=0.567; s2$GII[i]=0.482}
  else if(s2$country[i]=='TAN') {s2$countryname[i]='Tanzania'; s2$GGGI[i]=0.713; s2$GII[i]=0.556}
  else if(s2$country[i]=='THA') {s2$countryname[i]='Thailand'; s2$GGGI[i]=0.708; s2$GII[i]=0.359}
  else if(s2$country[i]=='TJK') {s2$countryname[i]='Tajikistan'; s2$GGGI[i]=0.626; s2$GII[i]=0.314}
  else if(s2$country[i]=='TLS') {s2$countryname[i]='Timor-Leste'; s2$GGGI[i]=0.662; s2$GII[i]=NA}
  else if(s2$country[i]=='TOG') {s2$countryname[i]='Togo'; s2$GGGI[i]=NA; s2$GII[i]=0.573}
  else if(s2$country[i]=='TTO') {s2$countryname[i]='Trinidad & Tobago'; s2$GGGI[i]=0.756; s2$GII[i]=0.323}
  else if(s2$country[i]=='TUN') {s2$countryname[i]='Tunisia'; s2$GGGI[i]=0.644; s2$GII[i]=0.296}
  else if(s2$country[i]=='TUR') {s2$countryname[i]='Turkey'; s2$GGGI[i]=0.635; s2$GII[i]=0.306}
  else if(s2$country[i]=='UAE') {s2$countryname[i]='United Arab Emirates'; s2$GGGI[i]=0.655; s2$GII[i]=0.079}
  else if(s2$country[i]=='UGA') {s2$countryname[i]='Uganda'; s2$GGGI[i]=0.717; s2$GII[i]=0.535}
  else if(s2$country[i]=='UKR') {s2$countryname[i]='Ukraine'; s2$GGGI[i]=0.721; s2$GII[i]=0.234}
  else if(s2$country[i]=='URU') {s2$countryname[i]='Uruguay'; s2$GGGI[i]=0.737; s2$GII[i]=0.288}
  else if(s2$country[i]=='USA') {s2$countryname[i]='United States of America'; s2$GGGI[i]=0.724; s2$GII[i]=0.204}
  else if(s2$country[i]=='UZB') {s2$countryname[i]='Uzbekistan'; s2$GGGI[i]=NA; s2$GII[i]=0.288}
  else if(s2$country[i]=='VEN') {s2$countryname[i]='Venezuela'; s2$GGGI[i]=0.713; s2$GII[i]=0.479}
  else if(s2$country[i]=='VIE') {s2$countryname[i]='Vietnam'; s2$GGGI[i]=0.7; s2$GII[i]=0.296}
  else if(s2$country[i]=='WLS') {s2$countryname[i]='UK'; s2$GGGI[i]=0.767; s2$GII[i]=0.118}
  else if(s2$country[i]=='YEM') {s2$countryname[i]='Yemen'; s2$GGGI[i]=0.494; s2$GII[i]=0.795}
  else if(s2$country[i]=='ZAM') {s2$countryname[i]='Zambia'; s2$GGGI[i]=0.731; s2$GII[i]=0.539}
  else if(s2$country[i]=='ZIM') {s2$countryname[i]='Zimbabwe'; s2$GGGI[i]=0.73; s2$GII[i]=0.527}
  else if(s2$country[i]=='CHA') {s2$countryname[i]='Chad'; s2$GGGI[i]=0.596; s2$GII[i]=0.71}
  else if(s2$country[i]=='COD') {s2$countryname[i]='Democratic republic of Congo'; s2$GGGI[i]=NA; s2$GII[i]=0.617}
  else if(s2$country[i]=='CPV') {s2$countryname[i]='cape verde'; s2$GGGI[i]=0.725; s2$GII[i]=0.397}
  else if(s2$country[i]=='LBN') {s2$countryname[i]='Lebanon'; s2$GGGI[i]=0.599; s2$GII[i]=0.411}
  else if(s2$country[i]=='LBR') {s2$countryname[i]='liberia'; s2$GGGI[i]=0.685; s2$GII[i]=0.65}
  else if(s2$country[i]=='LCA') {s2$countryname[i]='saint lucia'; s2$GGGI[i]=NA; s2$GII[i]=0.401}
  else if(s2$country[i]=='SGP') {s2$countryname[i]='singapore'; s2$GGGI[i]=0.724; s2$GII[i]=0.065}
  else if(s2$country[i]=='AHO') {s2$countryname[i]='Netherlands Antilles'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='AND') {s2$countryname[i]='Andorra'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='ARU') {s2$countryname[i]='Aruba'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='BER') {s2$countryname[i]='Bermuda'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='COM') {s2$countryname[i]='Comoros Islands'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='FAI') {s2$countryname[i]='Faroe Islands'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='GCI') {s2$countryname[i]='Guernsey'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='GUM') {s2$countryname[i]='Guam'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='HKG') {s2$countryname[i]='Hong Kong'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='ISV') {s2$countryname[i]='US Virgin Islands'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='IVB') {s2$countryname[i]='British Virgin Islands'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='JCI') {s2$countryname[i]='Jersey'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='LIE') {s2$countryname[i]='Liechtenstein'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='MAC') {s2$countryname[i]='Macau'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='MNC') {s2$countryname[i]='Monaco'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='PLE') {s2$countryname[i]='Palestine'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='PLW') {s2$countryname[i]='Palau'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='PUR') {s2$countryname[i]='Puerto Rico'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='SEY') {s2$countryname[i]='Seychelles'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='SMR') {s2$countryname[i]='San Marino'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='SOL') {s2$countryname[i]='Solomon Islands'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='SOM') {s2$countryname[i]='Somalia'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='TKM') {s2$countryname[i]='Turkmenistan'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='TPE') {s2$countryname[i]='Taiwan'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='ANT') {s2$countryname[i]='Antigua & Barbuda'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='CAY') {s2$countryname[i]='Cayman islands'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='DJI') {s2$countryname[i]='djibouti'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='ERI') {s2$countryname[i]='eritrea'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='GEQ') {s2$countryname[i]='equatorial guinea'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='GRN') {s2$countryname[i]='Grenada'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='KOS') {s2$countryname[i]='kosovo'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='NRU') {s2$countryname[i]='Nauru'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='SKN') {s2$countryname[i]='Saint Kitts and Nevis'; s2$GGGI[i]=NA; s2$GII[i]=NA}
  else if(s2$country[i]=='SSD') {s2$countryname[i]='south sudan'; s2$GGGI[i]=NA; s2$GII[i]=NA}
}

s2=subset(s2,!is.na(GGGI) | !is.na(GII))
s2=subset(s2,country!="FID") #remove participants with no country affiliation

library(janitor)
#View(tabyl(s2$birthday))
#the distribution of players reveals some birth years that are improbable (1010-1915, 8 total players), and 2018-2019 (2 players)
#the first year withsubstantially  more than 2 players is 1920
s2=subset(s2,is.na(birthday) | birthday>1919) #excluding only those born before 1920. 27,472 have missing data for birth year
s2=subset(s2,is.na(birthday) | birthday<2018) #excluding only those born before after 2018. 2 participants
s2=subset(s2,!is.na(countryname)) #LEADS TO SAMPLE SIZE OF 803,485

s2$count=1

bycountry <- s2 %>%
  group_by(countryname, sex, GGGI, GII) %>%
  dplyr::summarise(count = sum(count))
#View(bycountry)

bycountry_wide <- spread(bycountry, sex, count)
#View(bycountry_wide)

bycountry_wide$countrytotal=rowSums(bycountry_wide[,c(4:5)], na.rm=T)
bycountry_wide$proportion=1-bycountry_wide$M/bycountry_wide$countrytotal

bycountry_wide=subset(bycountry_wide,countrytotal>1000)

bycountry_wide$GGGI_squared=scale(bycountry_wide$GGGI)*scale(bycountry_wide$GGGI)
bycountry_wide$GII_squared=scale(bycountry_wide$GII)*scale(bycountry_wide$GII)

#adding mean age
bycountrybirthday <- s2 %>%
  group_by(countryname) %>%
  dplyr::summarise(meanage = round(2020-mean(birthday, na.rm=T),2))
#View(bycountrybirthday)

bycountry_wide <- left_join(bycountry_wide, bycountrybirthday, by = "countryname")


#NEW CODE FOR REPLY-TO-COMMENTARY

setwd() #set working directory. 
demographics <- read.csv("20221106CIA world factbook data_91countries.csv", header=TRUE)
bycountry_wide_demographics=merge(bycountry_wide, demographics, by = c("countryname"))

#for both lines below, 1 is for countries higher in gender equality
bycountry_wide_demographics$GGGI_split=ifelse(bycountry_wide_demographics$GGGI<.681, 0,1) 
bycountry_wide_demographics$GII_split=ifelse(bycountry_wide_demographics$GII<.320, 1,0)

bytrend_GGGI <- bycountry_wide_demographics %>%
  group_by(GGGI_split) %>%
  dplyr::summarise(GGGI_median_age_cor = round(cor.test(GGGI, median_age)["estimate"][[1]][[1]],2),
                   GGGI_median_age_p = round(cor.test(GGGI, median_age)["p.value"][[1]],3),
                   GGGI_median_age_lowerCI = round(cor.test(GGGI, median_age)["conf.int"][[1]][[1]],3),
                   GGGI_median_age_higherCI = round(cor.test(GGGI, median_age)["conf.int"][[1]][[2]],3),
                   GGGI_birth_rate_cor = round(cor.test(GGGI, birth_rate)["estimate"][[1]][[1]],2),
                   GGGI_birth_rate_p = round(cor.test(GGGI, birth_rate)["p.value"][[1]],3),
                   GGGI_birth_rate_lowerCI = round(cor.test(GGGI, birth_rate)["conf.int"][[1]][[1]],3),
                   GGGI_birth_rate_higherCI = round(cor.test(GGGI, birth_rate)["conf.int"][[1]][[2]],3),
                   GGGI_life_expectancy_cor = round(cor.test(GGGI, life_expectancy)["estimate"][[1]][[1]],2),
                   GGGI_life_expectancy_p = round(cor.test(GGGI, life_expectancy)["p.value"][[1]],3),
                   GGGI_life_expectancy_lowerCI = round(cor.test(GGGI, life_expectancy)["conf.int"][[1]][[1]],3),
                   GGGI_life_expectancy_higherCI = round(cor.test(GGGI, life_expectancy)["conf.int"][[1]][[2]],3),
                   proportion_median_age_cor = round(cor.test(proportion, median_age)["estimate"][[1]][[1]],2),
                   proportion_median_age_p = round(cor.test(proportion, median_age)["p.value"][[1]],3),
                   proportion_median_age_lowerCI = round(cor.test(proportion, median_age)["conf.int"][[1]][[1]],3),
                   proportion_median_age_higherCI = round(cor.test(proportion, median_age)["conf.int"][[1]][[2]],3),
                   proportion_birth_rate_cor = round(cor.test(proportion, birth_rate)["estimate"][[1]][[1]],2),
                   proportion_birth_rate_p = round(cor.test(proportion, birth_rate)["p.value"][[1]],3),
                   proportion_birth_rate_lowerCI = round(cor.test(proportion, birth_rate)["conf.int"][[1]][[1]],3),
                   proportion_birth_rate_higherCI = round(cor.test(proportion, birth_rate)["conf.int"][[1]][[2]],3),
                   proportion_life_expectancy_cor = round(cor.test(proportion, life_expectancy)["estimate"][[1]][[1]],2),
                   proportion_life_expectancy_p = round(cor.test(proportion, life_expectancy)["p.value"][[1]],3),
                   proportion_life_expectancy_lowerCI = round(cor.test(proportion, life_expectancy)["conf.int"][[1]][[1]],3),
                   proportion_life_expectancy_higherCI = round(cor.test(proportion, life_expectancy)["conf.int"][[1]][[2]],3),
                   chessmeanage_median_age_cor = round(cor.test(meanage, median_age)["estimate"][[1]][[1]],2),
                   chessmeanage_median_age_p = round(cor.test(meanage, median_age)["p.value"][[1]],3),
                   chessmeanage_median_age_lowerCI = round(cor.test(meanage, median_age)["conf.int"][[1]][[1]],3),
                   chessmeanage_median_age_higherCI = round(cor.test(meanage, median_age)["conf.int"][[1]][[2]],3),
                   chessmeanage_birth_rate_cor = round(cor.test(meanage, birth_rate)["estimate"][[1]][[1]],2),
                   chessmeanage_birth_rate_p = round(cor.test(meanage, birth_rate)["p.value"][[1]],3),
                   chessmeanage_birth_rate_lowerCI = round(cor.test(meanage, birth_rate)["conf.int"][[1]][[1]],3),
                   chessmeanage_birth_rate_higherCI = round(cor.test(meanage, birth_rate)["conf.int"][[1]][[2]],3),
                   chessmeanage_life_expectancy_cor = round(cor.test(meanage, life_expectancy)["estimate"][[1]][[1]],2),
                   chessmeanage_life_expectancy_p = round(cor.test(meanage, life_expectancy)["p.value"][[1]],3),
                   chessmeanage_life_expectancy_lowerCI = round(cor.test(meanage, life_expectancy)["conf.int"][[1]][[1]],3),
                   chessmeanage_life_expectancy_higherCI = round(cor.test(meanage, life_expectancy)["conf.int"][[1]][[2]],3)
                   )
#View(bytrend_GGGI)
write.csv(bytrend_GGGI, file="bytrend_GGGI.csv")

bytrend_GII <- bycountry_wide_demographics %>%
  group_by(GII_split) %>%
  dplyr::summarise(GII_median_age_cor = round(cor.test(GII, median_age)["estimate"][[1]][[1]],2),
                   GII_median_age_p = round(cor.test(GII, median_age)["p.value"][[1]],3),
                   GII_median_age_lowerCI = round(cor.test(GII, median_age)["conf.int"][[1]][[1]],3),
                   GII_median_age_higherCI = round(cor.test(GII, median_age)["conf.int"][[1]][[2]],3),
                   GII_birth_rate_cor = round(cor.test(GII, birth_rate)["estimate"][[1]][[1]],2),
                   GII_birth_rate_p = round(cor.test(GII, birth_rate)["p.value"][[1]],3),
                   GII_birth_rate_lowerCI = round(cor.test(GII, birth_rate)["conf.int"][[1]][[1]],3),
                   GII_birth_rate_higherCI = round(cor.test(GII, birth_rate)["conf.int"][[1]][[2]],3),
                   GII_life_expectancy_cor = round(cor.test(GII, life_expectancy)["estimate"][[1]][[1]],2),
                   GII_life_expectancy_p = round(cor.test(GII, life_expectancy)["p.value"][[1]],3),
                   GII_life_expectancy_lowerCI = round(cor.test(GII, life_expectancy)["conf.int"][[1]][[1]],3),
                   GII_life_expectancy_higherCI = round(cor.test(GII, life_expectancy)["conf.int"][[1]][[2]],3),
                   proportion_median_age_cor = round(cor.test(proportion, median_age)["estimate"][[1]][[1]],2),
                   proportion_median_age_p = round(cor.test(proportion, median_age)["p.value"][[1]],3),
                   proportion_median_age_lowerCI = round(cor.test(proportion, median_age)["conf.int"][[1]][[1]],3),
                   proportion_median_age_higherCI = round(cor.test(proportion, median_age)["conf.int"][[1]][[2]],3),
                   proportion_birth_rate_cor = round(cor.test(proportion, birth_rate)["estimate"][[1]][[1]],2),
                   proportion_birth_rate_p = round(cor.test(proportion, birth_rate)["p.value"][[1]],3),
                   proportion_birth_rate_lowerCI = round(cor.test(proportion, birth_rate)["conf.int"][[1]][[1]],3),
                   proportion_birth_rate_higherCI = round(cor.test(proportion, birth_rate)["conf.int"][[1]][[2]],3),
                   proportion_life_expectancy_cor = round(cor.test(proportion, life_expectancy)["estimate"][[1]][[1]],2),
                   proportion_life_expectancy_p = round(cor.test(proportion, life_expectancy)["p.value"][[1]],3),
                   proportion_life_expectancy_lowerCI = round(cor.test(proportion, life_expectancy)["conf.int"][[1]][[1]],3),
                   proportion_life_expectancy_higherCI = round(cor.test(proportion, life_expectancy)["conf.int"][[1]][[2]],3),
                   chessmeanage_median_age_cor = round(cor.test(meanage, median_age)["estimate"][[1]][[1]],2),
                   chessmeanage_median_age_p = round(cor.test(meanage, median_age)["p.value"][[1]],3),
                   chessmeanage_median_age_lowerCI = round(cor.test(meanage, median_age)["conf.int"][[1]][[1]],3),
                   chessmeanage_median_age_higherCI = round(cor.test(meanage, median_age)["conf.int"][[1]][[2]],3),
                   chessmeanage_birth_rate_cor = round(cor.test(meanage, birth_rate)["estimate"][[1]][[1]],2),
                   chessmeanage_birth_rate_p = round(cor.test(meanage, birth_rate)["p.value"][[1]],3),
                   chessmeanage_birth_rate_lowerCI = round(cor.test(meanage, birth_rate)["conf.int"][[1]][[1]],3),
                   chessmeanage_birth_rate_higherCI = round(cor.test(meanage, birth_rate)["conf.int"][[1]][[2]],3),
                   chessmeanage_life_expectancy_cor = round(cor.test(meanage, life_expectancy)["estimate"][[1]][[1]],2),
                   chessmeanage_life_expectancy_p = round(cor.test(meanage, life_expectancy)["p.value"][[1]],3),
                   chessmeanage_life_expectancy_lowerCI = round(cor.test(meanage, life_expectancy)["conf.int"][[1]][[1]],3),
                   chessmeanage_life_expectancy_higherCI = round(cor.test(meanage, life_expectancy)["conf.int"][[1]][[2]],3)
                   )
#View(bytrend_GII)
write.csv(bytrend_GII, file="bytrend_GII.csv")