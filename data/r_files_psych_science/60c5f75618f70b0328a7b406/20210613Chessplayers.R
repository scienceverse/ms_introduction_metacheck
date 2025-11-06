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

#View(tabyl(s2$countryname))

s2$count=1

bycountry <- s2 %>%
  group_by(countryname, sex, GGGI, GII) %>%
  dplyr::summarise(count = sum(count))
#View(bycountry)

bycountry_wide <- spread(bycountry, sex, count)
#View(bycountry_wide)

bycountry_wide$countrytotal=rowSums(bycountry_wide[,c(4:5)], na.rm=T)
bycountry_wide$proportion=1-bycountry_wide$M/bycountry_wide$countrytotal

sum(bycountry_wide$countrytotal)#total number of participants
sum(bycountry_wide$M)/sum(bycountry_wide$countrytotal) #proportion of men relative to all
cor.test(bycountry_wide$GGGI,bycountry_wide$GII) #correlation between gender equality measures

describe(bycountry_wide$proportion)
describe(bycountry_wide$GGGI)
describe(bycountry_wide$GII)

reg1=lm(scale(proportion)~scale(GGGI),data=bycountry_wide,weight=countrytotal)
summary(reg1); confint(reg1)

reg2=lm(scale(proportion)~scale(GII),data=bycountry_wide,weight=countrytotal)
summary(reg2); confint(reg2)

#looking at squared effects
bycountry_wide$GGGI_squared=scale(bycountry_wide$GGGI)*scale(bycountry_wide$GGGI)
bycountry_wide$GII_squared=scale(bycountry_wide$GII)*scale(bycountry_wide$GII)

reg1=lm(scale(proportion)~scale(GGGI)+scale(GGGI_squared),data=bycountry_wide,weight=countrytotal)
summary(reg1); confint(reg1)

reg2=lm(scale(proportion)~scale(GII)+scale(GII_squared),data=bycountry_wide,weight=countrytotal)
summary(reg2); confint(reg2)


#limit number of cases in order to run analyses without weights
#bycountry_wide=subset(bycountry_wide,countrytotal>100)
#bycountry_wide=subset(bycountry_wide,countrytotal>1000)

reg1=lm(scale(proportion)~scale(GGGI),data=bycountry_wide)
summary(reg1); confint(reg1)

reg2=lm(scale(proportion)~scale(GII),data=bycountry_wide)
summary(reg2); confint(reg2)

#looking at squared effects
bycountry_wide$GGGI_squared=scale(bycountry_wide$GGGI)*scale(bycountry_wide$GGGI)
bycountry_wide$GII_squared=scale(bycountry_wide$GII)*scale(bycountry_wide$GII)

reg2=lm(scale(proportion)~scale(GGGI)+scale(GGGI_squared),data=bycountry_wide)
summary(reg2); confint(reg2)

reg2=lm(scale(proportion)~scale(GII)+scale(GII_squared),data=bycountry_wide)
summary(reg2); confint(reg2)


#adding country abbrevations for graphs
bycountryabbrev <- s2 %>%
  group_by(countryname,country)%>%
  dplyr::summarise()
#View(bycountryabbrev)
bycountryabbrev=bycountryabbrev[-c(151,152),]
bycountry_wide <- left_join(bycountry_wide, bycountryabbrev, by = "countryname")
#View(bycountry_wide)

plot <- ggplot(data = bycountry_wide, aes(x = GGGI, y = proportion)) +
  theme_classic() +
  geom_line(method = "lm", size = 1, color = "black", alpha = 0.5, stat = "smooth") +
  geom_point(size = 1) +
  geom_text_repel(aes(label = country),
                  size = 3.5,
                  segment.size = 0.5,
                  box.padding = 0.3) +
  theme(legend.position = "right", axis.line = element_line(size = 0.75), axis.ticks = element_line(size = 0.75),
        axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = "GGGI", y = "Proportion of female players")

ggsave('Fig1achess.jpg', plot, width = 7, height = 7)


plot <- ggplot(data = bycountry_wide, aes(x = GII, y = proportion)) +
  theme_classic() +
  geom_line(method = "lm", size = 1, color = "black", alpha = 0.5, stat = "smooth") +
  geom_point(size = 1) +
  geom_text_repel(aes(label = country),
                  size = 3.5,
                  segment.size = 0.5,
                  box.padding = 0.3) +
  theme(legend.position = "right", axis.line = element_line(size = 0.75), axis.ticks = element_line(size = 0.75),
        axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = "GII", y = "Proportion of female players")

ggsave('Fig1bchess.jpg', plot, width = 7, height = 7)

#adding mean age
bycountrybirthday <- s2 %>%
  group_by(countryname) %>%
  dplyr::summarise(meanage = round(2020-mean(birthday, na.rm=T),2))
#View(bycountrybirthday)

bycountry_wide <- left_join(bycountry_wide, bycountrybirthday, by = "countryname")
#View(bycountry_wide)

describe(bycountry_wide$meanage)

reg3=lm(scale(proportion)~scale(meanage),data=bycountry_wide,weight=countrytotal)
summary(reg3)

reg4=lm(scale(GGGI)~scale(meanage),data=bycountry_wide,weight=countrytotal)
summary(reg4)

reg5=lm(scale(GII)~scale(meanage),data=bycountry_wide,weight=countrytotal)
summary(reg5)

#plotting participation by age
#bycountry_wide=subset(bycountry_wide,countrytotal>1000)
cor.test(bycountry_wide$proportion,bycountry_wide$meanage)

plot <- ggplot(data = bycountry_wide, aes(x = meanage, y = proportion)) +
  theme_classic() +
  geom_line(method = "lm", size = 1, color = "black", alpha = 0.5, stat = "smooth") +
  geom_point(size = 1) +
  geom_text_repel(aes(label = country),
                  size = 3.5,
                  segment.size = 0.5,
                  box.padding = 0.3) +
  theme(legend.position = "right", axis.line = element_line(size = 0.75), axis.ticks = element_line(size = 0.75),
        axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
  labs(x = "Mean age", y = "Proportion of female players")

ggsave('participationbyage.jpg', plot, width = 7, height = 7)


#mediation analysis with nonlinear effects
#bycountry_wide$GGGI_squared=scale(bycountry_wide$GGGI)*scale(bycountry_wide$GGGI) #need to recompute after filtering countries with small Ns
#bycountry_wide$GII_squared=scale(bycountry_wide$GII)*scale(bycountry_wide$GII)#need to recompute after filtering countries with small Ns

reg1=lm(scale(proportion)~scale(GGGI)+scale(GGGI_squared),data=bycountry_wide,weight=countrytotal)
summary(reg1)

reg2=lm(scale(meanage)~scale(GGGI)+scale(GGGI_squared),data=bycountry_wide,weight=countrytotal)
summary(reg2)

reg3=lm(scale(proportion)~scale(GGGI)+scale(GGGI_squared)+scale(meanage),data=bycountry_wide,weight=countrytotal)
summary(reg3)

mediation.1 <- mediate(model.m = reg2, model.y = reg3,  
                       treat = "scale(GGGI)", mediator = "scale(meanage)", 
                       boot = FALSE,
                       sims = 5000, data=bycountry_wide, conf.level=.95)
summary (mediation.1)

mediation.1 <- mediate(model.m = reg2, model.y = reg3,  
                       treat = "scale(GGGI_squared)", mediator = "scale(meanage)", 
                       boot = FALSE,
                       sims = 5000, data=bycountry_wide, conf.level=.95)
summary (mediation.1)


reg1=lm(scale(proportion)~scale(GII)+scale(GII_squared),data=bycountry_wide,weight=countrytotal)
summary(reg1)

reg2=lm(scale(meanage)~scale(GII)+scale(GII_squared),data=bycountry_wide,weight=countrytotal)
summary(reg2)

reg3=lm(scale(proportion)~scale(GII)+scale(GII_squared)+scale(meanage),data=bycountry_wide,weight=countrytotal)
summary(reg3)

mediation.1 <- mediate(model.m = reg2, model.y = reg3,  
                       treat = "scale(GII)", mediator = "scale(meanage)", 
                       boot = FALSE,
                       sims = 5000, data=bycountry_wide, conf.level=.95)
summary (mediation.1)

mediation.1 <- mediate(model.m = reg2, model.y = reg3,  
                       treat = "scale(GII_squared)", mediator = "scale(meanage)", 
                       boot = FALSE,
                       sims = 5000, data=bycountry_wide, conf.level=.95)
summary (mediation.1)

#limit number of cases in order to run mediationanalyses without weights
#bycountry_wide=subset(bycountry_wide,countrytotal>100)
#bycountry_wide=subset(bycountry_wide,countrytotal>1000)
#bycountry_wide$GGGI_squared=scale(bycountry_wide$GGGI)*scale(bycountry_wide$GGGI) #need to recompute after filtering countries
#bycountry_wide$GII_squared=scale(bycountry_wide$GII)*scale(bycountry_wide$GII)#need to recompute after filtering countries

#MEDIATION ANALYSES
reg1=lm(scale(proportion)~scale(GGGI)+scale(GGGI_squared),data=bycountry_wide)
summary(reg1)

reg2=lm(scale(meanage)~scale(GGGI)+scale(GGGI_squared),data=bycountry_wide)
summary(reg2)

reg3=lm(scale(proportion)~scale(GGGI)+scale(GGGI_squared)+scale(meanage),data=bycountry_wide)
summary(reg3)

mediation.1 <- mediate(model.m = reg2, model.y = reg3,  
                       treat = "scale(GGGI)", mediator = "scale(meanage)", 
                       boot = FALSE,
                       sims = 5000, data=bycountry_wide, conf.level=.95)
summary (mediation.1)

mediation.1 <- mediate(model.m = reg2, model.y = reg3,  
                       treat = "scale(GGGI_squared)", mediator = "scale(meanage)", 
                       boot = FALSE,
                       sims = 5000, data=bycountry_wide, conf.level=.95)
summary (mediation.1)


reg1=lm(scale(proportion)~scale(GII)+scale(GII_squared),data=bycountry_wide)
summary(reg1)

reg2=lm(scale(meanage)~scale(GII)+scale(GII_squared),data=bycountry_wide)
summary(reg2)

reg3=lm(scale(proportion)~scale(GII)+scale(GII_squared)+scale(meanage),data=bycountry_wide)
summary(reg3)

mediation.1 <- mediate(model.m = reg2, model.y = reg3,  
                       treat = "scale(GII)", mediator = "scale(meanage)", 
                       boot = FALSE,
                       sims = 5000, data=bycountry_wide, conf.level=.95)
summary (mediation.1)

mediation.1 <- mediate(model.m = reg2, model.y = reg3,  
                       treat = "scale(GII_squared)", mediator = "scale(meanage)", 
                       boot = FALSE,
                       sims = 5000, data=bycountry_wide, conf.level=.95)
summary (mediation.1)

#Simonsohn's two line test
#source("http://webstimate.org/twolines/twolines.R")
twolines(proportion~GGGI,data=bycountry_wide)
twolines(proportion~GII,data=bycountry_wide)

#in each country with at least 1,000 players, testing whether proportion of women increased or decreased by age cohort
#results: significantly increased in all countries but 2, China & South Korea
a1=subset(s2,countryname=='Algeria');bycountry <- a1 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg1=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg1); confint(reg1)
a2=subset(s2,countryname=='Angola');bycountry <- a2 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg2=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg2); confint(reg2)
a3=subset(s2,countryname=='Argentina');bycountry <- a3 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg3=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg3); confint(reg3)
a4=subset(s2,countryname=='Armenia');bycountry <- a4 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg4=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg4); confint(reg4)
a5=subset(s2,countryname=='Australia');bycountry <- a5 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg5=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg5); confint(reg5)
a6=subset(s2,countryname=='Austria');bycountry <- a6 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg6=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg6); confint(reg6)
a7=subset(s2,countryname=='Azerbaijan');bycountry <- a7 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg7=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg7); confint(reg7)
a8=subset(s2,countryname=='Bangladesh');bycountry <- a8 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg8=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg8); confint(reg8)
a9=subset(s2,countryname=='Belarus');bycountry <- a9 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg9=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg9); confint(reg9)
a10=subset(s2,countryname=='Belgium');bycountry <- a10 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg10=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg10); confint(reg10)
a11=subset(s2,countryname=='Bolivia');bycountry <- a11 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg11=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg11); confint(reg11)
a12=subset(s2,countryname=='Bosnia & Herzegovina');bycountry <- a12 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg12=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg12); confint(reg12)
a13=subset(s2,countryname=='Botswana');bycountry <- a13 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg13=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg13); confint(reg13)
a14=subset(s2,countryname=='Brazil');bycountry <- a14 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg14=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg14); confint(reg14)
a15=subset(s2,countryname=='Bulgaria');bycountry <- a15 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg15=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg15); confint(reg15)
a16=subset(s2,countryname=='Canada');bycountry <- a16 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg16=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg16); confint(reg16)
a17=subset(s2,countryname=='Chile');bycountry <- a17 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg17=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg17); confint(reg17)
a18=subset(s2,countryname=='China');bycountry <- a18 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg18=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg18); confint(reg18)
a19=subset(s2,countryname=='Colombia');bycountry <- a19 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg19=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg19); confint(reg19)
a20=subset(s2,countryname=='Costa Rica');bycountry <- a20 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg20=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg20); confint(reg20)
a21=subset(s2,countryname=='Croatia');bycountry <- a21 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg21=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg21); confint(reg21)
a22=subset(s2,countryname=='Cuba');bycountry <- a22 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg22=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg22); confint(reg22)
a23=subset(s2,countryname=='Czech Republic');bycountry <- a23 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg23=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg23); confint(reg23)
a24=subset(s2,countryname=='Denmark');bycountry <- a24 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg24=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg24); confint(reg24)
a25=subset(s2,countryname=='Dominican Republic');bycountry <- a25 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg25=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg25); confint(reg25)
a26=subset(s2,countryname=='Ecuador');bycountry <- a26 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg26=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg26); confint(reg26)
a27=subset(s2,countryname=='Egypt');bycountry <- a27 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg27=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg27); confint(reg27)
a28=subset(s2,countryname=='Estonia');bycountry <- a28 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg28=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg28); confint(reg28)
a29=subset(s2,countryname=='Finland');bycountry <- a29 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg29=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg29); confint(reg29)
a30=subset(s2,countryname=='France');bycountry <- a30 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg30=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg30); confint(reg30)
a31=subset(s2,countryname=='Georgia');bycountry <- a31 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg31=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg31); confint(reg31)
a32=subset(s2,countryname=='Germany');bycountry <- a32 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg32=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg32); confint(reg32)
a33=subset(s2,countryname=='Greece');bycountry <- a33 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg33=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg33); confint(reg33)
a34=subset(s2,countryname=='Hungary');bycountry <- a34 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg34=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg34); confint(reg34)
a35=subset(s2,countryname=='Iceland');bycountry <- a35 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg35=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg35); confint(reg35)
a36=subset(s2,countryname=='India');bycountry <- a36 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg36=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg36); confint(reg36)
a37=subset(s2,countryname=='Indonesia');bycountry <- a37 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg37=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg37); confint(reg37)
a38=subset(s2,countryname=='Iran');bycountry <- a38 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg38=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg38); confint(reg38)
a39=subset(s2,countryname=='Iraq');bycountry <- a39 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg39=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg39); confint(reg39)
a40=subset(s2,countryname=='Ireland');bycountry <- a40 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg40=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg40); confint(reg40)
a41=subset(s2,countryname=='Israel');bycountry <- a41 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg41=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg41); confint(reg41)
a42=subset(s2,countryname=='Italy');bycountry <- a42 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg42=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg42); confint(reg42)
a43=subset(s2,countryname=='Jordan');bycountry <- a43 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg43=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg43); confint(reg43)
a44=subset(s2,countryname=='Kazakhstan');bycountry <- a44 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg44=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg44); confint(reg44)
a45=subset(s2,countryname=='Kenya');bycountry <- a45 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg45=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg45); confint(reg45)
a46=subset(s2,countryname=='Kyrgyzstan');bycountry <- a46 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg46=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg46); confint(reg46)
a47=subset(s2,countryname=='Latvia');bycountry <- a47 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg47=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg47); confint(reg47)
a48=subset(s2,countryname=='Lebanon');bycountry <- a48 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg48=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg48); confint(reg48)
a49=subset(s2,countryname=='Libya');bycountry <- a49 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg49=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg49); confint(reg49)
a50=subset(s2,countryname=='Lithuania');bycountry <- a50 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg50=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg50); confint(reg50)
a51=subset(s2,countryname=='Malaysia');bycountry <- a51 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg51=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg51); confint(reg51)
a52=subset(s2,countryname=='Mexico');bycountry <- a52 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg52=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg52); confint(reg52)
a53=subset(s2,countryname=='Mongolia');bycountry <- a53 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg53=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg53); confint(reg53)
a54=subset(s2,countryname=='Morocco');bycountry <- a54 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg54=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg54); confint(reg54)
a55=subset(s2,countryname=='Myanmar');bycountry <- a55 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg55=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg55); confint(reg55)
a56=subset(s2,countryname=='Nepal');bycountry <- a56 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg56=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg56); confint(reg56)
a57=subset(s2,countryname=='Netherlands');bycountry <- a57 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg57=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg57); confint(reg57)
a58=subset(s2,countryname=='New Zealand');bycountry <- a58 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg58=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg58); confint(reg58)
a59=subset(s2,countryname=='Norway');bycountry <- a59 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg59=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg59); confint(reg59)
a60=subset(s2,countryname=='Oman');bycountry <- a60 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg60=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg60); confint(reg60)
a61=subset(s2,countryname=='Panama');bycountry <- a61 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg61=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg61); confint(reg61)
a62=subset(s2,countryname=='Peru');bycountry <- a62 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg62=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg62); confint(reg62)
a63=subset(s2,countryname=='Philippines');bycountry <- a63 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg63=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg63); confint(reg63)
a64=subset(s2,countryname=='Poland');bycountry <- a64 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg64=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg64); confint(reg64)
a65=subset(s2,countryname=='Portugal');bycountry <- a65 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg65=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg65); confint(reg65)
a66=subset(s2,countryname=='Romania');bycountry <- a66 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg66=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg66); confint(reg66)
a67=subset(s2,countryname=='Russia');bycountry <- a67 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg67=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg67); confint(reg67)
a68=subset(s2,countryname=='Saudi Arabia');bycountry <- a68 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg68=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg68); confint(reg68)
a69=subset(s2,countryname=='Serbia');bycountry <- a69 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg69=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg69); confint(reg69)
a70=subset(s2,countryname=='singapore');bycountry <- a70 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg70=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg70); confint(reg70)
a71=subset(s2,countryname=='Slovakia');bycountry <- a71 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg71=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg71); confint(reg71)
a72=subset(s2,countryname=='Slovenia');bycountry <- a72 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg72=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg72); confint(reg72)
a73=subset(s2,countryname=='South Africa');bycountry <- a73 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg73=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg73); confint(reg73)
a74=subset(s2,countryname=='South Korea');bycountry <- a74 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg74=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg74); confint(reg74)
a75=subset(s2,countryname=='Spain');bycountry <- a75 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg75=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg75); confint(reg75)
a76=subset(s2,countryname=='Sri Lanka');bycountry <- a76 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg76=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg76); confint(reg76)
a77=subset(s2,countryname=='Sweden');bycountry <- a77 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg77=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg77); confint(reg77)
a78=subset(s2,countryname=='Switzerland');bycountry <- a78 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg78=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg78); confint(reg78)
a79=subset(s2,countryname=='Thailand');bycountry <- a79 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg79=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg79); confint(reg79)
a80=subset(s2,countryname=='Tunisia');bycountry <- a80 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg80=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg80); confint(reg80)
a81=subset(s2,countryname=='Turkey');bycountry <- a81 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg81=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg81); confint(reg81)
a82=subset(s2,countryname=='Uganda');bycountry <- a82 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg82=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg82); confint(reg82)
a83=subset(s2,countryname=='UK');bycountry <- a83 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg83=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg83); confint(reg83)
a84=subset(s2,countryname=='Ukraine');bycountry <- a84 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg84=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg84); confint(reg84)
a85=subset(s2,countryname=='United Arab Emirates');bycountry <- a85 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg85=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg85); confint(reg85)
a86=subset(s2,countryname=='United States of America');bycountry <- a86 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg86=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg86); confint(reg86)
a87=subset(s2,countryname=='Uruguay');bycountry <- a87 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg87=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg87); confint(reg87)
a88=subset(s2,countryname=='Uzbekistan');bycountry <- a88 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg88=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg88); confint(reg88)
a89=subset(s2,countryname=='Venezuela');bycountry <- a89 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg89=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg89); confint(reg89)
a90=subset(s2,countryname=='Vietnam');bycountry <- a90 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg90=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg90); confint(reg90)
a91=subset(s2,countryname=='Zambia');bycountry <- a91 %>% group_by(birthday, sex) %>%dplyr::summarise(count = sum(count)); byyear_wide <- spread(bycountry, sex, count); byyear_wide$yeartotal=rowSums(byyear_wide[,c(2:3)], na.rm=T);byyear_wide$proportion=(1-byyear_wide$M/byyear_wide$yeartotal)*100;reg91=lm(proportion~birthday,data=byyear_wide,weight=yeartotal);summary(reg91); confint(reg91)

country1 = round(c(summary(reg1)$coefficients[c(2,4,6,8)],confint(reg1)[2],confint(reg1)[4]),3)
country2 = round(c(summary(reg2)$coefficients[c(2,4,6,8)],confint(reg2)[2],confint(reg2)[4]),3)
country3 = round(c(summary(reg3)$coefficients[c(2,4,6,8)],confint(reg3)[2],confint(reg3)[4]),3)
country4 = round(c(summary(reg4)$coefficients[c(2,4,6,8)],confint(reg4)[2],confint(reg4)[4]),3)
country5 = round(c(summary(reg5)$coefficients[c(2,4,6,8)],confint(reg5)[2],confint(reg5)[4]),3)
country6 = round(c(summary(reg6)$coefficients[c(2,4,6,8)],confint(reg6)[2],confint(reg6)[4]),3)
country7 = round(c(summary(reg7)$coefficients[c(2,4,6,8)],confint(reg7)[2],confint(reg7)[4]),3)
country8 = round(c(summary(reg8)$coefficients[c(2,4,6,8)],confint(reg8)[2],confint(reg8)[4]),3)
country9 = round(c(summary(reg9)$coefficients[c(2,4,6,8)],confint(reg9)[2],confint(reg9)[4]),3)
country10 = round(c(summary(reg10)$coefficients[c(2,4,6,8)],confint(reg10)[2],confint(reg10)[4]),3)
country11 = round(c(summary(reg11)$coefficients[c(2,4,6,8)],confint(reg11)[2],confint(reg11)[4]),3)
country12 = round(c(summary(reg12)$coefficients[c(2,4,6,8)],confint(reg12)[2],confint(reg12)[4]),3)
country13 = round(c(summary(reg13)$coefficients[c(2,4,6,8)],confint(reg13)[2],confint(reg13)[4]),3)
country14 = round(c(summary(reg14)$coefficients[c(2,4,6,8)],confint(reg14)[2],confint(reg14)[4]),3)
country15 = round(c(summary(reg15)$coefficients[c(2,4,6,8)],confint(reg15)[2],confint(reg15)[4]),3)
country16 = round(c(summary(reg16)$coefficients[c(2,4,6,8)],confint(reg16)[2],confint(reg16)[4]),3)
country17 = round(c(summary(reg17)$coefficients[c(2,4,6,8)],confint(reg17)[2],confint(reg17)[4]),3)
country18 = round(c(summary(reg18)$coefficients[c(2,4,6,8)],confint(reg18)[2],confint(reg18)[4]),3)
country19 = round(c(summary(reg19)$coefficients[c(2,4,6,8)],confint(reg19)[2],confint(reg19)[4]),3)
country20 = round(c(summary(reg20)$coefficients[c(2,4,6,8)],confint(reg20)[2],confint(reg20)[4]),3)
country21 = round(c(summary(reg21)$coefficients[c(2,4,6,8)],confint(reg21)[2],confint(reg21)[4]),3)
country22 = round(c(summary(reg22)$coefficients[c(2,4,6,8)],confint(reg22)[2],confint(reg22)[4]),3)
country23 = round(c(summary(reg23)$coefficients[c(2,4,6,8)],confint(reg23)[2],confint(reg23)[4]),3)
country24 = round(c(summary(reg24)$coefficients[c(2,4,6,8)],confint(reg24)[2],confint(reg24)[4]),3)
country25 = round(c(summary(reg25)$coefficients[c(2,4,6,8)],confint(reg25)[2],confint(reg25)[4]),3)
country26 = round(c(summary(reg26)$coefficients[c(2,4,6,8)],confint(reg26)[2],confint(reg26)[4]),3)
country27 = round(c(summary(reg27)$coefficients[c(2,4,6,8)],confint(reg27)[2],confint(reg27)[4]),3)
country28 = round(c(summary(reg28)$coefficients[c(2,4,6,8)],confint(reg28)[2],confint(reg28)[4]),3)
country29 = round(c(summary(reg29)$coefficients[c(2,4,6,8)],confint(reg29)[2],confint(reg29)[4]),3)
country30 = round(c(summary(reg30)$coefficients[c(2,4,6,8)],confint(reg30)[2],confint(reg30)[4]),3)
country31 = round(c(summary(reg31)$coefficients[c(2,4,6,8)],confint(reg31)[2],confint(reg31)[4]),3)
country32 = round(c(summary(reg32)$coefficients[c(2,4,6,8)],confint(reg32)[2],confint(reg32)[4]),3)
country33 = round(c(summary(reg33)$coefficients[c(2,4,6,8)],confint(reg33)[2],confint(reg33)[4]),3)
country34 = round(c(summary(reg34)$coefficients[c(2,4,6,8)],confint(reg34)[2],confint(reg34)[4]),3)
country35 = round(c(summary(reg35)$coefficients[c(2,4,6,8)],confint(reg35)[2],confint(reg35)[4]),3)
country36 = round(c(summary(reg36)$coefficients[c(2,4,6,8)],confint(reg36)[2],confint(reg36)[4]),3)
country37 = round(c(summary(reg37)$coefficients[c(2,4,6,8)],confint(reg37)[2],confint(reg37)[4]),3)
country38 = round(c(summary(reg38)$coefficients[c(2,4,6,8)],confint(reg38)[2],confint(reg38)[4]),3)
country39 = round(c(summary(reg39)$coefficients[c(2,4,6,8)],confint(reg39)[2],confint(reg39)[4]),3)
country40 = round(c(summary(reg40)$coefficients[c(2,4,6,8)],confint(reg40)[2],confint(reg40)[4]),3)
country41 = round(c(summary(reg41)$coefficients[c(2,4,6,8)],confint(reg41)[2],confint(reg41)[4]),3)
country42 = round(c(summary(reg42)$coefficients[c(2,4,6,8)],confint(reg42)[2],confint(reg42)[4]),3)
country43 = round(c(summary(reg43)$coefficients[c(2,4,6,8)],confint(reg43)[2],confint(reg43)[4]),3)
country44 = round(c(summary(reg44)$coefficients[c(2,4,6,8)],confint(reg44)[2],confint(reg44)[4]),3)
country45 = round(c(summary(reg45)$coefficients[c(2,4,6,8)],confint(reg45)[2],confint(reg45)[4]),3)
country46 = round(c(summary(reg46)$coefficients[c(2,4,6,8)],confint(reg46)[2],confint(reg46)[4]),3)
country47 = round(c(summary(reg47)$coefficients[c(2,4,6,8)],confint(reg47)[2],confint(reg47)[4]),3)
country48 = round(c(summary(reg48)$coefficients[c(2,4,6,8)],confint(reg48)[2],confint(reg48)[4]),3)
country49 = round(c(summary(reg49)$coefficients[c(2,4,6,8)],confint(reg49)[2],confint(reg49)[4]),3)
country50 = round(c(summary(reg50)$coefficients[c(2,4,6,8)],confint(reg50)[2],confint(reg50)[4]),3)
country51 = round(c(summary(reg51)$coefficients[c(2,4,6,8)],confint(reg51)[2],confint(reg51)[4]),3)
country52 = round(c(summary(reg52)$coefficients[c(2,4,6,8)],confint(reg52)[2],confint(reg52)[4]),3)
country53 = round(c(summary(reg53)$coefficients[c(2,4,6,8)],confint(reg53)[2],confint(reg53)[4]),3)
country54 = round(c(summary(reg54)$coefficients[c(2,4,6,8)],confint(reg54)[2],confint(reg54)[4]),3)
country55 = round(c(summary(reg55)$coefficients[c(2,4,6,8)],confint(reg55)[2],confint(reg55)[4]),3)
country56 = round(c(summary(reg56)$coefficients[c(2,4,6,8)],confint(reg56)[2],confint(reg56)[4]),3)
country57 = round(c(summary(reg57)$coefficients[c(2,4,6,8)],confint(reg57)[2],confint(reg57)[4]),3)
country58 = round(c(summary(reg58)$coefficients[c(2,4,6,8)],confint(reg58)[2],confint(reg58)[4]),3)
country59 = round(c(summary(reg59)$coefficients[c(2,4,6,8)],confint(reg59)[2],confint(reg59)[4]),3)
country60 = round(c(summary(reg60)$coefficients[c(2,4,6,8)],confint(reg60)[2],confint(reg60)[4]),3)
country61 = round(c(summary(reg61)$coefficients[c(2,4,6,8)],confint(reg61)[2],confint(reg61)[4]),3)
country62 = round(c(summary(reg62)$coefficients[c(2,4,6,8)],confint(reg62)[2],confint(reg62)[4]),3)
country63 = round(c(summary(reg63)$coefficients[c(2,4,6,8)],confint(reg63)[2],confint(reg63)[4]),3)
country64 = round(c(summary(reg64)$coefficients[c(2,4,6,8)],confint(reg64)[2],confint(reg64)[4]),3)
country65 = round(c(summary(reg65)$coefficients[c(2,4,6,8)],confint(reg65)[2],confint(reg65)[4]),3)
country66 = round(c(summary(reg66)$coefficients[c(2,4,6,8)],confint(reg66)[2],confint(reg66)[4]),3)
country67 = round(c(summary(reg67)$coefficients[c(2,4,6,8)],confint(reg67)[2],confint(reg67)[4]),3)
country68 = round(c(summary(reg68)$coefficients[c(2,4,6,8)],confint(reg68)[2],confint(reg68)[4]),3)
country69 = round(c(summary(reg69)$coefficients[c(2,4,6,8)],confint(reg69)[2],confint(reg69)[4]),3)
country70 = round(c(summary(reg70)$coefficients[c(2,4,6,8)],confint(reg70)[2],confint(reg70)[4]),3)
country71 = round(c(summary(reg71)$coefficients[c(2,4,6,8)],confint(reg71)[2],confint(reg71)[4]),3)
country72 = round(c(summary(reg72)$coefficients[c(2,4,6,8)],confint(reg72)[2],confint(reg72)[4]),3)
country73 = round(c(summary(reg73)$coefficients[c(2,4,6,8)],confint(reg73)[2],confint(reg73)[4]),3)
country74 = round(c(summary(reg74)$coefficients[c(2,4,6,8)],confint(reg74)[2],confint(reg74)[4]),3)
country75 = round(c(summary(reg75)$coefficients[c(2,4,6,8)],confint(reg75)[2],confint(reg75)[4]),3)
country76 = round(c(summary(reg76)$coefficients[c(2,4,6,8)],confint(reg76)[2],confint(reg76)[4]),3)
country77 = round(c(summary(reg77)$coefficients[c(2,4,6,8)],confint(reg77)[2],confint(reg77)[4]),3)
country78 = round(c(summary(reg78)$coefficients[c(2,4,6,8)],confint(reg78)[2],confint(reg78)[4]),3)
country79 = round(c(summary(reg79)$coefficients[c(2,4,6,8)],confint(reg79)[2],confint(reg79)[4]),3)
country80 = round(c(summary(reg80)$coefficients[c(2,4,6,8)],confint(reg80)[2],confint(reg80)[4]),3)
country81 = round(c(summary(reg81)$coefficients[c(2,4,6,8)],confint(reg81)[2],confint(reg81)[4]),3)
country82 = round(c(summary(reg82)$coefficients[c(2,4,6,8)],confint(reg82)[2],confint(reg82)[4]),3)
country83 = round(c(summary(reg83)$coefficients[c(2,4,6,8)],confint(reg83)[2],confint(reg83)[4]),3)
country84 = round(c(summary(reg84)$coefficients[c(2,4,6,8)],confint(reg84)[2],confint(reg84)[4]),3)
country85 = round(c(summary(reg85)$coefficients[c(2,4,6,8)],confint(reg85)[2],confint(reg85)[4]),3)
country86 = round(c(summary(reg86)$coefficients[c(2,4,6,8)],confint(reg86)[2],confint(reg86)[4]),3)
country87 = round(c(summary(reg87)$coefficients[c(2,4,6,8)],confint(reg87)[2],confint(reg87)[4]),3)
country88 = round(c(summary(reg88)$coefficients[c(2,4,6,8)],confint(reg88)[2],confint(reg88)[4]),3)
country89 = round(c(summary(reg89)$coefficients[c(2,4,6,8)],confint(reg89)[2],confint(reg89)[4]),3)
country90 = round(c(summary(reg90)$coefficients[c(2,4,6,8)],confint(reg90)[2],confint(reg90)[4]),3)
country91 = round(c(summary(reg91)$coefficients[c(2,4,6,8)],confint(reg91)[2],confint(reg91)[4]),3)

tableA2=rbind(country1,country2,country3,country4,country5,country6,country7,country8,country9,country10,country11,country12,country13,country14,country15,country16,country17,country18,country19,country20,country21,country22,country23,country24,country25,country26,country27,country28,country29,country30,country31,country32,country33,country34,country35,country36,country37,country38,country39,country40,country41,country42,country43,country44,country45,country46,country47,country48,country49,country50,country51,country52,country53,country54,country55,country56,country57,country58,country59,country60,country61,country62,country63,country64,country65,country66,country67,country68,country69,country70,country71,country72,country73,country74,country75,country76,country77,country78,country79,country80,country81,country82,country83,country84,country85,country86,country87,country88,country89,country90,country91)
View(tableA2)