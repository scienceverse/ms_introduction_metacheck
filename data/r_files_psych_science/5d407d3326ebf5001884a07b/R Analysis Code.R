require(nlme)
require(psy)
require(psych)
require(data.table)
require(sjstats)
options(scipen=999)
memory.limit(size=12000)


###Data Import, Renaming, and Cleaning
Gallup=read.csv(...)
#Demographics
setnames(Gallup,'YEAR_CALENDAR','Year')
setnames(Gallup,'WP5','Country')
setnames(Gallup,'WP1220','Age');Gallup$Age[which(Gallup$Age>98)]=NA;Gallup$Age[which(Gallup$Age<20)]=NA#omit <20
setnames(Gallup,'WP1219','Gender');Gallup$Gender[which(Gallup$Gender==1)]=-1;Gallup$Gender[which(Gallup$Gender==2)]=1#female is 1, male, -1, so that intercept is grand mean
setnames(Gallup,'WP1223','Marital');Gallup$Marital[which(Gallup$Marital==6)]=NA;Gallup$Marital[which(Gallup$Marital==7)]=NA;Gallup$Marital[which(Gallup$Marital==1)]=0;Gallup$Marital[which(Gallup$Marital>2&Gallup$Marital<6)]=0;Gallup$Marital[which(Gallup$Marital==2)]=1;Gallup$Marital[which(Gallup$Marital==8)]=1#married (2) and domestic (8) are coded as 1, all else, 0 (aside from 6 and 7)
setnames(Gallup,'EMP_2010','Employ');Gallup$Employ[which(Gallup$Employ<4)]=1;Gallup$Employ[which(Gallup$Employ==4)]=0;Gallup$Employ[which(Gallup$Employ==6)]=NA;Gallup$Employ[which(Gallup$Employ==5)]=1
#SWB
setnames(Gallup,'wp16','LS');Gallup$LS[which(Gallup$LS>10)]=NA
setnames(Gallup,'WP63','Smile');Gallup$Smile[which(Gallup$Smile>2)]=NA;Gallup$Smile[which(Gallup$Smile==2)]=0
setnames(Gallup,'WP67','Enjoyment');Gallup$Enjoyment[which(Gallup$Enjoyment>2)]=NA;Gallup$Enjoyment[which(Gallup$Enjoyment==2)]=0
setnames(Gallup,'WP6878','Happiness');Gallup$Happiness[which(Gallup$Happiness>2)]=NA;Gallup$Happiness[which(Gallup$Happiness==2)]=0
setnames(Gallup,'WP69','Worry');Gallup$Worry[which(Gallup$Worry>2)]=NA;Gallup$Worry[which(Gallup$Worry==2)]=0
setnames(Gallup,'WP70','Sadness');Gallup$Sadness[which(Gallup$Sadness>2)]=NA;Gallup$Sadness[which(Gallup$Sadness==2)]=0
setnames(Gallup,'WP71','Stress');Gallup$Stress[which(Gallup$Stress>2)]=NA;Gallup$Stress[which(Gallup$Stress==2)]=0
setnames(Gallup,'WP73','Depression');Gallup$Depression[which(Gallup$Depression>2)]=NA;Gallup$Depression[which(Gallup$Depression==2)]=0
setnames(Gallup,'WP1418','Fear');Gallup$Fear[which(Gallup$Fear>2)]=NA;Gallup$Fear[which(Gallup$Fear==2)]=0
#Prosocial Indicators
setnames(Gallup,'WP108','Donate');Gallup$Donate[which(Gallup$Donate==2)]=0;Gallup$Donate[which(Gallup$Donate>2)]=NA
setnames(Gallup,'WP109','Vol');Gallup$Vol[which(Gallup$Vol==2)]=0;Gallup$Vol[which(Gallup$Vol>2)]=NA
setnames(Gallup,'WP110','Help');Gallup$Help[which(Gallup$Help==2)]=0;Gallup$Help[which(Gallup$Help>2)]=NA
#Meaning Indicators
setnames(Gallup,'WP64','Proud');Gallup$Proud[which(Gallup$Proud>2)]=NA;Gallup$Proud[which(Gallup$Proud==2)]=0
setnames(Gallup,'WP61','Respect');Gallup$Respect[which(Gallup$Respect>2)]=NA;Gallup$Respect[which(Gallup$Respect==2)]=0
setnames(Gallup,'WP21','Meaning');Gallup$Meaning[which(Gallup$Meaning>2)]=NA;Gallup$Meaning[which(Gallup$Meaning==2)]=0
#Remove Unused Variables from Data Frame
Gallup["WP10200"]=NULL;Gallup["wp10202"]=NULL;Gallup["wp10232"]=NULL;Gallup["wp12"]=NULL;Gallup["wp1230"]=NULL;Gallup["wp14448"]=NULL;Gallup["wp27"]=NULL;Gallup["wp4656"]=NULL;Gallup["wp52"]=NULL;Gallup["wp53"]=NULL;Gallup["wp80"]=NULL;Gallup["wp9030"]=NULL
Gallup["WP10233"]=NULL;Gallup["WP10236"]=NULL;Gallup["WP10238"]=NULL;Gallup['WP111']=NULL#Voice;
Gallup["WP10266"]=NULL;Gallup["WP11357"]=NULL;Gallup["WP11358"]=NULL;Gallup['REG_GLOBAL']=NULL#Region
Gallup["WP119"]=NULL;Gallup["WP12"]=NULL;Gallup["WP1230"]=NULL;Gallup["WP134"]=NULL;Gallup["WP4656"]=NULL
Gallup["wp17"]=NULL;Gallup["wp18"]=NULL;Gallup["WP27"]=NULL;Gallup["WP30"]=NULL;Gallup["wp3117"]=NULL
Gallup["WP59"]=NULL;Gallup["WP62"]=NULL;Gallup["WP65"]=NULL;Gallup["wp72"]=NULL;Gallup["WP9039"]=NULL
Gallup["WP74"]=NULL;Gallup["WP76"]=NULL;Gallup["WP79"]=NULL;Gallup["WP80"]=NULL;Gallup["WP14"]=NULL
Gallup["WP14448"]=NULL;Gallup["INCOME_1"]=NULL;Gallup["income_2"]=NULL;Gallup["income_2.1"]=NULL;Gallup["INCOME_7"]=NULL;Gallup["FIELD_DATE"]=NULL
#Create Indices
Gallup$PA=rowMeans(data.frame(Gallup["Enjoyment"],Gallup["Happiness"],Gallup["Smile"]),na.rm=TRUE);Gallup$PA=Gallup$PA*10
Gallup$Neg=rowMeans(data.frame(Gallup["Worry"],Gallup["Sadness"],Gallup["Stress"],Gallup["Fear"],Gallup["Depression"]),na.rm=TRUE);Gallup$Neg=Gallup$Neg*10
Gallup$Prosocial=NA;Gallup['Prosocial']=rowMeans(data.frame(Gallup$Donate,Gallup$Vol,Gallup$Help),na.rm=T);Gallup$Prosocial[which(Gallup$Prosocial>0)]=1
#Make dummies for year
Gallup["y05"]=as.numeric(Gallup$Year==2005);Gallup$y05[which(Gallup$Year==2016)]=-1;Gallup["y06"]=as.numeric(Gallup$Year==2006);Gallup$y06[which(Gallup$Year==2016)]=-1;Gallup["y07"]=as.numeric(Gallup$Year==2007);Gallup$y07[which(Gallup$Year==2016)]=-1;Gallup["y08"]=as.numeric(Gallup$Year==2008);Gallup$y08[which(Gallup$Year==2016)]=-1;Gallup["y09"]=as.numeric(Gallup$Year==2009);Gallup$y09[which(Gallup$Year==2016)]=-1;Gallup["y10"]=as.numeric(Gallup$Year==2010);Gallup$y10[which(Gallup$Year==2016)]=-1;Gallup["y11"]=as.numeric(Gallup$Year==2011);Gallup$y11[which(Gallup$Year==2016)]=-1;Gallup["y12"]=as.numeric(Gallup$Year==2012);Gallup$y12[which(Gallup$Year==2016)]=-1;Gallup["y13"]=as.numeric(Gallup$Year==2013);Gallup$y13[which(Gallup$Year==2016)]=-1;Gallup["y14"]=as.numeric(Gallup$Year==2014);Gallup$y14[which(Gallup$Year==2016)]=-1;Gallup["y15"]=as.numeric(Gallup$Year==2015);Gallup$y15[which(Gallup$Year==2016)]=-1#y16 is reference
#create weights
Gallup=scale_weights(Gallup,cluster.id=Country,pweight=wgt)

#Code Nations Into Regions
Gallup$Region=NA
#Anglo=1
Gallup$Region[which(Gallup$Country==1)]=1#United States
Gallup$Region[which(Gallup$Country==12)]=1#United Kingdom
Gallup$Region[which(Gallup$Country==46)]=1#Canada
Gallup$Region[which(Gallup$Country==47)]=1#Australia
Gallup$Region[which(Gallup$Country==55)]=1#New Zealand
Gallup$Region[which(Gallup$Country==132)]=1#Ireland
#Latin Europe=2
Gallup$Region[which(Gallup$Country==13)]=2#France
Gallup$Region[which(Gallup$Country==17)]=2#Spain
Gallup$Region[which(Gallup$Country==18)]=2#Italy
Gallup$Region[which(Gallup$Country==38)]=2#Israel
Gallup$Region[which(Gallup$Country==75)]=2#Moldova
Gallup$Region[which(Gallup$Country==148)]=2#Malta
Gallup$Region[which(Gallup$Country==166)]=2#Portugal
#Germanic=3
Gallup$Region[which(Gallup$Country==14)]=3#Germany
Gallup$Region[which(Gallup$Country==15)]=3#Netherlands
Gallup$Region[which(Gallup$Country==16)]=3#Belgium
Gallup$Region[which(Gallup$Country==89)]=3#Austria
Gallup$Region[which(Gallup$Country==144)]=3#Luxembourg
Gallup$Region[which(Gallup$Country==184)]=3#Switzerland
#Nordic=4
Gallup$Region[which(Gallup$Country==23)]=4#Sweden
Gallup$Region[which(Gallup$Country==25)]=4#Denmark
Gallup$Region[which(Gallup$Country==119)]=4#Estonia
Gallup$Region[which(Gallup$Country==121)]=4#Finland
Gallup$Region[which(Gallup$Country==130)]=4#Iceland
Gallup$Region[which(Gallup$Country==138)]=4#Latvia
Gallup$Region[which(Gallup$Country==143)]=4#Lithuania
Gallup$Region[which(Gallup$Country==160)]=4#Norway
#Eastern Europe=5
Gallup$Region[which(Gallup$Country==19)]=5#Poland
Gallup$Region[which(Gallup$Country==20)]=5#Hungary
Gallup$Region[which(Gallup$Country==21)]=5#Czech Republic
Gallup$Region[which(Gallup$Country==22)]=5#Romania
Gallup$Region[which(Gallup$Country==24)]=5#Greece
Gallup$Region[which(Gallup$Country==71)]=5#Belarus
Gallup$Region[which(Gallup$Country==72)]=5#Georgia
Gallup$Region[which(Gallup$Country==73)]=5#Kazakhstan
Gallup$Region[which(Gallup$Country==74)]=5#Kyrgyzstan
Gallup$Region[which(Gallup$Country==76)]=5#Russia
Gallup$Region[which(Gallup$Country==77)]=5#Ukraine
Gallup$Region[which(Gallup$Country==83)]=5#Albania
Gallup$Region[which(Gallup$Country==88)]=5#Armenia
Gallup$Region[which(Gallup$Country==97)]=5#Bosnia & Herzegovina
Gallup$Region[which(Gallup$Country==99)]=5#Bulgaria
Gallup$Region[which(Gallup$Country==109)]=5#Croatia
Gallup$Region[which(Gallup$Country==111)]=5#Cyprus
Gallup$Region[which(Gallup$Country==145)]=5#Macedonia
Gallup$Region[which(Gallup$Country==154)]=5#Montenegro
Gallup$Region[which(Gallup$Country==173)]=5#Serbia
Gallup$Region[which(Gallup$Country==175)]=5#Slovakia
Gallup$Region[which(Gallup$Country==176)]=5#Slovenia
Gallup$Region[which(Gallup$Country==198)]=5#Kosovo
Gallup$Region[which(Gallup$Country==202)]=5#Northern Cyprus
#Confucian Asia=6
Gallup$Region[which(Gallup$Country==27)]=6#Hong Kong
Gallup$Region[which(Gallup$Country==28)]=6#Singapore
Gallup$Region[which(Gallup$Country==29)]=6#Japan
Gallup$Region[which(Gallup$Country==30)]=6#China
Gallup$Region[which(Gallup$Country==50)]=6#Vietnam
Gallup$Region[which(Gallup$Country==68)]=6#South Korea
Gallup$Region[which(Gallup$Country==69)]=6#Taiwan
Gallup$Region[which(Gallup$Country==153)]=6#Mongolia
#Latin America=7
Gallup$Region[which(Gallup$Country==32)]=7#Venezuela
Gallup$Region[which(Gallup$Country==33)]=7#Brazil
Gallup$Region[which(Gallup$Country==34)]=7#Mexico
Gallup$Region[which(Gallup$Country==82)]=7#Costa Rica
Gallup$Region[which(Gallup$Country==87)]=7#Argentina
Gallup$Region[which(Gallup$Country==94)]=7#Belize
Gallup$Region[which(Gallup$Country==96)]=7#Bolivia
Gallup$Region[which(Gallup$Country==104)]=7#Chile
Gallup$Region[which(Gallup$Country==105)]=7#Colombia
Gallup$Region[which(Gallup$Country==110)]=7#Cuba
Gallup$Region[which(Gallup$Country==114)]=7#Dominican Republic
Gallup$Region[which(Gallup$Country==115)]=7#Ecuador
Gallup$Region[which(Gallup$Country==116)]=7#El Salvador
Gallup$Region[which(Gallup$Country==124)]=7#Guatemala
Gallup$Region[which(Gallup$Country==127)]=7#Guyana
Gallup$Region[which(Gallup$Country==128)]=7#Haiti
Gallup$Region[which(Gallup$Country==129)]=7#Honduras
Gallup$Region[which(Gallup$Country==135)]=7#Jamaica
Gallup$Region[which(Gallup$Country==158)]=7#Nicaragua
Gallup$Region[which(Gallup$Country==163)]=7#Panama
Gallup$Region[which(Gallup$Country==164)]=7#Paraguay
Gallup$Region[which(Gallup$Country==165)]=7#Peru
Gallup$Region[which(Gallup$Country==167)]=7#Puerto Rico
Gallup$Region[which(Gallup$Country==182)]=7#Suriname
Gallup$Region[which(Gallup$Country==189)]=7#Trinidad & TObago
Gallup$Region[which(Gallup$Country==194)]=7#Uruguay
#Southern Asia=8
Gallup$Region[which(Gallup$Country==9)]=8#Pakistan
Gallup$Region[which(Gallup$Country==10)]=8#Indonesia
Gallup$Region[which(Gallup$Country==11)]=8#Bangladesh
Gallup$Region[which(Gallup$Country==31)]=8#India
Gallup$Region[which(Gallup$Country==48)]=8#Philippines
Gallup$Region[which(Gallup$Country==49)]=8#Sri Lanka
Gallup$Region[which(Gallup$Country==51)]=8#Thailand
Gallup$Region[which(Gallup$Country==52)]=8#Cambodia
Gallup$Region[which(Gallup$Country==53)]=8#Laos
Gallup$Region[which(Gallup$Country==54)]=8#Myanmar
Gallup$Region[which(Gallup$Country==70)]=8#Afghanistan
Gallup$Region[which(Gallup$Country==95)]=8#Bhutan
Gallup$Region[which(Gallup$Country==146)]=8#Malaysia
Gallup$Region[which(Gallup$Country==157)]=8#Nepal
Gallup$Region[which(Gallup$Country==185)]=8#Tajikistan
#Arab=9
Gallup$Region[which(Gallup$Country==2)]=9#Egypt
Gallup$Region[which(Gallup$Country==3)]=9#Morocco
Gallup$Region[which(Gallup$Country==4)]=9#Lebanon
Gallup$Region[which(Gallup$Country==5)]=9#Saudi Arabia
Gallup$Region[which(Gallup$Country==6)]=9#Jordan
Gallup$Region[which(Gallup$Country==7)]=9#Syria
Gallup$Region[which(Gallup$Country==8)]=9#Turkey
Gallup$Region[which(Gallup$Country==26)]=9#Iran
Gallup$Region[which(Gallup$Country==39)]=9#Palestinian Territories
Gallup$Region[which(Gallup$Country==84)]=9#Algeria
Gallup$Region[which(Gallup$Country==90)]=9#Azerbaijan
Gallup$Region[which(Gallup$Country==92)]=9#Bahrain
Gallup$Region[which(Gallup$Country==131)]=9#Iraq
Gallup$Region[which(Gallup$Country==137)]=9#Kuwait
Gallup$Region[which(Gallup$Country==141)]=9#Libya
Gallup$Region[which(Gallup$Country==161)]=9#Oman
Gallup$Region[which(Gallup$Country==168)]=9#Qatar
Gallup$Region[which(Gallup$Country==190)]=9#Tunisia
Gallup$Region[which(Gallup$Country==191)]=9#Turkmenistan
Gallup$Region[which(Gallup$Country==193)]=9#United Arab Emirates
Gallup$Region[which(Gallup$Country==195)]=9#Uzbekistan
Gallup$Region[which(Gallup$Country==197)]=9#Yemen
Gallup$Region[which(Gallup$Country==203)]=9#Nagorno-Karabakh Republic
#Sub-Sahara Africa=10
Gallup$Region[which(Gallup$Country==35)]=10#Nigeria
Gallup$Region[which(Gallup$Country==36)]=10#Kenya
Gallup$Region[which(Gallup$Country==37)]=10#Tanzania
Gallup$Region[which(Gallup$Country==40)]=10#Ghana
Gallup$Region[which(Gallup$Country==41)]=10#Uganda
Gallup$Region[which(Gallup$Country==42)]=10#Benin
Gallup$Region[which(Gallup$Country==43)]=10#Madagascar
Gallup$Region[which(Gallup$Country==44)]=10#Malawi
Gallup$Region[which(Gallup$Country==45)]=10#South Africa
Gallup$Region[which(Gallup$Country==56)]=10#Angola
Gallup$Region[which(Gallup$Country==57)]=10#Botswana
Gallup$Region[which(Gallup$Country==60)]=10#Ethiopia
Gallup$Region[which(Gallup$Country==61)]=10#Mali
Gallup$Region[which(Gallup$Country==62)]=10#Mauritania
Gallup$Region[which(Gallup$Country==63)]=10#Mozambique
Gallup$Region[which(Gallup$Country==64)]=10#Niger
Gallup$Region[which(Gallup$Country==65)]=10#Rwanda
Gallup$Region[which(Gallup$Country==66)]=10#Senegal
Gallup$Region[which(Gallup$Country==67)]=10#Zambia
Gallup$Region[which(Gallup$Country==78)]=10#Burkina Faso
Gallup$Region[which(Gallup$Country==79)]=10#Cameroon
Gallup$Region[which(Gallup$Country==80)]=10#Sierra Leone
Gallup$Region[which(Gallup$Country==81)]=10#Zimbabwe
Gallup$Region[which(Gallup$Country==100)]=10#Burundi
Gallup$Region[which(Gallup$Country==102)]=10#Central African Republic
Gallup$Region[which(Gallup$Country==103)]=10#Chad
Gallup$Region[which(Gallup$Country==106)]=10#Comoros
Gallup$Region[which(Gallup$Country==107)]=10#Democratic Republic of Congo
Gallup$Region[which(Gallup$Country==108)]=10#Congo
Gallup$Region[which(Gallup$Country==112)]=10#Djibouti
Gallup$Region[which(Gallup$Country==122)]=10#Gabon
Gallup$Region[which(Gallup$Country==125)]=10#Guinea
Gallup$Region[which(Gallup$Country==134)]=10#Cote d'Ivoire
Gallup$Region[which(Gallup$Country==139)]=10#Lesotho
Gallup$Region[which(Gallup$Country==140)]=10#Liberia
Gallup$Region[which(Gallup$Country==150)]=10#Mauritius
Gallup$Region[which(Gallup$Country==155)]=10#Namibia
Gallup$Region[which(Gallup$Country==178)]=10#Somalia
Gallup$Region[which(Gallup$Country==181)]=10#Sudan
Gallup$Region[which(Gallup$Country==183)]=10#Swaziland
Gallup$Region[which(Gallup$Country==187)]=10#Togo
Gallup$Region[which(Gallup$Country==199)]=10#Somaliland Region
Gallup$Region[which(Gallup$Country==205)]=10#South Sudan


###Preliminary Statistics
#Reliabilities of Composites
cronbach(cbind(Gallup["Enjoyment"],Gallup["Happiness"],Gallup["Smile"]))
cronbach(cbind(Gallup["Worry"],Gallup["Sadness"],Gallup["Stress"],Gallup["Fear"],Gallup["Depression"]))
#Descriptives
#take nation-level means out of correlations
for (i in 1:205){
  Gallup$LS[which(Gallup$Country==i)]=Gallup$LS[which(Gallup$Country==i)]-mean(Gallup$LS[which(Gallup$Country==i)],na.rm=TRUE)
  Gallup$PA[which(Gallup$Country==i)]=Gallup$PA[which(Gallup$Country==i)]-mean(Gallup$PA[which(Gallup$Country==i)],na.rm=TRUE)
  Gallup$Neg[which(Gallup$Country==i)]=Gallup$Neg[which(Gallup$Country==i)]-mean(Gallup$Neg[which(Gallup$Country==i)],na.rm=TRUE)
  Gallup$Age[which(Gallup$Country==i)]=Gallup$Age[which(Gallup$Country==i)]-mean(Gallup$Age[which(Gallup$Country==i)],na.rm=TRUE)
  Gallup$Marital[which(Gallup$Country==i)]=Gallup$Marital[which(Gallup$Country==i)]-mean(Gallup$Marital[which(Gallup$Country==i)],na.rm=TRUE)
  Gallup$Employ[which(Gallup$Country==i)]=Gallup$Employ[which(Gallup$Country==i)]-mean(Gallup$Employ[which(Gallup$Country==i)],na.rm=TRUE)
  Gallup$Prosocial[which(Gallup$Country==i)]=Gallup$Prosocial[which(Gallup$Country==i)]-mean(Gallup$Prosocial[which(Gallup$Country==i)],na.rm=TRUE)
  Gallup$Meaning[which(Gallup$Country==i)]=Gallup$Meaning[which(Gallup$Country==i)]-mean(Gallup$Meaning[which(Gallup$Country==i)],na.rm=TRUE)
}
corr.test(Gallup[c("LS","PA","Neg","Age","Marital","Employ",'Prosocial','Meaning')])
#for one dichotomous measure (made from an underlying continuous variable)
biserial(x=Gallup$LS,y=Gallup$Meaning)
biserial(x=Gallup[c("LS","PA","Neg","Age")],y=Gallup[c('Prosocial',"Meaning")])
#for two dichotomous measures
phi(t=Gallup[c("Marital","Employ")])
phi(t=Gallup[c("Marital","Prosocial")])
phi(t=Gallup[c("Marital","Meaning")])
phi(t=Gallup[c("Employ","Prosocial")])
phi(t=Gallup[c("Employ","Meaning")])
phi(t=Gallup[c("Prosocial","Meaning")])


####Main Analyses
###Age
#Fit Curvilinear Models
Gallup['Age_C']=Gallup$Age-mean(Gallup$Age,na.rm=T)
Gallup['Age2']=Gallup$Age_C^2
LSage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=LS~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(LSage2)
#get region-level effects
LSage2intercepts=LSage2$coefficients$fixed[1]+LSage2$coefficients$random$Region[,1]
LSage2coefs1=LSage2$coefficients$fixed[2]+LSage2$coefficients$random$Region[,2]
LSage2coefs2=LSage2$coefficients$fixed[3]+LSage2$coefficients$random$Region[,3]
#get country-level effects
#coef(LSage2)
#Compare two groups (Latin Europe,Eastern Europe, Latin America vs. rest)
Gallup['Groups']=0
Gallup$Groups[which(Gallup$Region==2|Gallup$Region==5|Gallup$Region==7)]=1
Gallup['GroupsXAge']=Gallup$Groups*Gallup$Age_C
Gallup['GroupsXAge2']=Gallup$Groups*Gallup$Age2
LSage2Groups=lme(data=Gallup,weights=~I(1/svywght_a),fixed=LS~Age_C+Age2+Groups+GroupsXAge+GroupsXAge2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
y=LSage2$coefficients$fixed[1]+LSage2$coefficients$fixed[2]*x+LSage2$coefficients$fixed[3]*x2
y[6001]-y[1]#20 vs. 80-year olds
y[3001]-y[1]#20 vs. 50-year olds
#Regions
for (i in 1:10){
  y=LSage2intercepts[i]+LSage2coefs1[i]*x+LSage2coefs2[i]*x2
  print(i)#region
  print(y[6001]-y[1])#20 vs. 80-year olds
  print(y[3001]-y[1])#20 vs. 50-year olds
}

#PA
PAage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=PA~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(PAage2)
#get region-level effects
PAage2intercepts=PAage2$coefficients$fixed[1]+PAage2$coefficients$random$Region[,1]
PAage2coefs1=PAage2$coefficients$fixed[2]+PAage2$coefficients$random$Region[,2]
PAage2coefs2=PAage2$coefficients$fixed[3]+PAage2$coefficients$random$Region[,3]
#get country-level effects
#coef(PAage2)
#Compare two groups (Anglo and Confucian Asia vs. rest)
Gallup['Groups']=0
Gallup$Groups[which(Gallup$Region==1|Gallup$Region==3|Gallup$Region==6|Gallup$Region==7)]=1
Gallup['GroupsXAge']=Gallup$Groups*Gallup$Age_C
Gallup['GroupsXAge2']=Gallup$Groups*Gallup$Age2
PAage2Groups=lme(data=Gallup,weights=~I(1/svywght_a),fixed=PA~Age_C+Age2+Groups+GroupsXAge+GroupsXAge2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
y=PAage2$coefficients$fixed[1]+PAage2$coefficients$fixed[2]*x+PAage2$coefficients$fixed[3]*x2
y[6001]-y[1]#20 vs. 80-year olds
y[3001]-y[1]#20 vs. 50-year olds
#Regions
for (i in 1:10){
  y=PAage2intercepts[i]+PAage2coefs1[i]*x+PAage2coefs2[i]*x2
  print(i)#region
  print(y[6001]-y[1])#20 vs. 80-year olds
  print(y[3001]-y[1])#20 vs. 50-year olds
}

#NA
NAage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=Neg~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(NAage2)
#get region-level effects
NAage2intercepts=NAage2$coefficients$fixed[1]+NAage2$coefficients$random$Region[,1]
NAage2coefs1=NAage2$coefficients$fixed[2]+NAage2$coefficients$random$Region[,2]
NAage2coefs2=NAage2$coefficients$fixed[3]+NAage2$coefficients$random$Region[,3]
#get country-level effects
#coef(NAage2)
#Compare two groups (Anglo,Germanic,Nordic,Confucian vs. rest)
Gallup['Groups']=0
Gallup$Groups[which(Gallup$Region==1|Gallup$Region==3|Gallup$Region==4|Gallup$Region==6)]=1
Gallup['GroupsXAge']=Gallup$Groups*Gallup$Age_C
Gallup['GroupsXAge2']=Gallup$Groups*Gallup$Age2
NAage2Groups=lme(data=Gallup,weights=~I(1/svywght_a),fixed=Neg~Age_C+Age2+Groups+GroupsXAge+GroupsXAge2+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
y=NAage2$coefficients$fixed[1]+NAage2$coefficients$fixed[2]*x+NAage2$coefficients$fixed[3]*x2
y[6001]-y[1]#20 vs. 80-year olds
y[3001]-y[1]#20 vs. 50-year olds
#Regions
for (i in 1:10){
  y=NAage2intercepts[i]+NAage2coefs1[i]*x+NAage2coefs2[i]*x2
  print(i)#region
  print(y[6001]-y[1])#20 vs. 80-year olds
  print(y[3001]-y[1])#20 vs. 50-year olds
}


###Predictor 1: Marriage
#create terms
Gallup['Age_C']=Gallup$Age-mean(Gallup$Age,na.rm=T)
Gallup['Age2']=Gallup$Age_C^2
Gallup['MaritalXAge']=Gallup$Marital*Gallup$Age_C
Gallup['MaritalXAge2']=Gallup$Marital*Gallup$Age2
#LS
LSmaritalXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=LS~Marital+Age_C+Age2+MaritalXAge+MaritalXAge2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Marital+Age_C+Age2+MaritalXAge+MaritalXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(LSmaritalXage2)
#get region-level effects
LSmaritalintercepts=LSmaritalXage2$coefficients$fixed[1]+LSmaritalXage2$coefficients$random$Region[,1]#random effects are expressed as deviations from the fixed effect. So to get region-level slope, add the fixed effect
LSmaritalcoefs1=LSmaritalXage2$coefficients$fixed[2]+LSmaritalXage2$coefficients$random$Region[,2]
LSmaritalcoefs2=LSmaritalXage2$coefficients$fixed[3]+LSmaritalXage2$coefficients$random$Region[,3]
LSmaritalcoefs3=LSmaritalXage2$coefficients$fixed[4]+LSmaritalXage2$coefficients$random$Region[,4]
LSmaritalcoefs4=LSmaritalXage2$coefficients$fixed[5]+LSmaritalXage2$coefficients$random$Region[,5]
LSmaritalcoefs5=LSmaritalXage2$coefficients$fixed[6]+LSmaritalXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=LSmaritalXage2$coefficients$fixed[1]+LSmaritalXage2$coefficients$fixed[3]*x+LSmaritalXage2$coefficients$fixed[4]*x2#when marital=0, there are no higher-order coefficients in the equation
m1=LSmaritalXage2$coefficients$fixed[1]+LSmaritalXage2$coefficients$fixed[2]+LSmaritalXage2$coefficients$fixed[3]*x+LSmaritalXage2$coefficients$fixed[4]*x2+LSmaritalXage2$coefficients$fixed[5]*x+LSmaritalXage2$coefficients$fixed[6]*x2#when marital=1, include all 5 terms in the equation
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=LSmaritalintercepts[i]+LSmaritalcoefs2[i]*x+LSmaritalcoefs3[i]*x2
  m1=LSmaritalintercepts[i]+LSmaritalcoefs1[i]+LSmaritalcoefs2[i]*x+LSmaritalcoefs3[i]*x2+LSmaritalcoefs4[i]*x+LSmaritalcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

#PA
PAmaritalXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=PA~Marital+Age_C+Age2+MaritalXAge+MaritalXAge2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Marital+Age_C+Age2+MaritalXAge+MaritalXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(PAmaritalXage2)
#get region-level effects
PAmaritalintercepts=PAmaritalXage2$coefficients$fixed[1]+PAmaritalXage2$coefficients$random$Region[,1]
PAmaritalcoefs1=PAmaritalXage2$coefficients$fixed[2]+PAmaritalXage2$coefficients$random$Region[,2]
PAmaritalcoefs2=PAmaritalXage2$coefficients$fixed[3]+PAmaritalXage2$coefficients$random$Region[,3]
PAmaritalcoefs3=PAmaritalXage2$coefficients$fixed[4]+PAmaritalXage2$coefficients$random$Region[,4]
PAmaritalcoefs4=PAmaritalXage2$coefficients$fixed[5]+PAmaritalXage2$coefficients$random$Region[,5]
PAmaritalcoefs5=PAmaritalXage2$coefficients$fixed[6]+PAmaritalXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=PAmaritalXage2$coefficients$fixed[1]+PAmaritalXage2$coefficients$fixed[3]*x+PAmaritalXage2$coefficients$fixed[4]*x2
m1=PAmaritalXage2$coefficients$fixed[1]+PAmaritalXage2$coefficients$fixed[2]+PAmaritalXage2$coefficients$fixed[3]*x+PAmaritalXage2$coefficients$fixed[4]*x2+PAmaritalXage2$coefficients$fixed[5]*x+PAmaritalXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=PAmaritalintercepts[i]+PAmaritalcoefs2[i]*x+PAmaritalcoefs3[i]*x2
  m1=PAmaritalintercepts[i]+PAmaritalcoefs1[i]+PAmaritalcoefs2[i]*x+PAmaritalcoefs3[i]*x2+PAmaritalcoefs4[i]*x+PAmaritalcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}
#NA
NAmaritalXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=Neg~Marital+Age_C+Age2+MaritalXAge+MaritalXAge2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Marital+Age_C+Age2+MaritalXAge+MaritalXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(NAmaritalXage2)
#get region-level effects
NAmaritalintercepts=NAmaritalXage2$coefficients$fixed[1]+NAmaritalXage2$coefficients$random$Region[,1]
NAmaritalcoefs1=NAmaritalXage2$coefficients$fixed[2]+NAmaritalXage2$coefficients$random$Region[,2]
NAmaritalcoefs2=NAmaritalXage2$coefficients$fixed[3]+NAmaritalXage2$coefficients$random$Region[,3]
NAmaritalcoefs3=NAmaritalXage2$coefficients$fixed[4]+NAmaritalXage2$coefficients$random$Region[,4]
NAmaritalcoefs4=NAmaritalXage2$coefficients$fixed[5]+NAmaritalXage2$coefficients$random$Region[,5]
NAmaritalcoefs5=NAmaritalXage2$coefficients$fixed[6]+NAmaritalXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=NAmaritalXage2$coefficients$fixed[1]+NAmaritalXage2$coefficients$fixed[3]*x+NAmaritalXage2$coefficients$fixed[4]*x2
m1=NAmaritalXage2$coefficients$fixed[1]+NAmaritalXage2$coefficients$fixed[2]+NAmaritalXage2$coefficients$fixed[3]*x+NAmaritalXage2$coefficients$fixed[4]*x2+NAmaritalXage2$coefficients$fixed[5]*x+NAmaritalXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=NAmaritalintercepts[i]+NAmaritalcoefs2[i]*x+NAmaritalcoefs3[i]*x2
  m1=NAmaritalintercepts[i]+NAmaritalcoefs1[i]+NAmaritalcoefs2[i]*x+NAmaritalcoefs3[i]*x2+NAmaritalcoefs4[i]*x+NAmaritalcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

####Predictor 2: Employment
#create terms
Gallup['Age_C']=Gallup$Age-mean(Gallup$Age,na.rm=T)
Gallup['Age2']=Gallup$Age_C^2
Gallup['EmployXAge']=Gallup$Employ*Gallup$Age_C
Gallup['EmployXAge2']=Gallup$Employ*Gallup$Age2
#LS
LSemployXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=LS~Employ+Age_C+Age2+EmployXAge+EmployXAge2+Gender,random=~Employ+Age_C+Age2+EmployXAge+EmployXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(LSemployXage2)
#get region-level effects
LSemployintercepts=LSemployXage2$coefficients$fixed[1]+LSemployXage2$coefficients$random$Region[,1]
LSemploycoefs1=LSemployXage2$coefficients$fixed[2]+LSemployXage2$coefficients$random$Region[,2]
LSemploycoefs2=LSemployXage2$coefficients$fixed[3]+LSemployXage2$coefficients$random$Region[,3]
LSemploycoefs3=LSemployXage2$coefficients$fixed[4]+LSemployXage2$coefficients$random$Region[,4]
LSemploycoefs4=LSemployXage2$coefficients$fixed[5]+LSemployXage2$coefficients$random$Region[,5]
LSemploycoefs5=LSemployXage2$coefficients$fixed[6]+LSemployXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=LSemployXage2$coefficients$fixed[1]+LSemployXage2$coefficients$fixed[3]*x+LSemployXage2$coefficients$fixed[4]*x2
m1=LSemployXage2$coefficients$fixed[1]+LSemployXage2$coefficients$fixed[2]+LSemployXage2$coefficients$fixed[3]*x+LSemployXage2$coefficients$fixed[4]*x2+LSemployXage2$coefficients$fixed[5]*x+LSemployXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=LSemployintercepts[i]+LSemploycoefs2[i]*x+LSemploycoefs3[i]*x2
  m1=LSemployintercepts[i]+LSemploycoefs1[i]+LSemploycoefs2[i]*x+LSemploycoefs3[i]*x2+LSemploycoefs4[i]*x+LSemploycoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

#PA
PAemployXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=PA~Employ+Age_C+Age2+EmployXAge+EmployXAge2+Gender,random=~Employ+Age_C+Age2+EmployXAge+EmployXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(PAemployXage2)
#get region-level effects
PAemployintercepts=PAemployXage2$coefficients$fixed[1]+PAemployXage2$coefficients$random$Region[,1]
PAemploycoefs1=PAemployXage2$coefficients$fixed[2]+PAemployXage2$coefficients$random$Region[,2]
PAemploycoefs2=PAemployXage2$coefficients$fixed[3]+PAemployXage2$coefficients$random$Region[,3]
PAemploycoefs3=PAemployXage2$coefficients$fixed[4]+PAemployXage2$coefficients$random$Region[,4]
PAemploycoefs4=PAemployXage2$coefficients$fixed[5]+PAemployXage2$coefficients$random$Region[,5]
PAemploycoefs5=PAemployXage2$coefficients$fixed[6]+PAemployXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=PAemployXage2$coefficients$fixed[1]+PAemployXage2$coefficients$fixed[3]*x+PAemployXage2$coefficients$fixed[4]*x2
m1=PAemployXage2$coefficients$fixed[1]+PAemployXage2$coefficients$fixed[2]+PAemployXage2$coefficients$fixed[3]*x+PAemployXage2$coefficients$fixed[4]*x2+PAemployXage2$coefficients$fixed[5]*x+PAemployXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=PAemployintercepts[i]+PAemploycoefs2[i]*x+PAemploycoefs3[i]*x2#when employ=0, there are no higher-order coefficients in the equation
  m1=PAemployintercepts[i]+PAemploycoefs1[i]+PAemploycoefs2[i]*x+PAemploycoefs3[i]*x2+PAemploycoefs4[i]*x+PAemploycoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

#NA
NAemployXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=Neg~Employ+Age_C+Age2+EmployXAge+EmployXAge2+Gender,random=~Employ+Age_C+Age2+EmployXAge+EmployXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(NAemployXage2)
#get region-level effects
NAemployintercepts=NAemployXage2$coefficients$fixed[1]+NAemployXage2$coefficients$random$Region[,1]
NAemploycoefs1=NAemployXage2$coefficients$fixed[2]+NAemployXage2$coefficients$random$Region[,2]
NAemploycoefs2=NAemployXage2$coefficients$fixed[3]+NAemployXage2$coefficients$random$Region[,3]
NAemploycoefs3=NAemployXage2$coefficients$fixed[4]+NAemployXage2$coefficients$random$Region[,4]
NAemploycoefs4=NAemployXage2$coefficients$fixed[5]+NAemployXage2$coefficients$random$Region[,5]
NAemploycoefs5=NAemployXage2$coefficients$fixed[6]+NAemployXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=NAemployXage2$coefficients$fixed[1]+NAemployXage2$coefficients$fixed[3]*x+NAemployXage2$coefficients$fixed[4]*x2
m1=NAemployXage2$coefficients$fixed[1]+NAemployXage2$coefficients$fixed[2]+NAemployXage2$coefficients$fixed[3]*x+NAemployXage2$coefficients$fixed[4]*x2+NAemployXage2$coefficients$fixed[5]*x+NAemployXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=NAemployintercepts[i]+NAemploycoefs2[i]*x+NAemploycoefs3[i]*x2
  m1=NAemployintercepts[i]+NAemploycoefs1[i]+NAemploycoefs2[i]*x+NAemploycoefs3[i]*x2+NAemploycoefs4[i]*x+NAemploycoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

###Predictor 3: Prosocial
#create terms
Gallup['Age_C']=Gallup$Age-mean(Gallup$Age,na.rm=T)
Gallup['Age2']=Gallup$Age_C^2
Gallup['ProsocialXAge']=Gallup$Prosocial*Gallup$Age_C
Gallup['ProsocialXAge2']=Gallup$Prosocial*Gallup$Age2
#LS
LSprosocialXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=LS~Prosocial+Age_C+Age2+ProsocialXAge+ProsocialXAge2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Prosocial+Age_C+Age2+ProsocialXAge+ProsocialXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(LSprosocialXage2)
#get region-level effects
LSprosocialintercepts=LSprosocialXage2$coefficients$fixed[1]+LSprosocialXage2$coefficients$random$Region[,1]
LSprosocialcoefs1=LSprosocialXage2$coefficients$fixed[2]+LSprosocialXage2$coefficients$random$Region[,2]
LSprosocialcoefs2=LSprosocialXage2$coefficients$fixed[3]+LSprosocialXage2$coefficients$random$Region[,3]
LSprosocialcoefs3=LSprosocialXage2$coefficients$fixed[4]+LSprosocialXage2$coefficients$random$Region[,4]
LSprosocialcoefs4=LSprosocialXage2$coefficients$fixed[5]+LSprosocialXage2$coefficients$random$Region[,5]
LSprosocialcoefs5=LSprosocialXage2$coefficients$fixed[6]+LSprosocialXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=LSprosocialXage2$coefficients$fixed[1]+LSprosocialXage2$coefficients$fixed[3]*x+LSprosocialXage2$coefficients$fixed[4]*x2
m1=LSprosocialXage2$coefficients$fixed[1]+LSprosocialXage2$coefficients$fixed[2]+LSprosocialXage2$coefficients$fixed[3]*x+LSprosocialXage2$coefficients$fixed[4]*x2+LSprosocialXage2$coefficients$fixed[5]*x+LSprosocialXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=LSprosocialintercepts[i]+LSprosocialcoefs2[i]*x+LSprosocialcoefs3[i]*x2
  m1=LSprosocialintercepts[i]+LSprosocialcoefs1[i]+LSprosocialcoefs2[i]*x+LSprosocialcoefs3[i]*x2+LSprosocialcoefs4[i]*x+LSprosocialcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

#PA
PAprosocialXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=PA~Prosocial+Age_C+Age2+ProsocialXAge+ProsocialXAge2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Prosocial+Age_C+Age2+ProsocialXAge+ProsocialXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(PAprosocialXage2)
#get region-level effects
PAprosocialintercepts=PAprosocialXage2$coefficients$fixed[1]+PAprosocialXage2$coefficients$random$Region[,1]
PAprosocialcoefs1=PAprosocialXage2$coefficients$fixed[2]+PAprosocialXage2$coefficients$random$Region[,2]
PAprosocialcoefs2=PAprosocialXage2$coefficients$fixed[3]+PAprosocialXage2$coefficients$random$Region[,3]
PAprosocialcoefs3=PAprosocialXage2$coefficients$fixed[4]+PAprosocialXage2$coefficients$random$Region[,4]
PAprosocialcoefs4=PAprosocialXage2$coefficients$fixed[5]+PAprosocialXage2$coefficients$random$Region[,5]
PAprosocialcoefs5=PAprosocialXage2$coefficients$fixed[6]+PAprosocialXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=PAprosocialXage2$coefficients$fixed[1]+PAprosocialXage2$coefficients$fixed[3]*x+PAprosocialXage2$coefficients$fixed[4]*x2
m1=PAprosocialXage2$coefficients$fixed[1]+PAprosocialXage2$coefficients$fixed[2]+PAprosocialXage2$coefficients$fixed[3]*x+PAprosocialXage2$coefficients$fixed[4]*x2+PAprosocialXage2$coefficients$fixed[5]*x+PAprosocialXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=PAprosocialintercepts[i]+PAprosocialcoefs2[i]*x+PAprosocialcoefs3[i]*x2
  m1=PAprosocialintercepts[i]+PAprosocialcoefs1[i]+PAprosocialcoefs2[i]*x+PAprosocialcoefs3[i]*x2+PAprosocialcoefs4[i]*x+PAprosocialcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

#NA
NAprosocialXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=Neg~Prosocial+Age_C+Age2+ProsocialXAge+ProsocialXAge2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Prosocial+Age_C+Age2+ProsocialXAge+ProsocialXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(NAprosocialXage2)
#get region-level effects
NAprosocialintercepts=NAprosocialXage2$coefficients$fixed[1]+NAprosocialXage2$coefficients$random$Region[,1]
NAprosocialcoefs1=NAprosocialXage2$coefficients$fixed[2]+NAprosocialXage2$coefficients$random$Region[,2]
NAprosocialcoefs2=NAprosocialXage2$coefficients$fixed[3]+NAprosocialXage2$coefficients$random$Region[,3]
NAprosocialcoefs3=NAprosocialXage2$coefficients$fixed[4]+NAprosocialXage2$coefficients$random$Region[,4]
NAprosocialcoefs4=NAprosocialXage2$coefficients$fixed[5]+NAprosocialXage2$coefficients$random$Region[,5]
NAprosocialcoefs5=NAprosocialXage2$coefficients$fixed[6]+NAprosocialXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=NAprosocialXage2$coefficients$fixed[1]+NAprosocialXage2$coefficients$fixed[3]*x+NAprosocialXage2$coefficients$fixed[4]*x2
m1=NAprosocialXage2$coefficients$fixed[1]+NAprosocialXage2$coefficients$fixed[2]+NAprosocialXage2$coefficients$fixed[3]*x+NAprosocialXage2$coefficients$fixed[4]*x2+NAprosocialXage2$coefficients$fixed[5]*x+NAprosocialXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=NAprosocialintercepts[i]+NAprosocialcoefs2[i]*x+NAprosocialcoefs3[i]*x2
  m1=NAprosocialintercepts[i]+NAprosocialcoefs1[i]+NAprosocialcoefs2[i]*x+NAprosocialcoefs3[i]*x2+NAprosocialcoefs4[i]*x+NAprosocialcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

###Predictor 4: Meaning
#create terms
Gallup['Age_C']=Gallup$Age-mean(Gallup$Age,na.rm=T)
Gallup['Age2']=Gallup$Age_C^2
Gallup['MeaningXAge']=Gallup$Meaning*Gallup$Age_C
Gallup['MeaningXAge2']=Gallup$Meaning*Gallup$Age2
#LS
LSmeaningXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=LS~Meaning+Age_C+Age2+MeaningXAge+MeaningXAge2+Gender,random=~Meaning+Age_C+Age2+MeaningXAge+MeaningXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(LSmeaningXage2)
#get region-level effects
LSmeaningintercepts=LSmeaningXage2$coefficients$fixed[1]+LSmeaningXage2$coefficients$random$Region[,1]
LSmeaningcoefs1=LSmeaningXage2$coefficients$fixed[2]+LSmeaningXage2$coefficients$random$Region[,2]
LSmeaningcoefs2=LSmeaningXage2$coefficients$fixed[3]+LSmeaningXage2$coefficients$random$Region[,3]
LSmeaningcoefs3=LSmeaningXage2$coefficients$fixed[4]+LSmeaningXage2$coefficients$random$Region[,4]
LSmeaningcoefs4=LSmeaningXage2$coefficients$fixed[5]+LSmeaningXage2$coefficients$random$Region[,5]
LSmeaningcoefs5=LSmeaningXage2$coefficients$fixed[6]+LSmeaningXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=LSmeaningXage2$coefficients$fixed[1]+LSmeaningXage2$coefficients$fixed[3]*x+LSmeaningXage2$coefficients$fixed[4]*x2
m1=LSmeaningXage2$coefficients$fixed[1]+LSmeaningXage2$coefficients$fixed[2]+LSmeaningXage2$coefficients$fixed[3]*x+LSmeaningXage2$coefficients$fixed[4]*x2+LSmeaningXage2$coefficients$fixed[5]*x+LSmeaningXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=LSmeaningintercepts[i]+LSmeaningcoefs2[i]*x+LSmeaningcoefs3[i]*x2
  m1=LSmeaningintercepts[i]+LSmeaningcoefs1[i]+LSmeaningcoefs2[i]*x+LSmeaningcoefs3[i]*x2+LSmeaningcoefs4[i]*x+LSmeaningcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

#PA
PAmeaningXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=PA~Meaning+Age_C+Age2+MeaningXAge+MeaningXAge2+Gender,random=~Meaning+Age_C+Age2+MeaningXAge+MeaningXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#curvilinear interaction not significant; refit without
PAmeaningXage=lme(data=Gallup,weights=~I(1/svywght_a),fixed=PA~Meaning+Age_C+Age2+MeaningXAge+Gender,random=~Meaning+Age_C+Age2+MeaningXAge|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(PAmeaningXage2)
#get region-level effects
PAmeaningintercepts=PAmeaningXage2$coefficients$fixed[1]+PAmeaningXage2$coefficients$random$Region[,1]
PAmeaningcoefs1=PAmeaningXage2$coefficients$fixed[2]+PAmeaningXage2$coefficients$random$Region[,2]
PAmeaningcoefs2=PAmeaningXage2$coefficients$fixed[3]+PAmeaningXage2$coefficients$random$Region[,3]
PAmeaningcoefs3=PAmeaningXage2$coefficients$fixed[4]+PAmeaningXage2$coefficients$random$Region[,4]
PAmeaningcoefs4=PAmeaningXage2$coefficients$fixed[5]+PAmeaningXage2$coefficients$random$Region[,5]
PAmeaningcoefs5=PAmeaningXage2$coefficients$fixed[6]+PAmeaningXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=PAmeaningXage2$coefficients$fixed[1]+PAmeaningXage2$coefficients$fixed[3]*x+PAmeaningXage2$coefficients$fixed[4]*x2
m1=PAmeaningXage2$coefficients$fixed[1]+PAmeaningXage2$coefficients$fixed[2]+PAmeaningXage2$coefficients$fixed[3]*x+PAmeaningXage2$coefficients$fixed[4]*x2+PAmeaningXage2$coefficients$fixed[5]*x+PAmeaningXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=PAmeaningintercepts[i]+PAmeaningcoefs2[i]*x+PAmeaningcoefs3[i]*x2
  m1=PAmeaningintercepts[i]+PAmeaningcoefs1[i]+PAmeaningcoefs2[i]*x+PAmeaningcoefs3[i]*x2+PAmeaningcoefs4[i]*x+PAmeaningcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

#NA
NAmeaningXage2=lme(data=Gallup,weights=~I(1/svywght_a),fixed=Neg~Meaning+Age_C+Age2+MeaningXAge+MeaningXAge2+Gender,random=~Meaning+Age_C+Age2+MeaningXAge+MeaningXAge2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
#get fixed effects
summary(NAmeaningXage2)
#get region-level effects
NAmeaningintercepts=NAmeaningXage2$coefficients$fixed[1]+NAmeaningXage2$coefficients$random$Region[,1]
NAmeaningcoefs1=NAmeaningXage2$coefficients$fixed[2]+NAmeaningXage2$coefficients$random$Region[,2]
NAmeaningcoefs2=NAmeaningXage2$coefficients$fixed[3]+NAmeaningXage2$coefficients$random$Region[,3]
NAmeaningcoefs3=NAmeaningXage2$coefficients$fixed[4]+NAmeaningXage2$coefficients$random$Region[,4]
NAmeaningcoefs4=NAmeaningXage2$coefficients$fixed[5]+NAmeaningXage2$coefficients$random$Region[,5]
NAmeaningcoefs5=NAmeaningXage2$coefficients$fixed[6]+NAmeaningXage2$coefficients$random$Region[,6]
#Get predicted effects between ages 20 vs. 50 and 80-year-olds
x=seq(20-mean(Gallup$Age,na.rm=T),80-mean(Gallup$Age,na.rm=T),.01)
x2=x^2
#Global
m0=NAmeaningXage2$coefficients$fixed[1]+NAmeaningXage2$coefficients$fixed[3]*x+NAmeaningXage2$coefficients$fixed[4]*x2
m1=NAmeaningXage2$coefficients$fixed[1]+NAmeaningXage2$coefficients$fixed[2]+NAmeaningXage2$coefficients$fixed[3]*x+NAmeaningXage2$coefficients$fixed[4]*x2+NAmeaningXage2$coefficients$fixed[5]*x+NAmeaningXage2$coefficients$fixed[6]*x2
diff=m1-m0
diff[1];diff[3001];diff[6001]#get predicted effects at ages 20,50,and 80
#Regions
for (i in 1:10){
  m0=NAmeaningintercepts[i]+NAmeaningcoefs2[i]*x+NAmeaningcoefs3[i]*x2
  m1=NAmeaningintercepts[i]+NAmeaningcoefs1[i]+NAmeaningcoefs2[i]*x+NAmeaningcoefs3[i]*x2+NAmeaningcoefs4[i]*x+NAmeaningcoefs5[i]*x2
  diff=m1-m0
  print(i)#region
  print(diff[1]);print(diff[3001]);print(diff[6001])
}

###Sub-Analyses
#Measurement Validation Study
data=read.csv("C:/Users/atjeb_000/Desktop/Projects (Old Backup)/Mike Morrison/Qualtrics Data/Gallup Validation Data.csv")
data=data[-1,]#remove item descriptions
data[,13:152]=apply(data[,13:152],2,FUN=function(X) as.numeric(as.character(X)))#make numeric;warning means blanks become NAs
data["GProsocial"]=NA;data['GProsocial']=rowMeans(data.frame(data$G_4,data$G_5,data$G_6),na.rm=T);data$GProsocial[which(data$GProsocial>0)]=1
corr.test(data["SRA"],data["GProsocial"])#self-report Altrusim scale
corr.test(data["Meaning"],data["G_1"])#meaning of CIT
biserial(data["SRA"],data["GProsocial"])#self-report Altrusim scale
biserial(data["Meaning"],data["G_1"])#meaning of CIT

#Measurement Invariance
require(semTools)
require(sirt)
InvData=Gallup[,c('Region','Donate','Vol','Help',"Enjoyment","Happiness","Smile","Worry","Sadness","Stress","Fear","Depression")]
modelcfa1='F=~Donate+Vol+Help
F ~~ 1*F'
modelcfa2='F=~Enjoyment+Happiness+Smile
F ~~ 1*F'
modelcfa3='F=~Worry+Sadness+Stress+Fear+Depression
F ~~ 1*F'
#chi-sq not meaningful bc of N
model1=measurementInvariance(model=modelcfa1, std.lv=T,data=InvData, group="Region",strict=F,fit.measures="default",method="satorra.bentler.2001")
model2=measurementInvariance(model=modelcfa2, std.lv=T,data=InvData, group="Region",strict=F,fit.measures="default",method="satorra.bentler.2001")
model3=measurementInvariance(model=modelcfa3, std.lv=T,data=InvData, group="Region",strict=F,fit.measures="default",method="satorra.bentler.2001")


#Examining Acquiescence Response style
AcqData=read.csv("C:/Users/atjeb_000/Desktop/Andrew's Work/Mike Morrison/Acq Data.csv")
AcqData["Country"]=AcqData["WP5"];AcqData["WP5"]=NULL
AcqData["LikeDo"]=AcqData["WP10233"];AcqData["WP10233"]=NULL;AcqData$LikeDo[which(AcqData$LikeDo>2)]=NA;AcqData$LikeDo[which(AcqData$LikeDo==2)]=0
AcqData["Helped"]=AcqData["WP110"];AcqData["WP110"]=NULL;AcqData$Helped[which(AcqData$Helped>2)]=NA;AcqData$Helped[which(AcqData$Helped==2)]=0
AcqData["Goals"]=AcqData["WP11358"];AcqData["WP11358"]=NULL;AcqData$Goals[which(AcqData$Goals>2)]=NA;AcqData$Goals[which(AcqData$Goals==2)]=0
AcqData["Business"]=AcqData["WP11607"];AcqData["WP11607"]=NULL;AcqData$Business[which(AcqData$Business>2)]=NA;AcqData$Business[which(AcqData$Business==2)]=0
AcqData["Children"]=AcqData["WP129"];AcqData["WP129"]=NULL;AcqData$Children[which(AcqData$Children>2)]=NA;AcqData$Children[which(AcqData$Children==2)]=0
AcqData["Count"]=AcqData["WP27"];AcqData["WP27"]=NULL;AcqData$Count[which(AcqData$Count>2)]=NA;AcqData$Count[which(AcqData$Count==2)]=0
#Calculate Correlations Among Items 
cs=cor(AcqData,use="pairwise.complete.obs")
mean(c(abs(cs[2:7,1]),cs[3:7,2],abs(cs[4:7,3]),cs[5:7,4],abs(cs[6:7,5]),cs[7,6]),na.rm=T)
#Calculate Acquiescence by Individual First and Then Aggregate to Region
AcqData$pos=NA;AcqData$neg=NA;AcqData$tot=NA;AcqData$posprop=NA;AcqData$negprop=NA;AcqData$balance=NA
AcqData$pos=rowSums(AcqData[,3:8],na.rm=T)#number of 'agrees'
AcqData$tot=rowSums(!is.na(AcqData[,3:8]))#total number of valid cases
AcqData$neg=AcqData$tot-AcqData$pos#number of 'disagrees'
AcqData$posprop=AcqData$pos/AcqData$tot#proportion of responses per subject that are 'agrees'
AcqData$negprop=AcqData$neg/AcqData$tot#proportion of responses per subject that are 'disagrees'
acq=rep(NA,10)#acquiescence per region
for (i in 1:10){
  acq[i]=mean(AcqData$posprop[which(AcqData$Region==i)],na.rm=T)
}

dis=rep(NA,10)#disacquiescence per region
for (i in 1:10){
  dis[i]=mean(AcqData$negprop[which(AcqData$Region==i)],na.rm=T)
}
#balance per region (unused because it is just the same as the the two prior measures but less intuitive)
balance=acq-dis

#Weights Analysis
require(sjstats)
Gallup=scale_weights(Gallup,cluster.id=Country,pweight=wgt)
Gallup['Age_C']=Gallup$Age-mean(Gallup$Age,na.rm=T)
Gallup['Age2']=Gallup$Age_C^2
LSage2U=lme(data=Gallup,fixed=LS~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
LSage2WA=lme(data=Gallup,weights=~I(1/svywght_a),fixed=LS~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
LSage2WB=lme(data=Gallup,weights=~I(1/svywght_b),fixed=LS~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
PAage2U=lme(data=Gallup,fixed=PA~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
PAage2WA=lme(data=Gallup,weights=~I(1/svywght_a),fixed=PA~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
PAage2WB=lme(data=Gallup,weights=~I(1/svywght_b),fixed=PA~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
NAage2U=lme(data=Gallup,fixed=Neg~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
NAage2WA=lme(data=Gallup,weights=~I(1/svywght_a),fixed=Neg~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
NAage2WB=lme(data=Gallup,weights=~I(1/svywght_b),fixed=Neg~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)


#Compared MLM vs. Fixed-effects models
#Create 165 (N-1) dummies for fixed effects approach (1 US is referent)
c12=as.numeric(Gallup$Country==12);c12[which(Gallup$Country==1)]=-1#United Kingdom
c46=as.numeric(Gallup$Country==46);c46[which(Gallup$Country==1)]=-1#Canada
c47=as.numeric(Gallup$Country==47);c47[which(Gallup$Country==1)]=-1#Australia
c55=as.numeric(Gallup$Country==55);c55[which(Gallup$Country==1)]=-1#New Zealand
c132=as.numeric(Gallup$Country==132);c132[which(Gallup$Country==1)]=-1#Ireland
#Latin-European=2
c13=as.numeric(Gallup$Country==13);c13[which(Gallup$Country==1)]=-1#France
c17=as.numeric(Gallup$Country==17);c17[which(Gallup$Country==1)]=-1#Spain
c18=as.numeric(Gallup$Country==18);c18[which(Gallup$Country==1)]=-1#Italy
c38=as.numeric(Gallup$Country==38);c38[which(Gallup$Country==1)]=-1#Israel
c75=as.numeric(Gallup$Country==75);c75[which(Gallup$Country==1)]=-1#Moldova
c148=as.numeric(Gallup$Country==148);c148[which(Gallup$Country==1)]=-1#Malta
c166=as.numeric(Gallup$Country==166);c166[which(Gallup$Country==1)]=-1#Portugal
#German=3
c14=as.numeric(Gallup$Country==14);c14[which(Gallup$Country==1)]=-1#Germany
c15=as.numeric(Gallup$Country==15);c15[which(Gallup$Country==1)]=-1#Netherlands
c16=as.numeric(Gallup$Country==16);c16[which(Gallup$Country==1)]=-1#Belgium
c89=as.numeric(Gallup$Country==89);c89[which(Gallup$Country==1)]=-1#Austria
c144=as.numeric(Gallup$Country==144);c144[which(Gallup$Country==1)]=-1#Luxembourg
c184=as.numeric(Gallup$Country==184);c184[which(Gallup$Country==1)]=-1#Switzerland
#Nordic=4
c23=as.numeric(Gallup$Country==23);c23[which(Gallup$Country==1)]=-1#Sweden
c25=as.numeric(Gallup$Country==25);c25[which(Gallup$Country==1)]=-1#Denmark
c119=as.numeric(Gallup$Country==119);c119[which(Gallup$Country==1)]=-1#Estonia
c121=as.numeric(Gallup$Country==121);c121[which(Gallup$Country==1)]=-1#Finland
c130=as.numeric(Gallup$Country==130);c130[which(Gallup$Country==1)]=-1#Iceland
c138=as.numeric(Gallup$Country==138);c138[which(Gallup$Country==1)]=-1#Latvia
c143=as.numeric(Gallup$Country==143);c143[which(Gallup$Country==1)]=-1#Lithuania
c160=as.numeric(Gallup$Country==160);c160[which(Gallup$Country==1)]=-1#Norway
#Eastern European=5
c19=as.numeric(Gallup$Country==19);c19[which(Gallup$Country==1)]=-1#Poland
c20=as.numeric(Gallup$Country==20);c20[which(Gallup$Country==1)]=-1#Hungary
c21=as.numeric(Gallup$Country==21);c21[which(Gallup$Country==1)]=-1#Czech Republic
c22=as.numeric(Gallup$Country==22);c22[which(Gallup$Country==1)]=-1#Romania
c24=as.numeric(Gallup$Country==24);c24[which(Gallup$Country==1)]=-1#Greece
c71=as.numeric(Gallup$Country==71);c71[which(Gallup$Country==1)]=-1#Belarus
c72=as.numeric(Gallup$Country==72);c72[which(Gallup$Country==1)]=-1#Georgia
c73=as.numeric(Gallup$Country==73);c73[which(Gallup$Country==1)]=-1#Kazakhstan
c74=as.numeric(Gallup$Country==74);c74[which(Gallup$Country==1)]=-1#Kyrgyzstan
c76=as.numeric(Gallup$Country==76);c76[which(Gallup$Country==1)]=-1#Russia
c77=as.numeric(Gallup$Country==77);c77[which(Gallup$Country==1)]=-1#Ukraine
c83=as.numeric(Gallup$Country==83);c83[which(Gallup$Country==1)]=-1#Albania
c88=as.numeric(Gallup$Country==88);c88[which(Gallup$Country==1)]=-1#Armenia
c97=as.numeric(Gallup$Country==97);c97[which(Gallup$Country==1)]=-1#Bosnia & Herzegovina
c99=as.numeric(Gallup$Country==99);c99[which(Gallup$Country==1)]=-1#Bulgaria
c109=as.numeric(Gallup$Country==109);c109[which(Gallup$Country==1)]=-1#Croatia
c111=as.numeric(Gallup$Country==111);c111[which(Gallup$Country==1)]=-1#Cyprus
c145=as.numeric(Gallup$Country==145);c145[which(Gallup$Country==1)]=-1#Macedonia
c154=as.numeric(Gallup$Country==154);c154[which(Gallup$Country==1)]=-1#Montenegro
c173=as.numeric(Gallup$Country==173);c173[which(Gallup$Country==1)]=-1#Serbia
c175=as.numeric(Gallup$Country==175);c175[which(Gallup$Country==1)]=-1#Slovakia
c176=as.numeric(Gallup$Country==176);c176[which(Gallup$Country==1)]=-1#Slovenia
c198=as.numeric(Gallup$Country==198);c198[which(Gallup$Country==1)]=-1#Kosovo
c202=as.numeric(Gallup$Country==202);c202[which(Gallup$Country==1)]=-1#Northern Cyprus
#Confucian=6
c27=as.numeric(Gallup$Country==27);c27[which(Gallup$Country==1)]=-1#Hong Kong
c28=as.numeric(Gallup$Country==28);c28[which(Gallup$Country==1)]=-1#Singapore
c29=as.numeric(Gallup$Country==29);c29[which(Gallup$Country==1)]=-1#Japan
c30=as.numeric(Gallup$Country==30);c30[which(Gallup$Country==1)]=-1#China
c50=as.numeric(Gallup$Country==50);c50[which(Gallup$Country==1)]=-1#Vietnam
c68=as.numeric(Gallup$Country==68);c68[which(Gallup$Country==1)]=-1#South Korea
c69=as.numeric(Gallup$Country==69);c69[which(Gallup$Country==1)]=-1#Taiwan
c153=as.numeric(Gallup$Country==153);c153[which(Gallup$Country==1)]=-1#Mongolia
#Latin American=7
c32=as.numeric(Gallup$Country==32);c32[which(Gallup$Country==1)]=-1#Venezuela
c33=as.numeric(Gallup$Country==33);c33[which(Gallup$Country==1)]=-1#Brazil
c34=as.numeric(Gallup$Country==34);c34[which(Gallup$Country==1)]=-1#Mexico
c82=as.numeric(Gallup$Country==82);c82[which(Gallup$Country==1)]=-1#Costa Rica
c87=as.numeric(Gallup$Country==87);c87[which(Gallup$Country==1)]=-1#Argentina
c94=as.numeric(Gallup$Country==94);c94[which(Gallup$Country==1)]=-1#Belize
c96=as.numeric(Gallup$Country==96);c96[which(Gallup$Country==1)]=-1#Bolivia
c104=as.numeric(Gallup$Country==104);c104[which(Gallup$Country==1)]=-1#Chile
c105=as.numeric(Gallup$Country==105);c105[which(Gallup$Country==1)]=-1#Colombia
c110=as.numeric(Gallup$Country==110);c110[which(Gallup$Country==1)]=-1#Cuba
c114=as.numeric(Gallup$Country==114);c114[which(Gallup$Country==1)]=-1#Dominican Republic
c115=as.numeric(Gallup$Country==115);c115[which(Gallup$Country==1)]=-1#Ecuador
c116=as.numeric(Gallup$Country==116);c116[which(Gallup$Country==1)]=-1#El Salvador
c124=as.numeric(Gallup$Country==124);c124[which(Gallup$Country==1)]=-1#Guatemala
c127=as.numeric(Gallup$Country==127);c127[which(Gallup$Country==1)]=-1#Guyana
c128=as.numeric(Gallup$Country==128);c128[which(Gallup$Country==1)]=-1#Haiti
c129=as.numeric(Gallup$Country==129);c129[which(Gallup$Country==1)]=-1#Honduras
c135=as.numeric(Gallup$Country==135);c135[which(Gallup$Country==1)]=-1#Jamaica
c158=as.numeric(Gallup$Country==158);c158[which(Gallup$Country==1)]=-1#Nicaragua
c163=as.numeric(Gallup$Country==163);c163[which(Gallup$Country==1)]=-1#Panama
c164=as.numeric(Gallup$Country==164);c164[which(Gallup$Country==1)]=-1#Paraguay
c165=as.numeric(Gallup$Country==165);c165[which(Gallup$Country==1)]=-1#Peru
c167=as.numeric(Gallup$Country==167);c167[which(Gallup$Country==1)]=-1#Puerto Rico
c182=as.numeric(Gallup$Country==182);c182[which(Gallup$Country==1)]=-1#Suriname
c189=as.numeric(Gallup$Country==189);c189[which(Gallup$Country==1)]=-1#Trinidad & TObago
c194=as.numeric(Gallup$Country==194);c194[which(Gallup$Country==1)]=-1#Uruguay
#Southeast Asian=8
c9=as.numeric(Gallup$Country==9);c9[which(Gallup$Country==1)]=-1#Pakistan
c10=as.numeric(Gallup$Country==10);c10[which(Gallup$Country==1)]=-1#Indonesia
c11=as.numeric(Gallup$Country==11);c11[which(Gallup$Country==1)]=-1#Bangladesh
c31=as.numeric(Gallup$Country==31);c31[which(Gallup$Country==1)]=-1#India
c48=as.numeric(Gallup$Country==48);c48[which(Gallup$Country==1)]=-1#Philippines
c49=as.numeric(Gallup$Country==49);c49[which(Gallup$Country==1)]=-1#Sri Lanka
c51=as.numeric(Gallup$Country==51);c51[which(Gallup$Country==1)]=-1#Thailand
c52=as.numeric(Gallup$Country==52);c52[which(Gallup$Country==1)]=-1#Cambodia
c53=as.numeric(Gallup$Country==53);c53[which(Gallup$Country==1)]=-1#Laos
c54=as.numeric(Gallup$Country==54);c54[which(Gallup$Country==1)]=-1#Myanmar
c70=as.numeric(Gallup$Country==70);c70[which(Gallup$Country==1)]=-1#Afghanistan
c95=as.numeric(Gallup$Country==95);c95[which(Gallup$Country==1)]=-1#Bhutan
c146=as.numeric(Gallup$Country==146);c146[which(Gallup$Country==1)]=-1#Malaysia
c157=as.numeric(Gallup$Country==157);c157[which(Gallup$Country==1)]=-1#Nepal
c185=as.numeric(Gallup$Country==185);c185[which(Gallup$Country==1)]=-1#Tajikistan
#Middle Eastern=9
c2=as.numeric(Gallup$Country==2);c2[which(Gallup$Country==1)]=-1#Egypt
c3=as.numeric(Gallup$Country==3);c3[which(Gallup$Country==1)]=-1#Morocco
c4=as.numeric(Gallup$Country==4);c4[which(Gallup$Country==1)]=-1#Lebanon
c5=as.numeric(Gallup$Country==5);c5[which(Gallup$Country==1)]=-1#Saudi Arabia
c6=as.numeric(Gallup$Country==6);c6[which(Gallup$Country==1)]=-1#Jordan
c7=as.numeric(Gallup$Country==7);c7[which(Gallup$Country==1)]=-1#Syria
c8=as.numeric(Gallup$Country==8);c8[which(Gallup$Country==1)]=-1#Turkey
c26=as.numeric(Gallup$Country==26);c26[which(Gallup$Country==1)]=-1#Iran
c39=as.numeric(Gallup$Country==39);c39[which(Gallup$Country==1)]=-1#Palestinian Territories
c84=as.numeric(Gallup$Country==84);c84[which(Gallup$Country==1)]=-1#Algeria
c90=as.numeric(Gallup$Country==90);c90[which(Gallup$Country==1)]=-1#Azerbaijan
c92=as.numeric(Gallup$Country==92);c92[which(Gallup$Country==1)]=-1#Bahrain
c131=as.numeric(Gallup$Country==131);c131[which(Gallup$Country==1)]=-1#Iraq
c137=as.numeric(Gallup$Country==137);c137[which(Gallup$Country==1)]=-1#Kuwait
c141=as.numeric(Gallup$Country==141);c141[which(Gallup$Country==1)]=-1#Libya
c161=as.numeric(Gallup$Country==161);c161[which(Gallup$Country==1)]=-1#Oman
c168=as.numeric(Gallup$Country==168);c168[which(Gallup$Country==1)]=-1#Qatar
c190=as.numeric(Gallup$Country==190);c190[which(Gallup$Country==1)]=-1#Tunisia
c191=as.numeric(Gallup$Country==191);c191[which(Gallup$Country==1)]=-1#Turkmenistan
c193=as.numeric(Gallup$Country==193);c193[which(Gallup$Country==1)]=-1#United Arab Emirates
c195=as.numeric(Gallup$Country==195);c195[which(Gallup$Country==1)]=-1#Uzbekistan
c197=as.numeric(Gallup$Country==197);c197[which(Gallup$Country==1)]=-1#Yemen
c203=as.numeric(Gallup$Country==203);c203[which(Gallup$Country==1)]=-1#Nagorno-Karabakh Republic
#African=10
c35=as.numeric(Gallup$Country==35);c35[which(Gallup$Country==1)]=-1#Nigeria
c36=as.numeric(Gallup$Country==36);c36[which(Gallup$Country==1)]=-1#Kenya
c37=as.numeric(Gallup$Country==37);c37[which(Gallup$Country==1)]=-1#Tanzania
c40=as.numeric(Gallup$Country==40);c40[which(Gallup$Country==1)]=-1#Ghana
c41=as.numeric(Gallup$Country==41);c41[which(Gallup$Country==1)]=-1#Uganda
c42=as.numeric(Gallup$Country==42);c42[which(Gallup$Country==1)]=-1#Benin
c43=as.numeric(Gallup$Country==43);c43[which(Gallup$Country==1)]=-1#Madagascar
c44=as.numeric(Gallup$Country==44);c44[which(Gallup$Country==1)]=-1#Malawi
c45=as.numeric(Gallup$Country==45);c45[which(Gallup$Country==1)]=-1#South Africa
c56=as.numeric(Gallup$Country==56);c56[which(Gallup$Country==1)]=-1#Angola
c57=as.numeric(Gallup$Country==57);c57[which(Gallup$Country==1)]=-1#Botswana
c60=as.numeric(Gallup$Country==60);c60[which(Gallup$Country==1)]=-1#Ethiopia
c61=as.numeric(Gallup$Country==61);c61[which(Gallup$Country==1)]=-1#Mali
c62=as.numeric(Gallup$Country==62);c62[which(Gallup$Country==1)]=-1#Mauritania
c63=as.numeric(Gallup$Country==63);c63[which(Gallup$Country==1)]=-1#Mozambique
c64=as.numeric(Gallup$Country==64);c64[which(Gallup$Country==1)]=-1#Niger
c65=as.numeric(Gallup$Country==65);c65[which(Gallup$Country==1)]=-1#Rwanda
c66=as.numeric(Gallup$Country==66);c66[which(Gallup$Country==1)]=-1#Senegal
c67=as.numeric(Gallup$Country==67);c67[which(Gallup$Country==1)]=-1#Zambia
c78=as.numeric(Gallup$Country==78);c78[which(Gallup$Country==1)]=-1#Burkina Faso
c79=as.numeric(Gallup$Country==79);c79[which(Gallup$Country==1)]=-1#Cameroon
c80=as.numeric(Gallup$Country==80);c80[which(Gallup$Country==1)]=-1#Sierra Leone
c81=as.numeric(Gallup$Country==81);c81[which(Gallup$Country==1)]=-1#Zimbabwe
c100=as.numeric(Gallup$Country==100);c100[which(Gallup$Country==1)]=-1#Burundi
c102=as.numeric(Gallup$Country==102);c102[which(Gallup$Country==1)]=-1#Central African Republic
c103=as.numeric(Gallup$Country==103);c103[which(Gallup$Country==1)]=-1#Chad
c106=as.numeric(Gallup$Country==106);c106[which(Gallup$Country==1)]=-1#Comoros
c107=as.numeric(Gallup$Country==107);c107[which(Gallup$Country==1)]=-1#Democratic Republic of Congo
c108=as.numeric(Gallup$Country==108);c108[which(Gallup$Country==1)]=-1#Congo
c112=as.numeric(Gallup$Country==112);c112[which(Gallup$Country==1)]=-1#Djibouti
c122=as.numeric(Gallup$Country==122);c122[which(Gallup$Country==1)]=-1#Gabon
c125=as.numeric(Gallup$Country==125);c125[which(Gallup$Country==1)]=-1#Guinea
c134=as.numeric(Gallup$Country==134);c134[which(Gallup$Country==1)]=-1#Cote d'Ivoire
c139=as.numeric(Gallup$Country==139);c139[which(Gallup$Country==1)]=-1#Lesotho
c140=as.numeric(Gallup$Country==140);c140[which(Gallup$Country==1)]=-1#Liberia
c150=as.numeric(Gallup$Country==150);c150[which(Gallup$Country==1)]=-1#Mauritius
c155=as.numeric(Gallup$Country==155);c155[which(Gallup$Country==1)]=-1#Namibia
c178=as.numeric(Gallup$Country==178);c178[which(Gallup$Country==1)]=-1#Somalia
c181=as.numeric(Gallup$Country==181);c181[which(Gallup$Country==1)]=-1#Sudan
c183=as.numeric(Gallup$Country==183);c183[which(Gallup$Country==1)]=-1#Swaziland
c187=as.numeric(Gallup$Country==187);c187[which(Gallup$Country==1)]=-1#Togo
c199=as.numeric(Gallup$Country==199);c199[which(Gallup$Country==1)]=-1#Somaliland Region
c205=as.numeric(Gallup$Country==205);c205[which(Gallup$Country==1)]=-1#South Sudan

#Controls
Age_C=Gallup$Age-mean(Gallup$Age,na.rm=T)
Age2=Age_C^2
LS=Gallup$LS
PA=Gallup$PA
Neg=Gallup$Neg
Gender=Gallup$Gender
y05=Gallup$y05
y06=Gallup$y06
y07=Gallup$y07
y08=Gallup$y08
y09=Gallup$y09
y10=Gallup$y10
y11=Gallup$y11
y12=Gallup$y12
y13=Gallup$y13
y14=Gallup$y14
y15=Gallup$y15
Gallup['Age_C']=Gallup$Age-mean(Gallup$Age,na.rm=T)
Gallup['Age2']=Gallup$Age_C^2
#LS
MLLS=lme(data=Gallup,fixed=LS~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
FELS=lm(LS~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15+c12+c46+c47+c55+c132+c13+c17+c18+c38+c75+c148+c166+c14+c15+c16+c89+c144+c184+c23+c25+c119+c121+c130+c138+c143+c160+c19+c20+c21+c22+c24+c71+c72+c73+c74+c76+c77+c83+c88+c97+c99+c109+c111+c145+c154+c173+c175+c176+c198+c202+c27+c28+c29+c30+c50+c68+c69+c153+c32+c33+c34+c82+c87+c94+c96+c104+c105+c110+c114+c115+c116+c124+c127+c128+c129+c135+c158+c163+c164+c165+c167+c182+c189+c194+c9+c10+c11+c31+c48+c49+c51+c52+c53+c54+c70+c95+c146+c157+c185+c2+c3+c4+c5+c6+c7+c8+c26+c39+c84+c90+c92+c131+c137+c141+c161+c168+c190+c191+c193+c195+c197+c203+c35+c36+c37+c40+c41+c42+c43+c44+c45+c56+c57+c60+c61+c62+c63+c64+c65+c66+c67+c78+c79+c80+c81+c100+c102+c103+c106+c107+c108+c112+c122+c125+c134+c139+c140+c150+c155+c178+c181+c183+c187+c199+c205)
#compare via plots
x=seq(20,80,by=.01)
x2=x^2
y1=MLM$coefficients$fixed[1]+x*MLM$coefficients$fixed[2]+x2*MLM$coefficients$fixed[3]
y2=FELS$coefficients[1]+x*FELS$coefficients[2]+x2*FELS$coefficients[3]
plot(y1~x,type='l',ylim=c(0,10),lwd=3)
lines(x=x,y=y2,lwd=3)
#PA
MLPA=lme(data=Gallup,fixed=PA~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
FEPA=lm(PA~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15+c12+c46+c47+c55+c132+c13+c17+c18+c38+c75+c148+c166+c14+c15+c16+c89+c144+c184+c23+c25+c119+c121+c130+c138+c143+c160+c19+c20+c21+c22+c24+c71+c72+c73+c74+c76+c77+c83+c88+c97+c99+c109+c111+c145+c154+c173+c175+c176+c198+c202+c27+c28+c29+c30+c50+c68+c69+c153+c32+c33+c34+c82+c87+c94+c96+c104+c105+c110+c114+c115+c116+c124+c127+c128+c129+c135+c158+c163+c164+c165+c167+c182+c189+c194+c9+c10+c11+c31+c48+c49+c51+c52+c53+c54+c70+c95+c146+c157+c185+c2+c3+c4+c5+c6+c7+c8+c26+c39+c84+c90+c92+c131+c137+c141+c161+c168+c190+c191+c193+c195+c197+c203+c35+c36+c37+c40+c41+c42+c43+c44+c45+c56+c57+c60+c61+c62+c63+c64+c65+c66+c67+c78+c79+c80+c81+c100+c102+c103+c106+c107+c108+c112+c122+c125+c134+c139+c140+c150+c155+c178+c181+c183+c187+c199+c205)
x=seq(20,80,by=.01)
x2=x^2
y1=MLPA$coefficients$fixed[1]+x*MLPA$coefficients$fixed[2]+x2*MLPA$coefficients$fixed[3]
y2=FEPA$coefficients[1]+x*FEPA$coefficients[2]+x2*FEPA$coefficients[3]
plot(y1~x,type='l',ylim=c(0,10),lwd=3)
lines(x=x,y=y2,lwd=3)
#NA
MLNA=lme(data=Gallup,fixed=Neg~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15,random=~Age_C+Age2|Region/Country,control=lmeControl(opt='optim'),na.action=na.omit)
FENA=lm(Neg~Age_C+Age2+Gender+y05+y06+y07+y08+y09+y10+y11+y12+y13+y14+y15+c12+c46+c47+c55+c132+c13+c17+c18+c38+c75+c148+c166+c14+c15+c16+c89+c144+c184+c23+c25+c119+c121+c130+c138+c143+c160+c19+c20+c21+c22+c24+c71+c72+c73+c74+c76+c77+c83+c88+c97+c99+c109+c111+c145+c154+c173+c175+c176+c198+c202+c27+c28+c29+c30+c50+c68+c69+c153+c32+c33+c34+c82+c87+c94+c96+c104+c105+c110+c114+c115+c116+c124+c127+c128+c129+c135+c158+c163+c164+c165+c167+c182+c189+c194+c9+c10+c11+c31+c48+c49+c51+c52+c53+c54+c70+c95+c146+c157+c185+c2+c3+c4+c5+c6+c7+c8+c26+c39+c84+c90+c92+c131+c137+c141+c161+c168+c190+c191+c193+c195+c197+c203+c35+c36+c37+c40+c41+c42+c43+c44+c45+c56+c57+c60+c61+c62+c63+c64+c65+c66+c67+c78+c79+c80+c81+c100+c102+c103+c106+c107+c108+c112+c122+c125+c134+c139+c140+c150+c155+c178+c181+c183+c187+c199+c205)
x=seq(20,80,by=.01)
x2=x^2
y1=MLNA$coefficients$fixed[1]+x*MLNA$coefficients$fixed[2]+x2*MLNA$coefficients$fixed[3]
y2=FENA$coefficients[1]+x*FENA$coefficients[2]+x2*FENA$coefficients[3]
plot(y1~x,type='l',ylim=c(0,10),lwd=3)
lines(x=x,y=y2,lwd=3)



