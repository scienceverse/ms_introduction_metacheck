library('BayesFactor')

GearyData=read.table("Archive.txt", na.strings = ".", header=F)
   colnames(GearyData)=c('ID', 'edcon', 'nviq', 'viq', 'y1ef', 'y2ef', 'Pals', 'y2temascore', 'CPMK', 'WRK', 'NOK',
                         'y1gnt1', 'gngain', 'cpk', 'y1nrt1', 'nrgain', 'y1vct1', 'vcgain', 'y1dqdacct1', 'ansgain', 'y1ocscoreT1', 'ocsgain',
                         'number', 'age1', 'age2', 'age3', 'age4', 'age7', 'age9', 'age10', 'age11', 'age12', 'age14', 'age16')
str(GearyData)

### NOTE: Some values for the Quantitative variables are not integers because some values were imputted due to missing values (see text)

# Parent Education Categories
edcon=GearyData$edcon

# Preschool Yr 1 and Yr 2 Executive Functions
y1ef=GearyData$y1ef
y2ef=GearyData$y2ef

# Preschool Yr 1 Nonverbal and Verbal IQ (scale scores)
nviq=GearyData$nviq
viq=GearyData$viq

# Preschool Yr 1 Alphabet Recognition
PALS=GearyData$Pals

# Kindergarten Progressive Coloured Matrices (raw scores)
CPMK=GearyData$CPMK

# Kindergarten Word Reading (WR) and Numerical Operations (NO) raw scores
WRK=GearyData$WRK
NOK=GearyData$NOK

# End of Preschool Temascore (standardized)
y2temascore=GearyData$y2temascore


# Preschool Give-a-number, beginning, gains, and timing of CPK
y1gnt1=GearyData$y1gnt1
gngain=GearyData$gngain
cpk=GearyData$cpk

# Preschool Verbal Counting, beginning, gains
y1vct1=GearyData$y1vct1
vcgain=GearyData$vcgain

# Preschool Numeral Recognition, beginning, gains
y1nrt1=GearyData$y1nrt1
nrgain=GearyData$nrgain

# Preschool Discreate Quantity Discrimination, beginning, gains
y1dqdacct1=GearyData$y1dqdacct1
ansgain=GearyData$ansgain

# Preschool Object Comparison, beginning, gains
y1ocscoreT1=GearyData$y1ocscoreT1
ocsgain=GearyData$ocsgain

# Beginning of First Grade composite Number System Knowledge
number=GearyData$number

### Ages of various assessments in sequence found in Table 1 of Geary et al. 
age1=GearyData$age1
age2=GearyData$age2
age3=GearyData$age3
age4=GearyData$age4
age7=GearyData$age7
age9=GearyData$age9
age10=GearyData$age10
age11=GearyData$age11
age12=GearyData$age12
age14=GearyData$age14
age16=GearyData$age16


# Combined Bayes  cpk + y1nrt1 + nrgain + y1dqdacct1 + ansgain + edcon + y2ef + PALS + CPMK 


Com=data.frame(number, PALS, CPMK, cpk, y1nrt1, nrgain, y1dqdacct1)
str(Com)

bf_Com = regressionBF(number ~ PALS + CPMK + cpk + y1nrt1 + nrgain + y1dqdacct1, data=Com)
length(bf_Com)
bf_Com["number ~ PALS + CPMK + cpk + y1nrt1 + nrgain + y1dqdacct1"]
head(bf_Com,n=30)
tail(bf_Com,n=30)
which.max(bf_Com)

### ## Once the top model is identified, specify alternate models and compare when dropping variables one at a time, and assessing independent contribution:

### Must be in same sequence as original equation

## Top model CPMK + cpk + y1nrt1 + nrgain 

prebf1 <- bf_Com['CPMK + cpk + y1nrt1']
prebf2 <- bf_Com['CPMK + cpk + nrgain']
prebf3 <- bf_Com['CPMK + y1nrt1 + nrgain']
prebf4 <- bf_Com['cpk + y1nrt1 + nrgain']

## create an object of all models specified: 

bf <- c(prebf1,prebf2,prebf3,prebf4)

## compare models:

comp <- bf/max(bf_Com)
comp

#best model including all
bf_Com = head(bf_Com) / max(bf_Com)
bf_Com
plot(bf_Com) #shows best models


# QUANT Bayes

Quant=data.frame(number, y1gnt1, gngain, cpk, y1ocscoreT1, ocsgain, y1vct1, vcgain, y1nrt1, nrgain, y1dqdacct1, ansgain)
str(Quant)

bf_Quant = regressionBF(number ~ y1gnt1 + gngain + cpk + y1ocscoreT1 + ocsgain + y1vct1 + vcgain + y1nrt1
                        + nrgain + y1dqdacct1 + ansgain, data=Quant)
length(bf_Quant)
bf_Quant["number ~ y1gnt1 + gngain + cpk + y1ocscoret1 + ocsgain + y1vct1 + vcgain + y1nrt1 + nrgain + y1dqdacct1 + ansgain"]
head(bf_Quant,n=30)
tail(bf_Quant,n=30)
which.max(bf_Quant)

### ## Once the top model is identified, specify alternate models and compare when dropping variables one at a time, and assessing independent contribution:


### Must be in same sequence as original equation

## Top model cpk + y1nrt1 + nrgain + y1dqdacct1 + ansgain 

prebf1 <- bf_Quant['cpk + y1nrt1 + nrgain + y1dqdacct1']
prebf2 <- bf_Quant['cpk + y1nrt1 + nrgain + ansgain']
prebf3 <- bf_Quant['cpk + y1nrt1 + y1dqdacct1 + ansgain']
prebf4 <- bf_Quant['cpk + nrgain + y1dqdacct1 + ansgain']
prebf5 <- bf_Quant['y1nrt1 + nrgain + y1dqdacct1 + ansgain']

## create an object of all models specified: 

bf <- c(prebf1,prebf2,prebf3,prebf4,prebf5)

## compare models:

comp <- bf/max(bf_Quant)
comp

#best model including all
bf_Quant = head(bf_Quant) / max(bf_Quant)
bf_Quant
plot(bf_Quant) #shows best models


# DOMAIN GENERAL Bayes

DG=data.frame(number, edcon, nviq, viq, y1ef, y2ef, PALS, CPMK)
str(DG)

bf_DG = regressionBF(number ~ edcon + nviq + viq + y1ef + y2ef + PALS + CPMK, data=DG)
length(bf_DG)
bf_DG["edcon + nviq + viq + y1ef + y2ef + PALS + CPMK"]
head(bf_DG,n=30)
tail(bf_DG,n=30)
which.max(bf_DG)

### ## Once the top model is identified, specify alternate models and compare when dropping variables one at a time, and assessing independent contribution:


### Must be in same sequence as original equation

## Top model edcon + y2ef + PALS + CPMK 

prebf1 <- bf_DG['edcon + y2ef + PALS']
prebf2 <- bf_DG['edcon + y2ef + CPMK']
prebf3 <- bf_DG['edcon + PALS + CPMK']
prebf4 <- bf_DG['y2ef + PALS + CPMK']
#prebf5 <- bf_DG['BD + VIQ + PALS']

## create an object of all models specified: 

bf <- c(prebf1,prebf2,prebf3,prebf4)

## compare models:

comp <- bf/max(bf_DG)
comp

#best model including all
bf_DG = head(bf_DG) / max(bf_DG)
bf_DG
plot(bf_DG) #shows best models

