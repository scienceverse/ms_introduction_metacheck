#nzavs analyses and visualizations
#rm(list=ls())
library(ggplot2)
library(dplyr)
library(clipr)
library(lavaan)

setwd("C:/Users/Ted/Desktop")

long <- read.csv("nzavs_long2.csv")
wide <- read.csv("nzavs_wide2.csv")

#average concerns over years
#first, get scores at each year
d09 <- filter(long, year == 2009); d10 <- filter(long, year == 2010);d11 <- filter(long, year == 2011);d12 <- filter(long, year == 2012);d13 <- filter(long, year == 2013);d14 <- filter(long, year == 2014);d15 <- filter(long, year == 2015);d16 <- filter(long, year == 2016);d17 <- filter(long, year == 2017);d18 <- filter(long, year == 2018);
#second, make a dataframe of the average scores (standardized to 2009 mean = 0) 
mean2 <- function(x) {mean(x, na.rm = TRUE)}
mEeff <- c(mean2(d09$mEeff),mean2(d10$mEeff),mean2(d11$mEeff),mean2(d12$mEeff),mean2(d13$mEeff),mean2(d14$mEeff),mean2(d15$mEeff),mean2(d16$mEeff),mean2(d17$mEeff),mean2(d18$mEeff))
mEsac <- c(mean2(d09$mEsac),mean2(d10$mEsac),mean2(d11$mEsac),mean2(d12$mEsac),mean2(d13$mEsac),mean2(d14$mEsac),mean2(d15$mEsac),mean2(d16$mEsac),mean2(d17$mEsac),mean2(d18$mEsac))
mEbelief <- c(mean2(d09$mEbelief),mean2(d10$mEbelief),mean2(d11$mEbelief),mean2(d12$mEbelief),mean2(d13$mEbelief),mean2(d14$mEbelief),mean2(d15$mEbelief),mean2(d16$mEbelief),mean2(d17$mEbelief),mean2(d18$mEbelief))
mEgreens <- c(mean2(d09$mEgreens),mean2(d10$mEgreens),mean2(d11$mEgreens),mean2(d12$mEgreens),mean2(d13$mEgreens),mean2(d14$mEgreens),mean2(d15$mEgreens),mean2(d16$mEgreens),mean2(d17$mEgreens),mean2(d18$mEgreens))
mEvalue <- c(mean2(d09$mEvalue),mean2(d10$mEvalue),mean2(d11$mEvalue),mean2(d12$mEvalue),mean2(d13$mEvalue),mean2(d14$mEvalue),mean2(d15$mEvalue),mean2(d16$mEvalue),mean2(d17$mEvalue),mean2(d18$mEvalue))
mEchgconcern <- c(mean2(d09$mEchgconcern),mean2(d10$mEchgconcern),mean2(d11$mEchgconcern),mean2(d12$mEchgconcern),mean2(d13$mEchgconcern),mean2(d14$mEchgconcern),mean2(d15$mEchgconcern),mean2(d16$mEchgconcern),mean2(d17$mEchgconcern),mean2(d18$mEchgconcern))
year <- c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)
yearsavg <- data.frame(year, mEeff, mEsac, mEbelief, mEgreens, mEvalue, mEchgconcern)

#now plot the scores over time
ggplot(data = yearsavg, aes(x = year, y = mEeff))+
  geom_line(size = 1, color = "1b9e77")+
  geom_line(size = 1, aes(y = mEsac, color = "#d95f02"))+
  geom_line(size = 1, aes(y = mEbelief), color = "#7570b3")+
  geom_line(size = 1, aes(y = mEgreens), color = "#e7298a")+
  geom_line(size = 1, aes(y = mEvalue), color = "#66a61e")+
  geom_line(size = 1, aes(y = mEchgconcern), color = "#e6ab02")+
  scale_y_continuous(name = "Sustainability Attitude/Behavior",
                     breaks = c(-.1,0,.1,.2,.3,.4,.5,.6),
                     labels = c("-.1","0",".1",".2",".3",".4",".5",".6"))+
  scale_x_continuous(name = "Year",
                     breaks =  c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                     labels = c("2009","","2011","","2013","","2015","","2017","2018"))+
  theme_classic()
  
#age trends by decade cohort
#replace the y variable with the variable of interest
ggplot(data = longfull, aes(x = age, y = sEvalue)) + 
  geom_smooth() + 
  geom_smooth(method = "lm", data = filter(longfull, decadeborn == "1929 or earlier"))+
  geom_smooth(method = "lm", data = filter(longfull, decadeborn == "1930s"))+
  geom_smooth(method = "lm", data = filter(longfull, decadeborn == "1940s"))+
  geom_smooth(method = "lm", data = filter(longfull, decadeborn == "1950s"))+
  geom_smooth(method = "lm", data = filter(longfull, decadeborn == "1960s"))+
  geom_smooth(method = "lm", data = filter(longfull, decadeborn == "1970s"))+
  geom_smooth(method = "lm", data = filter(longfull, decadeborn == "1980s"))+
  geom_smooth(method = "lm", data = filter(longfull, decadeborn == "1990 or later"))+
  theme_classic()

#descriptives tables
mean2 <- function(x) {mean(x, na.rm = TRUE)}
sd2 <- function(x) {sd(x, na.rm = TRUE)}

aEeff <- c(mean2(d09$Eeff),mean2(d10$Eeff),mean2(d11$Eeff),mean2(d12$Eeff),mean2(d13$Eeff),mean2(d14$Eeff),mean2(d15$Eeff),mean2(d16$Eeff),mean2(d17$Eeff),mean2(d18$Eeff))
aEsac <- c(mean2(d09$Esac),mean2(d10$Esac),mean2(d11$Esac),mean2(d12$Esac),mean2(d13$Esac),mean2(d14$Esac),mean2(d15$Esac),mean2(d16$Esac),mean2(d17$Esac),mean2(d18$Esac))
aEbelief <- c(mean2(d09$Ebelief),mean2(d10$Ebelief),mean2(d11$Ebelief),mean2(d12$Ebelief),mean2(d13$Ebelief),mean2(d14$Ebelief),mean2(d15$Ebelief),mean2(d16$Ebelief),mean2(d17$Ebelief),mean2(d18$Ebelief))
aEgreens <- c(mean2(d09$Egreens),mean2(d10$Egreens),mean2(d11$Egreens),mean2(d12$Egreens),mean2(d13$Egreens),mean2(d14$Egreens),mean2(d15$Egreens),mean2(d16$Egreens),mean2(d17$Egreens),mean2(d18$Egreens))
aEvalue <- c(mean2(d09$Evalue),mean2(d10$Evalue),mean2(d11$Evalue),mean2(d12$Evalue),mean2(d13$Evalue),mean2(d14$Evalue),mean2(d15$Evalue),mean2(d16$Evalue),mean2(d17$Evalue),mean2(d18$Evalue))
aEchgconcern <- c(mean2(d09$Echgconcern),mean2(d10$Echgconcern),mean2(d11$Echgconcern),mean2(d12$Echgconcern),mean2(d13$Echgconcern),mean2(d14$Echgconcern),mean2(d15$Echgconcern),mean2(d16$Echgconcern),mean2(d17$Echgconcern),mean2(d18$Echgconcern))
aext <- c(mean2(d09$ext),mean2(d10$ext),mean2(d11$ext),mean2(d12$ext),mean2(d13$ext),mean2(d14$ext),mean2(d15$ext),mean2(d16$ext),mean2(d17$ext),mean2(d18$ext))
aagr <- c(mean2(d09$agr),mean2(d10$agr),mean2(d11$agr),mean2(d12$agr),mean2(d13$agr),mean2(d14$agr),mean2(d15$agr),mean2(d16$agr),mean2(d17$agr),mean2(d18$agr))
acon <- c(mean2(d09$con),mean2(d10$con),mean2(d11$con),mean2(d12$con),mean2(d13$con),mean2(d14$con),mean2(d15$con),mean2(d16$con),mean2(d17$con),mean2(d18$con))
aneu <- c(mean2(d09$neu),mean2(d10$neu),mean2(d11$neu),mean2(d12$neu),mean2(d13$neu),mean2(d14$neu),mean2(d15$neu),mean2(d16$neu),mean2(d17$neu),mean2(d18$neu))
aope <- c(mean2(d09$ope),mean2(d10$ope),mean2(d11$ope),mean2(d12$ope),mean2(d13$ope),mean2(d14$ope),mean2(d15$ope),mean2(d16$ope),mean2(d17$ope),mean2(d18$ope))
ahon <- c(mean2(d09$hon),mean2(d10$hon),mean2(d11$hon),mean2(d12$hon),mean2(d13$hon),mean2(d14$hon),mean2(d15$hon),mean2(d16$hon),mean2(d17$hon),mean2(d18$hon))

bEeff <- c(sd2(d09$Eeff),sd2(d10$Eeff),sd2(d11$Eeff),sd2(d12$Eeff),sd2(d13$Eeff),sd2(d14$Eeff),sd2(d15$Eeff),sd2(d16$Eeff),sd2(d17$Eeff),sd2(d18$Eeff))
bEsac <- c(sd2(d09$Esac),sd2(d10$Esac),sd2(d11$Esac),sd2(d12$Esac),sd2(d13$Esac),sd2(d14$Esac),sd2(d15$Esac),sd2(d16$Esac),sd2(d17$Esac),sd2(d18$Esac))
bEbelief <- c(sd2(d09$Ebelief),sd2(d10$Ebelief),sd2(d11$Ebelief),sd2(d12$Ebelief),sd2(d13$Ebelief),sd2(d14$Ebelief),sd2(d15$Ebelief),sd2(d16$Ebelief),sd2(d17$Ebelief),sd2(d18$Ebelief))
bEgreens <- c(sd2(d09$Egreens),sd2(d10$Egreens),sd2(d11$Egreens),sd2(d12$Egreens),sd2(d13$Egreens),sd2(d14$Egreens),sd2(d15$Egreens),sd2(d16$Egreens),sd2(d17$Egreens),sd2(d18$Egreens))
bEvalue <- c(sd2(d09$Evalue),sd2(d10$Evalue),sd2(d11$Evalue),sd2(d12$Evalue),sd2(d13$Evalue),sd2(d14$Evalue),sd2(d15$Evalue),sd2(d16$Evalue),sd2(d17$Evalue),sd2(d18$Evalue))
bEchgconcern <- c(sd2(d09$Echgconcern),sd2(d10$Echgconcern),sd2(d11$Echgconcern),sd2(d12$Echgconcern),sd2(d13$Echgconcern),sd2(d14$Echgconcern),sd2(d15$Echgconcern),sd2(d16$Echgconcern),sd2(d17$Echgconcern),sd2(d18$Echgconcern))
bext <- c(sd2(d09$ext),sd2(d10$ext),sd2(d11$ext),sd2(d12$ext),sd2(d13$ext),sd2(d14$ext),sd2(d15$ext),sd2(d16$ext),sd2(d17$ext),sd2(d18$ext))
bagr <- c(sd2(d09$agr),sd2(d10$agr),sd2(d11$agr),sd2(d12$agr),sd2(d13$agr),sd2(d14$agr),sd2(d15$agr),sd2(d16$agr),sd2(d17$agr),sd2(d18$agr))
bcon <- c(sd2(d09$con),sd2(d10$con),sd2(d11$con),sd2(d12$con),sd2(d13$con),sd2(d14$con),sd2(d15$con),sd2(d16$con),sd2(d17$con),sd2(d18$con))
bneu <- c(sd2(d09$neu),sd2(d10$neu),sd2(d11$neu),sd2(d12$neu),sd2(d13$neu),sd2(d14$neu),sd2(d15$neu),sd2(d16$neu),sd2(d17$neu),sd2(d18$neu))
bope <- c(sd2(d09$ope),sd2(d10$ope),sd2(d11$ope),sd2(d12$ope),sd2(d13$ope),sd2(d14$ope),sd2(d15$ope),sd2(d16$ope),sd2(d17$ope),sd2(d18$ope))
bhon <- c(sd2(d09$hon),sd2(d10$hon),sd2(d11$hon),sd2(d12$hon),sd2(d13$hon),sd2(d14$hon),sd2(d15$hon),sd2(d16$hon),sd2(d17$hon),sd2(d18$hon))

cEeff <- c(sum(!is.na(d09$Eeff)),sum(!is.na(d10$Eeff)),sum(!is.na(d11$Eeff)),sum(!is.na(d12$Eeff)),sum(!is.na(d13$Eeff)),sum(!is.na(d14$Eeff)),sum(!is.na(d15$Eeff)),sum(!is.na(d16$Eeff)),sum(!is.na(d17$Eeff)),sum(!is.na(d18$Eeff)))
cEsac <- c(sum(!is.na(d09$Esac)),sum(!is.na(d10$Esac)),sum(!is.na(d11$Esac)),sum(!is.na(d12$Esac)),sum(!is.na(d13$Esac)),sum(!is.na(d14$Esac)),sum(!is.na(d15$Esac)),sum(!is.na(d16$Esac)),sum(!is.na(d17$Esac)),sum(!is.na(d18$Esac)))
cEbelief <- c(sum(!is.na(d09$Ebelief)),sum(!is.na(d10$Ebelief)),sum(!is.na(d11$Ebelief)),sum(!is.na(d12$Ebelief)),sum(!is.na(d13$Ebelief)),sum(!is.na(d14$Ebelief)),sum(!is.na(d15$Ebelief)),sum(!is.na(d16$Ebelief)),sum(!is.na(d17$Ebelief)),sum(!is.na(d18$Ebelief)))
cEgreens <- c(sum(!is.na(d09$Egreens)),sum(!is.na(d10$Egreens)),sum(!is.na(d11$Egreens)),sum(!is.na(d12$Egreens)),sum(!is.na(d13$Egreens)),sum(!is.na(d14$Egreens)),sum(!is.na(d15$Egreens)),sum(!is.na(d16$Egreens)),sum(!is.na(d17$Egreens)),sum(!is.na(d18$Egreens)))
cEvalue <- c(sum(!is.na(d09$Evalue)),sum(!is.na(d10$Evalue)),sum(!is.na(d11$Evalue)),sum(!is.na(d12$Evalue)),sum(!is.na(d13$Evalue)),sum(!is.na(d14$Evalue)),sum(!is.na(d15$Evalue)),sum(!is.na(d16$Evalue)),sum(!is.na(d17$Evalue)),sum(!is.na(d18$Evalue)))
cEchgconcern <- c(sum(!is.na(d09$Echgconcern)),sum(!is.na(d10$Echgconcern)),sum(!is.na(d11$Echgconcern)),sum(!is.na(d12$Echgconcern)),sum(!is.na(d13$Echgconcern)),sum(!is.na(d14$Echgconcern)),sum(!is.na(d15$Echgconcern)),sum(!is.na(d16$Echgconcern)),sum(!is.na(d17$Echgconcern)),sum(!is.na(d18$Echgconcern)))
cext <- c(sum(!is.na(d09$ext)),sum(!is.na(d10$ext)),sum(!is.na(d11$ext)),sum(!is.na(d12$ext)),sum(!is.na(d13$ext)),sum(!is.na(d14$ext)),sum(!is.na(d15$ext)),sum(!is.na(d16$ext)),sum(!is.na(d17$ext)),sum(!is.na(d18$ext)))
cagr <- c(sum(!is.na(d09$agr)),sum(!is.na(d10$agr)),sum(!is.na(d11$agr)),sum(!is.na(d12$agr)),sum(!is.na(d13$agr)),sum(!is.na(d14$agr)),sum(!is.na(d15$agr)),sum(!is.na(d16$agr)),sum(!is.na(d17$agr)),sum(!is.na(d18$agr)))
ccon <- c(sum(!is.na(d09$con)),sum(!is.na(d10$con)),sum(!is.na(d11$con)),sum(!is.na(d12$con)),sum(!is.na(d13$con)),sum(!is.na(d14$con)),sum(!is.na(d15$con)),sum(!is.na(d16$con)),sum(!is.na(d17$con)),sum(!is.na(d18$con)))
cneu <- c(sum(!is.na(d09$neu)),sum(!is.na(d10$neu)),sum(!is.na(d11$neu)),sum(!is.na(d12$neu)),sum(!is.na(d13$neu)),sum(!is.na(d14$neu)),sum(!is.na(d15$neu)),sum(!is.na(d16$neu)),sum(!is.na(d17$neu)),sum(!is.na(d18$neu)))
cope <- c(sum(!is.na(d09$ope)),sum(!is.na(d10$ope)),sum(!is.na(d11$ope)),sum(!is.na(d12$ope)),sum(!is.na(d13$ope)),sum(!is.na(d14$ope)),sum(!is.na(d15$ope)),sum(!is.na(d16$ope)),sum(!is.na(d17$ope)),sum(!is.na(d18$ope)))
chon <- c(sum(!is.na(d09$hon)),sum(!is.na(d10$hon)),sum(!is.na(d11$hon)),sum(!is.na(d12$hon)),sum(!is.na(d13$hon)),sum(!is.na(d14$hon)),sum(!is.na(d15$hon)),sum(!is.na(d16$hon)),sum(!is.na(d17$hon)),sum(!is.na(d18$hon)))

perdesc <- data.frame(year, cext, aext, bext, cagr, aagr, bagr, ccon, acon, bcon, cneu, aneu, bneu, cope, aope, bope, chon, ahon, bhon)
write_clip(perdesc)
envdesc <- data.frame(year, cEeff, aEeff, bEeff, cEsac, aEsac, bEsac, cEbelief, aEbelief, bEbelief, cEgreens, aEgreens, bEgreens, cEvalue, aEvalue, bEvalue, cEchgconcern, aEchgconcern, bEchgconcern)
write_clip(envdesc)

#correlation table
write_clip(cor(select(long, sext, sagr, scon, sneu, sope, shon, sEvalue, sEbelief, sEchgconcern, sEeff, sEsac, sEgreens, year, minage, male, avgses, avgpol, maxedu), use = "pairwise.complete.obs"))

#correlations between all SAB items
write_clip(cor(select(long, Evalue, Echgreal, Echgcause, Echgconcern, Eeff1, Eeff2, Esacwilling, Esacmade, Eroutwilling, Eroutmade, Egreens), use = "pairwise.complete.obs"))


#univariate growth curves
#substitute each variable name one by one into the formula
#and remove the years that the variable was not measured (adjust slope loadings accordingly!)
univariatemodel <- "int =~ 1*X09_sEeff + 1*X10_sEeff + 1*X11_sEeff + 1*X12_sEeff + 1*X13_sEeff + 1*X14_sEeff + 1*X16_sEeff + 1*X17_sEeff
                    slo =~ 0*X09_sEeff + 1*X10_sEeff + 2*X11_sEeff + 3*X12_sEeff + 4*X13_sEeff + 5*X14_sEeff + 7*X16_sEeff + 8*X17_sEeff"
univariatedev <- growth(model = univariatemodel, data = wide, missing = "ML")
fitMeasures(univariatedev)
summary(univariatedev)

#bivariate growth curves
#substitude each combo of variables into the formula (following the figure in the text)
#note: only estimate personality variables over the range of years that the environmental variable was estimated
#co-developmental associations come from the standardizedsolution() call
bivariatemodel <- "Eint =~ 1*X09_sEeff + 1*X10_sEeff + 1*X11_sEeff + 1*X12_sEeff + 1*X13_sEeff + 1*X14_sEeff + 1*X16_sEeff + 1*X17_sEeff
                   Eslo =~ 0*X09_sEeff + 1*X10_sEeff + 2*X11_sEeff + 3*X12_sEeff + 4*X13_sEeff + 5*X14_sEeff + 7*X16_sEeff + 8*X17_sEeff
                   Pint =~ 1*X09_sext + 1*X10_sext + 1*X11_sext + 1*X12_sext + 1*X13_sext + 1*X14_sext + 1*X15_sext + 1*X16_sext + 1*X17_sext
                   Pslo =~ 0*X09_sext + 1*X10_sext + 2*X11_sext + 3*X12_sext + 4*X13_sext + 5*X14_sext + 6*X15_sext + 7*X16_sext + 8*X17_sext"
bivariatedev <- growth(model = bivariatemodel, data = wide, missing = "ML")
fitMeasures(bivariatedev)
standardizedSolution(bivariatedev)

#adding covariates
#for each growth curve, sequentially add each of the four variables
#standardize big control variables to aid convergence
wide$sminage <- scale(wide$minage)
wide$savgses <- scale(wide$avgses)
wide$smaxedu <- scale(wide$maxedu)
#here is an example with all covariates added
controlsmodel  <- "Eint =~ 1*X09_sEeff + 1*X10_sEeff + 1*X11_sEeff + 1*X12_sEeff + 1*X13_sEeff + 1*X14_sEeff + 1*X16_sEeff + 1*X17_sEeff
                   Eslo =~ 0*X09_sEeff + 1*X10_sEeff + 2*X11_sEeff + 3*X12_sEeff + 4*X13_sEeff + 5*X14_sEeff + 7*X16_sEeff + 8*X17_sEeff
                   Pint =~ 1*X09_sext + 1*X10_sext + 1*X11_sext + 1*X12_sext + 1*X13_sext + 1*X14_sext + 1*X15_sext + 1*X16_sext + 1*X17_sext
                   Pslo =~ 0*X09_sext + 1*X10_sext + 2*X11_sext + 3*X12_sext + 4*X13_sext + 5*X14_sext + 6*X15_sext + 7*X16_sext + 8*X17_sext
                   Eint + Eslo + Pint + Pslo ~ male
                   Eint + Eslo + Pint + Pslo ~ sminage
                   Eint + Eslo + Pint + Pslo ~ savgses
                   Eint + Eslo + Pint + Pslo ~ smaxedu
                   male +  sminage +  savgses ~~ smaxedu
                   male +  sminage ~~ savgses
                   male ~~ sminage"
controlsdev <- growth(model = controlsmodel, data = wide, missing = "ML")
fitMeasures(controlsdev)
standardizedSolution(controlsdev)

#cross-group comparisons
#for each group, test groups unconstrained and constrainted, and compare fit between them
#five groups: female/male, silent/boomer/millenial+, liberals/conservatives, college/noncollege, lowses/highses
#lavaan throws a strange error if you don't remove the cases with no relevant data yourself
wide2 <- filter(wide, !is.na(X09_sEeff) | !is.na(X10_sEeff) | !is.na(X11_sEeff) | !is.na(X12_sEeff) | !is.na(X13_sEeff) | !is.na(X14_sEeff) | !is.na(X16_sEeff) | !is.na(X17_sEeff) | 
                      !is.na(X09_sext) | !is.na(X10_sext) | !is.na(X11_sext) | !is.na(X12_sext) | !is.na(X13_sext) | !is.na(X14_sext) | !is.na(X15_sext) | !is.na(X16_sext) | !is.na(X17_sext))
modelfree  <- "Eint =~ 1*X09_sEeff + 1*X10_sEeff + 1*X11_sEeff + 1*X12_sEeff + 1*X13_sEeff + 1*X14_sEeff + 1*X16_sEeff + 1*X17_sEeff
               Eslo =~ 0*X09_sEeff + 1*X10_sEeff + 2*X11_sEeff + 3*X12_sEeff + 4*X13_sEeff + 5*X14_sEeff + 7*X16_sEeff + 8*X17_sEeff
               Pint =~ 1*X09_sext + 1*X10_sext + 1*X11_sext + 1*X12_sext + 1*X13_sext + 1*X14_sext + 1*X15_sext + 1*X16_sext + 1*X17_sext
               Pslo =~ 0*X09_sext + 1*X10_sext + 2*X11_sext + 3*X12_sext + 4*X13_sext + 5*X14_sext + 6*X15_sext + 7*X16_sext + 8*X17_sext
               Eint ~~ c(a,b)*Pint
               Eint ~~ c(c,d)*Pslo
               Eslo ~~ c(e,f)*Pint
               Eslo ~~ c(g,h)*Pslo"
freedev <- growth(model = modelfree, data = wide2, missing = "ML", group = "male")

modelcons  <- "Eint =~ 1*X09_sEeff + 1*X10_sEeff + 1*X11_sEeff + 1*X12_sEeff + 1*X13_sEeff + 1*X14_sEeff + 1*X16_sEeff + 1*X17_sEeff
               Eslo =~ 0*X09_sEeff + 1*X10_sEeff + 2*X11_sEeff + 3*X12_sEeff + 4*X13_sEeff + 5*X14_sEeff + 7*X16_sEeff + 8*X17_sEeff
               Pint =~ 1*X09_sext + 1*X10_sext + 1*X11_sext + 1*X12_sext + 1*X13_sext + 1*X14_sext + 1*X15_sext + 1*X16_sext + 1*X17_sext
               Pslo =~ 0*X09_sext + 1*X10_sext + 2*X11_sext + 3*X12_sext + 4*X13_sext + 5*X14_sext + 6*X15_sext + 7*X16_sext + 8*X17_sext
               Eint ~~ c(a,a)*Pint
               Eint ~~ c(c,c)*Pslo
               Eslo ~~ c(e,e)*Pint
               Eslo ~~ c(g,g)*Pslo"
consdev <- growth(model = modelcons, data = wide2, missing = "ML", group = "male")

#check if model fits worse when associations are constrained across groups 
anova(freedev, consdev)
fitMeasures(freedev)
standardizedSolution(freedev)

#testing across generations has three groups: see below
modelfree  <- "Eint =~ 1*X09_sEeff + 1*X10_sEeff + 1*X11_sEeff + 1*X12_sEeff + 1*X13_sEeff + 1*X14_sEeff + 1*X16_sEeff + 1*X17_sEeff
               Eslo =~ 0*X09_sEeff + 1*X10_sEeff + 2*X11_sEeff + 3*X12_sEeff + 4*X13_sEeff + 5*X14_sEeff + 7*X16_sEeff + 8*X17_sEeff
               Pint =~ 1*X09_sext + 1*X10_sext + 1*X11_sext + 1*X12_sext + 1*X13_sext + 1*X14_sext + 1*X15_sext + 1*X16_sext + 1*X17_sext + 1*X18_sext
               Pslo =~ 0*X09_sext + 1*X10_sext + 2*X11_sext + 3*X12_sext + 4*X13_sext + 5*X14_sext + 6*X15_sext + 7*X16_sext + 8*X17_sext + 9*X18_sext
               Eint ~~ c(a,b,c)*Pint
               Eint ~~ c(d,e,f)*Pslo
               Eslo ~~ c(g,h,i)*Pint
               Eslo ~~ c(j,k,l)*Pslo"
freedev <- growth(model = modelfree, data = wide, missing = "ML", group = "generation")

modelcons  <- "Eint =~ 1*X09_sEeff + 1*X10_sEeff + 1*X11_sEeff + 1*X12_sEeff + 1*X13_sEeff + 1*X14_sEeff + 1*X16_sEeff + 1*X17_sEeff
               Eslo =~ 0*X09_sEeff + 1*X10_sEeff + 2*X11_sEeff + 3*X12_sEeff + 4*X13_sEeff + 5*X14_sEeff + 7*X16_sEeff + 8*X17_sEeff
               Pint =~ 1*X09_sext + 1*X10_sext + 1*X11_sext + 1*X12_sext + 1*X13_sext + 1*X14_sext + 1*X15_sext + 1*X16_sext + 1*X17_sext + 1*X18_sext
               Pslo =~ 0*X09_sext + 1*X10_sext + 2*X11_sext + 3*X12_sext + 4*X13_sext + 5*X14_sext + 6*X15_sext + 7*X16_sext + 8*X17_sext + 9*X18_sext
               Eint ~~ c(a,a,a)*Pint
               Eint ~~ c(c,c,c)*Pslo
               Eslo ~~ c(e,e,e)*Pint
               Eslo ~~ c(g,g,g)*Pslo"
consdev <- growth(model = modelcons, data = wide, missing = "ML", group = "generation")