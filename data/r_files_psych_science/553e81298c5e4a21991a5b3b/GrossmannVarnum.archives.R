#GROSSMANN & vARNUM TIME SERIES ON SOCIAL CHANGE IN aMERICA
#TIME SERIES FOR ALL SCORES
all.change<-read.csv(choose.files(), header=TRUE)
library(Hmisc)
library(forecast)
#get a fit line for each indicator
#names
Boys<-window(ts(all.change$boys,start=c(1860,1),frequency=1),start=1880,end=2012)
t<-seq(1880,2012,length=length(Boys))
Girls<-window(ts(all.change$girls,start=c(1860,1),frequency=1),start=1880,end=2012)
par(mfrow=c(1,2)) 
plot(Boys, main="Boy Naming Practices", xlab ="Year",ylab="Uniqueness (%)", axes=TRUE)
minor.tick(nx=5, ny=5)
plot(Girls, main="Girl Naming Practices", xlab ="Year",ylab="Uniqueness (%)", axes=TRUE)
minor.tick(nx=5, ny=5)
summary(lm(Boys~t))
summary(lm(Girls~t))
abline(abline.boys)
require(fpp)
BoxCox.lambda(Boys)
fit.names.b <- auto.arima(Boys, stepwise=FALSE, approximation=FALSE)
fcast.names.b <- forecast(fit.names.b,h=18)
summary(fit.names.b)
BoxCox.lambda(Girls)
fit.names.g <- auto.arima(Girls, stepwise=FALSE, approximation=FALSE, lambda=0.8239298)
fcast.names.g <- forecast(fit.names.g,h=18)
###graph naming practices
par(mfrow=c(1,2),mai = c(1.2, 1.2, .3, 0.1)) 
plot(fcast.names.b,main="Naming Practices: Boys", ylab="Uniqueness (%)", xlab ="Year",axes=FALSE)
axis(1, at=c(1880,1920,1960,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.boys<-lm(Boys~t)
abline(abline.boys)
summary(abline.boys)
lm_coef.boys <- round(coef(abline.boys), 2) # extract coefficients 
mtext(bquote(y == .(lm_coef.boys[1]) + .(lm_coef.boys[2])*x), side=1,adj=.1,line=-20) 
mtext("t = 15.14 R-sq = .63", side=1,adj=.1,line=-19) 
plot(fcast.names.g,main="Naming Practices: Girls", xlab ="Year", axes=FALSE)
axis(1, at=c(1880,1920,1960,2000,2030))
axis(2, at=c(45, 65, 95))
minor.tick(nx=5, ny=2.5)
abline.girls<-lm(Girls~t)
abline(abline.girls)
summary(abline.girls)
lm_coef.girls <- round(coef(abline.girls), 2) # extract coefficients 
mtext(bquote(y == .(lm_coef.girls[1]) + .(lm_coef.girls[2])*x), side=1,adj=.1,line=-20) 
mtext("t = 13.16 R-sq = .57", side=1,adj=.1,line=-19) 

#books
indiv<-ts(all.change$indiv,start=c(1860,1),frequency=1)
collect<-ts(all.change$collect,start=c(1860,1),frequency=1)
books<-ts(all.change$books,start=c(1860,1),frequency=1)
tb<-seq(1860,2006,length=length(books))
fit.books<-auto.arima(books)
fcast.books<-forecast(fit.books,h=24)
fit.indiv<-auto.arima(indiv)
fcast.indiv<-forecast(fit.indiv,h=24)
fit.collect<-auto.arima(collect)
fcast.collect<-forecast(fit.collect,h=24)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(fcast.books,main="Individualist vs. Collectivist Words", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1860,1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.books<-lm(books~tb)
abline(abline.books)
summary(abline.books)
lm_coef.books <- round(coef(abline.books), 6) # extract coefficients 
mtext("y = -1.25e-03 + 6.35e-07x", side=1,adj=.94,line=-3) 
mtext("t = 51.93 R-sq = .95", side=1,adj=.94,line=-2) 
plot(fcast.indiv,main="Individualist Words", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1860,1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.indiv<-lm(indiv~tb)
abline(abline.indiv)
summary(abline.indiv)
lm_coef.indiv <- round(coef(abline.indiv), 7) # extract coefficients 
mtext("y = -3.85e-04 + 2.37e-07x", side=1,adj=.9,line=-3) 
mtext("t = 32.83 R-sq = .88", side=1,adj=.9,line=-2) 
plot(fcast.collect,main="Collectivist Words", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1860,1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.collect<-lm(collect~tb)
abline(abline.collect)
summary(abline.collect)
lm_coef.collect <- round(coef(abline.collect), 7) # extract coefficients 
mtext("y = 8.69e-04 - 3.99e-07x", side=1,adj=.07,line=-3) 
mtext("t = 51.55 R-sq = .95", side=1,adj=.07,line=-2) 




##books and names together in onegraph
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(fcast.indiv,main="Individualist Words", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1860,1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.indiv<-lm(indiv~tb)
abline(abline.indiv)
summary(abline.indiv)
lm_coef.indiv <- round(coef(abline.indiv), 7) # extract coefficients 
mtext("y = -.0004 + 2.37e-07x", side=1,adj=.9,line=-2) 
mtext("t = 32.83 R-sq = .88", side=1,adj=.9,line=-1) 
plot(fcast.collect,main="Collectivist Words", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1860,1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.collect<-lm(collect~tb)
abline(abline.collect)
summary(abline.collect)
lm_coef.collect <- round(coef(abline.collect), 7) # extract coefficients 
mtext("y = .0009 - 3.99e-07x", side=1,adj=.07,line=-2) 
mtext("t = 51.55 R-sq = .95", side=1,adj=.07,line=-1) 
plot(fcast.names.b,main="Naming Practices: Boys", ylab="Uniqueness (%)", xlab ="Year",axes=FALSE)
axis(1, at=c(1880,1920,1960,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.boys<-lm(Boys~t)
abline(abline.boys)
summary(abline.boys)
lm_coef.boys <- round(coef(abline.boys), 2) # extract coefficients 
mtext(bquote(y == .(lm_coef.boys[1]) + .(lm_coef.boys[2])*x), side=1,adj=.1,line=-9) 
mtext("t = 15.14 R-sq = .63", side=1,adj=.1,line=-8) 
plot(fcast.names.g,main="Naming Practices: Girls", xlab ="Year", axes=FALSE)
axis(1, at=c(1880,1920,1960,2000,2030))
axis(2, at=c(45, 65, 95))
minor.tick(nx=5, ny=2.5)
abline.girls<-lm(Girls~t)
abline(abline.girls)
summary(abline.girls)
lm_coef.girls <- round(coef(abline.girls), 2) # extract coefficients 
mtext(bquote(y == .(lm_coef.girls[1]) + .(lm_coef.girls[2])*x), side=1,adj=.1,line=-9) 
mtext("t = 13.16 R-sq = .57", side=1,adj=.1,line=-8) 



#relationship variables
divorce<-window(ts(all.change$Divbymar,start=c(1860,1),frequency=1),start=1900,end=2012)
famsize<-ts(all.change$averagen.famsize,start=c(1860,1),frequency=1)
child1<-ts(all.change$singleratio,start=c(1860,1),frequency=1)
alone<-ts(all.change$alone,start=c(1860,1),frequency=1)
older.alone<-window(ts(all.change$older.alone.h,start=c(1860,1),frequency=1),start=1880,end=2012)
multigen<-window(ts(all.change$multi.gen.single.households,start=c(1860,1),frequency=1),start=1880,end=2012)
rel<-ts(all.change$rel,start=c(1860,1),frequency=1)




divorce.clean <-na.approx(divorce)
famsize.clean <-na.approx(famsize)
child1.clean <-na.approx(child1)
alone.clean <-na.approx(alone)
older.alone.clean <-na.approx(older.alone)
multigen.clean <-na.approx(multigen)
rel.clean<-na.approx(rel)



tb.divorce<-seq(1900,2012,length=length(divorce))
tb.famsize<-seq(1860,2012,length=length(famsize))
tb.child1<-seq(1860,2012,length=length(child1))
tb.alone<-seq(1860,2012,length=length(alone))
tb.older.alone<-seq(1880,2012,length=length(older.alone))
tb.multigen<-seq(1880,2012,length=length(multigen))





#with interpolation adjustment for graphing
fit.divorce<-auto.arima(divorce.clean)
fcast.divorce<-forecast(fit.divorce,h=18)
fit.famsize<-auto.arima(famsize.clean)
fcast.famsize<-forecast(fit.famsize,h=18)
fit.child1<-auto.arima(child1.clean)
fcast.child1<-forecast(fit.child1,h=18)
fit.alone<-auto.arima(alone.clean)
fcast.alone<-forecast(fit.alone,h=18)
fit.older.alone<-auto.arima(older.alone.clean)
fcast.older.alone<-forecast(fit.older.alone,h=18)
fit.multigen<-auto.arima(multigen.clean)
fcast.multigen<-forecast(fit.multigen,h=18)
fit.rel<-auto.arima(rel.clean)
fcast.rel<-forecast(fit.rel,h=18)

layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE))
plot(fcast.divorce,main="Divorce / Marriage ", xlab ="Year",ylab="ratio", axes=FALSE) 
axis(1, at=c(1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.divorce<-lm(divorce~tb.divorce)
abline(abline.divorce)
summary(abline.divorce)
mtext("y = -8.67 + .005x", side=1,adj=.94,line=-3) 
mtext("t = 17.56 R-sq = .89", side=1,adj=.94,line=-1) 
plot(fcast.famsize,main="Family Size", xlab ="Year",ylab="N", axes=FALSE) 
axis(1, at=c(1860,1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.famsize<-lm(famsize~tb.famsize)
abline(abline.famsize)
summary(abline.famsize)
mtext("y = 38.93 - .02x", side=1,adj=.1,line=-3) 
mtext("t = 38 R-sq = .98", side=1,adj=.1,line=-1) 
plot(fcast.child1,main="Single vs. Multi-child Family", xlab ="Year",ylab="ratio", axes=FALSE) 
axis(1, at=c(1860,1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.child1<-lm(child1~tb.child1)
abline(abline.child1)
summary(abline.child1)
mtext("y = -6.13 + .003x", side=1,adj=.94,line=-3) 
mtext("t = 17.47 R-sq = .92", side=1,adj=.94,line=-1) 
plot(fcast.alone,main="Living Alone", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.alone<-lm(alone~tb.alone)
abline(abline.alone)
summary(abline.alone)
mtext("y = -2.13 + .001x", side=1,adj=.94,line=-3) 
mtext("t = 14.78 R-sq = .92", side=1,adj=.94,line=-1) 
plot(fcast.older.alone,main="Older People Living Alone", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1880,1910,1940,1970,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.older.alone<-lm(older.alone~tb.older.alone)
abline(abline.older.alone)
summary(abline.older.alone)
mtext("y = -3.74 + .002x", side=1,adj=.94,line=-3) 
mtext("t = 21.92 R-sq = .95", side=1,adj=.94,line=-1) 
plot(fcast.multigen,main="Multi-generation households", xlab ="Year",ylab="ratio", axes=FALSE) 
axis(1, at=c(1880,1910,1940,1970,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.multigen<-lm(multigen~tb.multigen)
abline(abline.multigen)
summary(abline.multigen)
mtext("y = 14.75 - .007x", side=1,adj=.1,line=-3) 
mtext("t = 18.18 R-sq = .94", side=1,adj=.1,line=-1) 


#################transform for cross-correlations

data1900 <- subset(all.change, year > 1899)


#interpolate all vars, incl rel (books to do done separately, as it ends in 2006)

data1900$rel.clean <-na.approx(data1900$rel,na.rm=FALSE)
data1900$norel.clean <-na.approx(data1900$norel,na.rm=FALSE)
data1900$city.clean <-na.approx(data1900$city,na.rm=FALSE)
data1900$ses.clean <-na.approx(data1900$ses,na.rm=FALSE)
data1900$pathogen.clean <-na.approx(data1900$pathogen,na.rm=FALSE)


#separate for books below

data1900books<-subset(data1900, year < 2007)

data1900books$rel.clean <-na.approx(data1900books$rel,na.rm=FALSE)
data1900books$norel.clean <-na.approx(data1900books$norel,na.rm=FALSE)
data1900books$city.clean <-na.approx(data1900books$city,na.rm=FALSE)
data1900books$ses.clean <-na.approx(data1900books$ses,na.rm=FALSE)

require(forecast)
##BOX COX on 1900 to 2010
#books
BoxCox.lambda(data1900books$books) # 0.754
BoxCox.lambda(data1900books$ses.clean) # suggests a lambda of 2.0
BoxCox.lambda(data1900$city.clean) #2
rel<-read.csv(choose.files(), header=TRUE)
rel$norel.clean <-na.approx(rel$norel,na.rm=FALSE) #add interpolation
BoxCox.lambda(rel$norel.clean) # suggests a lambda of .58
BoxCox.lambda(data1900books$disaster.total) # .56

#rest
BoxCox.lambda(data1900$names) #suggests a lambda of .85
BoxCox.lambda(data1900$rel.clean) #suggests a lambda of 1.44
#ses and city  is fine, religion done on a sub-sample (since 1948) above
BoxCox.lambda(data1900$disaster.total) # .56
BoxCox.lambda(data1900$ses.clean) # 1.999924
#for pathogens
pathogen.data<-subset(all.change, year > 1911)


BoxCox.lambda(pathogen.data$pathogen) #suggests a lambda of -.68


## transform scores according to box jenkins
#for books, need to transform religion and disasters, the rest is fine.
data1900books$books.cox <-(data1900books$books+1.1704)^1.7539
min(data1900books$books)
histogram(data1900books$books.cox)
data1900books$ses.clean.cox <-(data1900books$ses.clean)^2
histogram(data1900books$ses.clean.cox)
data1900books$city.clean.cox <-(data1900books$city.clean)^2
histogram(data1900books$city.clean.cox)
data1900books$norel.clean.cox <-(data1900books$norel.clean)^.58
histogram(data1900books$norel.clean.cox)
min(data1900books$disaster.total)
data1900books$disasters.cox <-(data1900books$disaster.total+.25)^.56
histogram(data1900books$disasters.cox)
pathogen.data$pathogen.cox<-(pathogen.data$pathogen+.25)^-.68
histogram(pathogen.data$pathogen.cox)

data1900books$pathogen.r<-data1900books$pathogen*(-1)
                                                  
#perform cross-correlations for books
ccfbooks.ses<-ccf(data1900books$ses.clean.cox,data1900books$books.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.city<-ccf(data1900books$city.clean.cox,data1900books$books.cox,lag.max = 30,ylab="CCF",na.action=na.exclude)
ccfbooks.pathogenr<-ccf(data1900books$pathogen.r,data1900books$books.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.norel<-ccf(data1900books$norel.clean.cox,data1900books$books.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.disasters<-ccf(data1900books$disasters.cox,data1900books$books.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)

#for the rest, need to transform names,relationships, religion and disasters, the rest is fine.
data1900$norel.clean.cox <-(data1900$norel.clean)^.58
histogram(data1900$norel.clean.cox)
min(data1900$disaster.total)
data1900$disasters.cox <-(data1900$disaster.total+.25)^.56
histogram(data1900$disasters.cox)
min(data1900$rel.clean)
data1900$rel.clean.cox <-(data1900$rel.clean+1.759957+.25)^1.44
histogram(data1900$rel.clean.cox)
min(data1900$names)
data1900$names.cox <-(data1900$names+1.175223+.25)^.85
histogram(data1900$names.cox)
min(data1900$ses.clean)
data1900$ses.clean.cox <-(data1900$ses.clean)^1.999924
histogram(data1900$ses.clean.cox)
min(data1900$city.clean)
data1900$city.clean.cox <-(data1900$city.clean)^2
histogram(data1900$city.clean.cox)

data1900$pathogen.r<-data1900$pathogen*(-1)

#perform cross-correlations for names
ccfnames.ses<-ccf(data1900$ses.clean.cox,data1900$names.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfnames.city<-ccf(data1900$city.clean.cox,data1900$names.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfnames.pathogenr<-ccf(data1900$pathogen.r,data1900$names.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfnames.norel<-ccf(data1900$norel.clean.cox,data1900$names.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfnames.disasters<-ccf(data1900$disasters.cox,data1900$names.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)

#perform cross-correlations for relationships
ccfrel.ses<-ccf(data1900$ses.clean.cox,data1900$rel.clean.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfrel.city<-ccf(data1900$city.clean.cox,data1900$rel.clean.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfrel.norel<-ccf(data1900$norel.clean.cox,data1900$rel.clean.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfrel.pathogenr<-ccf(data1900$pathogen.r,data1900$rel.clean.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfrel.disasters<-ccf(data1900$disasters.cox,data1900$rel.clean.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)


#try to plot the 4 acfs per DV

#relationships
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(ccfrel.ses,main="Social Class", ylim=c(-0.3,0.9)) 
plot(ccfrel.city,main="Urbanization", ylim=c(-0.3,0.9)) 
plot(ccfrel.norel,main="Secularism", ylim=c(-0.3,0.9)) 
plot(ccfrel.disasters,main="Disasters", ylim=c(-0.3,0.9)) 

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),3,5, byrow = TRUE))
plot(ccfbooks.ses,main="", xlab="",ylab="Books Themes (ccf)", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.city,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.norel,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.pathogenr,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.disasters,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2) 

plot(ccfnames.ses,main="", xlab="",ylab="Naming Practice (ccf)", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfnames.city,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfnames.norel,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfnames.pathogenr,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2) 
plot(ccfnames.disasters,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2) 


plot(ccfrel.ses,main="",xlab="Lag (Social Class)", ylab="Relationship Structure (ccf)", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfrel.city,main="",xlab="Lag (Urbanization)", ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfrel.norel,main="",xlab="Lag (Secularism)", ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfrel.pathogenr,main="",xlab="Lag (Pathogens-reverse)", ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfrel.disasters,main="",xlab="Lag (Disasters)", ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2) 



###try grander causality test#######

require(lmtest)
#books
grangertest(data1900books$ses.clean,data1900books$books,order = 1, na.action=na.exclude)
grangertest(data1900books$ses.clean,data1900books$books,order = 5, na.action=na.exclude)

grangertest(data1900books$city.clean,data1900books$books,order = 1, na.action=na.omit)
grangertest(data1900books$city.clean,data1900books$books,order = 5, na.action=na.omit)

grangertest(data1900books$pathogen.r,data1900books$books,order = 1, na.action=na.exclude)
grangertest(data1900books$pathogen.r,data1900books$books,order = 5, na.action=na.exclude)

grangertest(data1900books$norel.clean,data1900books$books,order = 1, na.action=na.exclude)
grangertest(data1900books$norel.clean,data1900books$books,order = 5, na.action=na.exclude)

grangertest(data1900books$disaster.total,data1900books$books,order = 1, na.action=na.exclude)
grangertest(data1900books$disaster.total,data1900books$books,order = 5, na.action=na.exclude)

#names
grangertest(data1900$ses.clean,data1900$names,order = 1, na.action=na.exclude)
grangertest(data1900$ses.clean,data1900$names,order = 5, na.action=na.exclude)

grangertest(data1900$city.clean,data1900$names,order = 1, na.action=na.exclude)
grangertest(data1900$city.clean,data1900$names,order = 5, na.action=na.exclude)

grangertest(data1900$pathogen.r,data1900$names,order = 1, na.action=na.exclude)
grangertest(data1900$pathogen.r,data1900$names,order = 5, na.action=na.exclude)

grangertest(data1900$norel.clean,data1900$names,order = 1, na.action=na.exclude)
grangertest(data1900$norel.clean,data1900$names,order = 5, na.action=na.exclude)


grangertest(data1900$disasters.cox,data1900$names.cox,order = 1, na.action=na.exclude)
grangertest(data1900$disasters.cox,data1900$names.cox,order = 5, na.action=na.exclude)

#relationships

grangertest(data1900$ses.clean,data1900$rel,order = 1, na.action=na.exclude)
grangertest(data1900$ses.clean,data1900$rel,order = 5, na.action=na.exclude)


grangertest(data1900$city.clean,data1900$rel,order = 1, na.action=na.exclude)
grangertest(data1900$city.clean,data1900$rel,order = 5, na.action=na.exclude)

grangertest(data1900$pathogen.r,data1900$rel,order = 1, na.action=na.exclude)
grangertest(data1900$pathogen.r,data1900$rel,order = 5, na.action=na.exclude)

grangertest(data1900$norel.clean,data1900$rel,order = 1, na.action=na.exclude)
grangertest(data1900$norel.clean,data1900$rel,order = 5, na.action=na.exclude)


grangertest(data1900$disaster.total,data1900$rel,order = 1, na.action=na.exclude)
grangertest(data1900$disaster.total,data1900$rel,order = 5, na.action=na.exclude)



#######################################################################

####replication 

replication<-data1900books
replication$norel.clean <-na.approx(replication$norel,na.rm=FALSE)
replication$city.clean <-na.approx(replication$city,na.rm=FALSE)
replication$ses.clean <-na.approx(replication$ses,na.rm=FALSE)
replication$ses.clean.cox <-(replication$ses.clean)^2
histogram(replication$ses.clean.cox)
replication$city.clean.cox <-(replication$city.clean)^2
histogram(replication$city.clean.cox)
replication$norel.clean.cox <-(replication$norel.clean)^.58
histogram(replication$norel.clean.cox)
min(replication$disaster.total)
replication$disasters.cox <-(replication$disaster.total+.25)^.56
histogram(replication$disasters.cox)


replication$pathogen.r<-replication$pathogen*(-1)


#Greenfield
booksG<-ts(replication$books.G,start=c(1900,1),frequency=1)
booksT<-ts(replication$books.T,start=c(1900,1),frequency=1)

tbG<-seq(1900,2006,length=length(booksG))

#box-cox Greenfield
BoxCox.lambda(replication$books.G) # no need to transform!
#box-cox Twenge
BoxCox.lambda(replication$books.T) # 0.1658006
min(replication$books.T)
replication$books.T.cox <-(replication$books.T+.25)^.1658006
histogram(replication$books.T.cox)


par(mfrow=c(2,1),mai = c(1.2, 1.2, .3, 0.1)) 
plot(booksG,main="Individualist vs. Collectivist Words (List by Greenfield)", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.booksG<-lm(booksG~tbG)
abline(abline.booksG)
summary(abline.booksG)
mtext("y = -.002 + 8.696e-07x", side=1,adj=.94,line=-3) 
mtext("t = 63.03 R-sq = .97", side=1,adj=.9,line=-2) 
plot(booksT,main="Individualist vs. Collectivist Words (List by Twenge)", xlab ="Year",ylab="%", axes=FALSE) 
axis(1, at=c(1900,1940,1980,2000,2030))
axis(2)
minor.tick(nx=5, ny=2.5)
abline.booksT<-lm(booksT~tbG)
abline(abline.booksT)
summary(abline.booksT)
mtext("y = -.039 + 2.187e-05x", side=1,adj=.94,line=-3) 
mtext("t = 49.69 R-sq = .96", side=1,adj=.9,line=-2) 



#cross-correlations Greenfield
ccfbooks.G.ses<-ccf(replication$ses.clean.cox,replication$books.G,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.G.city<-ccf(replication$city.clean.cox,replication$books.G,lag.max = 30,ylab="CCF",na.action=na.exclude)
ccfbooks.G.norel<-ccf(replication$norel.clean.cox,replication$books.G,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.G.pathogenr<-ccf(replication$pathogen.r,replication$books.G,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.G.disasters<-ccf(replication$disasters.cox,replication$books.G,lag.max = 30, ylab="CCF",na.action=na.exclude)
#cross-correlations Twenge
ccfbooks.T.ses<-ccf(replication$ses.clean.cox,replication$books.T.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.T.city<-ccf(replication$city.clean.cox,replication$books.T.cox,lag.max = 30,ylab="CCF",na.action=na.exclude)
ccfbooks.T.norel<-ccf(replication$norel.clean.cox,replication$books.T.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.T.pathogenr<-ccf(replication$pathogen.r,replication$books.T,lag.max = 30, ylab="CCF",na.action=na.exclude)
ccfbooks.T.disasters<-ccf(replication$disasters.cox,replication$books.T.cox,lag.max = 30, ylab="CCF",na.action=na.exclude)


#graphs

##cross-correlations

layout(matrix(c(1,2,3,4,5,6,7,8,9,10),2,5, byrow = TRUE))
plot(ccfbooks.G.ses,main="", xlab="",ylab="Books Themes - Greenfield (ccf)", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.G.city,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.G.norel,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.G.pathogenr,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2) 
plot(ccfbooks.G.disasters,main="", xlab="",ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2) 

plot(ccfbooks.T.ses,main="",xlab="Lag (Social Class)", ylab="Books Themes - Twenge et al. (ccf)", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.T.city,main="",xlab="Lag (Urbanization)", ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.T.norel,main="",xlab="Lag (Secularism)", ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.T.pathogenr,main="",xlab="Lag (Pathogens-rev)", ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2)
plot(ccfbooks.T.disasters,main="",xlab="Lag (Disasters)", ylab="", ylim=c(-0.5,0.9), axes=FALSE) 
axis(1, at=c(-30,-20,-10,0,10,20,30))
axis(2) 









