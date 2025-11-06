#install.packages("ez") #Remove first # to install ez package if needed

#load data (location in script C:/)
data_replication <- read.table("C:/data.txt", dec=".", header = TRUE)

######Perform 2 x 2 repeated measures analysis forconfirmatory analysis#######

require(ez)
model<-ezANOVA(data  =	data_replication,	dv	=	.(performance),	wid	=	.(ppnr), within	=	.(trialtype,	condition),	type	=	3,	detailed	=	TRUE)	
#return results
model

###########perform two dependent t-tests for exploratory analysis#############

######################t-test for different trials#############################

x<-subset(data_replication, trialtype=="different" & condition=="AXBX")
x<-x$performance
y<-subset(data_replication, trialtype=="different" & condition=="CX")
y<-y$performance

t.test(y, x, alternative = "two.sided", paired = TRUE, var.equal = TRUE, conf.level = 0.95)

#calculate means, sd, CI for difference

r<-cor(x, y) #correlation between dependent measures
diff<-y-x #difference scores
m_diff<-mean(diff) #mean of difference scores
m_diff
s_diff<-sd(diff) #standard deviation of difference scores
sd1<-sd(x) #standard deviation of group 1
sd1
sd2<-sd(y) #standard deviation of group 2
sd2
mean(x)
mean(y)
tvalue<-t.test(y, x, alternative = "two.sided", paired = TRUE, var.equal = TRUE, conf.level = 0.95)$statistic #store t-value from dependent t-test
N <- length(x) #number of pairs
CI_diff<-t.test(y, x, alternative = "two.sided", paired = TRUE, var.equal = TRUE, conf.level = 0.95)$conf.int #store confidence interval of mean difference
CI_diff

#effect sizes

#install.packages("MBESS") #Remove first # to install MBESS package if needed
require(MBESS) # used for non-central t-distributions

#Cohen's d_av, using s_av as standardizer
s_av <- sqrt((sd1^2+sd2^2)/2) #calculate average standard deviation for effect size calculation
d_av <- m_diff/s_av
d_av
d_unb <- (1-(3/(4*(N-1)-1)))*d_av #note this is approximation of correction for Hedges'g - ESCI uses accurate correction, so should we.
d_unb
nct_limits <- conf.limits.nct(t.value = tvalue, df=N-1, conf.level = 0.95)
ci_l_d_av <- nct_limits$Lower.Limit*s_diff/(s_av*sqrt(N))
ci_u_d_av <- nct_limits$Upper.Limit*s_diff/(s_av*sqrt(N))
ci_l_d_av
ci_u_d_av

#Cohen's d_z, using s_diff as standardizer
d_z <- tvalue/sqrt(N)
d_z
ci_l_d_z <- nct_limits$Lower.Limit/sqrt(N) #Not sure about this formula, but gives same results as Wuensch's files
ci_u_d_z <- nct_limits$Upper.Limit/sqrt(N) #Not sure about this formula
ci_l_d_z
ci_u_d_z



###################t-test for same trials##########################

x<-subset(data_replication, trialtype=="same" & condition=="AXBX")
x<-x$performance
y<-subset(data_replication, trialtype=="same" & condition=="CX")
y<-y$performance

t.test(y, x, alternative = "two.sided", paired = TRUE, var.equal = TRUE, conf.level = 0.95)

#calculate means, sd, CI for difference

r<-cor(x, y) #correlation between dependent measures
diff<-y-x #difference scores
m_diff<-mean(diff) #mean of difference scores
m_diff
s_diff<-sd(diff) #standard deviation of difference scores
sd1<-sd(x) #standard deviation of group 1
sd1
sd2<-sd(y) #standard deviation of group 2
sd2
mean(x)
mean(y)
N <- length(x) #number of pairs
tvalue<-t.test(y, x, alternative = "two.sided", paired = TRUE, var.equal = TRUE, conf.level = 0.95)$statistic #store t-value from dependent t-test
CI_diff<-t.test(y, x, alternative = "two.sided", paired = TRUE, var.equal = TRUE, conf.level = 0.95)$conf.int #store confidence interval of mean difference
CI_diff

#effect sizes

#install.packages("MBESS") #Remove first # to install MBESS package if needed
require(MBESS) # used for non-central t-distributions

#Cohen's d_av, using s_av as standardizer
s_av <- sqrt((sd1^2+sd2^2)/2) #calculate average standard deviation for effect size calculation
d_av <- m_diff/s_av
d_av
d_unb <- (1-(3/(4*(N-1)-1)))*d_av #note this is approximation of correction for Hedges'g - ESCI uses accurate correction, so should we.
d_unb
nct_limits <- conf.limits.nct(t.value = tvalue, df=N-1, conf.level = 0.95)
ci_l_d_av <- nct_limits$Lower.Limit*s_diff/(s_av*sqrt(N))
ci_u_d_av <- nct_limits$Upper.Limit*s_diff/(s_av*sqrt(N))
ci_l_d_av
ci_u_d_av

#Cohen's d_z, using s_diff as standardizer
d_z <- tvalue/sqrt(N)
d_z
ci_l_d_z <- nct_limits$Lower.Limit/sqrt(N) #Not sure about this formula, but gives same results as Wuensch's files
ci_u_d_z <- nct_limits$Upper.Limit/sqrt(N) #Not sure about this formula
ci_l_d_z
ci_u_d_z
