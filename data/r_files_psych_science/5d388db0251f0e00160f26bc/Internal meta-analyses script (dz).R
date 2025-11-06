### Internal Meta-Analyses for "No Choice" Conditions ###

# install packages #
install.packages("metafor")

# load packages #
library(metafor)


## create data frames with study descriptive statistics ##
# (SNC = Stranger No Choice; KNC = Kin No Choice) #

KvS_Meta_Moral<-data.frame(
  study = c("1", "2","3","4","5"),
  SNC = c("SNC", "SNC", "SNC", "SNC", "SNC"),
  KNC = c("KNC", "KNC", "KNC", "KNC", "KNC"),
  mean1 = c(6.28,4.23,4.24,7.72,6.21),
  mean2 = c(6.07,4.02,4.14,7.62,6.06),
  sd1 = c(0.81,0.70,0.81, 1.29,1.03),
  sd2 = c(0.91,0.74,0.79,1.27,0.99),
  n = c(209,193,304,305,192),
  r = c(.549,.656,.585,.609,.583))

# Trustworthiness was not measured in Study 3 #
KvS_Meta_Trust<-data.frame(
  study = c("1", "2","4","5"),
  SNC = c("SNC", "SNC", "SNC", "SNC"),
  KNC = c("KNC", "KNC", "KNC", "KNC"),
  mean1 = c(6.19,4.04,7.31,5.94),
  mean2 = c(5.97,3.92,7.30,5.89),
  sd1 = c(0.89,0.73,1.48,1.13),
  sd2 = c(0.89,0.73,1.51,1.06),
  n = c(209,193,304,305),
  r = c(.658,.690,.587,.603))


## run meta-analyses ##

# moral goodness #
KvS_Meta_Moral<-escalc(measure="SMCC",m1i=mean1,m2i=mean2,sd1i=sd1,sd2i=sd2,ni=n,ri=r,data=KvS_Meta_Moral,slab=study)
KvS_Meta_Moral

res_KvS_Meta_Moral<-rma(yi,vi,data=KvS_Meta_Moral)
res_KvS_Meta_Moral

confint(res_KvS_Meta_Moral,digits=3)

forest(res_KvS_Meta_Moral)
funnel(res_KvS_Meta_Moral)

# trustworthiness #
KvS_Meta_Trust<-escalc(measure="SMCC",m1i=mean1,m2i=mean2,sd1i=sd1,sd2i=sd2,ni=n,ri=r,data=KvS_Meta_Trust,slab=study)
KvS_Meta_Trust

res_KvS_Meta_Trust<-rma(yi,vi,data=KvS_Meta_Trust)
res_KvS_Meta_Trust

confint(res_KvS_Meta_Trust,digits=3)

forest(res_KvS_Meta_Trust)
funnel(res_KvS_Meta_Trust)
