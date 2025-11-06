#Computing effect sizes and CIs for Correll's and replication samples
require(compute.es)

############
#PSD slopes
############
#Correll's study
fes(5.52,47,24) #d=.59 +/-.51
#replication #1
tes(.891,100,48) #d=.16 +/-.34
#replication #2
tes(.494,98,50) #d=-.09 +/-.34
#combined samples
tes(.277,198,98) #d=.03 +/-.25


#######################
#Behavioral results
#######################
#Correll's sample
#Bias: Number of errors
tes(1.2, 24,47)
#Bias: RTs
tes(1.72, 24,47)

#Our sample #1
#Bias: Number of errors
tes(2.061, 100,48)
#Bias: RTs
tes(1.931, 100,48)

#Our sample #2
#Bias: Number of errors
tes(1.289, 98,50)
#Bias: RTs
tes(2.518, 98,50)

#Combined samples
#Bias: Number of errors
tes(2.289, 198,98)
#Bias: RTs
tes(3.163, 198,98)


###########################################
#PSD slopes, with non-compliant Ps excluded
###########################################
#replication #1
tes(1.078,92,45) #d=.20
#replication #2
tes(.591,89,45) #d=-.11
#combined samples
tes(.361,181,90) #d=.05

#replication #2 with high-error rate P included (for FN #5)
tes(.42,99,50) #d=-.07