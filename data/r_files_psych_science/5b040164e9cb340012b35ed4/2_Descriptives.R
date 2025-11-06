############### DATA PREPARATION EXP 2 PART 2: Descriptives  ##############
## Last update: 12/12/2017 ##
## Author: Pieter Van Dessel ##
## Made in: RStudio Version 0.98.1103 ##
## R version 3.2.5 (2016-04-14) ##

##### Load libraries ####
library(Hmisc)

##### 1. Read in final data-file ####

Data<-read.table("Data1.txt")
describe(Data) #72 pps

##### 2. Check counterbalancing ####

# conditions: EC Bob pos/neg, hyp/ctrl
table(Data$EC,Data$hypnosis)