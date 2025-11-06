#### Load packages, import data ####

library(data.table)
library(stringr);library(plyr)
library(chron);library(dplyr)
library(psych);
library(MplusAutomation)
library(ggplot2);library(cowplot)

options(scipen=999)  # remove scientific notation

ear <- read.csv('./Data/ear.csv',stringsAsFactors = F)
esm <- read.csv('./Data/esm.csv',stringsAsFactors = F)
demographics <- read.csv('./Data/demographics.csv',stringsAsFactors = F)

# see codebook for description of items

#### Only keep hours that at least 3 coders thought were informative ####

nrow(ear)  # 34,940
ear$ID.fileDate.hour <- paste0(ear$ear.IDnum.w1,'.',ear$ear.fileDate.w1,
                                           '.',ear$ear.PRO03.w1)
str(unique(ear$ID.fileDate.hour))  # 5,222

# drop uninformative hours
informativeonly <- ear[which(ear$ear.hourInfo.w1==1),]
nrow(informativeonly)  # 31,174 rows

str(unique(informativeonly$ID.fileDate.hour))  # 4,708 hours

# count informative codings for each hour
hourinfo <- informativeonly[,c('ID.fileDate.hour','ear.coderID.w1')]
hourinfo <- data.table(hourinfo)
hourinfo[ , obs := .N, by = list(ID.fileDate.hour)] 
hourinfo <- distinct(hourinfo,ID.fileDate.hour,.keep_all=T)
nrow(hourinfo[which(hourinfo$obs>=3),])  # 4,415 hours
hourinfo <- data.frame(hourinfo)

# only keep hours with >=3 informative codings
ear <- informativeonly[which(informativeonly$ID.fileDate.hour %in%
                               hourinfo[which(hourinfo$obs>=3),'ID.fileDate.hour']),]

nrow(ear)  # 30,791
str(unique(ear$ID.fileDate.hour))  # 4,415

#### Setup EAR codings ####

# recode BFI reversed variables
ear[!is.na(ear$ear.BFI21.w1),'ear.BFI21r.w1'] <- 6 - ear[!is.na(ear$ear.BFI21.w1),'ear.BFI21.w1']
ear[!is.na(ear$ear.BFI09.w1),'ear.BFI09r.w1'] <- 6 - ear[!is.na(ear$ear.BFI09.w1),'ear.BFI09.w1']
ear[!is.na(ear$ear.BFI23.w1),'ear.BFI23r.w1'] <- 6 - ear[!is.na(ear$ear.BFI23.w1),'ear.BFI23.w1']
ear[!is.na(ear$ear.BFI37.w1),'ear.BFI37r.w1'] <- 6 - ear[!is.na(ear$ear.BFI37.w1),'ear.BFI37.w1']

# compute composites
ear$ear.EXT.w1 <- rowMeans(ear[,c('ear.BFI36.w1','ear.BFI21r.w1')],na.rm=T)
ear$ear.NEU.w1 <- rowMeans(ear[,c('ear.BFI19.w1','ear.BFI09r.w1','ear.BFI04.w1')],na.rm=T)
ear$ear.CON.w1 <- rowMeans(ear[,c('ear.BFI13.w1','ear.BFI23r.w1')],na.rm=T)
ear$ear.AGR.w1 <- rowMeans(ear[,c('ear.BFI32.w1','ear.BFI37r.w1')],na.rm=T)

ear$ID.fileDate.hour <- paste0(ear$ear.IDnum.w1,'.',ear$ear.fileDate.w1,
                                     '.',ear$ear.PRO03.w1)

# choose 6 most complete coders for training participant (new ID = 83)
training <- ear[which(ear$ear.IDnum.w1==83),]
training <- data.table(training)
training[ , obs := .N, by = list(ear.coderID.w1)] 
trainingobs <- distinct(training,ear.coderID.w1,.keep_all=TRUE)
trainingobs <- data.frame(trainingobs[,c('ear.coderID.w1','obs')])
max(trainingobs$obs)  # 14 coders coded 15 hour blocks
set.seed(20)
keepcoders <- sample(trainingobs[which(trainingobs$obs==max(trainingobs$obs)),'ear.coderID.w1'],6)

training <- data.frame(training)
training <- training[which(training$ear.coderID.w1 %in% keepcoders),!names(training) %in% c('obs')]

ear <- ear[-which(ear$ear.IDnum.w1==83),]
ear <- rbind(ear,training)
unique(ear[which(ear$ear.IDnum.w1==83),'ear.coderID.w1'])

# Count number of participants per coder
countcoders <- data.table(ear)
countcoders$p.coder <- paste0(countcoders$ear.IDnum.w1,'.',countcoders$ear.coderID.w1)
countcoders[ , obs := .N, by = list(p.coder)] 
countcoders <- distinct(countcoders,p.coder,.keep_all=TRUE)
countcoders <- data.frame(countcoders[,c('p.coder','ear.IDnum.w1','ear.coderID.w1','obs')])
countcoders <- setorder(countcoders,ear.IDnum.w1,-obs)

codersperp <- data.table(countcoders)
codersperp[ , obs := .N, by = list(ear.IDnum.w1)] 
codersperp <- distinct(codersperp,ear.IDnum.w1,.keep_all=TRUE)
table(codersperp$obs)  # actually looks like we ended up with more than 6 coders for most participants
mean(codersperp$obs)  # M = 7.90 coders

# most participants ended up with more than 6 coders,
# probably due to the issue of RAs leaving the lab
# before completing their participant
# we'll only use most complete 6 for the key MSEM analyses
# (exclusions applied below)

# subset to final variables
ear <- ear[,c('ear.IDnum.w1','ear.fileDate.w1','ear.PRO03.w1','ID.fileDate.hour','ear.coderID.w1',
              'ear.EXT.w1', 'ear.BFI21r.w1', 'ear.BFI36.w1', 
              'ear.AGR.w1', 'ear.BFI32.w1', 'ear.BFI37r.w1', 
              'ear.CON.w1', 'ear.BFI13.w1', 'ear.BFI23r.w1', 
              'ear.NEU.w1', 'ear.BFI19.w1', 'ear.BFI09r.w1', 'ear.BFI04.w1')]

ear[is.na(ear)] <- NA

# transform to wide (1 column per coder in each hour)
  # note that using my usual reshape method:
  # ear$coderIndex <- sequence(rle(as.character(ear$ID.fileDate.hour))$lengths)
  # ear <- reshape(ear,idvar = "ID.fileDate.hour", timevar = "coderIndex", direction = "wide")
  # coder 1 for a given hour may be different from
  # coder 1 for the next hour (for the same participant)
  # so: re-assign coder IDs based on completeness (coder 1 = most complete, coder 6 = least complete)
  # then subset to the 6 most complete coders
countcoders$coderIndex <- sequence(rle(as.character(countcoders$ear.IDnum.w1))$lengths)

ear <- merge(ear,countcoders[,c('ear.IDnum.w1','ear.coderID.w1','coderIndex')],
             by=c('ear.IDnum.w1','ear.coderID.w1'))

ear <- setorder(ear,ID.fileDate.hour,coderIndex)
earLong <- ear
ear <- reshape(ear,idvar = "ID.fileDate.hour", timevar = "coderIndex", direction = "wide")

# remove .1 from variables that apply across hours
setnames(ear, old = c('ear.fileDate.w1.1','ear.IDnum.w1.1','ear.PRO03.w1.1'), 
         new = c('ear.fileDate.w1','ear.IDnum.w1','ear.PRO03.w1'))

# calculate score across coders
earvars <- c('ear.EXT.w1', 'ear.BFI21r.w1', 'ear.BFI36.w1', 
             'ear.AGR.w1', 'ear.BFI32.w1', 'ear.BFI37r.w1', 
             'ear.CON.w1', 'ear.BFI13.w1', 'ear.BFI23r.w1', 
             'ear.NEU.w1', 'ear.BFI19.w1', 'ear.BFI09r.w1', 'ear.BFI04.w1')

# compute composites based on the first 6 coders only
for(i in earvars){
  ear[,paste0(i)] <- rowMeans(ear[,c(paste0(i,'.1'),paste0(i,'.2'),
                                           paste0(i,'.3'),paste0(i,'.4'),
                                           paste0(i,'.5'),paste0(i,'.6'))],
                                 na.rm=TRUE)
}

# remove extraneous variables
ear <- ear[,!names(ear) %in% c(paste0('ear.fileDate.w1.',1:6),paste0('ear.coderID.w1.',1:6),
                               paste0('ear.IDnum.w1.',1:6),paste0('ear.PRO03.w1.',1:6))]

# reorder so all "items" (i.e., codings) for each construct go together
ear <- ear[,c('ear.fileDate.w1','ear.IDnum.w1','ear.PRO03.w1','ID.fileDate.hour',
              earvars,  # composites first
              c(paste0('ear.EXT.w1.',1:6), paste0('ear.BFI21r.w1.',1:6), 
                        paste0('ear.BFI36.w1.',1:6), paste0('ear.AGR.w1.',1:6), 
                        paste0('ear.BFI32.w1.',1:6), paste0('ear.BFI37r.w1.',1:6),
                        paste0('ear.CON.w1.',1:6), paste0('ear.BFI13.w1.',1:6), 
                        paste0('ear.BFI23r.w1.',1:6), paste0('ear.NEU.w1.',1:6), 
                        paste0('ear.BFI19.w1.',1:6), paste0('ear.BFI09r.w1.',1:6),
                        paste0('ear.BFI04.w1.',1:6)))]

# weirdly, some of the EAR IDs, dates & hour blocks became NAs
# extract these from the ID.fileDate.hour variable

ear$ear.IDnum.w1 <- as.numeric(str_extract(ear$ID.fileDate.hour, "[^.]+"))

splits <- strsplit(ear$ID.fileDate.hour, split='[.]')
splits <- ldply(splits)
names(splits) <- c('ear.IDnum.w1','ear.fileDate.w1','ear.PRO03.w1')
ear$ear.fileDate.w1 <- splits$ear.fileDate.w1
ear$ear.PRO03.w1 <- splits$ear.PRO03.w1

ear[is.na(ear)] <- NA

#### Setup ESM ####

# extract dates
esm$esm.fileDate.w1 <- gsub( " .*$", "", esm$esm.endDateTime.w1 )
esm$esm.fileDate.w1 <- gsub( "\\(", "", esm$esm.fileDate.w1 )
esm$yy <- paste0('20',str_sub(esm$esm.fileDate.w1,7,8))
esm$mm <- str_sub(esm$esm.fileDate.w1,1,2)
esm$dd <- str_sub(esm$esm.fileDate.w1,4,5)
esm$esm.fileDate.w1 <- paste0(esm$yy,esm$mm,esm$dd)

# remove lab practice reports
esm <- esm[-which(esm$esm.PRO02.w1==6),]
names(esm)[names(esm) == 'esm.IDnum.w1'] <- 'ear.IDnum.w1'
names(esm)[names(esm) == 'esm.fileDate.w1'] <- 'ear.fileDate.w1'
names(esm)[names(esm) == 'esm.PRO03.w1'] <- 'ear.PRO03.w1'

# reverse score items
esm[!is.na(esm$esm.BFI21.w1),'esm.BFI21r.w1'] <- 6 - esm[!is.na(esm$esm.BFI21.w1),'esm.BFI21.w1']
esm[!is.na(esm$esm.BFI09.w1),'esm.BFI09r.w1'] <- 6 - esm[!is.na(esm$esm.BFI09.w1),'esm.BFI09.w1']
esm[!is.na(esm$esm.BFI23.w1),'esm.BFI23r.w1'] <- 6 - esm[!is.na(esm$esm.BFI23.w1),'esm.BFI23.w1']
esm[!is.na(esm$esm.BFI37.w1),'esm.BFI37r.w1'] <- 6 - esm[!is.na(esm$esm.BFI37.w1),'esm.BFI37.w1']

# recompute composites
esm$esm.EXT.w1 <- rowMeans(esm[,c('esm.BFI36.w1','esm.BFI21r.w1')],na.rm=T)
esm$esm.NEU.w1 <- rowMeans(esm[,c('esm.BFI19.w1','esm.BFI09r.w1','esm.BFI04.w1')],na.rm=T)
esm$esm.CON.w1 <- rowMeans(esm[,c('esm.BFI13.w1','esm.BFI23r.w1')],na.rm=T)
esm$esm.AGR.w1 <- rowMeans(esm[,c('esm.BFI32.w1','esm.BFI37r.w1')],na.rm=T)

esm$ID.fileDate.hour <- paste0(esm$ear.IDnum.w1,'.',esm$ear.fileDate.w1,'.',
                                  esm$ear.PRO03.w1)

nrow(esm)  # 10,949 reports
str(unique(esm$ear.IDnum.w1))  # 406 participants

# subset to final variables
esm <- esm[,c('ID.fileDate.hour',
              'esm.EXT.w1', 'esm.BFI21r.w1', 'esm.BFI36.w1', 
              'esm.AGR.w1', 'esm.BFI32.w1', 'esm.BFI37r.w1', 
              'esm.CON.w1', 'esm.BFI13.w1', 'esm.BFI23r.w1', 
              'esm.NEU.w1', 'esm.BFI19.w1', 'esm.BFI09r.w1', 'esm.BFI04.w1')]

esm[is.na(esm)] <- NA

#### Merge EAR & ESM ####
dat <- merge(ear,esm,by='ID.fileDate.hour')
nrow(dat)  # 3050 matched observations
str(unique(dat$ear.IDnum.w1))  # N = 289

# remove participants who have fewer than 5 matched EAR & ESM hours
dat <- data.table(dat)
dat[ , obs := .N, by = list(ear.IDnum.w1)] 
dat <- dat[which(dat$obs>=5),]
dat <- data.frame(dat)
nrow(dat)  # 2938 observations
str(unique(dat$ear.IDnum.w1))  # N = 248

# count number of matched observations for each Big Five domain.
# if number of matched obs is < 5 for that domain, 
# convert scores on that variable to NA for that participant

# extraversion 
extdat <- dat[,c('ID.fileDate.hour','ear.IDnum.w1','ear.EXT.w1','esm.EXT.w1')]
dim(extdat)
extdat <- extdat[complete.cases(extdat),]
extdat <- data.table(extdat)
extdat[ , obs := .N, by = list(ear.IDnum.w1)] 
extdat <- extdat[which(extdat$obs>=5),]
extdat <- data.frame(extdat)
dim(extdat)  # N = 2929
str(unique(extdat$ear.IDnum.w1))  # 248 participants

# for IDs not in extdat (i.e., fewer than 5 obs),
# make the EXT items NA

dat[which(dat$ear.IDnum.w1 %in% setdiff(unique(dat$ear.IDnum.w1),unique(extdat$ear.IDnum.w1))),
    c('ear.EXT.w1','ear.BFI36.w1', 'ear.BFI21r.w1',
      paste0('ear.EXT.w1.',1:6),
      paste0('ear.BFI36.w1.',1:6),
      paste0('ear.BFI21r.w1.',1:6),
      'esm.EXT.w1','esm.BFI36.w1', 'esm.BFI21r.w1')] <- NA

earLong[which(earLong$ear.IDnum.w1 %in% earLong$ear.IDnum.w1),c('ear.EXT.w1','ear.BFI36.w1', 'ear.BFI21r.w1')] <- NA
esm[which(esm$ear.IDnum.w1 %in% esm$ear.IDnum.w1),c('esm.EXT.w1','esm.BFI36.w1', 'esm.BFI21r.w1')] <- NA

# agreeableness
agrdat <- dat[,c('ID.fileDate.hour','ear.IDnum.w1','ear.AGR.w1','esm.AGR.w1')]
dim(agrdat)
agrdat <- agrdat[complete.cases(agrdat),]
agrdat <- data.table(agrdat)
agrdat[ , obs := .N, by = list(ear.IDnum.w1)] 
agrdat <- agrdat[which(agrdat$obs>=5),]
agrdat <- data.frame(agrdat)
dim(agrdat)  # N = 2023
str(unique(agrdat$ear.IDnum.w1))  # 217 participants

# for IDs not in agrdat (i.e., fewer than 5 obs),
# make the agreeableness items NA

dat[which(dat$ear.IDnum.w1 %in% setdiff(unique(dat$ear.IDnum.w1),unique(agrdat$ear.IDnum.w1))),
    c('ear.AGR.w1','ear.BFI32.w1', 'ear.BFI37r.w1',
      paste0('ear.AGR.w1.',1:6),
      paste0('ear.BFI32.w1.',1:6),
      paste0('ear.BFI37r.w1.',1:6),
      'esm.AGR.w1','esm.BFI32.w1', 'esm.BFI37r.w1')] <- NA

earLong[which(earLong$ear.IDnum.w1 %in% earLong$ear.IDnum.w1),c('ear.AGR.w1','ear.BFI32.w1', 'ear.BFI37r.w1')] <- NA
esm[which(esm$ear.IDnum.w1 %in% esm$ear.IDnum.w1),c('esm.AGR.w1','esm.BFI32.w1', 'esm.BFI37r.w1')] <- NA

# conscientiousness

condat <- dat[,c('ID.fileDate.hour','ear.IDnum.w1','ear.CON.w1','esm.CON.w1')]
dim(condat)
condat <- condat[complete.cases(condat),]
condat <- data.table(condat)
condat[ , obs := .N, by = list(ear.IDnum.w1)] 
condat <- condat[which(condat$obs>=5),]
condat <- data.frame(condat)
dim(condat)  # N = 2927
str(unique(condat$ear.IDnum.w1))  # 248 participants

# for IDs not in condat (i.e., fewer than 5 obs),
# make the conscientiousness items NA

dat[which(dat$ear.IDnum.w1 %in% setdiff(unique(dat$ear.IDnum.w1),unique(condat$ear.IDnum.w1))),
    c('ear.CON.w1','ear.BFI13.w1', 'ear.BFI23r.w1',
      paste0('ear.CON.w1.',1:6),
      paste0('ear.BFI13.w1.',1:6),
      paste0('ear.BFI23r.w1.',1:6),
      'esm.CON.w1','esm.BFI13.w1', 'esm.BFI23r.w1')] <- NA

earLong[which(earLong$ear.IDnum.w1 %in% earLong$ear.IDnum.w1),c('ear.CON.w1','ear.BFI13.w1', 'ear.BFI23r.w1')] <- NA
esm[which(esm$ear.IDnum.w1 %in% esm$ear.IDnum.w1),c('esm.CON.w1','esm.BFI13.w1', 'esm.BFI23r.w1')] <- NA

# neuroticism
neudat <- dat[,c('ID.fileDate.hour','ear.IDnum.w1','ear.NEU.w1','esm.NEU.w1')]
dim(neudat)
neudat <- neudat[complete.cases(neudat),]
neudat <- data.table(neudat)
neudat[ , obs := .N, by = list(ear.IDnum.w1)] 
neudat <- neudat[which(neudat$obs>=5),]
neudat <- data.frame(neudat)
dim(neudat)  # N = 2928
str(unique(neudat$ear.IDnum.w1))  # 248 participants

# for IDs not in neudat (i.e., fewer than 5 obs),
# make the neuroticism items NA

dat[which(dat$ear.IDnum.w1 %in% setdiff(unique(dat$ear.IDnum.w1),unique(neudat$ear.IDnum.w1))),
    c('ear.NEU.w1','ear.BFI19.w1', 'ear.BFI09r.w1', 'ear.BFI04.w1',
      paste0('ear.NEU.w1.',1:6),
      paste0('ear.BFI19.w1.',1:6),
      paste0('ear.BFI09r.w1.',1:6),
      paste0('ear.BFI04.w1.',1:6),
      'esm.NEU.w1','esm.BFI19.w1', 'esm.BFI09r.w1', 'esm.BFI04.w1')] <- NA

earLong[which(earLong$ear.IDnum.w1 %in% earLong$ear.IDnum.w1),
        c('ear.NEU.w1','ear.BFI19.w1', 'ear.BFI09r.w1', 'ear.BFI04.w1')] <- NA
esm[which(esm$ear.IDnum.w1 %in% esm$ear.IDnum.w1),
    c('esm.NEU.w1','esm.BFI19.w1', 'esm.BFI09r.w1', 'esm.BFI04.w1')] <- NA

nrow(dat)

# summarize number of matched observations included in each analysis
# but note that Mplus uses all available information, so
# more observations were actually used
# (i.e., rows that contained either ESM or EAR data)
nrow(extdat)
nrow(agrdat)
nrow(condat)
nrow(neudat)

#### How many hours had at least 6 coders ####

codersperhour <- earLong[which(earLong$ID.fileDate.hour %in% dat$ID.fileDate.hour),]
codersperhour <- data.table(codersperhour)
codersperhour[ , obs := .N, by = list(ID.fileDate.hour)] 
codersperhour <- distinct(codersperhour,ID.fileDate.hour,.keep_all=TRUE)
nrow(codersperhour[which(codersperhour$obs>=6),])/nrow(codersperhour)

#### Export dataset for Mplus analyses ####

dat <- setorder(dat,ear.IDnum.w1,ear.fileDate.w1,ear.PRO03.w1)

mplusdat <- dat[,c('ear.IDnum.w1',
                   # ESM items
                   'esm.BFI21r.w1', 'esm.BFI36.w1',  # EXT
                   'esm.BFI32.w1', 'esm.BFI37r.w1',  # AGR
                   'esm.BFI13.w1', 'esm.BFI23r.w1',  # CON
                   'esm.BFI19.w1', 'esm.BFI09r.w1', 'esm.BFI04.w1',
                   # EAR composites by coder
                   paste0('ear.EXT.w1.',1:6),
                   paste0('ear.AGR.w1.',1:6),
                   paste0('ear.CON.w1.',1:6),
                   paste0('ear.NEU.w1.',1:6),
                   # EAR items by coder
                   paste0('ear.BFI21r.w1.',1:6),  # quiet
                   paste0('ear.BFI36.w1.',1:6),  # outgoing, sociable
                   paste0('ear.BFI32.w1.',1:6),  # kind, considerate
                   paste0('ear.BFI37r.w1.',1:6),  # rude
                   paste0('ear.BFI13.w1.',1:6),  # reliable
                   paste0('ear.BFI23r.w1.',1:6),  # lazy
                   paste0('ear.BFI19.w1.',1:6),  # worried
                   paste0('ear.BFI09r.w1.',1:6),  # relaxed
                   paste0('ear.BFI04.w1.',1:6)  # depressed, blue
)]

names(mplusdat)

# mplus names
# id  ! participant ID
# 
# ! ESM items
# esme1 esme2 esma1 esma2 
# esmc1 esmc2 
# esmn1 esmn2 esmn3; 
# 
# ! EAR composites by coder 
# ext_1 ext_2 ext_3 ext_4 ext_5 ext_6 
# agr_1 agr_2 agr_3 agr_4 agr_5 agr_6 
# con_1 con_2 con_3 con_4 con_5 con_6 
# neu_1 neu_2 neu_3 neu_4 neu_5 neu_6 
# 
# ! EAR items by coder
# eare1_1 eare1_2 eare1_3 eare1_4 eare1_5 eare1_6  ! quiet
# eare2_1 eare2_2 eare2_3 eare2_4 eare2_5 eare2_6  ! outgoing, sociable
# eara1_1 eara1_2 eara1_3 eara1_4 eara1_5 eara1_6  ! considerate, kind
# eara2_1 eara2_2 eara2_3 eara2_4 eara2_5 eara2_6  ! rude
# earc1_1 earc1_2 earc1_3 earc1_4 earc1_5 earc1_6  ! reliable
# earc2_1 earc2_2 earc2_3 earc2_4 earc2_5 earc2_6  ! lazy
# earn1_1 earn1_2 earn1_3 earn1_4 earn1_5 earn1_6  ! worried
# earn2_1 earn2_2 earn2_3 earn2_4 earn2_5 earn2_6  ! relaxed
# earn3_1 earn3_2 earn3_3 earn3_4 earn3_5 earn3_6  ! depressed, blue

write.table(mplusdat,'./mplus-msem/mplusdat.csv',row.names=F,
            col.names=F,na='*',sep=',')
write.table(mplusdat,'./mplus-descriptives/mplusdat.csv',row.names=F,
            col.names=F,na='*',sep=',')

#### Set up demographics ####

# subset demographics to participants included in the final subset
demographics <- demographics[which(demographics$ts.IDnum.w1 %in% unique(dat$ear.IDnum.w1)),]

# Recode ethnicity codes above 6 to 6 (other) and NAs to 0 (none)
demographics[which(demographics$ts.DEM07.w1>5),'ts.DEM07.w1'] <- 6
demographics[is.na(demographics$ts.DEM07.w1),'ts.DEM07.w1'] <- 0

# Recode factors
demographics[which(demographics$ts.DEM01.w1==2),'gender'] <- 'Female'
demographics[which(demographics$ts.DEM01.w1==1),'gender'] <- 'Male'

demographics[which(demographics$ts.DEM07.w1==0),'ethnicity'] <- 'Not Reported'
demographics[which(demographics$ts.DEM07.w1==1),'ethnicity'] <- 'American Indian or Alaska Native'
demographics[which(demographics$ts.DEM07.w1==2),'ethnicity'] <- 'Asian or Asian American'
demographics[which(demographics$ts.DEM07.w1==3),'ethnicity'] <- 'Black or African American'
demographics[which(demographics$ts.DEM07.w1==4),'ethnicity'] <- 'Native Hawaiian or other Pacific Islander'
demographics[which(demographics$ts.DEM07.w1==5),'ethnicity'] <- 'White'
demographics[which(demographics$ts.DEM07.w1==6),'ethnicity'] <- 'Other or Multiple'

names(demographics)[names(demographics) == 'ts.DEM02.w1'] <- 'age'

demographics <- demographics[,c('ts.IDnum.w1','gender','age','ethnicity')]

demographics <- demographics[which(demographics$ts.IDnum.w1 %in% unique(dat$ear.IDnum.w1)),]

