## clear workspace
rm(list = ls())

## necessary libraries
if (!require(jsonlite)) {install.packages("jsonlite"); require(jsonlite)}

###########################################
###########################################

## set "base" directory (i.e., the main experiment folder, where your "data" directory should be stored)
# script should be in:   basedir > analysis
# raw data should be in: basedir > data > raw_data
# data will output to:   basedir > data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory (if not using RStudio, modify to the path you'd like)
basedir <- file.path(getwd(), '..')

datadir <- file.path(basedir, 'data')
datadir.raw <- file.path(datadir, 'raw_data')
outdir <- datadir

###########################################
###########################################
## load data

datalist = list()

subjlist = list()
durationlist = list()
durationtotallist = list()
durationpreloadlist = list()
unixtimelist = list()
pq_strategy = list()
pq_im = list()
pq_purpose = list()
commentslist = list()

#import data using jsonlite [automate this, by defining list of data frames]
files <- list.files(path = datadir.raw, pattern=('*txt'))

# loop through and combine data
for (i in 1:length(files)) {
   curData <- fromJSON(file.path(datadir.raw, files[i]), 
                       simplifyDataFrame = TRUE)
   # fix radio button direction assignment
   if (!('radioButtonDirection' %in% names(curData$experimentParams))) {
      # test if can't reconstruct radio button direction
      if ( all('r_c' == unique(curData$trialData$respButton)) ) {
         radioButtonDirection <- NA
      } else if (nrow(merge(data.frame(respButton=c('r_a','r_b','r_d','r_e'),response=c(1,2,4,5)),
                                curData$trialData[, c('respButton','response')])) > 0) {
         radioButtonDirection <- 'increasing_LtoR'
      } else {
         radioButtonDirection <- 'decreasing_LtoR'
      }
      curData$experimentParams$radioButtonDirection <- radioButtonDirection
      curData$trialData$radioButtonDirection <- radioButtonDirection
   }
   datalist[[i]] <- curData$trialData
   subjlist[[i]] <- curData$subjectID
   durationlist[[i]] <- curData$experimentDuration
   durationpreloadlist[[i]] <- curData$preloadDuration
   durationtotallist[[i]] <- curData$totalDuration_fromPreload
   unixtimelist[[i]] <- curData$startStudyTimestamp_unix
   pq_strategy[[i]] = curData$postQ_strategy
   pq_im[[i]] = curData$postQ_imageTypes
   pq_purpose[[i]] = curData$postQ_purpose
   commentslist[[i]] <- curData$commentText
}

data.raw.unmodified = do.call(rbind, datalist)

# other
subjlist = do.call(rbind, subjlist)
durations = do.call(rbind, durationlist)
durations.preload = do.call(rbind, durationpreloadlist)
durations.total = do.call(rbind, durationtotallist)
unixtimelist = do.call(rbind, unixtimelist)
pq_strategy = do.call(rbind, pq_strategy)
pq_im = do.call(rbind, pq_im)
pq_purpose = do.call(rbind, pq_purpose)
comments = do.call(rbind, commentslist)

# minutes
durations.minutes <- durations/60/1000
durations.preload.minutes <- durations.preload/60/1000
durations.total.minutes <- durations.total/60/1000

# summary
summary.data <- data.frame(subjectID = subjlist,
                           durations.minutes = durations.minutes,
                           durations.preload.minutes = durations.preload.minutes,
                           durations.total.minutes = durations.total.minutes,
                           comments = comments
)
head(summary.data)
summary(summary.data)
summary(summary.data$durations.minutes)
summary(summary.data$durations.preload.minutes)
summary(summary.data$durations.total.minutes)

# comments with subj
pq_strategy
pq_im
pq_purpose
comments

# save raw data to pre-process it
data.raw <- data.raw.unmodified

###########################################
###########################################
## anonymize subjectID

anon_lookup <- data.frame(subjectID = subjlist,
                          subjectID_anon = seq.int(0, length(subjlist)-1))
data.raw <- merge(data.raw, anon_lookup)
data.raw <- subset(data.raw, select = -subjectID)
names(data.raw)[names(data.raw) == 'subjectID_anon'] <- 'subjectID'

###########################################
###########################################
## pre-process

summary(data.raw)

# re-order
data.raw <- subset(data.raw, select = c(experimentName, experimentVersion, subjectID, radioButtonDirection, experimentCondition, 
                                        trialNum, trialType, identity, category, 
                                        respButton, response, rt))

###########################################
###########################################
## save as tab-delimited file

write.table(data.raw, file=file.path(outdir, 'trial_data.tsv'), 
            sep='\t', row.names=F)

###########################################a
###########################################
## write out comments file

comments.frame <- data.frame(subjectID = anon_lookup$subjectID_anon,
                             duration.exp = durations.minutes, duration.preload = durations.preload.minutes, duration.total = durations.total.minutes,
                             strategy = pq_strategy, imageTypesGuess = pq_im, purposeGuess = pq_purpose,
                             comments = comments)

write.table(comments.frame, file=file.path(outdir, 'comments_data.tsv'), 
            sep='\t', row.names=F)

