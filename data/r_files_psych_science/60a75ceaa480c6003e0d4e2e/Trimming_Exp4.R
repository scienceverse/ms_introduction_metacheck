
library(tidyverse)
library(glba) 
library(rtdists)
library(retimes)

# reads in the file
datlba<-read.csv(header=TRUE,file.choose())


#creating filtered data (rt_emo) acording to subject & condition:
dl <- datlba %>%
  filter(rt_emo>200) %>%  # keeps RT above 200
  group_by(subject_nr, Condition) %>%
  mutate(rawsubrt=mean(rt_emo),rawsubsd=sd(rt_emo)) %>%
  mutate(ZRT=(rt_emo-rawsubrt)/rawsubsd) %>%
  filter(ZRT<2) %>%  #excludes RTs above 2.5 SD from subject-condtion 
  mutate(adjRT=rt_emo/1000) # changes from milliseconds to seconds

#creating filtered data (rt_emo) acording to subjec:t  
dl <- datlba %>%
  filter(rt_emo>200) %>%  # keeps RT above 200
  group_by(subject_nr) %>%
  mutate(rawsubrt=mean(rt_emo),rawsubsd=sd(rt_emo)) %>%
  mutate(ZRT=(rt_emo-rawsubrt)/rawsubsd) %>%
  filter(ZRT<2) %>%  #excludes RTs above 2.5 SD from subject-condtion 
  mutate(adjRT=rt_emo/1000) # changes from milliseconds to seconds

#creating filtered data (rt_faces) acording to subject:
dl <- datlba %>%
  filter(rt_faces>200) %>%  # keeps RT above 200
  group_by(subject_nr) %>%
  mutate(rawsubrt=mean(rt_faces),rawsubsd=sd(rt_faces)) %>%
  mutate(ZRT=(rt_faces-rawsubrt)/rawsubsd) %>%
  filter(ZRT<2) %>%  #excludes RTs above 2.5 SD from subject-condtion 
  mutate(adjRT=rt_emo/1000) # changes from milliseconds to seconds

#removes missing values
dl<-na.omit(dl)

write.csv(dl,"self_Exp3_TrimmingEMO_PES.csv") # writes an external csv file.

# inputs what you saved for analysis
newdl<-read.csv(header=TRUE,file.choose())










