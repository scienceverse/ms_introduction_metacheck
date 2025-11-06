library(readxl)
library(dplyr)
library(xlsx)
library(estimatr)
library(parallel)
options(scipen = 999)

study1 <- read_excel('Study1_07_15_20.xlsx')
study1 <-as.data.frame(study1)

study2 <- read_excel('Study2_07_15_20.xlsx')
study2 <-as.data.frame(study2)

##Subsets data to only include eye movements-related trials that could be corrected
study1_valid_trials <- study1[study1$include_trial ==1,]
study2_valid_trials <- predict[study2$include_trial ==1,]

##Standardize eye movements
study1_valid_trials$body_average_first_fix_duration_SD<-scale(study1_valid_trials$body_average_first_fix_duration)
study1_valid_trials$body_average_first_pass_fix_SD<-scale(study1_valid_trials$body_average_first_pass_fix)
study1_valid_trials$body_average_first_pass_duration_SD<-scale(study1_valid_trials$body_average_first_pass_duration)
study1_valid_trials$body_average_regression_SD<-scale(study1_valid_trials$body_average_regression)
study1_valid_trials$body_average_fix_SD<-scale(study1_valid_trials$body_average_fix)
study1_valid_trials$body_average_duration_SD<-scale(study1_valid_trials$body_average_duration)

study2_valid_trials$body_average_first_fix_duration_SD<-scale(study2_valid_trials$body_average_first_fix_duration)
study2_valid_trials$body_average_first_pass_fix_SD<-scale(study2_valid_trials$body_average_first_pass_fix)
study2_valid_trials$body_average_first_pass_duration_SD<-scale(study2_valid_trials$body_average_first_pass_duration)
study2_valid_trials$body_average_regression_SD<-scale(study2_valid_trials$body_average_regression)
study2_valid_trials$body_average_fix_SD<-scale(study2_valid_trials$body_average_fix)
study2_valid_trials$body_average_duration_SD<-scale(study2_valid_trials$body_average_duration)

##Further subsets predict and postdict data to only trials in which lab participants rendered a directional vote
study1_valid_trials_only_votes <- study1_valid_trials[study1_valid_trials$abstain == '0',]
study2_valid_trials_only_votes <- study2_valid_trials[study2_valid_trials$abstain == '0',]

##Re-standardize within non-abstention cases
study1_valid_trials_only_votes$body_average_first_fix_duration_SD<-scale(study1_valid_trials_only_votes$body_average_first_fix_duration)
study1_valid_trials_only_votes$body_average_first_pass_fix_SD<-scale(study1_valid_trials_only_votes$body_average_first_pass_fix)
study1_valid_trials_only_votes$body_average_first_pass_duration_SD<-scale(study1_valid_trials_only_votes$body_average_first_pass_duration)
study1_valid_trials_only_votes$body_average_regression_SD<-scale(study1_valid_trials_only_votes$body_average_regression)
study1_valid_trials_only_votes$body_average_fix_SD<-scale(study1_valid_trials_only_votes$body_average_fix)
study1_valid_trials_only_votes$body_average_duration_SD<-scale(study1_valid_trials_only_votes$body_average_duration)

study2_valid_trials_only_votes$body_average_first_fix_duration_SD<-scale(study2_valid_trials_only_votes$body_average_first_fix_duration)
study2_valid_trials_only_votes$body_average_first_pass_fix_SD<-scale(study2_valid_trials_only_votes$body_average_first_pass_fix)
study2_valid_trials_only_votes$body_average_first_pass_duration_SD<-scale(study2_valid_trials_only_votes$body_average_first_pass_duration)
study2_valid_trials_only_votes$body_average_regression_SD<-scale(study2_valid_trials_only_votes$body_average_regression)
study2_valid_trials_only_votes$body_average_fix_SD<-scale(study2_valid_trials_only_votes$body_average_fix)
study2_valid_trials_only_votes$body_average_duration_SD<-scale(study2_valid_trials_only_votes$body_average_duration)

##Correlation Tables for Figures S1 and S2##

#Study 1 - Abstention Analyses#

cortab1.abstention<-cor(cbind(
  #Eye Movements
  study1_valid_trials$body_average_first_fix_duration,
  study1_valid_trials$body_average_first_pass_fix,
  study1_valid_trials$body_average_first_pass_duration,
  study1_valid_trials$body_average_regression,
  study1_valid_trials$body_average_fix,
  study1_valid_trials$body_average_duration,
  #Ballot Characteristics
  study1_valid_trials$modified_body_word_count,
  study1_valid_trials$position_within_other_proposals,
  study1_valid_trials$clauses_per_sentence_body,
  study1_valid_trials$norming_salience,
  study1_valid_trials$norming_importance,
  study1_valid_trials$norming_interest,
  #Participant Characteristics
  study1_valid_trials$income,
  study1_valid_trials$education,
  study1_valid_trials$political_knowledge,
  study1_valid_trials$age,
  study1_valid_trials$female,
  #Additional Variables
  study1_valid_trials$eds_total,
  study1_valid_trials$ballot_info_addition
), use="pairwise.complete.obs")

cortabL<-sprintf("%.2#g", cortab1.abstention)
cortabL[grepl("e", cortabL)]<-unlist(lapply(as.numeric(cortabL[grepl("e", cortabL)]), format, digits=2, scientific=FALSE, drop0trailing = FALSE))
appendTrail0<-function(num){
  list<-unlist(strsplit(num, ""))
  if(list[length(list)-1]=="." | list[length(list)-1]=="0"){
    return(paste(c(list, 0), sep="", collapse=""))
  }
  if(list[length(list)-1]!="." & list[length(list)-1]!="0"){
    return(paste(list, sep="", collapse=""))
  }
}
cortabL<-unlist(lapply(cortabL, appendTrail0))
cortab1.abstention<-matrix(cortabL, nrow=nrow(cortab1.abstention))
cortab1.abstention[upper.tri(cortab1.abstention, diag=T)]<-NA
diag(cortab1.abstention)<-"--"

names1<-c(
  "Avg. first fixation duration",
  "Avg. first pass fixations",
  "Avg. first pass duration",
  "Avg. regression fixations",
  "Avg. total fixations",
  "Avg. total fixation duration",
  "Word Count",
  "Position within other proposals in real world ballot",
  "Number of clauses per sentence",
  "Avg. norming familiarity rating",
  "Avg. norming importance rating",
  "Avg. norming interest rating",
  "Participant's income",
  "Participant's education",
  "Participant's political knowledge score",
  "Participant's age",
  "Participant's sex",
  "Total number of newspaper editorials",
  "Additional info added to real world ballot"
)

rownames(cortab1.abstention)<-names1
colnames(cortab1.abstention)<-names1

#Study 1 - Opposition Analyses#

cortab1.opposition<-cor(cbind(
  #Eye Movements
  study1_valid_trials_only_votes$body_average_first_fix_duration,
  study1_valid_trials_only_votes$body_average_first_pass_fix,
  study1_valid_trials_only_votes$body_average_first_pass_duration,
  study1_valid_trials_only_votes$body_average_regression,
  study1_valid_trials_only_votes$body_average_fix,
  study1_valid_trials_only_votes$body_average_duration,
  #Ballot Characteristics
  study1_valid_trials_only_votes$modified_body_word_count,
  study1_valid_trials_only_votes$position_within_other_proposals,
  study1_valid_trials_only_votes$clauses_per_sentence_body,
  study1_valid_trials_only_votes$norming_salience,
  study1_valid_trials_only_votes$norming_importance,
  study1_valid_trials_only_votes$norming_interest,
  #Participant Characteristics
  study1_valid_trials_only_votes$income,
  study1_valid_trials_only_votes$education,
  study1_valid_trials_only_votes$political_knowledge,
  study1_valid_trials_only_votes$age,
  study1_valid_trials_only_votes$female,
  #Additional Variables
  study1_valid_trials_only_votes$eds_total,
  study1_valid_trials_only_votes$ballot_info_addition
), use="pairwise.complete.obs")

cortabL<-sprintf("%.2#g", cortab1.opposition)
cortabL[grepl("e", cortabL)]<-unlist(lapply(as.numeric(cortabL[grepl("e", cortabL)]), format, digits=2, scientific=FALSE, drop0trailing = FALSE))
appendTrail0<-function(num){
  list<-unlist(strsplit(num, ""))
  if(list[length(list)-1]=="." | list[length(list)-1]=="0"){
    return(paste(c(list, 0), sep="", collapse=""))
  }
  if(list[length(list)-1]!="." & list[length(list)-1]!="0"){
    return(paste(list, sep="", collapse=""))
  }
}
cortabL<-unlist(lapply(cortabL, appendTrail0))
cortab1.opposition<-matrix(cortabL, nrow=nrow(cortab1.opposition))
cortab1.opposition[upper.tri(cortab1.opposition, diag=T)]<-NA
diag(cortab1.opposition)<-"--"

rownames(cortab1.opposition)<-names1
colnames(cortab1.opposition)<-names1

#Study 2 - Abstention Analyses#

cortab2.abstention<-cor(cbind(
  #Eye Movements
  study2_valid_trials$body_average_first_fix_duration,
  study2_valid_trials$body_average_first_pass_fix,
  study2_valid_trials$body_average_first_pass_duration,
  study2_valid_trials$body_average_regression,
  study2_valid_trials$body_average_fix,
  study2_valid_trials$body_average_duration,
  #Ballot Characteristics
  study2_valid_trials$modified_body_word_count,
  study2_valid_trials$position_within_other_proposals,
  study2_valid_trials$clauses_per_sentence_body,
  study2_valid_trials$norming_salience,
  study2_valid_trials$norming_importance,
  study2_valid_trials$norming_interest,
  #Participant Characteristics
  study2_valid_trials$income,
  study2_valid_trials$education,
  study2_valid_trials$political_knowledge,
  study2_valid_trials$age,
  study2_valid_trials$female,
  #Additional Variables
  study2_valid_trials$eds_total,
  study2_valid_trials$voters_likely_did_not_see,
  study2_valid_trials$ballot_info_addition,
  study2_valid_trials$ballot_money,
  study2_valid_trials$language_changed
), use="pairwise.complete.obs")

cortabL<-sprintf("%.2#g", cortab2.abstention)
cortabL[grepl("e", cortabL)]<-unlist(lapply(as.numeric(cortabL[grepl("e", cortabL)]), format, digits=2, scientific=FALSE, drop0trailing = FALSE))
appendTrail0<-function(num){
  list<-unlist(strsplit(num, ""))
  if(list[length(list)-1]=="." | list[length(list)-1]=="0"){
    return(paste(c(list, 0), sep="", collapse=""))
  }
  if(list[length(list)-1]!="." & list[length(list)-1]!="0"){
    return(paste(list, sep="", collapse=""))
  }
}
cortabL<-unlist(lapply(cortabL, appendTrail0))
cortab2.abstention<-matrix(cortabL, nrow=nrow(cortab2.abstention))
cortab2.abstention[upper.tri(cortab2.abstention, diag=T)]<-NA
diag(cortab2.abstention)<-"--"

names2<-c(
  "Avg. first fixation duration",
  "Avg. first pass fixations",
  "Avg. first pass duration",
  "Avg. regression fixations",
  "Avg. total fixations",
  "Avg. total fixation duration",
  "Word Count",
  "Position within other proposals in real world ballot",
  "Number of clauses per sentence",
  "Avg. norming familiarity rating",
  "Avg. norming importance rating",
  "Avg. norming interest rating",
  "Participant's income",
  "Participant's education",
  "Participant's political knowledge score",
  "Participant's age",
  "Participant's sex",
  "Total number of newspaper editorials",
  "Connecticut-associated trial",
  "Additional info added to real world ballot",
  "Real world ballot received money",
  "Language changed for real world ballot"
)

rownames(cortab2.abstention)<-names2
colnames(cortab2.abstention)<-names2

#Study 2 - Opposition Analyses#

cortab2.opposition<-cor(cbind(
  #Eye Movements
  study2_valid_trials_only_votes$body_average_first_fix_duration,
  study2_valid_trials_only_votes$body_average_first_pass_fix,
  study2_valid_trials_only_votes$body_average_first_pass_duration,
  study2_valid_trials_only_votes$body_average_regression,
  study2_valid_trials_only_votes$body_average_fix,
  study2_valid_trials_only_votes$body_average_duration,
  #Ballot Characteristics
  study2_valid_trials_only_votes$modified_body_word_count,
  study2_valid_trials_only_votes$position_within_other_proposals,
  study2_valid_trials_only_votes$clauses_per_sentence_body,
  study2_valid_trials_only_votes$norming_salience,
  study2_valid_trials_only_votes$norming_importance,
  study2_valid_trials_only_votes$norming_interest,
  #Participant Characteristics
  study2_valid_trials_only_votes$income,
  study2_valid_trials_only_votes$education,
  study2_valid_trials_only_votes$political_knowledge,
  study2_valid_trials_only_votes$age,
  study2_valid_trials_only_votes$female,
  #Additional Variables
  study2_valid_trials_only_votes$eds_total,
  study2_valid_trials_only_votes$voters_likely_did_not_see,
  study2_valid_trials_only_votes$ballot_info_addition,
  study2_valid_trials_only_votes$ballot_money,
  study2_valid_trials_only_votes$language_changed
), use="pairwise.complete.obs")

cortabL<-sprintf("%.2#g", cortab2.opposition)
cortabL[grepl("e", cortabL)]<-unlist(lapply(as.numeric(cortabL[grepl("e", cortabL)]), format, digits=2, scientific=FALSE, drop0trailing = FALSE))
appendTrail0<-function(num){
  list<-unlist(strsplit(num, ""))
  if(list[length(list)-1]=="." | list[length(list)-1]=="0"){
    return(paste(c(list, 0), sep="", collapse=""))
  }
  if(list[length(list)-1]!="." & list[length(list)-1]!="0"){
    return(paste(list, sep="", collapse=""))
  }
}
cortabL<-unlist(lapply(cortabL, appendTrail0))
cortab2.opposition<-matrix(cortabL, nrow=nrow(cortab2.opposition))
cortab2.opposition[upper.tri(cortab2.opposition, diag=T)]<-NA
diag(cortab2.opposition)<-"--"

rownames(cortab2.opposition)<-names2
colnames(cortab2.opposition)<-names2

##Plot figures S1 and S2##

#Study 1

names1<-gsub("Avg.", "Average", names1)

plottab1<-matrix(as.numeric(cortab1.abstention)*10000000, nrow=dim(cortab1.abstention)[1])
plottab1[upper.tri(plottab1, diag=F)]<-t(plottab1)[upper.tri(t(plottab1), diag=F)]
coltab1<-matrix(NA, nrow=dim(cortab1.abstention)[1], ncol=dim(cortab1.abstention)[1])

negpal<-colorRampPalette(c("gray95", "firebrick3"))
negcols<-negpal(10000000)
pospal<-colorRampPalette(c("gray95", "royalblue3"))
poscols<-pospal(10000000)

for(i in 1:dim(plottab1)[1]){
  for(j in 1:dim(plottab1)[1]){
    if(!is.na(plottab1[i,j])){
      if(plottab1[i,j]<0){
        coltab1[i,j]<-negcols[abs(plottab1[i,j])]
      }
      if(plottab1[i,j]>0){
        coltab1[i,j]<-poscols[plottab1[i,j]]
      }
    }
  }
}

pdf("Figure_S1.pdf", width=9, height=5.75)
par(mar=c(0.5, 2, 2, 0.5), fig=c(0, 0.66, 0.1, 1))
plot(1:dim(coltab1)[1], dim(coltab1)[1]:1, type="n", xlim=c(0.5, dim(coltab1)[1]+0.5), ylim=c(0.5, dim(coltab1)[1]+0.5), xaxs='i', yaxs='i', axes=F)
for(i in 1:dim(coltab1)[1]){
  for(j in 1:dim(coltab1)[1]){
    if(is.na(coltab1[i,j])){
      rect(i-0.5, abs(j-dim(coltab1)[1]-1)-0.5, i+0.5, abs(j-dim(coltab1)[1]-1)+0.5, col="black", border=NA)
    }
    if(!is.na(coltab1[i,j])){
      rect(i-0.5, abs(j-dim(coltab1)[1]-1)-0.5, i+0.5, abs(j-dim(coltab1)[1]-1)+0.5, col=coltab1[i,j], border=NA)
    }
  }
}
abline(v=6.5)
abline(v=12.5)
abline(v=17.5)
abline(h=13.5)
abline(h=7.5)
abline(h=2.5)
axis(2, at=seq(0, dim(coltab1)[1]+1, by=1), labels=seq(dim(coltab1)[1]+1, 0, by=-1), las=1, hadj=.6, cex.axis=0.75)
axis(3, at=seq(0, dim(coltab1)[1]+1, by=1), labels=seq(0, dim(coltab1)[1]+1, by=1), padj=.6, cex.axis=0.75)
par(mar=c(0.5, 0, 0.5, 0.5), fig=c(0.66, 1, 0, 1), new=T)

plot(1, 1, type="n", axes=F, main='', xlab='', ylab='', xlim=c(0, 1), ylim=c(0, 1), xaxs='i', yaxs='i')
rect(0, 0, 1, 1, col='grey90')
text(0.5, seq(1, 0, length.out=length(names1)+7)[2]+.01, "Study 1", font=4, cex=1)
text(0, seq(1, 0, length.out=length(names1)+7)[3], "Eye Movement Measures", font=2, cex=0.85, pos=4)
text(0.02, seq(1, 0, length.out=length(names1)+7)[4:9], c(1:6), font=3, cex=0.65, pos=4)
text(0.1, seq(1, 0, length.out=length(names1)+7)[4:9], names1[1:6], font=3, cex=0.65, pos=4)
text(0, seq(1, 0, length.out=length(names1)+7)[10], "Ballot Characteristics", font=2, cex=0.85, pos=4)
text(0.02, seq(1, 0, length.out=length(names1)+7)[11:16], c(7:12), font=3, cex=0.65, pos=4)
text(0.1, seq(1, 0, length.out=length(names1)+7)[11:16], names1[7:12], font=3, cex=0.65, pos=4)
text(0, seq(1, 0, length.out=length(names1)+7)[17], "Participant Characteristics", font=2, cex=0.85, pos=4)
text(0.02, seq(1, 0, length.out=length(names1)+7)[18:22], c(13:17), font=3, cex=0.65, pos=4)
text(0.1, seq(1, 0, length.out=length(names1)+7)[18:22], names1[13:17], font=3, cex=0.65, pos=4)
text(0, seq(1, 0, length.out=length(names1)+7)[23], "Additional Variables", font=2, cex=0.85, pos=4)
text(0.02, seq(1, 0, length.out=length(names1)+7)[24:25], c(18:19), font=3, cex=0.65, pos=4)
text(0.1, seq(1, 0, length.out=length(names1)+7)[24:25], names1[18:19], font=3, cex=0.65, pos=4)
par(mar=c(0.5, 0.5, 0, 0.5), fig=c(0, 0.66, 0, 0.1), new=T)

plot(1, 1, type="n", axes=F, main='', xlab='', ylab='', xlim=c(0, 1), ylim=c(0, 1), xaxs='i', yaxs='i')
#rect(0, 0, 1, 1, col='white')
segments(0.01, 0.3, 0.99, 0.3)
segments(seq(0.01, 0.99, length.out=9), 0.25, seq(0.01, 0.99, length.out=9), 0.3)
segments(seq(0.01, 0.5, length.out=1000), 0.35, seq(0.01, 0.5, length.out=1000), 0.5, col=rev(negpal(1000)))
segments(seq(0.5, 0.99, length.out=1000), 0.35, seq(0.5, 0.99, length.out=1000), 0.5, col=pospal(1000))
text(seq(0.01, 0.99, length.out=9)[c(1, 3, 5, 7, 9)], 0.125, c("-1", "-.5", "0", ".5", "1"), cex=0.5, xpd=T, font=2)
text(0.5, 0.8, "Correlation (r)", cex=1, xpd=T, font=4)
dev.off()

#Study 2

names2<-gsub("Avg.", "Average", names2)

plottab2<-matrix(as.numeric(cortab2.abstention)*10000000, nrow=dim(cortab2.abstention)[1])
plottab2[upper.tri(plottab2, diag=F)]<-t(plottab2)[upper.tri(t(plottab2), diag=F)]
coltab2<-matrix(NA, nrow=dim(cortab2.abstention)[1], ncol=dim(cortab2.abstention)[1])

negpal<-colorRampPalette(c("gray95", "firebrick3"))
negcols<-negpal(10000000)
pospal<-colorRampPalette(c("gray95", "royalblue3"))
poscols<-pospal(10000000)

for(i in 1:dim(plottab2)[1]){
  for(j in 1:dim(plottab2)[1]){
    if(!is.na(plottab2[i,j])){
      if(plottab2[i,j]<0){
        coltab2[i,j]<-negcols[abs(plottab2[i,j])]
      }
      if(plottab2[i,j]>0){
        coltab2[i,j]<-poscols[plottab2[i,j]]
      }
    }
  }
}

pdf("Figures_S2.pdf", width=9, height=5.75)
par(mar=c(0.5, 2, 2, 0.5), fig=c(0, 0.66, 0.1, 1))
plot(1:dim(coltab2)[1], dim(coltab2)[1]:1, type="n", xlim=c(0.5, dim(coltab2)[1]+0.5), ylim=c(0.5, dim(coltab2)[1]+0.5), xaxs='i', yaxs='i', axes=F)
for(i in 1:dim(coltab2)[1]){
  for(j in 1:dim(coltab2)[1]){
    if(is.na(coltab2[i,j])){
      rect(i-0.5, abs(j-dim(coltab2)[1]-1)-0.5, i+0.5, abs(j-dim(coltab2)[1]-1)+0.5, col="black", border=NA)
    }
    if(!is.na(coltab2[i,j])){
      rect(i-0.5, abs(j-dim(coltab2)[1]-1)-0.5, i+0.5, abs(j-dim(coltab2)[1]-1)+0.5, col=coltab2[i,j], border=NA)
    }
  }
}
abline(v=6.5)
abline(v=12.5)
abline(v=17.5)
abline(h=16.5)
abline(h=10.5)
abline(h=5.5)
axis(2, at=seq(0, dim(coltab2)[1]+1, by=1), labels=seq(dim(coltab2)[1]+1, 0, by=-1), las=1, hadj=.6, cex.axis=0.75)
axis(3, at=seq(0, dim(coltab2)[1]+1, by=1), labels=seq(0, dim(coltab2)[1]+1, by=1), padj=.6, cex.axis=0.75)
par(mar=c(0.5, 0, 0.5, 0.5), fig=c(0.66, 1, 0, 1), new=T)

plot(1, 1, type="n", axes=F, main='', xlab='', ylab='', xlim=c(0, 1), ylim=c(0, 1), xaxs='i', yaxs='i')
rect(0, 0, 1, 1, col='grey90')
text(0.5, seq(1, 0, length.out=length(names2)+7)[2]+.01, "Study 2", font=4, cex=1)
text(0, seq(1, 0, length.out=length(names2)+7)[3], "Eye Movement Measures", font=2, cex=0.85, pos=4)
text(0.02, seq(1, 0, length.out=length(names2)+7)[4:9], c(1:6), font=3, cex=0.65, pos=4)
text(0.1, seq(1, 0, length.out=length(names2)+7)[4:9], names2[1:6], font=3, cex=0.65, pos=4)
text(0, seq(1, 0, length.out=length(names2)+7)[10], "Ballot Characteristics", font=2, cex=0.85, pos=4)
text(0.02, seq(1, 0, length.out=length(names2)+7)[11:16], c(7:12), font=3, cex=0.65, pos=4)
text(0.1, seq(1, 0, length.out=length(names2)+7)[11:16], names2[7:12], font=3, cex=0.65, pos=4)
text(0, seq(1, 0, length.out=length(names2)+7)[17], "Participant Characteristics", font=2, cex=0.85, pos=4)
text(0.02, seq(1, 0, length.out=length(names2)+7)[18:22], c(13:17), font=3, cex=0.65, pos=4)
text(0.1, seq(1, 0, length.out=length(names2)+7)[18:22], names2[13:17], font=3, cex=0.65, pos=4)
text(0, seq(1, 0, length.out=length(names2)+7)[23], "Additional Variables", font=2, cex=0.85, pos=4)
text(0.02, seq(1, 0, length.out=length(names2)+7)[24:28], c(18:22), font=3, cex=0.65, pos=4)
text(0.1, seq(1, 0, length.out=length(names2)+7)[24:28], names2[18:22], font=3, cex=0.65, pos=4)
par(mar=c(0.5, 0.5, 0, 0.5), fig=c(0, 0.66, 0, 0.1), new=T)

plot(1, 1, type="n", axes=F, main='', xlab='', ylab='', xlim=c(0, 1), ylim=c(0, 1), xaxs='i', yaxs='i')
#rect(0, 0, 1, 1, col='white')
segments(0.01, 0.3, 0.99, 0.3)
segments(seq(0.01, 0.99, length.out=9), 0.25, seq(0.01, 0.99, length.out=9), 0.3)
segments(seq(0.01, 0.5, length.out=1000), 0.35, seq(0.01, 0.5, length.out=1000), 0.5, col=rev(negpal(1000)))
segments(seq(0.5, 0.99, length.out=1000), 0.35, seq(0.5, 0.99, length.out=1000), 0.5, col=pospal(1000))
text(seq(0.01, 0.99, length.out=9)[c(1, 3, 5, 7, 9)], 0.125, c("-1", "-.5", "0", ".5", "1"), cex=0.5, xpd=T, font=2)
text(0.5, 0.8, "Correlation (r)", cex=1, xpd=T, font=4)
dev.off()

###Mixed-effects regressions used for Figures 1 and 2###

###[SET 1] Real-world postdiction (study 1) abstention vote analyses with preregistered variables####

model1 <- lm_robust(ln_perc_abstensions ~ body_average_first_fix_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- data.frame(Model_Name="Model 1", Study=1, DV="Percent Abstain ln", IV="Avg. First Fixation Duration (sd)", Controls="Preregistered", Beta=model1$coefficients[2], SE_Lower=model1$coefficients[2]-model1$std.error[2], SE_Upper=model1$coefficients[2]+model1$std.error[2], CI_Lower=model1$conf.low[2], CI_Upper=model1$conf.high[2])

model2 <- lm_robust(ln_perc_abstensions ~ body_average_first_pass_fix_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 2", Study=1, DV="Percent Abstain ln", IV="Avg. First Pass Fixations (sd)", Controls="Preregistered", Beta=model2$coefficients[2], SE_Lower=model2$coefficients[2]-model2$std.error[2], SE_Upper=model2$coefficients[2]+model2$std.error[2], CI_Lower=model2$conf.low[2], CI_Upper=model2$conf.high[2]))

model3 <- lm_robust(ln_perc_abstensions ~ body_average_first_pass_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 3", Study=1, DV="Percent Abstain ln", IV="Avg. First Pass Fixation Duration (sd)", Controls="Preregistered", Beta=model3$coefficients[2], SE_Lower=model3$coefficients[2]-model3$std.error[2], SE_Upper=model3$coefficients[2]+model3$std.error[2], CI_Lower=model3$conf.low[2], CI_Upper=model3$conf.high[2]))

model4 <- lm_robust(ln_perc_abstensions ~ body_average_regression_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 4", Study=1, DV="Percent Abstain ln", IV="Avg. Regression Fixations (sd)", Controls="Preregistered", Beta=model4$coefficients[2], SE_Lower=model4$coefficients[2]-model4$std.error[2], SE_Upper=model4$coefficients[2]+model4$std.error[2], CI_Lower=model4$conf.low[2], CI_Upper=model4$conf.high[2]))

model5 <- lm_robust(ln_perc_abstensions ~ body_average_fix_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 5", Study=1, DV="Percent Abstain ln", IV="Avg. Total Fixations (sd)", Controls="Preregistered", Beta=model5$coefficients[2], SE_Lower=model5$coefficients[2]-model5$std.error[2], SE_Upper=model5$coefficients[2]+model5$std.error[2], CI_Lower=model5$conf.low[2], CI_Upper=model5$conf.high[2]))

model6 <- lm_robust(ln_perc_abstensions ~ body_average_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 6", Study=1, DV="Percent Abstain ln", IV="Avg. Total Fixation Duration (sd)", Controls="Preregistered", Beta=model6$coefficients[2], SE_Lower=model6$coefficients[2]-model6$std.error[2], SE_Upper=model6$coefficients[2]+model6$std.error[2], CI_Lower=model6$conf.low[2], CI_Upper=model6$conf.high[2]))

###[SET 2] Real-world prediction (study 2) abstention vote analyses with preregistered variables####

model7 <- lm_robust(ln_perc_abstensions ~ body_average_first_fix_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 7", Study=2, DV="Percent Abstain ln", IV="Avg. First Fixation Duration (sd)", Controls="Preregistered", Beta=model7$coefficients[2], SE_Lower=model7$coefficients[2]-model7$std.error[2], SE_Upper=model7$coefficients[2]+model7$std.error[2], CI_Lower=model7$conf.low[2], CI_Upper=model7$conf.high[2]))

model8 <- lm_robust(ln_perc_abstensions ~ body_average_first_pass_fix_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 8", Study=2, DV="Percent Abstain ln", IV="Avg. First Pass Fixations (sd)", Controls="Preregistered", Beta=model8$coefficients[2], SE_Lower=model8$coefficients[2]-model8$std.error[2], SE_Upper=model8$coefficients[2]+model8$std.error[2], CI_Lower=model8$conf.low[2], CI_Upper=model8$conf.high[2]))

model9 <- lm_robust(ln_perc_abstensions ~ body_average_first_pass_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 9", Study=2, DV="Percent Abstain ln", IV="Avg. First Pass Fixation Duration (sd)", Controls="Preregistered", Beta=model9$coefficients[2], SE_Lower=model9$coefficients[2]-model9$std.error[2], SE_Upper=model9$coefficients[2]+model9$std.error[2], CI_Lower=model9$conf.low[2], CI_Upper=model9$conf.high[2]))

model10 <- lm_robust(ln_perc_abstensions ~ body_average_regression_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 10", Study=2, DV="Percent Abstain ln", IV="Avg. Regression Fixations (sd)", Controls="Preregistered", Beta=model10$coefficients[2], SE_Lower=model10$coefficients[2]-model10$std.error[2], SE_Upper=model10$coefficients[2]+model10$std.error[2], CI_Lower=model10$conf.low[2], CI_Upper=model10$conf.high[2]))

model11 <- lm_robust(ln_perc_abstensions ~ body_average_fix_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 11", Study=2, DV="Percent Abstain ln", IV="Avg. Total Fixations (sd)", Controls="Preregistered", Beta=model11$coefficients[2], SE_Lower=model11$coefficients[2]-model11$std.error[2], SE_Upper=model11$coefficients[2]+model11$std.error[2], CI_Lower=model11$conf.low[2], CI_Upper=model11$conf.high[2]))

model12 <- lm_robust(ln_perc_abstensions ~ body_average_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 12", Study=2, DV="Percent Abstain ln", IV="Avg. Total Fixation Duration (sd)", Controls="Preregistered", Beta=model12$coefficients[2], SE_Lower=model12$coefficients[2]-model12$std.error[2], SE_Upper=model12$coefficients[2]+model12$std.error[2], CI_Lower=model12$conf.low[2], CI_Upper=model12$conf.high[2]))

###[SET 3] Real-world postdiction (study 1) directional vote analyses with preregistered variables####

model13 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_fix_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 13", Study=1, DV="Percent Oppose", IV="Avg. First Fixation Duration (sd)", Controls="Preregistered", Beta=model13$coefficients[2], SE_Lower=model13$coefficients[2]-model13$std.error[2], SE_Upper=model13$coefficients[2]+model13$std.error[2], CI_Lower=model13$conf.low[2], CI_Upper=model13$conf.high[2]))

model14 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_pass_fix_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 14", Study=1, DV="Percent Oppose", IV="Avg. First Pass Fixations (sd)", Controls="Preregistered", Beta=model14$coefficients[2], SE_Lower=model14$coefficients[2]-model14$std.error[2], SE_Upper=model14$coefficients[2]+model14$std.error[2], CI_Lower=model14$conf.low[2], CI_Upper=model14$conf.high[2]))

model15 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_pass_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 15", Study=1, DV="Percent Oppose", IV="Avg. First Pass Fixation Duration (sd)", Controls="Preregistered", Beta=model15$coefficients[2], SE_Lower=model15$coefficients[2]-model15$std.error[2], SE_Upper=model15$coefficients[2]+model15$std.error[2], CI_Lower=model15$conf.low[2], CI_Upper=model15$conf.high[2]))

model16 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_regression_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 16", Study=1, DV="Percent Oppose", IV="Avg. Regression Fixations (sd)", Controls="Preregistered", Beta=model16$coefficients[2], SE_Lower=model16$coefficients[2]-model16$std.error[2], SE_Upper=model16$coefficients[2]+model16$std.error[2], CI_Lower=model16$conf.low[2], CI_Upper=model16$conf.high[2]))

model17 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_fix_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 17", Study=1, DV="Percent Oppose", IV="Avg. Total Fixations (sd)", Controls="Preregistered", Beta=model17$coefficients[2], SE_Lower=model17$coefficients[2]-model17$std.error[2], SE_Upper=model17$coefficients[2]+model17$std.error[2], CI_Lower=model17$conf.low[2], CI_Upper=model17$conf.high[2]))

model18 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 18", Study=1, DV="Percent Oppose", IV="Avg. Total Fixation Duration (sd)", Controls="Preregistered", Beta=model18$coefficients[2], SE_Lower=model18$coefficients[2]-model18$std.error[2], SE_Upper=model18$coefficients[2]+model18$std.error[2], CI_Lower=model18$conf.low[2], CI_Upper=model18$conf.high[2]))

###[SET 4] Real-world prediction (study 2) directional vote analyses with preregistered variables####

model19 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_fix_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 19", Study=2, DV="Percent Oppose", IV="Avg. First Fixation Duration (sd)", Controls="Preregistered", Beta=model19$coefficients[2], SE_Lower=model19$coefficients[2]-model19$std.error[2], SE_Upper=model19$coefficients[2]+model19$std.error[2], CI_Lower=model19$conf.low[2], CI_Upper=model19$conf.high[2]))

model20 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_pass_fix_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 20", Study=2, DV="Percent Oppose", IV="Avg. First Pass Fixations (sd)", Controls="Preregistered", Beta=model20$coefficients[2], SE_Lower=model20$coefficients[2]-model20$std.error[2], SE_Upper=model20$coefficients[2]+model20$std.error[2], CI_Lower=model20$conf.low[2], CI_Upper=model20$conf.high[2]))

model21 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_pass_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 21", Study=2, DV="Percent Oppose", IV="Avg. First Pass Fixation Duration (sd)", Controls="Preregistered", Beta=model21$coefficients[2], SE_Lower=model21$coefficients[2]-model21$std.error[2], SE_Upper=model21$coefficients[2]+model21$std.error[2], CI_Lower=model21$conf.low[2], CI_Upper=model21$conf.high[2]))

model22 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_regression_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 22", Study=2, DV="Percent Oppose", IV="Avg. Regression Fixations (sd)", Controls="Preregistered", Beta=model22$coefficients[2], SE_Lower=model22$coefficients[2]-model22$std.error[2], SE_Upper=model22$coefficients[2]+model22$std.error[2], CI_Lower=model22$conf.low[2], CI_Upper=model22$conf.high[2]))

model23 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_fix_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 23", Study=2, DV="Percent Oppose", IV="Avg. Total Fixations (sd)", Controls="Preregistered", Beta=model23$coefficients[2], SE_Lower=model23$coefficients[2]-model23$std.error[2], SE_Upper=model23$coefficients[2]+model23$std.error[2], CI_Lower=model23$conf.low[2], CI_Upper=model23$conf.high[2]))

model24 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_duration_SD + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 24", Study=2, DV="Percent Oppose", IV="Avg. Total Fixation Duration (sd)", Controls="Preregistered", Beta=model24$coefficients[2], SE_Lower=model24$coefficients[2]-model24$std.error[2], SE_Upper=model24$coefficients[2]+model24$std.error[2], CI_Lower=model24$conf.low[2], CI_Upper=model24$conf.high[2]))

###[SET 5] Real-world postdiction (study 1) abstention vote analyses with preregistered variables and selection criteria deviation variables####

model25 <- lm_robust(ln_perc_abstensions ~ body_average_first_fix_duration_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 25", Study=1, DV="Percent Abstain ln", IV="Avg. First Fixation Duration (sd)", Controls="Added Covariates", Beta=model25$coefficients[2], SE_Lower=model25$coefficients[2]-model25$std.error[2], SE_Upper=model25$coefficients[2]+model25$std.error[2], CI_Lower=model25$conf.low[2], CI_Upper=model25$conf.high[2]))

model26 <- lm_robust(ln_perc_abstensions ~ body_average_first_pass_fix_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 26", Study=1, DV="Percent Abstain ln", IV="Avg. First Pass Fixations (sd)", Controls="Added Covariates", Beta=model26$coefficients[2], SE_Lower=model26$coefficients[2]-model26$std.error[2], SE_Upper=model26$coefficients[2]+model26$std.error[2], CI_Lower=model26$conf.low[2], CI_Upper=model26$conf.high[2]))

model27 <- lm_robust(ln_perc_abstensions ~ body_average_first_pass_duration_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 27", Study=1, DV="Percent Abstain ln", IV="Avg. First Pass Fixation Duration (sd)", Controls="Added Covariates", Beta=model27$coefficients[2], SE_Lower=model27$coefficients[2]-model27$std.error[2], SE_Upper=model27$coefficients[2]+model27$std.error[2], CI_Lower=model27$conf.low[2], CI_Upper=model27$conf.high[2]))

model28 <- lm_robust(ln_perc_abstensions ~ body_average_regression_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 28", Study=1, DV="Percent Abstain ln", IV="Avg. Regression Fixations (sd)", Controls="Added Covariates", Beta=model28$coefficients[2], SE_Lower=model28$coefficients[2]-model28$std.error[2], SE_Upper=model28$coefficients[2]+model28$std.error[2], CI_Lower=model28$conf.low[2], CI_Upper=model28$conf.high[2]))

model29 <- lm_robust(ln_perc_abstensions ~ body_average_fix_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 29", Study=1, DV="Percent Abstain ln", IV="Avg. Total Fixations (sd)", Controls="Added Covariates", Beta=model29$coefficients[2], SE_Lower=model29$coefficients[2]-model29$std.error[2], SE_Upper=model29$coefficients[2]+model29$std.error[2], CI_Lower=model29$conf.low[2], CI_Upper=model29$conf.high[2]))

model30 <- lm_robust(ln_perc_abstensions ~ body_average_duration_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 30", Study=1, DV="Percent Abstain ln", IV="Avg. Total Fixation Duration (sd)", Controls="Added Covariates", Beta=model30$coefficients[2], SE_Lower=model30$coefficients[2]-model30$std.error[2], SE_Upper=model30$coefficients[2]+model30$std.error[2], CI_Lower=model30$conf.low[2], CI_Upper=model30$conf.high[2]))

###[SET 6] Real-world prediction (study 2) abstention vote analyses with preregistered variables and selection criteria deviation variables###

model31 <- lm_robust(ln_perc_abstensions ~ body_average_first_fix_duration_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 31", Study=2, DV="Percent Abstain ln", IV="Avg. First Fixation Duration (sd)", Controls="Added Covariates", Beta=model31$coefficients[2], SE_Lower=model31$coefficients[2]-model31$std.error[2], SE_Upper=model31$coefficients[2]+model31$std.error[2], CI_Lower=model31$conf.low[2], CI_Upper=model31$conf.high[2]))

model32 <- lm_robust(ln_perc_abstensions ~ body_average_first_pass_fix_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 32", Study=2, DV="Percent Abstain ln", IV="Avg. First Pass Fixations (sd)", Controls="Added Covariates", Beta=model32$coefficients[2], SE_Lower=model32$coefficients[2]-model32$std.error[2], SE_Upper=model32$coefficients[2]+model32$std.error[2], CI_Lower=model32$conf.low[2], CI_Upper=model32$conf.high[2]))

model33 <- lm_robust(ln_perc_abstensions ~ body_average_first_pass_duration_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 33", Study=2, DV="Percent Abstain ln", IV="Avg. First Pass Fixation Duration (sd)", Controls="Added Covariates", Beta=model33$coefficients[2], SE_Lower=model33$coefficients[2]-model33$std.error[2], SE_Upper=model33$coefficients[2]+model33$std.error[2], CI_Lower=model33$conf.low[2], CI_Upper=model33$conf.high[2]))

model34 <- lm_robust(ln_perc_abstensions ~ body_average_regression_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 34", Study=2, DV="Percent Abstain ln", IV="Avg. Regression Fixations (sd)", Controls="Added Covariates", Beta=model34$coefficients[2], SE_Lower=model34$coefficients[2]-model34$std.error[2], SE_Upper=model34$coefficients[2]+model34$std.error[2], CI_Lower=model34$conf.low[2], CI_Upper=model34$conf.high[2]))

model35 <- lm_robust(ln_perc_abstensions ~ body_average_fix_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 35", Study=2, DV="Percent Abstain ln", IV="Avg. Total Fixations (sd)", Controls="Added Covariates", Beta=model35$coefficients[2], SE_Lower=model35$coefficients[2]-model35$std.error[2], SE_Upper=model35$coefficients[2]+model35$std.error[2], CI_Lower=model35$conf.low[2], CI_Upper=model35$conf.high[2]))

model36 <- lm_robust(ln_perc_abstensions ~ body_average_duration_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 36", Study=2, DV="Percent Abstain ln", IV="Avg. Total Fixation Duration (sd)", Controls="Added Covariates", Beta=model36$coefficients[2], SE_Lower=model36$coefficients[2]-model36$std.error[2], SE_Upper=model36$coefficients[2]+model36$std.error[2], CI_Lower=model36$conf.low[2], CI_Upper=model36$conf.high[2]))

###[SET 7] Real-world postdiction (study 1) directional vote analyses with preregistered variables and selection criteria deviation variables###

model37 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_fix_duration_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 37", Study=1, DV="Percent Oppose", IV="Avg. First Fixation Duration (sd)", Controls="Added Covariates", Beta=model37$coefficients[2], SE_Lower=model37$coefficients[2]-model37$std.error[2], SE_Upper=model37$coefficients[2]+model37$std.error[2], CI_Lower=model37$conf.low[2], CI_Upper=model37$conf.high[2]))

model38 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_pass_fix_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 38", Study=1, DV="Percent Oppose", IV="Avg. First Pass Fixations (sd)", Controls="Added Covariates", Beta=model38$coefficients[2], SE_Lower=model38$coefficients[2]-model38$std.error[2], SE_Upper=model38$coefficients[2]+model38$std.error[2], CI_Lower=model38$conf.low[2], CI_Upper=model38$conf.high[2]))

model39 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_pass_duration_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 39", Study=1, DV="Percent Oppose", IV="Avg. First Pass Fixation Duration (sd)", Controls="Added Covariates", Beta=model39$coefficients[2], SE_Lower=model39$coefficients[2]-model39$std.error[2], SE_Upper=model39$coefficients[2]+model39$std.error[2], CI_Lower=model39$conf.low[2], CI_Upper=model39$conf.high[2]))

model40 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_regression_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 40", Study=1, DV="Percent Oppose", IV="Avg. Regression Fixations (sd)", Controls="Added Covariates", Beta=model40$coefficients[2], SE_Lower=model40$coefficients[2]-model40$std.error[2], SE_Upper=model40$coefficients[2]+model40$std.error[2], CI_Lower=model40$conf.low[2], CI_Upper=model40$conf.high[2]))

model41 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_fix_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 41", Study=1, DV="Percent Oppose", IV="Avg. Total Fixations (sd)", Controls="Added Covariates", Beta=model41$coefficients[2], SE_Lower=model41$coefficients[2]-model41$std.error[2], SE_Upper=model41$coefficients[2]+model41$std.error[2], CI_Lower=model41$conf.low[2], CI_Upper=model41$conf.high[2]))

model42 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_duration_SD + eds_total + ballot_info_addition + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study1_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 42", Study=1, DV="Percent Oppose", IV="Avg. Total Fixation Duration (sd)", Controls="Added Covariates", Beta=model42$coefficients[2], SE_Lower=model42$coefficients[2]-model42$std.error[2], SE_Upper=model42$coefficients[2]+model42$std.error[2], CI_Lower=model42$conf.low[2], CI_Upper=model42$conf.high[2]))

###[SET 8] Real world prediction directional vote analyses with preregistered variables and selection criteria deviation variables###

model43 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_fix_duration_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 43", Study=2, DV="Percent Oppose", IV="Avg. First Fixation Duration (sd)", Controls="Added Covariates", Beta=model43$coefficients[2], SE_Lower=model43$coefficients[2]-model43$std.error[2], SE_Upper=model43$coefficients[2]+model43$std.error[2], CI_Lower=model43$conf.low[2], CI_Upper=model43$conf.high[2]))

model44 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_pass_fix_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 44", Study=2, DV="Percent Oppose", IV="Avg. First Pass Fixations (sd)", Controls="Added Covariates", Beta=model44$coefficients[2], SE_Lower=model44$coefficients[2]-model44$std.error[2], SE_Upper=model44$coefficients[2]+model44$std.error[2], CI_Lower=model44$conf.low[2], CI_Upper=model44$conf.high[2]))

model45 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_first_pass_duration_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 45", Study=2, DV="Percent Oppose", IV="Avg. First Pass Fixation Duration (sd)", Controls="Added Covariates", Beta=model45$coefficients[2], SE_Lower=model45$coefficients[2]-model45$std.error[2], SE_Upper=model45$coefficients[2]+model45$std.error[2], CI_Lower=model45$conf.low[2], CI_Upper=model45$conf.high[2]))

model46 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_regression_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 46", Study=2, DV="Percent Oppose", IV="Avg. Regression Fixations (sd)", Controls="Added Covariates", Beta=model46$coefficients[2], SE_Lower=model46$coefficients[2]-model46$std.error[2], SE_Upper=model46$coefficients[2]+model46$std.error[2], CI_Lower=model46$conf.low[2], CI_Upper=model46$conf.high[2]))

model47 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_fix_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 47", Study=2, DV="Percent Oppose", IV="Avg. Total Fixations (sd)", Controls="Added Covariates", Beta=model47$coefficients[2], SE_Lower=model47$coefficients[2]-model47$std.error[2], SE_Upper=model47$coefficients[2]+model47$std.error[2], CI_Lower=model47$conf.low[2], CI_Upper=model47$conf.high[2]))

model48 <- lm_robust(no_vote_perc_over_sub_votes ~ body_average_duration_SD + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, data = study2_valid_trials_only_votes, clusters = subj_id, se_type = "stata")
effects <- bind_rows(effects, data.frame(Model_Name="Model 48", Study=2, DV="Percent Oppose", IV="Avg. Total Fixation Duration (sd)", Controls="Added Covariates", Beta=model48$coefficients[2], SE_Lower=model48$coefficients[2]-model48$std.error[2], SE_Upper=model48$coefficients[2]+model48$std.error[2], CI_Lower=model48$conf.low[2], CI_Upper=model48$conf.high[2]))

#setEPS()
#postscript("PointEstimates_Study1.eps", width=13, height=16)
pdf("Figure_1.pdf", width=13, height=16)
layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2), nrow=13, ncol=1, byrow=T))
par(mar=c(8, 10.5, 6, 1.8))

plot(0, 0, type='n', xlab="", ylab="", main="", axes=F, xlim=c(0.8, 6.2), ylim=c(-0.04, 0.04))
rect(0.5, -1, 3.5, 1, col='grey90', border=NA)
rect(3.5, -1, 6.5, 1, col='grey95', border=NA)
axis(2, at=seq(-0.05, 0.05, by=0.01), labels=c("-5.00%", "-4.00%", "-3.00%", "-2.00%", "-1.00%", "0.00%", "1.00%", "2.00%", "3.00%", "4.00%", "5.00%"), cex.axis=1.8, font.axis=2, las=2)
text(0, 0, "Election Abstention Rate (ln)", font=4, cex=3, xpd=T, srt=90)
text(0, 0.04, "A", font=2, cex=4, xpd=T)
text(3.2, 0.04878049, "Study 1", font=2, cex=4.5, xpd=T)
abline(h=0, lty=2, lwd=1.5)
segments(1:6, -0.043, 1:6, -0.045, xpd=T)
text(1:6, -0.04829269, c("Avg. First\nFixation Duration", "Avg. First\nPass Fixations", "Avg. First Pass\nFixation Duration", "Avg. Regression\nFixations", "Avg. Total\nFixations", "Avg. Total\nFixation Duration"), font=4, cex=1.75, xpd=T)
segments(0.55, -0.05219512, 3.45, -0.05219512, xpd=T)
segments(0.55, -0.05024389, 0.55, -0.05219512, xpd=T)
segments(3.45, -0.05024389, 3.45, -0.05219512, xpd=T)
text(2, -0.05463413, "early-stage (in-lab)", xpd=T, cex=1.8)
segments(3.55, -0.05219512, 6.45, -0.05219512, xpd=T)
segments(3.55, -0.05024389, 3.55, -0.05219512, xpd=T)
segments(6.45, -0.05024389, 6.45, -0.05219512, xpd=T)
text(5, -0.05463413, "late-stage (in-lab)", xpd=T, cex=1.8)
segments((1:6)-0.1, effects$CI_Lower[c(1:6)], (1:6)-0.1, effects$CI_Upper[c(1:6)], col="firebrick2", lwd=3.5, xpd=T)
segments((1:6)-0.1, effects$SE_Lower[c(1:6)], (1:6)-0.1, effects$SE_Upper[c(1:6)], col="firebrick2", lwd=8, xpd=T)
points((1:6)-0.1, effects$Beta[c(1:6)], col="firebrick2", pch=16, cex=5)
segments((1:6)+0.1, effects$CI_Lower[c(25:30)], (1:6)+0.1, effects$CI_Upper[c(25:30)], col="steelblue3", lwd=3.5, xpd=T)
segments((1:6)+0.1, effects$SE_Lower[c(25:30)], (1:6)+0.1, effects$SE_Upper[c(25:30)], col="steelblue3", lwd=8, xpd=T)
points((1:6)+0.1, effects$Beta[c(25:30)], col="steelblue3", pch=18, cex=5)
rect(1.1, -0.0215, 3.105, -0.04, col='white')
text(mean(c(1.1, 3.105)), -0.025, "Model", font=2, cex=2.5)
points(1.24, -0.03, pch=16, col="firebrick2", cex=5)
text(1.3, -0.0305, "Preregistered", pos=4, cex=2)
points(1.24, -0.035, pch=18, col="steelblue3", cex=5)
text(1.3, -0.0355, "Preregistered + Added Covariates", pos=4, cex=2)

par(mar=c(8, 10.5, 0, 1.8))
plot(0, 0, type='n', xlab="", ylab="", main="", axes=F, xlim=c(0.8, 6.2), ylim=c(-0.015, 0.015))
rect(0.5, -1, 3.5, 1, col='grey90', border=NA)
rect(3.5, -1, 6.5, 1, col='grey95', border=NA)
axis(2, at=seq(-0.0175, 0.0175, by=0.0025), labels=c("", "-1.50%", "", "-1.00%", "", "-0.50%", "", "0.00%", "", "0.50%", "", "1.00%", "", "1.50%", ""), cex.axis=1.8, font.axis=2, las=2)
text(0, 0, "Election Opposition Rate", font=4, cex=3, xpd=T, srt=90)
text(0, 0.015, "B", font=2, cex=4, xpd=T)
abline(h=0, lty=2, lwd=1.5)
segments(1:6, -0.016125, 1:6, -0.016875, xpd=T)
text(1:6, -0.01810976, c("Avg. First\nFixation Duration", "Avg. First\nPass Fixations", "Avg. First Pass\nFixation Duration", "Avg. Regression\nFixations", "Avg. Total\nFixations", "Avg. Total\nFixation Duration"), font=4, cex=1.75, xpd=T)
segments(0.55, -0.01957317, 3.45, -0.01957317, xpd=T)
segments(0.55, -0.01884146, 0.55, -0.01957317, xpd=T)
segments(3.45, -0.01884146, 3.45, -0.01957317, xpd=T)
text(2, -0.0204878, "early-stage (in-lab)", xpd=T, cex=1.8)
segments(3.55, -0.01957317, 6.45, -0.01957317, xpd=T)
segments(3.55, -0.01884146, 3.55, -0.01957317, xpd=T)
segments(6.45, -0.01884146, 6.45, -0.01957317, xpd=T)
text(5, -0.0204878, "late-stage (in-lab)", xpd=T, cex=1.8)
segments((1:6)-0.1, effects$CI_Lower[c(13:18)], (1:6)-0.1, effects$CI_Upper[c(13:18)], col="firebrick2", lwd=3.5, xpd=T)
segments((1:6)-0.1, effects$SE_Lower[c(13:18)], (1:6)-0.1, effects$SE_Upper[c(13:18)], col="firebrick2", lwd=8, xpd=T)
points((1:6)-0.1, effects$Beta[c(13:18)], col="firebrick2", pch=16, cex=5)
segments((1:6)+0.1, effects$CI_Lower[c(37:42)], (1:6)+0.1, effects$CI_Upper[c(37:42)], col="steelblue3", lwd=3.5, xpd=T)
segments((1:6)+0.1, effects$SE_Lower[c(37:42)], (1:6)+0.1, effects$SE_Upper[c(37:42)], col="steelblue3", lwd=8, xpd=T)
points((1:6)+0.1, effects$Beta[c(37:42)], col="steelblue3", pch=18, cex=5)
rect(1.1, -0.0080625, 3.105, -.015, col='white')
text(mean(c(1.1, 3.105)), -0.009375, "Model", font=2, cex=2.5)
points(1.24, -0.01125, pch=16, col="firebrick2", cex=5)
text(1.3, -0.0114375, "Preregistered", pos=4, cex=2)
points(1.24, -0.013125, pch=18, col="steelblue3", cex=5)
text(1.3, -0.0133125, "Preregistered + Added Covariates", pos=4, cex=2)
dev.off()

#postscript("PointEstimates_Study2.eps", width=13, height=16)
pdf("Figure_2.pdf", width=13, height=16)
layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2), nrow=13, ncol=1, byrow=T))
par(mar=c(8, 10.5, 6, 1.8))

plot(0, 0, type='n', xlab="", ylab="", main="", axes=F, xlim=c(0.8, 6.2), ylim=c(-0.04, 0.04))
rect(0.5, -1, 3.5, 1, col='grey90', border=NA)
rect(3.5, -1, 6.5, 1, col='grey95', border=NA)
axis(2, at=seq(-0.05, 0.05, by=0.01), labels=c("-5.00%", "-4.00%", "-3.00%", "-2.00%", "-1.00%", "0.00%", "1.00%", "2.00%", "3.00%", "4.00%", "5.00%"), cex.axis=1.8, font.axis=2, las=2)
text(0, 0, "Election Abstention Rate (ln)", font=4, cex=3, xpd=T, srt=90)
text(0, 0.04, "A", font=2, cex=4, xpd=T)
text(3.2, 0.04878049, "Study 2", font=2, cex=4.5, xpd=T)
abline(h=0, lty=2, lwd=1.5)
segments(1:6, -0.043, 1:6, -0.045, xpd=T)
text(1:6, -0.04829269, c("Avg. First\nFixation Duration", "Avg. First\nPass Fixations", "Avg. First Pass\nFixation Duration", "Avg. Regression\nFixations", "Avg. Total\nFixations", "Avg. Total\nFixation Duration"), font=4, cex=1.75, xpd=T)
segments(0.55, -0.05219512, 3.45, -0.05219512, xpd=T)
segments(0.55, -0.05024389, 0.55, -0.05219512, xpd=T)
segments(3.45, -0.05024389, 3.45, -0.05219512, xpd=T)
text(2, -0.05463413, "early-stage (in-lab)", xpd=T, cex=1.8)
segments(3.55, -0.05219512, 6.45, -0.05219512, xpd=T)
segments(3.55, -0.05024389, 3.55, -0.05219512, xpd=T)
segments(6.45, -0.05024389, 6.45, -0.05219512, xpd=T)
text(5, -0.05463413, "late-stage (in-lab)", xpd=T, cex=1.8)
segments((1:6)-0.1, effects$CI_Lower[c(7:12)], (1:6)-0.1, effects$CI_Upper[c(7:12)], col="firebrick2", lwd=3.5, xpd=T)
segments((1:6)-0.1, effects$SE_Lower[c(7:12)], (1:6)-0.1, effects$SE_Upper[c(7:12)], col="firebrick2", lwd=8, xpd=T)
points((1:6)-0.1, effects$Beta[c(7:12)], col="firebrick2", pch=16, cex=5)
segments((1:6)+0.1, effects$CI_Lower[c(31:36)], (1:6)+0.1, effects$CI_Upper[c(31:36)], col="steelblue3", lwd=3.5, xpd=T)
segments((1:6)+0.1, effects$SE_Lower[c(31:36)], (1:6)+0.1, effects$SE_Upper[c(31:36)], col="steelblue3", lwd=8, xpd=T)
points((1:6)+0.1, effects$Beta[c(31:36)], col="steelblue3", pch=18, cex=5)
rect(1.1, -0.0215, 3.105, -0.04, col='white')
text(mean(c(1.1, 3.105)), -0.025, "Model", font=2, cex=2.5)
points(1.24, -0.03, pch=16, col="firebrick2", cex=5)
text(1.3, -0.0305, "Preregistered", pos=4, cex=2)
points(1.24, -0.035, pch=18, col="steelblue3", cex=5)
text(1.3, -0.0355, "Preregistered + Added Covariates", pos=4, cex=2)

par(mar=c(8, 10.5, 0, 1.8))
plot(0, 0, type='n', xlab="", ylab="", main="", axes=F, xlim=c(0.8, 6.2), ylim=c(-0.015, 0.015))
rect(0.5, -1, 3.5, 1, col='grey90', border=NA)
rect(3.5, -1, 6.5, 1, col='grey95', border=NA)
axis(2, at=seq(-0.0175, 0.0175, by=0.0025), labels=c("", "-1.50%", "", "-1.00%", "", "-0.50%", "", "0.00%", "", "0.50%", "", "1.00%", "", "1.50%", ""), cex.axis=1.8, font.axis=2, las=2)
text(0, 0, "Election Opposition Rate", font=4, cex=3, xpd=T, srt=90)
text(0, 0.015, "B", font=2, cex=4, xpd=T)
abline(h=0, lty=2, lwd=1.5)
segments(1:6, -0.016125, 1:6, -0.016875, xpd=T)
text(1:6, -0.01810976, c("Avg. First\nFixation Duration", "Avg. First\nPass Fixations", "Avg. First Pass\nFixation Duration", "Avg. Regression\nFixations", "Avg. Total\nFixations", "Avg. Total\nFixation Duration"), font=4, cex=1.75, xpd=T)
segments(0.55, -0.01957317, 3.45, -0.01957317, xpd=T)
segments(0.55, -0.01884146, 0.55, -0.01957317, xpd=T)
segments(3.45, -0.01884146, 3.45, -0.01957317, xpd=T)
text(2, -0.0204878, "early-stage (in-lab)", xpd=T, cex=1.8)
segments(3.55, -0.01957317, 6.45, -0.01957317, xpd=T)
segments(3.55, -0.01884146, 3.55, -0.01957317, xpd=T)
segments(6.45, -0.01884146, 6.45, -0.01957317, xpd=T)
text(5, -0.0204878, "late-stage (in-lab)", xpd=T, cex=1.8)
segments((1:6)-0.1, effects$CI_Lower[c(19:24)], (1:6)-0.1, effects$CI_Upper[c(19:24)], col="firebrick2", lwd=3.5, xpd=T)
segments((1:6)-0.1, effects$SE_Lower[c(19:24)], (1:6)-0.1, effects$SE_Upper[c(19:24)], col="firebrick2", lwd=8, xpd=T)
points((1:6)-0.1, effects$Beta[c(19:24)], col="firebrick2", pch=16, cex=5)
segments((1:6)+0.1, effects$CI_Lower[c(43:48)], (1:6)+0.1, effects$CI_Upper[c(43:48)], col="steelblue3", lwd=3.5, xpd=T)
segments((1:6)+0.1, effects$SE_Lower[c(43:48)], (1:6)+0.1, effects$SE_Upper[c(43:48)], col="steelblue3", lwd=8, xpd=T)
points((1:6)+0.1, effects$Beta[c(43:48)], col="steelblue3", pch=18, cex=5)
rect(1.1, -0.0080625, 3.105, -.015, col='white')
text(mean(c(1.1, 3.105)), -0.009375, "Model", font=2, cex=2.5)
points(1.24, -0.01125, pch=16, col="firebrick2", cex=5)
text(1.3, -0.0114375, "Preregistered", pos=4, cex=2)
points(1.24, -0.013125, pch=18, col="steelblue3", cex=5)
text(1.3, -0.0133125, "Preregistered + Added Covariates", pos=4, cex=2)
dev.off()
