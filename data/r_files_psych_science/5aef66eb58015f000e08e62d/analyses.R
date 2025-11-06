#### Demographics ####

paste0("the final subset of ",nrow(demographics)," participants (",
       nrow(demographics[which(demographics$gender=='Female'),])," women, ",
       nrow(demographics[which(demographics$gender=='Male'),])," men, ", 
       nrow(demographics[is.na(demographics$gender),]),
       " not reported) used in the current analyses ranged in age from ",
       min(demographics$age,na.rm = T), " to ",
       max(demographics$age,na.rm = T), " years (M = ",
       round(mean(demographics$age,na.rm=T),2),
       ", SD = ", round(sd(demographics$age,na.rm=T),2),
       ") and identified as Caucasian (n = ",
       nrow(demographics[which(demographics$ethnicity=='White'),]),
       "), Asian (n = ",
       nrow(demographics[which(demographics$ethnicity=='Asian or Asian American'),]),
       "), Black (n = ",
       nrow(demographics[which(demographics$ethnicity=='Black or African American'),]),
       "), American Indian or Alaska Native (n = ",
       nrow(demographics[which(demographics$ethnicity=='American Indian or Alaska Native'),]),
       "), Other or multiple (n = ",
       nrow(demographics[which(demographics$ethnicity=='Other or Multiple'),]),
       "), or did not disclose their ethnicity (n = ",
       nrow(demographics[which(demographics$ethnicity=='Not Reported'),]),").")

#### Run Mplus Models ####
# Uncomment these two lines if you have Mplus installed,
# and wish to re-run all models:

# runModels(paste0(getwd(),'/mplus-descriptives'))
# runModels(paste0(getwd(),'/mplus-msem'))

# Otherwise, you can still read the Mplus output files
# using the code below, even if you do not have Mplus installed.

#### Read Mplus Models ####
mplusdescriptives <- readModels(paste0(getwd(),'/mplus-descriptives'))
null <- mplusdescriptives[[1]]
omegas <- list(mplusdescriptives[[2]],mplusdescriptives[[3]],
               mplusdescriptives[[4]],mplusdescriptives[[5]],
               mplusdescriptives[[6]],mplusdescriptives[[7]],
               mplusdescriptives[[8]],mplusdescriptives[[9]])

names(omegas) <- c('omega_ear_agr.out', 
                   'omega_ear_con.out', 
                   'omega_ear_ext.out', 
                   'omega_ear_neu.out', 
                   'omega_esm_agr.out', 
                   'omega_esm_con.out', 
                   'omega_esm_ext.out', 
                   'omega_esm_neu.out')

msem <- readModels(paste0(getwd(),'/mplus-msem'))

#### Table 1: Descriptive Statistics ####

# aggregate means
allvars <- c('ear.EXT.w1','ear.AGR.w1','ear.CON.w1','ear.NEU.w1',
             'esm.EXT.w1','esm.AGR.w1','esm.CON.w1','esm.NEU.w1')

agmean <- data.frame(dat[,c('ear.IDnum.w1',allvars)] %>%
                       group_by(ear.IDnum.w1) %>% 
                       summarise_all(funs(mean(., na.rm = TRUE))))

agmean <- describe(agmean)
agmean <- data.frame(agmean)
agmean$.id <- row.names(agmean)
agmean <- agmean[-1,c('.id','mean')]
agmean$type <- toupper(str_sub(agmean$.id,1,3))
agmean$.id <- str_sub(agmean$.id,5,7)
table1 <- agmean[,c('type','.id','mean')]

# M	SDWP	SDBP	1-ICC(1)	omega

variances <- null$parameters$unstandardized

# Get between-person and within-person variances
# for each EAR and ESM variable
variances <- variances[which(variances$paramHeader=='Variances'),
                       c('param','est')]

variances$level <- str_sub(variances$param,-1)
variances$type <- str_sub(variances$param,1,3)
variances$.id <- str_sub(variances$param,-2,-2)
variances$.id <- gsub('A','AGR',variances$.id)
variances$.id <- gsub('E','EXT',variances$.id)
variances$.id <- gsub('N','NEU',variances$.id)
variances$.id <- gsub('C','CON',variances$.id)

var.w <- variances[which(variances$level=='W'),c('type','.id','est')]
var.b <- variances[which(variances$level=='B'),c('type','.id','est')]
names(var.w)[3] <- 'var.w'
names(var.b)[3] <- 'var.b'

variances <- merge(var.w,var.b,by=c('type','.id'))

# calculate 1 - ICC (% of variation attributable to within-person fluctuations)
variances$ricc <- variances$var.w/(variances$var.w+variances$var.b)

# calculate SD_W & SD_B
variances$SD_W <- sqrt(variances$var.w)
variances$SD_B <- sqrt(variances$var.b)

# get omega reliabilities
omegaws <- lapply(omegas,function(x){
  x <- x$parameters$unstandardized
  x[which(x$param=='OMEGAW'),'est']
})

omegaws <- ldply(omegaws)
names(omegaws)[2] <- 'omega'
omegaws$type <- toupper(str_sub(omegaws$.id,7,9))
omegaws$.id <- toupper(str_sub(omegaws$.id,-7,-5))
omegaws <- omegaws[,c('type','.id','omega')]

table1 <- merge(table1,variances[,c('.id','type',
                                    'SD_W','SD_B',
                                    'ricc')],
                by=c('type','.id'))

table1 <- merge(table1,omegaws,by=c('type','.id'))

table1[,3:7] <- sapply(table1[,3:7],function(x){round(x,2)})

table1.esm <- table1[which(table1$type=='ESM'),]
names(table1.esm)[3:7] <- paste0(names(table1.esm[3:7]),'_ESM')
table1.esm <- table1.esm[,-1]

table1.ear <- table1[which(table1$type=='EAR'),]
names(table1.ear)[3:7] <- paste0(names(table1.ear[3:7]),'_EAR')
table1.ear <- table1.ear[,3:7]

table1 <- cbind(table1.esm,NA,table1.ear)

table1 <- table1[match(c('EXT',
                         'AGR',
                         'CON',
                         'NEU'), table1$.id),]

table1$.id <- c('Extraversion','Agreeableness',
                'Conscientiousness','Neuroticism')

write.csv(table1,'table1.csv',row.names=F,na='')

#### Table S1: MSEMs ####

# standardized estimates
msemstd <- lapply(msem,function(x){
  x$parameters$stdyx.standardized
})

msemstd <- lapply(msemstd,function(x){
  x$paramHeader <- gsub('EARA','EAR',x$paramHeader)
  x$paramHeader <- gsub('EARE','EAR',x$paramHeader)
  x$paramHeader <- gsub('EARC','EAR',x$paramHeader)
  x$paramHeader <- gsub('EARN','EAR',x$paramHeader)
  x
})

msemstd <- lapply(msemstd,function(x){
  x[which(x$paramHeader=='S|EARW.ON'),c('est','pval','lower_2.5ci','upper_2.5ci')]
})

names(msemstd) <- c('Considerate, kind',
                    'Rude',
                    'Agreeableness',
                    'Reliable',
                    'Lazy',
                    'Conscientiousness',
                    'Quiet',
                    'Outgoing, sociable',
                    'Extraversion',
                    'Worried',
                    'Relaxed',
                    'Depressed, blue',
                    'Neuroticism')

msemstd <- ldply(msemstd)

formatbs <- function(x) trimws(format(round(x, 2), nsmall=2))

msemstd$ci <- paste0('[',formatbs(msemstd$lower_2.5ci),', ',
                     formatbs(msemstd$upper_2.5ci),']')

msemstd$est <- formatbs(msemstd$est)

msemstd <- msemstd[match(c('Extraversion',
                           'Quiet',
                           'Outgoing, sociable',
                           'Agreeableness',
                           'Considerate, kind',
                           'Rude',
                           'Conscientiousness',
                           'Reliable',
                           'Lazy',
                           'Neuroticism',
                           'Worried',
                           'Relaxed',
                           'Depressed, blue'), msemstd$.id),]

# unstandardized estimates
msemraw <- lapply(msem,function(x){
  x$parameters$unstandardized
})

msemraw <- lapply(msemraw,function(x){
  x$paramHeader <- gsub('EARA','EAR',x$paramHeader)
  x$paramHeader <- gsub('EARE','EAR',x$paramHeader)
  x$paramHeader <- gsub('EARC','EAR',x$paramHeader)
  x$paramHeader <- gsub('EARN','EAR',x$paramHeader)
  x
})

msemraw <- lapply(msemraw,function(x){
  x[which(x$paramHeader=='Means' & x$param=='S'),
    c('est','pval','lower_2.5ci','upper_2.5ci')]
})

names(msemraw) <- c('Considerate, kind',
                    'Rude',
                    'Agreeableness',
                    'Reliable',
                    'Lazy',
                    'Conscientiousness',
                    'Quiet',
                    'Outgoing, sociable',
                    'Extraversion',
                    'Worried',
                    'Relaxed',
                    'Depressed, blue',
                    'Neuroticism')

msemraw <- ldply(msemraw)

msemraw$ci <- paste0('[',formatbs(msemraw$lower_2.5ci),', ',
                     formatbs(msemraw$upper_2.5ci),']')

msemraw$est <- formatbs(msemraw$est)

msemraw <- msemraw[match(c('Extraversion',
                           'Quiet',
                           'Outgoing, sociable',
                           'Agreeableness',
                           'Considerate, kind',
                           'Rude',
                           'Conscientiousness',
                           'Reliable',
                           'Lazy',
                           'Neuroticism',
                           'Worried',
                           'Relaxed',
                           'Depressed, blue'), msemraw$.id),]


# merge unstandardized & standardized estimates
unstandardized <- msemraw[,c('.id','est','ci')]
standardized <- msemstd[,c('.id','est','ci')]
names(standardized)[2:3] <- paste0(names(standardized[2:3]),'_std')

tables1 <- cbind(unstandardized,NA,standardized[,2:3])
write.csv(tables1,'tables1.csv',row.names=F,na='')

#### Table S2: Between-Person Latent Correlation Matrix ####

cormat <- null$tech4$BETWEEN$latCorEst

cormat <- data.frame(cormat)

cormat <- cormat[c('ESMEB','ESMAB','ESMCB','ESMNB',
                   'EAREB','EARAB','EARCB','EARNB'),
                 c('ESMEB','ESMAB','ESMCB','ESMNB',
                   'EAREB','EARAB','EARCB','EARNB')]

cormat['ESMAB','ESMEB'] <- cormat['ESMEB','ESMAB']
cormat['EAREB','ESMCB'] <- cormat['ESMCB','EAREB']
cormat['EAREB','ESMNB'] <- cormat['ESMNB','EAREB']
cormat['EARAB','ESMEB'] <- cormat['ESMEB','EARAB']
cormat['EARAB','ESMCB'] <- cormat['ESMCB','EARAB']
cormat['EARAB','ESMNB'] <- cormat['ESMNB','EARAB']
cormat['EARAB','EAREB'] <- cormat['EAREB','EARAB']
cormat['EARCB','ESMNB'] <- cormat['ESMNB','EARCB']

cormat[upper.tri(cormat,diag=T)] <- NA

# define formatting function
formatrs <- function(x) { sub("^(-?)0.", "\\1.", sprintf("%.2f",x)) }
cormat[lower.tri(cormat,diag=F)] <- sapply(cormat[lower.tri(cormat,diag=F)], function(x) {round(x,2)})
cormat[lower.tri(cormat,diag=F)] <- sapply(cormat[lower.tri(cormat,diag=F)], function(x) {formatrs(x)})

# get between-person omegas
omegabs <- lapply(omegas,function(x){
  x <- x$parameters$unstandardized
  x[which(x$param=='OMEGAB'),'est']
})

omegabs <- ldply(omegabs)
names(omegabs)[2] <- 'omega_b'
omegabs$.id <- gsub('_','',omegabs$.id)
omegabs$.id <- gsub('omega','',omegabs$.id)
omegabs$.id <- gsub('.out','',omegabs$.id)
omegabs$.id <- toupper(omegabs$.id)
omegabs$.id <- str_sub(omegabs$.id,1,4)
omegabs$.id <- paste0(omegabs$.id,'B')
omegabs$omega_b <- round(omegabs$omega_b,2)
omegabs$omega_b <- formatrs(omegabs$omega_b)
omegabs$omega_b <- paste0('(',omegabs$omega_b,')')

# add between-person omegas to diagonal
for(i in omegabs$.id){
  cormat[i,i] <- omegabs[which(omegabs$.id==i),'omega_b']
}

write.csv(cormat,'tables2.csv',row.names=T,na='')

# which rs had 95% CIs that excluded 0?

allcors <- null$parameters$stdyx.standardized
allcors <- allcors[which(allcors$BetweenWithin=='Between'),]

allcors$type <- sub(".*\\.", "", allcors$paramHeader)
allcors <- allcors[which(allcors$type=='WITH'),
                   c('paramHeader','param',
                     'lower_2.5ci','est','upper_2.5ci','type')]

View(allcors)

#### Figure 1 ####

# Center ESM composites 

for(i in unique(dat$ear.IDnum.w1)){
  for(j in c('esm.EXT.w1','esm.AGR.w1','esm.CON.w1','esm.NEU.w1')){
    dat[which(dat$ear.IDnum.w1==i),paste0(j,'.m')] <- mean(dat[which(dat$ear.IDnum.w1==i),j],na.rm=TRUE)
    dat[,paste0(j,'.c')] <- dat[,j] - dat[,paste0(j,'.m')]
  }
}

min(dat$esm.AGR.w1.c,na.rm=T); max(dat$esm.AGR.w1.c,na.rm=T)
min(dat$esm.EXT.w1.c,na.rm=T); max(dat$esm.EXT.w1.c,na.rm=T)
min(dat$esm.CON.w1.c,na.rm=T); max(dat$esm.CON.w1.c,na.rm=T)
min(dat$esm.NEU.w1.c,na.rm=T); max(dat$esm.NEU.w1.c,na.rm=T)

AGR <- ggplot(data = dat, aes(x = esm.AGR.w1.c, y = ear.AGR.w1)) +
  geom_line(aes(group = as.factor(ear.IDnum.w1)),  # linear trend for each participant
            stat="smooth",method = "lm", 
            alpha = 0.5,color='steelblue',size=.4) +
  geom_smooth(aes(x = esm.AGR.w1.c, y = ear.AGR.w1),  # overall trend
              colour="#000000",size=1.25,method='lm',fill=NA) +
  geom_vline(xintercept=0,linetype="dashed",colour='black') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(face="bold",size=20),
        axis.title.y = element_text(face="bold",size=20)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  xlab("ESM Agreeableness") +  # x-axis label
  ylab("EAR Agreeableness") +  # y-axis label
  scale_y_continuous(limits=c(1,5),breaks=seq(1,5, .5),expand=c(0,0)) +
  #  scale_x_continuous(limits=c(-10,40),breaks=seq(-10,40,5),expand=c(0,0))  
  scale_x_continuous(limits=c(-3,3),breaks=seq(-3, 3, .5),expand=c(0,0))

CON <- ggplot(data = dat, aes(x = esm.CON.w1.c, y = ear.CON.w1)) +
  geom_line(aes(group = as.factor(ear.IDnum.w1)),  # linear trend for each participant
            stat="smooth",method = "lm", 
            alpha = 0.5,color='steelblue',size=.4) +
  geom_smooth(aes(x = esm.CON.w1.c, y = ear.CON.w1),  # overall trend
              colour="#000000",size=1.25,method='lm',fill=NA) +
  geom_vline(xintercept=0,linetype="dashed",colour='black') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(face="bold",size=20),
        axis.title.y = element_text(face="bold",size=20)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  xlab("ESM Conscientiousness") +  # x-axis label
  ylab("EAR Conscientiousness") +  # y-axis label
  scale_y_continuous(limits=c(1,5),breaks=seq(1,5, .5),expand=c(0,0)) +
  #  scale_x_continuous(limits=c(-10,40),breaks=seq(-10,40,5),expand=c(0,0))  
  scale_x_continuous(limits=c(-3,3),breaks=seq(-3, 3, .5),expand=c(0,0))

EXT <- ggplot(data = dat, aes(x = esm.EXT.w1.c, y = ear.EXT.w1)) +
  geom_line(aes(group = as.factor(ear.IDnum.w1)),  # linear trend for each participant
            stat="smooth",method = "lm", 
            alpha = 0.5,color='steelblue',size=.4) +
  geom_smooth(aes(x = esm.EXT.w1.c, y = ear.EXT.w1),  # overall trend
              colour="#000000",size=1.25,method='lm',fill=NA) +
  geom_vline(xintercept=0,linetype="dashed",colour='black') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(face="bold",size=20),
        axis.title.y = element_text(face="bold",size=20)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  xlab("ESM Extraversion") +  # x-axis label
  ylab("EAR Extraversion") +  # y-axis label
  scale_y_continuous(limits=c(1,5),breaks=seq(1,5, .5),expand=c(0,0)) +
  scale_x_continuous(limits=c(-3,3),breaks=seq(-3, 3, .5),expand=c(0,0))

NEU <- ggplot(data = dat, aes(x = esm.NEU.w1.c, y = ear.NEU.w1)) +
  geom_line(aes(group = as.factor(ear.IDnum.w1)),  # linear trend for each participant
            stat="smooth",method = "lm", 
            alpha = 0.5,color='steelblue',size=.4) +
  geom_smooth(aes(x = esm.NEU.w1.c, y = ear.NEU.w1),  # overall trend
              colour="#000000",size=1.25,method='lm',fill=NA) +
  geom_vline(xintercept=0,linetype="dashed",colour='black') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(face="bold",size=20),
        axis.title.y = element_text(face="bold",size=20)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  xlab("ESM Neuroticism") +  # x-axis label
  ylab("EAR Neuroticism") +  # y-axis label
  scale_y_continuous(limits=c(1,5),breaks=seq(1,5, .5),expand=c(0,0)) +
  scale_x_continuous(limits=c(-3,3),breaks=seq(-3, 3, .5),expand=c(0,0))

figure1 <- plot_grid(EXT,AGR,CON,NEU)

save_plot('figure1.png',figure1,base_height=10,base_width=12)

#### Computing Discrepancy Scores ####

# Here is how we computed the discrepancy scores included in the
# "transcripts.xlsx" file
# But note that the transcripts.xlsx file only includes the 
# participants who gave consent to have their transcripts published

dat <- ddply(dat, c("ear.IDnum.w1"), transform, 
                ear.EXT.std = scale(ear.EXT.w1),
                esm.EXT.std = scale(esm.EXT.w1),
                ear.AGR.std = scale(ear.AGR.w1),
                esm.AGR.std = scale(esm.AGR.w1),
                ear.NEU.std = scale(ear.NEU.w1),
                esm.NEU.std = scale(esm.NEU.w1), 
                ear.CON.std = scale(ear.CON.w1),
                esm.CON.std = scale(esm.CON.w1))

# compute all discrepancy scores
dat$discrep.AGR <- dat$esm.AGR.std-dat$ear.AGR.std
dat$discrep.NEU <- dat$esm.NEU.std-dat$ear.NEU.std
dat$discrep.CON <- dat$esm.CON.std-dat$ear.CON.std
dat$discrep.EXT <- dat$esm.NEU.std-dat$ear.EXT.std


