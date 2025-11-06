target <- read.csv('./Data/sample1-target.csv',stringsAsFactors = F)
informant <- read.csv('./Data/sample1-informant.csv',stringsAsFactors = F)
norms <- read.csv('./Data/sample1-norms.csv',stringsAsFactors = F)

library(stringr)

############################################################
#                    Target variable setup                 #
############################################################

#### Reverse-score items ####

reversebfichange <- c('tc.BFI2.31',  # shy, introverted
                      'tc.BFI2.37',  # rude
                      'tc.BFI2.12',  # tends to find fault with others
                      'tc.BFI2.8',  # lazy
                      'tc.BFI2.4',  # relaxed, handles stress well
                      'tc.BFI2.24',  # secure, comfortable with myself
                      'tc.BFI2.29')  # emotionally stable, not easily upset

reversebfi <- c('ts.BFI2.16', 'ts.BFI2.31',  # low sociability items
                'ts.BFI2.36', 'ts.BFI2.51',  # low assertiveness items
                'ts.BFI2.11', 'ts.BFI2.26',  # low energy level items
                'ts.BFI2.17', 'ts.BFI2.47',  # low compassion items
                'ts.BFI2.22', 'ts.BFI2.37',  # low respectfulness items
                'ts.BFI2.12', 'ts.BFI2.42',  # low trust items
                'ts.BFI2.3', 'ts.BFI2.48',  # low organization items
                'ts.BFI2.8', 'ts.BFI2.23',  # low productiveness items
                'ts.BFI2.28', 'ts.BFI2.58',  # low responsibility items
                'ts.BFI2.4', 'ts.BFI2.49',  # low anxiety items
                'ts.BFI2.9', 'ts.BFI2.24',  # low depression items
                'ts.BFI2.29', 'ts.BFI2.44',  # low emotional volatility items
                'ts.BFI2.25',  'ts.BFI2.55',  # low intellectual curiosity items
                'ts.BFI2.5', 'ts.BFI2.50',  # low aesthetic sensitivity items
                'ts.BFI2.30', 'ts.BFI2.45',  # low creative imagination items
                'ts.anger.2')  # low anger item

reversemcq <- c('ts.MCQ.GM2',  # low general morality
                'ts.MCQ.H1',  # low honesty
                'ts.MCQ.F3',  # low fairness
                'ts.MCQ.C4',  # low compassion
                'ts.MCQ.R2',  # low respectfulness
                'ts.MCQ.L2',  # low loyalty
                'ts.MCQ.P3')  # low purity

reversehh <- c('ts.HH.M3', 'ts.HH.M4', 'ts.HH.F1', 'ts.HH.F2', 'ts.HH.S3', 
               'ts.HH.GA2', 'ts.HH.F4', 'ts.HH.GA3', 'ts.HH.S1', 'ts.HH.GA4')

# reverse items that had a 5-point scale
target[,paste0(reversebfi,'r')] <- 6-target[,reversebfi] 
target[,paste0(reversemcq,'r')] <- 6-target[,reversemcq] 
target[,paste0(reversehh,'r')] <- 6-target[,reversehh] 

# reverse change goal items
target[,paste0(reversebfichange,'r')] <- -1*target[,reversebfichange]   

#### Compute composites ####
target$ts.EXT.S <- rowMeans(target[,c('ts.BFI2.1','ts.BFI2.16r',
                                      'ts.BFI2.31r','ts.BFI2.46')],na.rm=T)
target$ts.EXT.A <- rowMeans(target[,c('ts.BFI2.6','ts.BFI2.21',
                                      'ts.BFI2.36r','ts.BFI2.51r')],na.rm=T)
target$ts.EXT.E <- rowMeans(target[,c('ts.BFI2.11r','ts.BFI2.26r',
                                      'ts.BFI2.41','ts.BFI2.56')],na.rm=T)
target$ts.EXT <- rowMeans(target[,c('ts.EXT.S','ts.EXT.A','ts.EXT.E')],na.rm=T)

target$ts.AGR.C <- rowMeans(target[,c('ts.BFI2.2','ts.BFI2.17r',
                                      'ts.BFI2.32','ts.BFI2.47r')],na.rm=T)
target$ts.AGR.R <- rowMeans(target[,c('ts.BFI2.7','ts.BFI2.22r',
                                      'ts.BFI2.37r','ts.BFI2.52')],na.rm=T)
target$ts.AGR.T <- rowMeans(target[,c('ts.BFI2.12r','ts.BFI2.27',
                                      'ts.BFI2.42r','ts.BFI2.57')],na.rm=T)
target$ts.AGR <- rowMeans(target[,c('ts.AGR.C','ts.AGR.R','ts.AGR.T')],na.rm=T)

target$ts.CON.O <- rowMeans(target[,c('ts.BFI2.3r','ts.BFI2.18',
                                      'ts.BFI2.33','ts.BFI2.48r')],na.rm=T)
target$ts.CON.P <- rowMeans(target[,c('ts.BFI2.8r','ts.BFI2.23r',
                                      'ts.BFI2.38','ts.BFI2.53')],na.rm=T)
target$ts.CON.R <- rowMeans(target[,c('ts.BFI2.13','ts.BFI2.28r',
                                      'ts.BFI2.43','ts.BFI2.58r')],na.rm=T)
target$ts.CON <- rowMeans(target[,c('ts.CON.O','ts.CON.P','ts.CON.R')],na.rm=T)

target$ts.NEG.A <- rowMeans(target[,c('ts.BFI2.4r','ts.BFI2.19',
                                      'ts.BFI2.34','ts.BFI2.49r')],na.rm=T)
target$ts.NEG.D <- rowMeans(target[,c('ts.BFI2.9r','ts.BFI2.24r',
                                      'ts.BFI2.39','ts.BFI2.54')],na.rm=T)
target$ts.NEG.E <- rowMeans(target[,c('ts.BFI2.14','ts.BFI2.29r',
                                      'ts.BFI2.44r','ts.BFI2.59')],na.rm=T)
target$ts.NEG <- rowMeans(target[,c('ts.NEG.A','ts.NEG.D','ts.NEG.E')],na.rm=T)

target$ts.anger <- rowMeans(target[,c('ts.anger.1','ts.anger.2r')],na.rm=T)

target$ts.OPE.I <- rowMeans(target[,c('ts.BFI2.10','ts.BFI2.25r',
                                      'ts.BFI2.40','ts.BFI2.55r')],na.rm=T)
target$ts.OPE.A <- rowMeans(target[,c('ts.BFI2.5r','ts.BFI2.20',
                                      'ts.BFI2.50r')],na.rm=T)  # cut the values item
target$ts.OPE.C <- rowMeans(target[,c('ts.BFI2.15','ts.BFI2.30r',
                                      'ts.BFI2.45r','ts.BFI2.60')],na.rm=T)
target$ts.OPE <- rowMeans(target[,c('ts.OPE.I','ts.OPE.A','ts.OPE.C')],na.rm=T)

target$ts.HH <- rowMeans(target[,c(paste0(reversehh,'r'),
                                   'ts.HH.S2','ts.HH.GA1','ts.HH.F3',
                                   'ts.HH.S4','ts.HH.M1','ts.HH.M2')],na.rm=T)

target$ts.MCQ.GM <- rowMeans(target[,c('ts.MCQ.GM1','ts.MCQ.GM2r',
                                       'ts.MCQ.GM3','ts.MCQ.GM5')],na.rm=T)  # cut GM4 & GM6 (values items)

target$ts.MCQ.H <- rowMeans(target[,c('ts.MCQ.H2','ts.MCQ.H3')],  # cut H1r & H4 (values items)
                            na.rm=T)

target$ts.MCQ.F <- rowMeans(target[,c('ts.MCQ.F1','ts.MCQ.F2')],  # cut F3r & F4 (values items)
                            na.rm=T)

target$ts.MCQ.L <- rowMeans(target[,c('ts.MCQ.L1','ts.MCQ.L2r')],  # cut L1 & L2r (values items)
                            na.rm=T)

target$ts.MCQ.P <- rowMeans(target[,c('ts.MCQ.P1','ts.MCQ.P3r')],  # cut P2 & P4 (values items)
                            na.rm=T)

target$ts.ME <- rowMeans(target[,c('ts.ME.1','ts.ME.2','ts.ME.3',
                                   'ts.ME.4','ts.ME.5','ts.ME.6')],na.rm=T)

target$ts.IB <- rowMeans(target[,c('ts.IB.1','ts.IB.2','ts.IB.3',
                                   'ts.IB.4','ts.IB.5')],na.rm=T)

target$ts.MFQ.H <- rowMeans(target[,c('ts.MFQ.H1','ts.MFQ.H2',
                                      'ts.MFQ.H3','ts.MFQ.H4')],na.rm=T)
target$ts.MFQ.F <- rowMeans(target[,c('ts.MFQ.F1','ts.MFQ.F2',
                                      'ts.MFQ.F3','ts.MFQ.F4')],na.rm=T)
target$ts.MFQ.I <- rowMeans(target[,c('ts.MFQ.I1','ts.MFQ.I2',
                                      'ts.MFQ.I3','ts.MFQ.I4')],na.rm=T)
target$ts.MFQ.A <- rowMeans(target[,c('ts.MFQ.A1','ts.MFQ.A2',
                                      'ts.MFQ.A3','ts.MFQ.A4')],na.rm=T)
target$ts.MFQ.P <- rowMeans(target[,c('ts.MFQ.P1','ts.MFQ.P2',
                                      'ts.MFQ.P3','ts.MFQ.P4')],na.rm=T)

target$tc.EXT.S <- rowMeans(target[,c('tc.BFI2.1','tc.BFI2.31r')],na.rm=T)
target$tc.EXT.A <- rowMeans(target[,c('tc.BFI2.6','tc.BFI2.21')],na.rm=T)
target$tc.EXT.E <- rowMeans(target[,c('tc.BFI2.41','tc.BFI2.56')],na.rm=T)
target$tc.EXT <- rowMeans(target[,c('tc.EXT.S','tc.EXT.A','tc.EXT.E')],na.rm=T)

target$tc.AGR.C <- rowMeans(target[,c('tc.BFI2.2','tc.BFI2.32')],na.rm=T)
target$tc.AGR.R <- rowMeans(target[,c('tc.BFI2.7','tc.BFI2.37r')],na.rm=T)
target$tc.AGR.T <- rowMeans(target[,c('tc.BFI2.12r','tc.BFI2.27')],na.rm=T)
target$tc.AGR <- rowMeans(target[,c('tc.AGR.C','tc.AGR.R','tc.AGR.T')],na.rm=T)

target$tc.CON.O <- rowMeans(target[,c('tc.BFI2.3','tc.BFI2.33')],na.rm=T)
target$tc.CON.P <- rowMeans(target[,c('tc.BFI2.8r','tc.BFI2.53')],na.rm=T)
target$tc.CON.R <- rowMeans(target[,c('tc.BFI2.13','tc.BFI2.43')],na.rm=T)
target$tc.CON <- rowMeans(target[,c('tc.CON.O','tc.CON.P','tc.CON.R')],na.rm=T)

target$tc.NEG.A <- rowMeans(target[,c('tc.BFI2.4r','tc.BFI2.34')],na.rm=T)
target$tc.NEG.D <- rowMeans(target[,c('tc.BFI2.24r','tc.BFI2.54')],na.rm=T)
target$tc.NEG.E <- rowMeans(target[,c('tc.BFI2.14','tc.BFI2.29r')],na.rm=T)
target$tc.NEG <- rowMeans(target[,c('tc.NEG.A','tc.NEG.D','tc.NEG.E')],na.rm=T)

target$tc.anger <- rowMeans(target[,c('tc.anger.1','tc.anger.2')],na.rm=T)

target$tc.OPE.I <- rowMeans(target[,c('tc.BFI2.10','tc.BFI2.40')],na.rm=T)
target$tc.OPE.A <- rowMeans(target[,c('tc.BFI2.5','tc.BFI2.20')],na.rm=T)
target$tc.OPE.C <- rowMeans(target[,c('tc.BFI2.15','tc.BFI2.60')],na.rm=T)
target$tc.OPE <- rowMeans(target[,c('tc.OPE.I','tc.OPE.A','tc.OPE.C')],na.rm=T)

target$tc.MCQ.GM <- rowMeans(target[,c('tc.MCQ.GM3','tc.MCQ.GM5')],na.rm=T)
target$tc.MCQ.H <- rowMeans(target[,c('tc.MCQ.H3','tc.MCQ.H4')],na.rm=T)
target$tc.MCQ.F <- rowMeans(target[,c('tc.MCQ.F1','tc.MCQ.F4')],na.rm=T)
target$tc.MCQ.L <- rowMeans(target[,c('tc.MCQ.L1','tc.MCQ.L4')],na.rm=T)
target$tc.MCQ.P <- rowMeans(target[,c('tc.MCQ.P1','tc.MCQ.P4')],na.rm=T)

#### Create new indices ####

focalitems <- c('BFI2.1', 'BFI2.31', 'BFI2.6', 'BFI2.21', 'BFI2.41', 'BFI2.56', 'BFI2.2', 'BFI2.32', 'BFI2.7', 'BFI2.37', 
                'BFI2.12', 'BFI2.27', 'BFI2.3', 'BFI2.33', 'BFI2.8', 'BFI2.53', 'BFI2.13', 'BFI2.43', 'BFI2.4', 
                'BFI2.34', 'BFI2.24', 'BFI2.54', 'BFI2.14', 'BFI2.29', 'BFI2.10', 'BFI2.40', 'BFI2.5', 'BFI2.20', 
                'BFI2.15', 'BFI2.60', 'anger.1', 'anger.2', 'MCQ.GM3', 'MCQ.GM5', 'MCQ.H3', 'MCQ.H4', 
                'MCQ.F1', 'MCQ.F4', 'MCQ.L1', 'MCQ.L4', 'MCQ.P1', 'MCQ.P4')

tc.items.raw <- paste0('tc.',focalitems)

# overall absolute change index
target$tc.overall <- rowMeans(abs(target[,tc.items.raw]),na.rm=T)

#### Recode the top3 and top1 variables ####
# Split top 3 traits into multiple column
target[,c('tc.top3.1.id','tc.top3.2.id','tc.top3.3.id')] <- str_split_fixed(target$tc.top3, ",", 3)

# Fill in the corresponding traits
target[,c('tc.top3.1.trait','tc.top3.2.trait',
          'tc.top3.3.trait','tc.top1.trait')] <- sapply(target[,c('tc.top3.1.id','tc.top3.2.id',
                                                                  'tc.top3.3.id','tc.top1')],function(x){
                                                                    x[which(x==1)] <- 'tc.BFI2.41'
                                                                    x[which(x==2)] <- 'tc.BFI2.43'
                                                                    x[which(x==3)] <- 'tc.MCQ.H3'
                                                                    x[which(x==4)] <- 'tc.MCQ.P1'
                                                                    x[which(x==5)] <- 'tc.BFI2.14'
                                                                    x[which(x==6)] <- 'tc.BFI2.32'
                                                                    x[which(x==7)] <- 'tc.BFI2.34'
                                                                    x[which(x==8)] <- 'tc.MCQ.L4'
                                                                    x[which(x==9)] <- 'tc.MCQ.GM5'
                                                                    x[which(x==10)] <- 'tc.BFI2.4'
                                                                    x[which(x==11)] <- 'tc.anger.1'
                                                                    x[which(x==12)] <- 'tc.BFI2.13'
                                                                    x[which(x==13)] <- 'tc.BFI2.27'
                                                                    x[which(x==14)] <- 'tc.BFI2.24'
                                                                    x[which(x==15)] <- 'tc.BFI2.56'
                                                                    x[which(x==16)] <- 'tc.BFI2.53'
                                                                    x[which(x==17)] <- 'tc.BFI2.5'
                                                                    x[which(x==18)] <- 'tc.MCQ.H4'
                                                                    x[which(x==19)] <- 'tc.MCQ.F1'
                                                                    x[which(x==20)] <- 'tc.BFI2.1'
                                                                    x[which(x==21)] <- 'tc.BFI2.12'
                                                                    x[which(x==22)] <- 'tc.BFI2.29'
                                                                    x[which(x==23)] <- 'tc.MCQ.L1'
                                                                    x[which(x==24)] <- 'tc.BFI2.15'
                                                                    x[which(x==25)] <- 'tc.BFI2.60'
                                                                    x[which(x==26)] <- 'tc.BFI2.6'
                                                                    x[which(x==27)] <- 'tc.BFI2.8'
                                                                    x[which(x==28)] <- 'tc.BFI2.37'
                                                                    x[which(x==29)] <- 'tc.BFI2.7'
                                                                    x[which(x==30)] <- 'tc.BFI2.10'
                                                                    x[which(x==31)] <- 'tc.BFI2.20'
                                                                    x[which(x==32)] <- 'tc.anger.2'
                                                                    x[which(x==33)] <- 'tc.BFI2.21'
                                                                    x[which(x==34)] <- 'tc.BFI2.33'
                                                                    x[which(x==35)] <- 'tc.MCQ.GM3'
                                                                    x[which(x==36)] <- 'tc.BFI2.31'
                                                                    x[which(x==37)] <- 'tc.MCQ.P4'
                                                                    x[which(x==38)] <- 'tc.BFI2.2'
                                                                    x[which(x==39)] <- 'tc.BFI2.40'
                                                                    x[which(x==40)] <- 'tc.BFI2.3'
                                                                    x[which(x==41)] <- 'tc.MCQ.F4'
                                                                    x[which(x==42)] <- 'tc.BFI2.54'
                                                                    x
                                                                  })

# Fill in participants' desired direction and amount of change for these traits

for(i in c('top3.1','top3.2','top3.3','top1')){  # for each of these priorities (top 3 & top 1)
  for(j in tc.items.raw){  # and each of the items
    target[which(target[,paste0('tc.',i,'.trait')]==j),  # if the participant chose that trait,
           paste0('tc.',i,'.direction')] <- target[which(target[,paste0('tc.',i,'.trait')]==j),j]  # then fill in the direction from their raw change goal responses
  }
}

# sanity-checking that this code is doing what is intended
# View(target[,c('tc.BFI2.41','tc.BFI2.34','tc.BFI2.43','tc.top3.1.trait','tc.top3.1.direction')])
# View(target[,c('tc.BFI2.4','tc.BFI2.14','tc.BFI2.56','tc.top3.2.trait','tc.top3.2.direction')])

# Create simplified version of top3 & top1 traits (Trait + Direction)

for(i in c('top3.1','top3.2','top3.3','top1')){
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.1') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Sociable'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.31') &  # shy, introverted
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'Less Sociable'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.6','tc.BFI2.21') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Assertive'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.41','tc.BFI2.56') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Energetic' 
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.2','tc.BFI2.32') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Compassionate' 
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.7') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Respectful' 
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.37') &  # rude
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'Less Respectful' 
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.12') &  # tends to find fault with others
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'Less Trusting'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.27') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Trusting'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.3','tc.BFI2.33') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Organized'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.8') &  # lazy
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'Less Productive'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.53') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Productive'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.13','tc.BFI2.43') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Responsible'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.4') &    # relaxed, handles stress well
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'Less Anxious'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.34') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Anxious'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.24') &  # secure, comfortable with self
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'Less Depressed'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.54') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Depressed'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.29') &  # emotionally stable, not easily upset
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'Less Emotionally Volatile'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.14') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Emotionally Volatile'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.anger.1','tc.anger.2') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Angry'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.10','tc.BFI2.40') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Intellectually Curious'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.5','tc.BFI2.20') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Aesthetically Sensitive'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.15','tc.BFI2.60') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Creative/Imaginative'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.GM3','tc.MCQ.GM5') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Moral'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.H3','tc.MCQ.H4') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Honest'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.F1','tc.MCQ.F4') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Fair'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.L1','tc.MCQ.L4') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Loyal'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.P1','tc.MCQ.P4') &
                 target[,paste0('tc.',i,'.direction')]>0),paste0('tc.',i)] <- 'More Pure'
  
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.1') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Sociable'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.31') &  # shy, introverted
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'More Sociable'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.6','tc.BFI2.21') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Assertive'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.41','tc.BFI2.56') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Energetic' 
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.2','tc.BFI2.32') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Compassionate' 
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.37') &  # rude
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'More Respectful' 
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.7') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Respectful' 
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.12') &  # tends to find fault in others
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'More Trusting'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.27') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Trusting'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.3','tc.BFI2.33') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Organized'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.8') &  # lazy
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'More Productive'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.53') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Productive'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.13','tc.BFI2.43') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Responsible'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.4') &  # relaxed, handles stress well
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'More Anxious'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.34') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Anxious'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.24') &  # secure, comfortable with self
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'More Depressed'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.54') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Depressed'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.29') &  # emotionally stable, not easily upset
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'More Emotionally Volatile'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.14') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Emotionally Volatile'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.anger.1','tc.anger.2') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Angry'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.10','tc.BFI2.40') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Intellectually Curious'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.5','tc.BFI2.20') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Aesthetically Sensitive'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.BFI2.15','tc.BFI2.60') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Creative/Imaginative'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.GM3','tc.MCQ.GM5') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Moral'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.H3','tc.MCQ.H4') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Honest'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.F1','tc.MCQ.F4') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Fair'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.L1','tc.MCQ.L4') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Loyal'
  target[which(target[,paste0('tc.',i,'.trait')] %in% c('tc.MCQ.P1','tc.MCQ.P4') &
                 target[,paste0('tc.',i,'.direction')]<0),paste0('tc.',i)] <- 'Less Pure'
}

# sanity-checking that this code is doing what is intended
# View(target[,c('tc.BFI2.41','tc.BFI2.34','tc.BFI2.32','tc.BFI2.43','tc.BFI2.31',
# 'tc.top3.1.trait','tc.top3.1.direction','tc.top3.1',
# 'tc.top3.2.trait','tc.top3.2.direction','tc.top3.2')])

############################################################
#                 Informant variable setup                 #
############################################################

#### Reverse-Score Items ####

ic.reversebfichange <- gsub('tc.','ic.',reversebfichange)
it.reversebfi <- gsub('ts.','it.',reversebfi)
it.reversemcq <- gsub('ts.','it.',reversemcq)
it.reversehh <- gsub('ts.','it.',reversehh)

informant[,paste0(it.reversebfi,'r')] <- 6-informant[,it.reversebfi] 
informant[,paste0(it.reversemcq,'r')] <- 6-informant[,it.reversemcq] 
informant[,paste0(it.reversehh,'r')] <- 6-informant[,it.reversehh] 

informant[,paste0(ic.reversebfichange,'r')] <- -1*informant[,ic.reversebfichange]   

#### Recode the top3 and top1 variables ####
# Split top 3 traits into multiple columns
informant[,c('ic.top3.1.id','ic.top3.2.id','ic.top3.3.id')] <- str_split_fixed(informant$ic.top3, ",", 3)

informant[,c('ic.top3.1.trait','ic.top3.2.trait',
             'ic.top3.3.trait','ic.top1.trait')] <- NA

# Fill in the corresponding traits for the male version
informant[which(informant$it.gender==1),
          c('ic.top3.1.trait','ic.top3.2.trait',
            'ic.top3.3.trait','ic.top1.trait')] <- sapply(informant[which(informant$it.gender==1),
                                                                    c('ic.top3.1.id','ic.top3.2.id',
                                                                      'ic.top3.3.id','ic.top1')],
                                                          function(x){
                                                            x[which(x==1)] <- 'ic.BFI2.27'
                                                            x[which(x==2)] <- 'ic.BFI2.7'
                                                            x[which(x==3)] <- 'ic.BFI2.53'
                                                            x[which(x==4)] <- 'ic.BFI2.21'
                                                            x[which(x==5)] <- 'ic.BFI2.40'
                                                            x[which(x==6)] <- 'ic.MCQ.GM5'
                                                            x[which(x==7)] <- 'ic.MCQ.H4'
                                                            x[which(x==8)] <- 'ic.BFI2.1'
                                                            x[which(x==9)] <- 'ic.BFI2.12'
                                                            x[which(x==10)] <- 'ic.BFI2.43'
                                                            x[which(x==11)] <- 'ic.anger.1'
                                                            x[which(x==12)] <- 'ic.MCQ.P1'
                                                            x[which(x==13)] <- 'ic.BFI2.54'
                                                            x[which(x==14)] <- 'ic.BFI2.31'
                                                            x[which(x==15)] <- 'ic.BFI2.20'
                                                            x[which(x==16)] <- 'ic.BFI2.3'
                                                            x[which(x==17)] <- 'ic.BFI2.4'
                                                            x[which(x==18)] <- 'ic.BFI2.14'
                                                            x[which(x==19)] <- 'ic.BFI2.60'
                                                            x[which(x==20)] <- 'ic.BFI2.24'
                                                            x[which(x==21)] <- 'ic.MCQ.GM3'
                                                            x[which(x==22)] <- 'ic.BFI2.10'
                                                            x[which(x==23)] <- 'ic.BFI2.29'
                                                            x[which(x==24)] <- 'ic.MCQ.L1'
                                                            x[which(x==25)] <- 'ic.MCQ.H3'
                                                            x[which(x==26)] <- 'ic.BFI2.33'
                                                            x[which(x==27)] <- 'ic.BFI2.13'
                                                            x[which(x==28)] <- 'ic.BFI2.41'
                                                            x[which(x==29)] <- 'ic.MCQ.F4'
                                                            x[which(x==30)] <- 'ic.MCQ.F1'
                                                            x[which(x==31)] <- 'ic.BFI2.6'
                                                            x[which(x==32)] <- 'ic.anger.2'
                                                            x[which(x==33)] <- 'ic.BFI2.34'
                                                            x[which(x==34)] <- 'ic.BFI2.2'
                                                            x[which(x==35)] <- 'ic.MCQ.L4'
                                                            x[which(x==36)] <- 'ic.BFI2.37'
                                                            x[which(x==37)] <- 'ic.BFI2.56'
                                                            x[which(x==38)] <- 'ic.BFI2.5'
                                                            x[which(x==39)] <- 'ic.BFI2.8'
                                                            x[which(x==40)] <- 'ic.BFI2.32'
                                                            x[which(x==41)] <- 'ic.BFI2.15'
                                                            x[which(x==42)] <- 'ic.MCQ.P4'
                                                            x
                                                          })

# Fill in the corresponding traits for the female version
informant[which(informant$it.gender==2),
          c('ic.top3.1.trait','ic.top3.2.trait',
            'ic.top3.3.trait','ic.top1.trait')] <- sapply(informant[which(informant$it.gender==2),
                                                                    c('ic.top3.1.id','ic.top3.2.id',
                                                                      'ic.top3.3.id','ic.top1')],
                                                          function(x){
                                                            x[which(x==1)] <- 'ic.BFI2.6'
                                                            x[which(x==2)] <- 'ic.BFI2.32'
                                                            x[which(x==3)] <- 'ic.BFI2.10'
                                                            x[which(x==4)] <- 'ic.MCQ.L4'
                                                            x[which(x==5)] <- 'ic.BFI2.34'
                                                            x[which(x==6)] <- 'ic.MCQ.P4'
                                                            x[which(x==7)] <- 'ic.MCQ.GM5'
                                                            x[which(x==8)] <- 'ic.MCQ.H4'
                                                            x[which(x==9)] <- 'ic.BFI2.43'
                                                            x[which(x==10)] <- 'ic.BFI2.2'
                                                            x[which(x==11)] <- 'ic.anger.1'
                                                            x[which(x==12)] <- 'ic.BFI2.37'
                                                            x[which(x==13)] <- 'ic.BFI2.24'
                                                            x[which(x==14)] <- 'ic.BFI2.5'
                                                            x[which(x==15)] <- 'ic.BFI2.41'
                                                            x[which(x==16)] <- 'ic.BFI2.60'
                                                            x[which(x==17)] <- 'ic.BFI2.15'
                                                            x[which(x==18)] <- 'ic.MCQ.L1'
                                                            x[which(x==19)] <- 'ic.BFI2.20'
                                                            x[which(x==20)] <- 'ic.BFI2.8'
                                                            x[which(x==21)] <- 'ic.BFI2.1'
                                                            x[which(x==22)] <- 'ic.BFI2.27'
                                                            x[which(x==23)] <- 'ic.BFI2.14'
                                                            x[which(x==24)] <- 'ic.BFI2.12'
                                                            x[which(x==25)] <- 'ic.BFI2.4'
                                                            x[which(x==26)] <- 'ic.MCQ.F1'
                                                            x[which(x==27)] <- 'ic.MCQ.GM3'
                                                            x[which(x==28)] <- 'ic.BFI2.29'
                                                            x[which(x==29)] <- 'ic.BFI2.13'
                                                            x[which(x==30)] <- 'ic.BFI2.31'
                                                            x[which(x==31)] <- 'ic.BFI2.33'
                                                            x[which(x==32)] <- 'ic.anger.2'
                                                            x[which(x==33)] <- 'ic.BFI2.54'
                                                            x[which(x==34)] <- 'ic.MCQ.H3'
                                                            x[which(x==35)] <- 'ic.MCQ.P1'
                                                            x[which(x==36)] <- 'ic.BFI2.53'
                                                            x[which(x==37)] <- 'ic.BFI2.21'
                                                            x[which(x==38)] <- 'ic.BFI2.40'
                                                            x[which(x==39)] <- 'ic.BFI2.3'
                                                            x[which(x==40)] <- 'ic.MCQ.F4'
                                                            x[which(x==41)] <- 'ic.BFI2.56'
                                                            x[which(x==42)] <- 'ic.BFI2.7'
                                                            x
                                                          })

# Fill in the corresponding traits for the gender-neutral version
informant[which(informant$it.gender==3),
          c('ic.top3.1.trait','ic.top3.2.trait',
            'ic.top3.3.trait','ic.top1.trait')] <- sapply(informant[which(informant$it.gender==3),
                                                                    c('ic.top3.1.id','ic.top3.2.id',
                                                                      'ic.top3.3.id','ic.top1')],function(x){
                                                                        x[which(x==1)] <- 'ic.BFI2.43'
                                                                        x[which(x==2)] <- 'ic.BFI2.6'
                                                                        x[which(x==3)] <- 'ic.MCQ.GM3'
                                                                        x[which(x==4)] <- 'ic.BFI2.53'
                                                                        x[which(x==5)] <- 'ic.BFI2.20'
                                                                        x[which(x==6)] <- 'ic.BFI2.3'
                                                                        x[which(x==7)] <- 'ic.BFI2.32'
                                                                        x[which(x==8)] <- 'ic.BFI2.60'
                                                                        x[which(x==9)] <- 'ic.BFI2.31'
                                                                        x[which(x==10)] <- 'ic.MCQ.L4'
                                                                        x[which(x==11)] <- 'ic.anger.1'
                                                                        x[which(x==12)] <- 'ic.BFI2.21'
                                                                        x[which(x==13)] <- 'ic.BFI2.14'
                                                                        x[which(x==14)] <- 'ic.MCQ.GM5'
                                                                        x[which(x==15)] <- 'ic.BFI2.29'
                                                                        x[which(x==16)] <- 'ic.BFI2.27'
                                                                        x[which(x==17)] <- 'ic.BFI2.37'
                                                                        x[which(x==18)] <- 'ic.BFI2.5'
                                                                        x[which(x==19)] <- 'ic.BFI2.41'
                                                                        x[which(x==20)] <- 'ic.MCQ.F1'
                                                                        x[which(x==21)] <- 'ic.BFI2.2'
                                                                        x[which(x==22)] <- 'ic.BFI2.8'
                                                                        x[which(x==23)] <- 'ic.BFI2.15'
                                                                        x[which(x==24)] <- 'ic.BFI2.54'
                                                                        x[which(x==25)] <- 'ic.BFI2.40'
                                                                        x[which(x==26)] <- 'ic.BFI2.4'
                                                                        x[which(x==27)] <- 'ic.BFI2.12'
                                                                        x[which(x==28)] <- 'ic.MCQ.F4'
                                                                        x[which(x==29)] <- 'ic.MCQ.H3'
                                                                        x[which(x==30)] <- 'ic.BFI2.1'
                                                                        x[which(x==31)] <- 'ic.BFI2.24'
                                                                        x[which(x==32)] <- 'ic.anger.2'
                                                                        x[which(x==33)] <- 'ic.MCQ.L1'
                                                                        x[which(x==34)] <- 'ic.BFI2.33'
                                                                        x[which(x==35)] <- 'ic.MCQ.P1'
                                                                        x[which(x==36)] <- 'ic.MCQ.H4'
                                                                        x[which(x==37)] <- 'ic.BFI2.56'
                                                                        x[which(x==38)] <- 'ic.BFI2.34'
                                                                        x[which(x==39)] <- 'ic.MCQ.P4'
                                                                        x[which(x==40)] <- 'ic.BFI2.13'
                                                                        x[which(x==41)] <- 'ic.BFI2.10'
                                                                        x[which(x==42)] <- 'ic.BFI2.7'
                                                                        x
                                                                        })

# Replace blanks with NA
informant[,c('ic.top3.1.trait','ic.top3.2.trait',
             'ic.top3.3.trait','ic.top1.trait')] <- apply(informant[,c('ic.top3.1.trait','ic.top3.2.trait',
                                                                       'ic.top3.3.trait','ic.top1.trait')],
                                                          2,function(x) gsub("^$|^ $", NA, x))

#### Fill in informants' desired direction and amount of change for top 3/1 traits ####

ic.items.raw <- paste0('ic.',focalitems)

for(i in c('top3.1','top3.2','top3.3','top1')){
  for(j in ic.items.raw){  # and each of the items
    informant[which(informant[,paste0('ic.',i,'.trait')]==j),  # if the informant chose that trait,
           paste0('ic.',i,'.direction')] <- informant[which(informant[,paste0('ic.',i,'.trait')]==j),j]  # then fill in the direction from their raw change goal responses
  }
}

# Create simplified version of top3 & top1 traits (Trait + Direction)

for(i in c('top3.1','top3.2','top3.3','top1')){
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.1') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Sociable'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.31') &  # shy, introverted
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'Less Sociable'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.6','ic.BFI2.21') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Assertive'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.41','ic.BFI2.56') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Energetic' 
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.2','ic.BFI2.32') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Compassionate' 
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.7') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Respectful' 
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.37') &  # rude
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'Less Respectful' 
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.12') &  # tends to find fault with others
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'Less Trusting'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.27') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Trusting'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.3','ic.BFI2.33') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Organized'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.8') &  # lazy
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'Less Productive'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.53') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Productive'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.13','ic.BFI2.43') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Responsible'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.4') &    # relaxed, handles stress well
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'Less Anxious'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.34') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Anxious'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.24') &  # secure, comfortable with self
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'Less Depressed'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.54') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Depressed'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.29') &  # emotionally stable, not easily upset
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'Less Emotionally Volatile'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.14') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Emotionally Volatile'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.anger.1','ic.anger.2') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Angry'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.10','ic.BFI2.40') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Intellectually Curious'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.5','ic.BFI2.20') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Aesthetically Sensitive'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.15','ic.BFI2.60') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Creative/Imaginative'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.GM3','ic.MCQ.GM5') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Moral'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.H3','ic.MCQ.H4') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Honest'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.F1','ic.MCQ.F4') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Fair'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.L1','ic.MCQ.L4') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Loyal'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.P1','ic.MCQ.P4') &
                 informant[,paste0('ic.',i,'.direction')]>0),paste0('ic.',i)] <- 'More Pure'
  
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.1') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Sociable'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.31') &  # shy, introverted
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'More Sociable'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.6','ic.BFI2.21') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Assertive'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.41','ic.BFI2.56') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Energetic' 
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.2','ic.BFI2.32') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Compassionate' 
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.37') &  # rude
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'More Respectful' 
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.7') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Respectful' 
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.12') &  # tends to find fault in others
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'More Trusting'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.27') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Trusting'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.3','ic.BFI2.33') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Organized'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.8') &  # lazy
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'More Productive'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.53') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Productive'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.13','ic.BFI2.43') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Responsible'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.4') &  # relaxed, handles stress well
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'More Anxious'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.34') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Anxious'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.24') &  # secure, comfortable with self
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'More Depressed'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.54') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Depressed'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.29') &  # emotionally stable, not easily upset
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'More Emotionally Volatile'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.14') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Emotionally Volatile'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.anger.1','ic.anger.2') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Angry'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.10','ic.BFI2.40') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Intellectually Curious'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.5','ic.BFI2.20') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Aesthetically Sensitive'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.BFI2.15','ic.BFI2.60') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Creative/Imaginative'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.GM3','ic.MCQ.GM5') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Moral'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.H3','ic.MCQ.H4') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Honest'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.F1','ic.MCQ.F4') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Fair'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.L1','ic.MCQ.L4') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Loyal'
  informant[which(informant[,paste0('ic.',i,'.trait')] %in% c('ic.MCQ.P1','ic.MCQ.P4') &
                 informant[,paste0('ic.',i,'.direction')]<0),paste0('ic.',i)] <- 'Less Pure'
}

#### Compute Composites ####

informant$it.EXT.S <- rowMeans(informant[,c('it.BFI2.1','it.BFI2.16r',
                                      'it.BFI2.31r','it.BFI2.46')],na.rm=T)
informant$it.EXT.A <- rowMeans(informant[,c('it.BFI2.6','it.BFI2.21',
                                      'it.BFI2.36r','it.BFI2.51r')],na.rm=T)
informant$it.EXT.E <- rowMeans(informant[,c('it.BFI2.11r','it.BFI2.26r',
                                      'it.BFI2.41','it.BFI2.56')],na.rm=T)
informant$it.EXT <- rowMeans(informant[,c('it.EXT.S','it.EXT.A','it.EXT.E')],na.rm=T)

informant$it.AGR.C <- rowMeans(informant[,c('it.BFI2.2','it.BFI2.17r',
                                      'it.BFI2.32','it.BFI2.47r')],na.rm=T)
informant$it.AGR.R <- rowMeans(informant[,c('it.BFI2.7','it.BFI2.22r',
                                      'it.BFI2.37r','it.BFI2.52')],na.rm=T)
informant$it.AGR.T <- rowMeans(informant[,c('it.BFI2.12r','it.BFI2.27',
                                      'it.BFI2.42r','it.BFI2.57')],na.rm=T)
informant$it.AGR <- rowMeans(informant[,c('it.AGR.C','it.AGR.R','it.AGR.T')],na.rm=T)

informant$it.CON.O <- rowMeans(informant[,c('it.BFI2.3r','it.BFI2.18',
                                      'it.BFI2.33','it.BFI2.48r')],na.rm=T)
informant$it.CON.P <- rowMeans(informant[,c('it.BFI2.8r','it.BFI2.23r',
                                      'it.BFI2.38','it.BFI2.53')],na.rm=T)
informant$it.CON.R <- rowMeans(informant[,c('it.BFI2.13','it.BFI2.28r',
                                      'it.BFI2.43','it.BFI2.58r')],na.rm=T)
informant$it.CON <- rowMeans(informant[,c('it.CON.O','it.CON.P','it.CON.R')],na.rm=T)

informant$it.NEG.A <- rowMeans(informant[,c('it.BFI2.4r','it.BFI2.19',
                                      'it.BFI2.34','it.BFI2.49r')],na.rm=T)
informant$it.NEG.D <- rowMeans(informant[,c('it.BFI2.9r','it.BFI2.24r',
                                      'it.BFI2.39','it.BFI2.54')],na.rm=T)
informant$it.NEG.E <- rowMeans(informant[,c('it.BFI2.14','it.BFI2.29r',
                                      'it.BFI2.44r','it.BFI2.59')],na.rm=T)
informant$it.NEG <- rowMeans(informant[,c('it.NEG.A','it.NEG.D','it.NEG.E')],na.rm=T)

informant$it.anger <- rowMeans(informant[,c('it.anger.1','it.anger.2r')],na.rm=T)

informant$it.OPE.I <- rowMeans(informant[,c('it.BFI2.10','it.BFI2.25r',
                                      'it.BFI2.40','it.BFI2.55r')],na.rm=T)
informant$it.OPE.A <- rowMeans(informant[,c('it.BFI2.5r','it.BFI2.20',
                                      'it.BFI2.50r')],na.rm=T)  # cut the values item
informant$it.OPE.C <- rowMeans(informant[,c('it.BFI2.15','it.BFI2.30r',
                                      'it.BFI2.45r','it.BFI2.60')],na.rm=T)
informant$it.OPE <- rowMeans(informant[,c('it.OPE.I','it.OPE.A','it.OPE.C')],na.rm=T)

informant$it.HH <- rowMeans(informant[,c(paste0(it.reversehh,'r'),
                                   'it.HH.S2','it.HH.GA1','it.HH.F3',
                                   'it.HH.S4','it.HH.M1','it.HH.M2')],na.rm=T)

informant$it.MCQ.GM <- rowMeans(informant[,c('it.MCQ.GM1','it.MCQ.GM2r',
                                       'it.MCQ.GM3','it.MCQ.GM5')],na.rm=T)  # cut GM4 & GM6 (values items)

informant$it.MCQ.H <- rowMeans(informant[,c('it.MCQ.H2','it.MCQ.H3')],  # cut H1r & H4 (values items)
                            na.rm=T)

informant$it.MCQ.F <- rowMeans(informant[,c('it.MCQ.F1','it.MCQ.F2')],  # cut F3r & F4 (values items)
                            na.rm=T)

informant$it.MCQ.L <- rowMeans(informant[,c('it.MCQ.L1','it.MCQ.L2r')],  # cut L1 & L2r (values items)
                            na.rm=T)

informant$it.MCQ.P <- rowMeans(informant[,c('it.MCQ.P1','it.MCQ.P3r')],  # cut P2 & P4 (values items)
                            na.rm=T)

informant$ic.EXT.S <- rowMeans(informant[,c('ic.BFI2.1','ic.BFI2.31r')],na.rm=T)
informant$ic.EXT.A <- rowMeans(informant[,c('ic.BFI2.6','ic.BFI2.21')],na.rm=T)
informant$ic.EXT.E <- rowMeans(informant[,c('ic.BFI2.41','ic.BFI2.56')],na.rm=T)
informant$ic.EXT <- rowMeans(informant[,c('ic.EXT.S','ic.EXT.A','ic.EXT.E')],na.rm=T)

informant$ic.AGR.C <- rowMeans(informant[,c('ic.BFI2.2','ic.BFI2.32')],na.rm=T)
informant$ic.AGR.R <- rowMeans(informant[,c('ic.BFI2.7','ic.BFI2.37r')],na.rm=T)
informant$ic.AGR.T <- rowMeans(informant[,c('ic.BFI2.12r','ic.BFI2.27')],na.rm=T)
informant$ic.AGR <- rowMeans(informant[,c('ic.AGR.C','ic.AGR.R','ic.AGR.T')],na.rm=T)

informant$ic.CON.O <- rowMeans(informant[,c('ic.BFI2.3','ic.BFI2.33')],na.rm=T)
informant$ic.CON.P <- rowMeans(informant[,c('ic.BFI2.8r','ic.BFI2.53')],na.rm=T)
informant$ic.CON.R <- rowMeans(informant[,c('ic.BFI2.13','ic.BFI2.43')],na.rm=T)
informant$ic.CON <- rowMeans(informant[,c('ic.CON.O','ic.CON.P','ic.CON.R')],na.rm=T)

informant$ic.NEG.A <- rowMeans(informant[,c('ic.BFI2.4r','ic.BFI2.34')],na.rm=T)
informant$ic.NEG.D <- rowMeans(informant[,c('ic.BFI2.24r','ic.BFI2.54')],na.rm=T)
informant$ic.NEG.E <- rowMeans(informant[,c('ic.BFI2.14','ic.BFI2.29r')],na.rm=T)
informant$ic.NEG <- rowMeans(informant[,c('ic.NEG.A','ic.NEG.D','ic.NEG.E')],na.rm=T)

informant$ic.anger <- rowMeans(informant[,c('ic.anger.1','ic.anger.2')],na.rm=T)

informant$ic.OPE.I <- rowMeans(informant[,c('ic.BFI2.10','ic.BFI2.40')],na.rm=T)
informant$ic.OPE.A <- rowMeans(informant[,c('ic.BFI2.5','ic.BFI2.20')],na.rm=T)
informant$ic.OPE.C <- rowMeans(informant[,c('ic.BFI2.15','ic.BFI2.60')],na.rm=T)
informant$ic.OPE <- rowMeans(informant[,c('ic.OPE.I','ic.OPE.A','ic.OPE.C')],na.rm=T)

informant$ic.MCQ.GM <- rowMeans(informant[,c('ic.MCQ.GM3','ic.MCQ.GM5')],na.rm=T)
informant$ic.MCQ.H <- rowMeans(informant[,c('ic.MCQ.H3','ic.MCQ.H4')],na.rm=T)
informant$ic.MCQ.F <- rowMeans(informant[,c('ic.MCQ.F1','ic.MCQ.F4')],na.rm=T)
informant$ic.MCQ.L <- rowMeans(informant[,c('ic.MCQ.L1','ic.MCQ.L4')],na.rm=T)
informant$ic.MCQ.P <- rowMeans(informant[,c('ic.MCQ.P1','ic.MCQ.P4')],na.rm=T)

# create absolute change index
informant$ic.overall <- rowMeans(abs(informant[,ic.items.raw]),na.rm=T)

########################################################
####          Merge target & informant data         ####
########################################################

# Aggregate informant reports
# informant$targetid <- substr(informant$id,1,4)
# informantag <- aggregate(.~id, informant, mean)

informantag <- informant[,!names(informant) %in% c('it.gender','id','it.relationship','ic.top3.1.trait',
                                                   'ic.top3.2.trait','ic.top3.3.trait','ic.top1.trait',
                                                   'ic.top3.1','ic.top3.2','ic.top3.3',
                                                   'ic.top1','ic.top3','ic.top3.1.id','ic.top3.2.id','ic.top3.3.id')] %>%
  group_by(targetid) %>% 
  summarise_all(mean,na.rm=T)

# warning message is just because targetid is a character variable; this is ignorable

# Merge self & informant data
dat <- merge(target,informantag,by='targetid',all=T)

# Compute self-informant composites for current levels
allfacets <- c('EXT.S','EXT.A','EXT.E','AGR.C','AGR.R',
               'AGR.T','CON.O','CON.P','CON.R','NEG.A',
               'NEG.D','NEG.E','anger','OPE.I','OPE.A',
               'OPE.C','MCQ.GM','MCQ.H','MCQ.F','MCQ.L',
               'MCQ.P')

for(i in allfacets){
  dat[,paste0('tsit.',i)] <- rowMeans(dat[,c(paste0('ts.',i),paste0('it.',i))],na.rm=T) 
}

# Count number of informants per target
informant <- data.table(informant)
informant[ , numinformants := .N, by = list(targetid)] 
informant <- data.frame(informant)

# Add number of informants to main dataframe
num.informants <- distinct(informant,targetid,.keep_all=T)
dat <- merge(dat,num.informants[,c('targetid','numinformants')],by='targetid',all.x=T)
dat[is.na(dat$numinformants),'numinformants'] <- 0

# Export data
write.csv(dat,'./Data/sample1-dat-clean.csv',row.names=F)
write.csv(informant,'./Data/sample1-informant-clean.csv',row.names=F)

########################################################
###    Compute composites for trait norming data    ####
########################################################

# 1. Take the absolute value of the moral valence rating for each item
allitems.raw <- c('BFI2.1','BFI2.31','BFI2.6','BFI2.21',
                  'BFI2.41','BFI2.56','BFI2.2','BFI2.32',
                  'BFI2.7','BFI2.37','BFI2.12','BFI2.27',
                  'BFI2.3','BFI2.33','BFI2.8','BFI2.53',
                  'BFI2.13','BFI2.43','BFI2.4','BFI2.34',
                  'BFI2.24','BFI2.54','BFI2.14','BFI2.29',
                  'anger.1','anger.2','BFI2.10','BFI2.40',
                  'BFI2.5','BFI2.20','BFI2.15','BFI2.60',
                  'MCQ.GM3','MCQ.GM5','MCQ.H3','MCQ.H4',
                  'MCQ.F1','MCQ.F4','MCQ.L1','MCQ.L4','MCQ.P1','MCQ.P4')

norms[,paste0('mr.',allitems.raw)] <- abs(norms[,paste0('mv.',allitems.raw)])

# 2. Compute 2-item composites for all norming variables

for(i in c('mr.','ch.')){
  norms[,paste0(i,'EXT.S')] <- rowMeans(norms[,c(paste0(i,'BFI2.1'),paste0(i,'BFI2.31'))],na.rm=T)
  norms[,paste0(i,'EXT.A')] <- rowMeans(norms[,c(paste0(i,'BFI2.6'),paste0(i,'BFI2.21'))],na.rm=T)
  norms[,paste0(i,'EXT.E')] <- rowMeans(norms[,c(paste0(i,'BFI2.41'),paste0(i,'BFI2.56'))],na.rm=T)
  norms[,paste0(i,'EXT')] <- rowMeans(norms[,c(paste0(i,'EXT.S'),paste0(i,'EXT.A'),paste0(i,'EXT.E'))],na.rm=T)
  
  norms[,paste0(i,'AGR.C')] <- rowMeans(norms[,c(paste0(i,'BFI2.2'),paste0(i,'BFI2.32'))],na.rm=T)
  norms[,paste0(i,'AGR.R')] <- rowMeans(norms[,c(paste0(i,'BFI2.7'),paste0(i,'BFI2.37'))],na.rm=T)
  norms[,paste0(i,'AGR.T')] <- rowMeans(norms[,c(paste0(i,'BFI2.12'),paste0(i,'BFI2.27'))],na.rm=T)
  norms[,paste0(i,'AGR')] <- rowMeans(norms[,c(paste0(i,'AGR.C'),paste0(i,'AGR.R'),paste0(i,'AGR.T'))],na.rm=T)
  
  norms[,paste0(i,'CON.O')]<- rowMeans(norms[,c(paste0(i,'BFI2.3'),paste0(i,'BFI2.33'))],na.rm=T)
  norms[,paste0(i,'CON.P')] <- rowMeans(norms[,c(paste0(i,'BFI2.8'),paste0(i,'BFI2.53'))],na.rm=T)
  norms[,paste0(i,'CON.R')] <- rowMeans(norms[,c(paste0(i,'BFI2.13'),paste0(i,'BFI2.43'))],na.rm=T)
  norms[,paste0(i,'CON')] <- rowMeans(norms[,c(paste0(i,'CON.O'),paste0(i,'CON.P'),paste0(i,'CON.R'))],na.rm=T)
  
  norms[,paste0(i,'NEG.A')] <- rowMeans(norms[,c(paste0(i,'BFI2.4'),paste0(i,'BFI2.34'))],na.rm=T)
  norms[,paste0(i,'NEG.D')] <- rowMeans(norms[,c(paste0(i,'BFI2.24'),paste0(i,'BFI2.54'))],na.rm=T)
  norms[,paste0(i,'NEG.E')] <- rowMeans(norms[,c(paste0(i,'BFI2.14'),paste0(i,'BFI2.29'))],na.rm=T)
  norms[,paste0(i,'NEG')] <- rowMeans(norms[,c(paste0(i,'NEG.A'),paste0(i,'NEG.D'),paste0(i,'NEG.E'))],na.rm=T)
  
  norms[,paste0(i,'anger')] <- rowMeans(norms[,c(paste0(i,'anger.1'),paste0(i,'anger.2'))],na.rm=T)
  
  norms[,paste0(i,'OPE.I')] <- rowMeans(norms[,c(paste0(i,'BFI2.10'),paste0(i,'BFI2.40'))],na.rm=T)
  norms[,paste0(i,'OPE.A')] <- rowMeans(norms[,c(paste0(i,'BFI2.5'),paste0(i,'BFI2.20'))],na.rm=T)
  norms[,paste0(i,'OPE.C')] <- rowMeans(norms[,c(paste0(i,'BFI2.15'),paste0(i,'BFI2.60'))],na.rm=T)
  norms[,paste0(i,'OPE')] <- rowMeans(norms[,c(paste0(i,'OPE.I'),paste0(i,'OPE.A'),paste0(i,'OPE.C'))],na.rm=T)
  
  norms[,paste0(i,'MCQ.GM')] <- rowMeans(norms[,c(paste0(i,'MCQ.GM3'),paste0(i,'MCQ.GM5'))],na.rm=T)
  norms[,paste0(i,'MCQ.H')] <- rowMeans(norms[,c(paste0(i,'MCQ.H3'),paste0(i,'MCQ.H4'))],na.rm=T)
  norms[,paste0(i,'MCQ.F')] <- rowMeans(norms[,c(paste0(i,'MCQ.F1'),paste0(i,'MCQ.F4'))],na.rm=T)
  norms[,paste0(i,'MCQ.L')] <- rowMeans(norms[,c(paste0(i,'MCQ.L1'),paste0(i,'MCQ.L4'))],na.rm=T)
  norms[,paste0(i,'MCQ.P')] <- rowMeans(norms[,c(paste0(i,'MCQ.P1'),paste0(i,'MCQ.P4'))],na.rm=T)
}

# Also reverse items and compute composites for moral valence norms

reversemv <- paste0('mv.',c('BFI2.31','BFI2.37','BFI2.12','BFI2.8',
                            'BFI2.4','BFI2.24','BFI2.29'))

norms[,paste0(reversemv,'r')] <- -1*norms[,reversemv]

norms[,'mv.EXT.S'] <- rowMeans(norms[,c('mv.BFI2.1','mv.BFI2.31r')],na.rm=T)
norms[,'mv.EXT.A'] <- rowMeans(norms[,c('mv.BFI2.6','mv.BFI2.21')],na.rm=T)
norms[,'mv.EXT.E'] <- rowMeans(norms[,c('mv.BFI2.41','mv.BFI2.56')],na.rm=T)
norms[,'mv.EXT'] <- rowMeans(norms[,c('mv.EXT.S','mv.EXT.A','mv.EXT.E')],na.rm=T)

norms[,'mv.AGR.C'] <- rowMeans(norms[,c('mv.BFI2.2','mv.BFI2.32')],na.rm=T)
norms[,'mv.AGR.R'] <- rowMeans(norms[,c('mv.BFI2.7','mv.BFI2.37r')],na.rm=T)
norms[,'mv.AGR.T'] <- rowMeans(norms[,c('mv.BFI2.12r','mv.BFI2.27')],na.rm=T)
norms[,'mv.AGR'] <- rowMeans(norms[,c('mv.AGR.C','mv.AGR.R','mv.AGR.T')],na.rm=T)

norms[,'mv.CON.O']<- rowMeans(norms[,c('mv.BFI2.3','mv.BFI2.33')],na.rm=T)
norms[,'mv.CON.P'] <- rowMeans(norms[,c('mv.BFI2.8r','mv.BFI2.53')],na.rm=T)
norms[,'mv.CON.R'] <- rowMeans(norms[,c('mv.BFI2.13','mv.BFI2.43')],na.rm=T)
norms[,'mv.CON'] <- rowMeans(norms[,c('mv.CON.O','mv.CON.P','mv.CON.R')],na.rm=T)

norms[,'mv.NEG.A'] <- rowMeans(norms[,c('mv.BFI2.4r','mv.BFI2.34')],na.rm=T)
norms[,'mv.NEG.D'] <- rowMeans(norms[,c('mv.BFI2.24r','mv.BFI2.54')],na.rm=T)
norms[,'mv.NEG.E'] <- rowMeans(norms[,c('mv.BFI2.14','mv.BFI2.29r')],na.rm=T)
norms[,'mv.NEG'] <- rowMeans(norms[,c('mv.NEG.A','mv.NEG.D','mv.NEG.E')],na.rm=T)

norms[,'mv.anger'] <- rowMeans(norms[,c('mv.anger.1','mv.anger.2')],na.rm=T)

norms[,'mv.OPE.I'] <- rowMeans(norms[,c('mv.BFI2.10','mv.BFI2.40')],na.rm=T)
norms[,'mv.OPE.A'] <- rowMeans(norms[,c('mv.BFI2.5','mv.BFI2.20')],na.rm=T)
norms[,'mv.OPE.C'] <- rowMeans(norms[,c('mv.BFI2.15','mv.BFI2.60')],na.rm=T)
norms[,'mv.OPE'] <- rowMeans(norms[,c('mv.OPE.I','mv.OPE.A','mv.OPE.C')],na.rm=T)

norms[,'mv.MCQ.GM'] <- rowMeans(norms[,c('mv.MCQ.GM3','mv.MCQ.GM5')],na.rm=T)
norms[,'mv.MCQ.H'] <- rowMeans(norms[,c('mv.MCQ.H3','mv.MCQ.H4')],na.rm=T)
norms[,'mv.MCQ.F'] <- rowMeans(norms[,c('mv.MCQ.F1','mv.MCQ.F4')],na.rm=T)
norms[,'mv.MCQ.L'] <- rowMeans(norms[,c('mv.MCQ.L1','mv.MCQ.L4')],na.rm=T)
norms[,'mv.MCQ.P'] <- rowMeans(norms[,c('mv.MCQ.P1','mv.MCQ.P4')],na.rm=T)

norms[is.na(norms)] <- NA

write.csv(norms,'./Data/sample1-norms-clean.csv',row.names=F)

