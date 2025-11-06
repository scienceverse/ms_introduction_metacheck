###Children’s Cognitive Reflection Predicts Conceptual Understanding in Science and Mathematics 
###Descriptive analyses reported in main text

##########################################################################################
###Load packages & data, make variables, & calculate Omega Total / alpha (when applicable) 
##########################################################################################


library(ggplot2)
library(psych)
library(plyr)

#Data is available at https://osf.io/e72ka/?view_only=57c8b2f8cf7b4193af2df40af14c71d7
data=read.csv("CRTD_DATA.csv")

###CRTD
CRTD<- data[,c("crt.cows","crt.emily","crt.butterfly",
               "crt.pound","crt.sheep","crt.santa",
               "crt.race","crt.apples","crt.mo")]
data$CRTD <- rowSums(CRTD)
psych::alpha(CRTD)$total$raw_alpha # scale alpha


#Omega Total following Revelle & Condon (2018)
CRTD_nfact <- fa.parallel(fa="fa",CRTD,cor="tet",quant=.95,n.iter=300)$nfact
omega(CRTD,nfactors = CRTD_nfact,flip=FALSE,plot=FALSE)$omega.tot


###Denominator Neglect
DN <- data[,c("T.1.9.vs.9.91","T.1.9.vs.8.92","T.2.8.vs.18.82","T.2.8.vs.19.81",
              "T.3.7.vs.29.71","T.3.7.vs.28.72")]
data$DN <- rowSums(DN)
psych::alpha(DN)$total$raw_alpha #scale alpha

DN_nfact <- fa.parallel(fa="fa",DN,cor="tet",quant=.95,n.iter=300)$nfact
omega(DN,nfactors = DN_nfact,flip=FALSE,plot=FALSE)$omega.tot


###Base Rate Sensitivity
BRS <- data[,c("brs1","brs2","brs3","brs4","brs5")]
data$BRS <- rowSums(BRS)
psych::alpha(BRS)

BRS_nfact <- fa.parallel(fa="fa",BRS,cor="tet",quant=.95,n.iter=300)$nfact
omega(BRS,nfactors = BRS_nfact,flip=FALSE,plot=FALSE)$omega.tot


###NFC
NFC <- data[,c("NFC_1","NFC_2","NFC_3","NFC_4","NFC_5","NFC_6","NFC_7",                 
               "NFC_8","NFC_9","NFC_10","NFC_11","NFC_12", "NFC_13","NFC_14")]
data$NFC <- rowMeans(NFC)
psych::alpha(NFC)$total$raw_alpha #scale alpha

NFC_nfact <- fa.parallel(fa="fa",NFC,cor="poly",quant=.95,correct=TRUE,n.iter=300)$nfact
omega(NFC,nfactors = NFC_nfact,flip=FALSE,plot=FALSE)$omega.tot 


###AOT
AOT <- data[,c("OMT_10","OMT_11","OMT_12","OMT_13_R","OMT_14_R","OMT_15_R","OMT_16_R")]  
#Scale alpha. Note: warning suggests that items need to be reversed, but this has already been done in the data set
alpha(AOT)$total$raw_alpha

AOT_nfact <- fa.parallel(fa="fa",AOT,cor="poly",correct = TRUE,quant=.95,n.iter=300)$nfact
omega(AOT,nfactors = AOT_nfact,flip=FALSE,plot=FALSE)$omega.tot

ci.reliability(AOT)
###Mathematical Equivalence: Number Correct and Any Correct
ME <- data[,c("ME1_correct","ME2_correct","ME3_correct","ME4_correct")]
alpha(ME)$total$raw_alpha

ME_nfact <- fa.parallel(fa="fa",ME,cor="tet",correct = TRUE,quant=.95,n.iter=300)$nfact
omega(ME,nfactors = ME_nfact,flip=FALSE,plot=FALSE)$omega.tot

data$ME_Sum <- rowSums(ME,na.rm=FALSE)
data$ME_Any<- ifelse(data$ME_Sum >0,1,0)

###BIO Composite
BIO <- data[,c("BodyParts","ANIMISM_Sum")]
BIO <- scale(BIO)
data$BIOcomp <- rowMeans(scale(BIO),na.rm=FALSE)

#########################################
####Descriptive Statistics & Correlations
#########################################

###Data for Table 2
table_data <- data[,c( "CRTD" ,
                        "MOS", 
                        "DCCS","FLANKER",
                        "VF","SPAN",
                        "DN","BRS",
                        "NFC",
                        "BIOcomp","BodyParts","ANIMISM_Sum","ME_Sum")] 


###Varible Descriptives
describe(table_data)

###Variable Correlations
psych::corr.test(table_data)

###CRTD by Gender Descriptives
describeBy(data$CRTD,data$Sex)

###Math Equivalence Distribution
table(data$ME_Sum)/86

###FIGURE 1
figdata <- data

#Months to Years
figdata$years <- as.factor(floor(figdata$MOS/12))
figdata <- subset(figdata,years != 4) #Ignore 58 month old (see Figure 1 caption)

###add year sample sizes
figdata$years <- revalue(figdata$years, c("5"="5\n(25)", 
                                          "6"="6\n(20)", 
                                          "7"="7\n(25)",
                                          "8"="8\n(31)",
                                          "9"="9\n(17)",
                                          "10"="10\n(16)",
                                          "11"="11\n(11)",
                                          "12"="12\n(4)"))

Fig1 <- ggplot(figdata, aes(x=years, y=CRTD))+
  stat_summary(fun.y=mean, geom="point", color="red",shape=18,size=3)+
  geom_count(shape=1,show.legend=FALSE,stroke=2) +    # stroke = 2 looks good for pdf
  scale_y_continuous(name="CRT-D Score",limits=c(-.25,9.25),expand=c(0,0),breaks=c(0,1,2,3,4,5,6,7,8,9))+
  scale_x_discrete(name="Age")+
  theme_bw() +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)

Fig1
#ggsave("Fig1.pdf", width = 3.25, height = 3.25)
