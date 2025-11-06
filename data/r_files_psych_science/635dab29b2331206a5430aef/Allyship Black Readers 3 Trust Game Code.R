
#### Study 3 - Detecting prejudice from egalitarianism 

library(lmerTest) 
library(MASS)
library(ggplot2)
library(psych)
library(car)
library(pscl)
library(dplyr)
library(jtools)
library(MuMIn)


AllyshipB3 <- read.csv("Allyship Black Reader 3 - Long.csv")

#Creating new variable so that "Money" is in cents instead of the 11-point scale
AllyshipB3$moneyR <- car::recode(AllyshipB3$Money,"1=0;2=.02;3=.04;4=.06;5=.08;6=.1;7=.12;8=.14;9=.16;10=.18;11=.20")


##mean center moneyR
AllyshipB3$moneyR_c <- scale(AllyshipB3$moneyR, center = TRUE, scale = FALSE)
AllyshipB3$Money_c <- scale(AllyshipB3$Money, center = TRUE, scale = FALSE)
AllyshipB3$moneyR_z <- scale(AllyshipB3$moneyR, center = TRUE, scale = TRUE)
AllyshipB3$Money_z <- scale(AllyshipB3$Money, center = TRUE, scale = TRUE)
describe(AllyshipB3$Money)
describe(AllyshipB3$Money_z) 
describe(AllyshipB3$moneyR)
describe(AllyshipB3$moneyR_z) 
colnames(AllyshipB3)
View(AllyshipB3[,c(66:67)])

AllyshipB3$ParticipantActualIMS_c <- scale(AllyshipB3$ParticipantActualIMS, center = TRUE, scale = FALSE)
AllyshipB3$ParticipantActualMR_c <- scale(AllyshipB3$ParticipantActualMR, center = TRUE, scale = FALSE)
AllyshipB3$ParticipantActualIMS_z <- scale(AllyshipB3$ParticipantActualIMS, center = TRUE, scale = TRUE)
AllyshipB3$ParticipantActualMR_z <- scale(AllyshipB3$ParticipantActualMR, center = TRUE, scale = TRUE)
AllyshipB3$ParticipantActualEMS_z <- scale(AllyshipB3$ParticipantActualEMS, center = TRUE, scale = TRUE)
describe(AllyshipB3$ParticipantActualIMS_c)
describe(AllyshipB3$ParticipantActualMR_c)
describe(AllyshipB3$ParticipantActualIMS_z)
describe(AllyshipB3$ParticipantActualMR_z)


describe(AllyshipB3$ParticipantActualIMS)
hist(AllyshipB3$ParticipantActualIMS)
describe(AllyshipB3$ParticipantActualMR)
hist(AllyshipB3$ParticipantActualMR)



####  GRAPHS
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family="Times New Roman"),
        legend.title=element_blank())

##### APA STYLE
theme_apa()
theme_apa(legend.pos="right", legend.use.title=FALSE, legend.font.size=12, y.font.size=12, facet.title.size=12, remove.y.gridlines=TRUE, remove.x.gridlines=TRUE)



###### MODELS USED IN EXPERIMENT 3


#IMS model
mod2ims.1.z <- lmer(moneyR_z~ParticipantActualIMS_z + (1 | ParticipantID) + (1 | ActualWriterID), data=AllyshipB3); summary(mod2ims.1.z); 
confint(mod2ims.1.z) 


apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family="Times New Roman"),
        legend.title=element_blank())

##### APA STYLE
theme_apa()
theme_apa(legend.pos="right", legend.use.title=FALSE, legend.font.size=12, y.font.size=12, facet.title.size=12, remove.y.gridlines=TRUE, remove.x.gridlines=TRUE)




###--- IMS plot
pdf(file="~/Downloads/ally_imsPlot.pdf",
    width = 4,
    height = 5) #go to your Downloads folder to find this graph

p.ims.apa2 <- ggplot(AllyshipB3, aes(x=ParticipantActualIMS, y=moneyR)) + 
  geom_smooth(method=lm, fullrange=TRUE, color='black') + 
  theme_bw() + 
  scale_color_manual(values=c("black")) +
  apatheme +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,.2)) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(1,7)) +
  xlab("White Writers' IMS Score") + ylab("Money Shared by Black Perceivers")

p.ims.apa2 + theme_apa()
dev.off()


plotIMS <- p2 + theme_bw() +
  geom_smooth(method=lm, fullrange=TRUE, color='black') +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,.2)) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(1,7)) +
  ggtitle(" ") + ylab("Money Shared by Black Perceivers") + xlab("White Writers' IMS Score") +
  theme(legend.position = c(0.9, 0.8), text=element_text(size=16),
        plot.title = element_text(color="black", size=16, face="bold", hjust=0.5),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color="black", size=16),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.text.y = element_text(color="black", size=16),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        legend.title = element_text(color="black", size=14, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line=element_line(colour = "black"))

#ggsave("plotIMS.png", width = 15, height = 10, dpi=800) #download high-res image





###--- MRS model
mod2mr.1.z <- lmer(moneyR_z~ParticipantActualMR_z + (1 | ParticipantID) + (1 | ActualWriterID), data=AllyshipB3); summary(mod2mr.1.z); 
confint(mod2mr.1.z) 


###--- MRS plot 
pdf(file="~/Downloads/ally_mrsPlot.pdf",
    width = 4,
    height = 5)

p.mr.apa2 <- ggplot(AllyshipB3, aes(x=ParticipantActualMR, y=moneyR)) + 
  geom_smooth(method=lm, fullrange=TRUE, color='black') + 
  theme_bw() + 
  scale_color_manual(values=c("black")) +
  apatheme +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,.2)) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(1,7)) +
  xlab("White Writers' MRS Score") + ylab("Money Shared by Black Perceivers")

p.mr.apa2 +theme_apa()
dev.off()


###--- EMS model
mod2ems.1.z <- lmer(moneyR_z~ParticipantActualEMS_z + (1 | ParticipantID) + (1 | ActualWriterID), data=AllyshipB3); summary(mod2mr.1.z); 
confint(mod2mr.1.z) 

###--- EMS plot 
pdf(file="~/Downloads/ally_emsPlot.pdf",
    width = 4,
    height = 5)

p.ems.apa2 <- ggplot(AllyshipB3, aes(x=ParticipantActualEMS, y=moneyR)) + 
  geom_smooth(method=lm, fullrange=TRUE, color='black') + 
  theme_bw() + 
  scale_color_manual(values=c("black")) +
  apatheme +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,.2)) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(1,7)) +
  xlab("White Writers' EMS Score") + ylab("Money Shared by Black Perceivers")

p.ems.apa2 +theme_apa()
dev.off()
