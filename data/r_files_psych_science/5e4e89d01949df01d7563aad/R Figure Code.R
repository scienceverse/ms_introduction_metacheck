require(ggplot2)
require(RColorBrewer)
Gallmean=mean(Gallup$Age,na.rm=T)

###Note that the model was fitted when the age variable was mean-centered.
###When plotting, we manually change the x-axis limits to 20-80 years for more intuitive graphs. 

###Figures: Age Trends
#LS
#get parameters
LSage2intercepts=LSage2$coefficients$fixed[1]+LSage2$coefficients$random$Region[,1]
LSage2coefs1=LSage2$coefficients$fixed[2]+LSage2$coefficients$random$Region[,2]
LSage2coefs2=LSage2$coefficients$fixed[3]+LSage2$coefficients$random$Region[,3]
#data for the lines 
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl=LSage2$coefficients$fixed[1]+LSage2$coefficients$fixed[2]*x+LSage2$coefficients$fixed[3]*x2
A=LSage2intercepts[1]+LSage2coefs1[1]*x+LSage2coefs2[1]*x2
LE=LSage2intercepts[2]+LSage2coefs1[2]*x+LSage2coefs2[2]*x2
GE=LSage2intercepts[3]+LSage2coefs1[3]*x+LSage2coefs2[3]*x2
NE=LSage2intercepts[4]+LSage2coefs1[4]*x+LSage2coefs2[4]*x2
EE=LSage2intercepts[5]+LSage2coefs1[5]*x+LSage2coefs2[5]*x2
CA=LSage2intercepts[6]+LSage2coefs1[6]*x+LSage2coefs2[6]*x2
LA=LSage2intercepts[7]+LSage2coefs1[7]*x+LSage2coefs2[7]*x2
SA=LSage2intercepts[8]+LSage2coefs1[8]*x+LSage2coefs2[8]*x2
Ar=LSage2intercepts[9]+LSage2coefs1[9]*x+LSage2coefs2[9]*x2
Af=LSage2intercepts[10]+LSage2coefs1[10]*x+LSage2coefs2[10]*x2
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))#regional factor to code each subplot
LS=c(Gl,A,LE,GE,NE,EE,CA,LA,SA,Ar,Af,rep(Gl[1],6001),rep(A[1],6001),rep(LE[1],6001),rep(GE[1],6001),rep(NE[1],6001),rep(EE[1],6001),rep(CA[1],6001),rep(LA[1],6001),rep(SA[1],6001),rep(Ar[1],6001),rep(Af[1],6001))
Age=rep(x,22)
compare=factor(c(rep(1,66011),rep(0,66011)),levels=c('1','0'))#factor codes the trendline vs. 20-year-olds
plotdata=data.frame(LS,Region,Age,compare)
#data for the fill
baseline=c(rep(Gl[1],6001),rep(A[1],6001),rep(LE[1],6001),rep(GE[1],6001),rep(NE[1],6001),rep(EE[1],6001),rep(CA[1],6001),rep(LA[1],6001),rep(SA[1],6001),rep(Ar[1],6001),rep(Af[1],6001))#SWB at 20 years
mins=pmin(baseline,LS[1:66011])#get min y value (comparing baseline vs. all others)
maxs=pmax(baseline,LS[1:66011])#get max y value
differences=c(Gl-Gl[1],A-A[1],LE-LE[1],GE-GE[1],NE-NE[1],EE-EE[1],CA-CA[1],LA-LA[1],SA-SA[1],Ar-Ar[1],Af-Af[1])
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'#vector coding size of all the effects
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]#age range that has small effect
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]#age range that has medium effect
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]#age range that has large effect
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]#age range that has small effect
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]#age range that has medium effect
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]#age range that has large effect
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=LS,linetype=compare),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+#edit breaks because we mean-centered
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+#orders the 11 plots in 4 cols
  theme_bw()+#removes grey background
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Trend","20-year-olds"))+#name must have blanks, else ggplot puts other legend first
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())#adjust position of legends and remove minor x gridlines
plot


#PA
#get parameters
PAage2intercepts=PAage2$coefficients$fixed[1]+PAage2$coefficients$random$Region[,1]
PAage2coefs1=PAage2$coefficients$fixed[2]+PAage2$coefficients$random$Region[,2]
PAage2coefs2=PAage2$coefficients$fixed[3]+PAage2$coefficients$random$Region[,3]
#data for the lines 
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl=PAage2$coefficients$fixed[1]+PAage2$coefficients$fixed[2]*x+PAage2$coefficients$fixed[3]*x2
A=PAage2intercepts[1]+PAage2coefs1[1]*x+PAage2coefs2[1]*x2
LE=PAage2intercepts[2]+PAage2coefs1[2]*x+PAage2coefs2[2]*x2
GE=PAage2intercepts[3]+PAage2coefs1[3]*x+PAage2coefs2[3]*x2
NE=PAage2intercepts[4]+PAage2coefs1[4]*x+PAage2coefs2[4]*x2
EE=PAage2intercepts[5]+PAage2coefs1[5]*x+PAage2coefs2[5]*x2
CA=PAage2intercepts[6]+PAage2coefs1[6]*x+PAage2coefs2[6]*x2
LA=PAage2intercepts[7]+PAage2coefs1[7]*x+PAage2coefs2[7]*x2
SA=PAage2intercepts[8]+PAage2coefs1[8]*x+PAage2coefs2[8]*x2
Ar=PAage2intercepts[9]+PAage2coefs1[9]*x+PAage2coefs2[9]*x2
Af=PAage2intercepts[10]+PAage2coefs1[10]*x+PAage2coefs2[10]*x2
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
PA=c(Gl,A,LE,GE,NE,EE,CA,LA,SA,Ar,Af,rep(Gl[1],6001),rep(A[1],6001),rep(LE[1],6001),rep(GE[1],6001),rep(NE[1],6001),rep(EE[1],6001),rep(CA[1],6001),rep(LA[1],6001),rep(SA[1],6001),rep(Ar[1],6001),rep(Af[1],6001))
Age=rep(x,22)
compare=factor(c(rep(1,66011),rep(0,66011)),levels=c('1','0'))
plotdata=data.frame(PA,Region,Age,compare)
#data for the fill
baseline=c(rep(Gl[1],6001),rep(A[1],6001),rep(LE[1],6001),rep(GE[1],6001),rep(NE[1],6001),rep(EE[1],6001),rep(CA[1],6001),rep(LA[1],6001),rep(SA[1],6001),rep(Ar[1],6001),rep(Af[1],6001))#SWB at 20 years
mins=pmin(baseline,PA[1:66011])
maxs=pmax(baseline,PA[1:66011])
differences=c(Gl-Gl[1],A-A[1],LE-LE[1],GE-GE[1],NE-NE[1],EE-EE[1],CA-CA[1],LA-LA[1],SA-SA[1],Ar-Ar[1],Af-Af[1])
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=PA,linetype=compare),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Trend","20-year-olds"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot


#NA
#get parameters
NAage2intercepts=NAage2$coefficients$fixed[1]+NAage2$coefficients$random$Region[,1]
NAage2coefs1=NAage2$coefficients$fixed[2]+NAage2$coefficients$random$Region[,2]
NAage2coefs2=NAage2$coefficients$fixed[3]+NAage2$coefficients$random$Region[,3]
#data for the lines 
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl=NAage2$coefficients$fixed[1]+NAage2$coefficients$fixed[2]*x+NAage2$coefficients$fixed[3]*x2
A=NAage2intercepts[1]+NAage2coefs1[1]*x+NAage2coefs2[1]*x2
LE=NAage2intercepts[2]+NAage2coefs1[2]*x+NAage2coefs2[2]*x2
GE=NAage2intercepts[3]+NAage2coefs1[3]*x+NAage2coefs2[3]*x2
NE=NAage2intercepts[4]+NAage2coefs1[4]*x+NAage2coefs2[4]*x2
EE=NAage2intercepts[5]+NAage2coefs1[5]*x+NAage2coefs2[5]*x2
CA=NAage2intercepts[6]+NAage2coefs1[6]*x+NAage2coefs2[6]*x2
LA=NAage2intercepts[7]+NAage2coefs1[7]*x+NAage2coefs2[7]*x2
SA=NAage2intercepts[8]+NAage2coefs1[8]*x+NAage2coefs2[8]*x2
Ar=NAage2intercepts[9]+NAage2coefs1[9]*x+NAage2coefs2[9]*x2
Af=NAage2intercepts[10]+NAage2coefs1[10]*x+NAage2coefs2[10]*x2
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
Neg=c(Gl,A,LE,GE,NE,EE,CA,LA,SA,Ar,Af,rep(Gl[1],6001),rep(A[1],6001),rep(LE[1],6001),rep(GE[1],6001),rep(NE[1],6001),rep(EE[1],6001),rep(CA[1],6001),rep(LA[1],6001),rep(SA[1],6001),rep(Ar[1],6001),rep(Af[1],6001))
Age=rep(x,22)
compare=factor(c(rep(1,66011),rep(0,66011)),levels=c('1','0'))
plotdata=data.frame(PA,Region,Age,compare)
#data for the fill
baseline=c(rep(Gl[1],6001),rep(A[1],6001),rep(LE[1],6001),rep(GE[1],6001),rep(NE[1],6001),rep(EE[1],6001),rep(CA[1],6001),rep(LA[1],6001),rep(SA[1],6001),rep(Ar[1],6001),rep(Af[1],6001))#SWB at 20 years
mins=pmin(baseline,Neg[1:66011])
maxs=pmax(baseline,Neg[1:66011])
differences=c(Gl-Gl[1],A-A[1],LE-LE[1],GE-GE[1],NE-NE[1],EE-EE[1],CA-CA[1],LA-LA[1],SA-SA[1],Ar-Ar[1],Af-Af[1])
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=Neg,linetype=compare),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(name='NA',limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Trend","20-year-olds"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

###Aggregate Figure
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
LSmar1=(LSmaritalXage2$coefficients$fixed[1]+LSmaritalXage2$coefficients$fixed[2]+LSmaritalXage2$coefficients$fixed[3]*x+LSmaritalXage2$coefficients$fixed[4]*x2+LSmaritalXage2$coefficients$fixed[5]*x+LSmaritalXage2$coefficients$fixed[6]*x2)
LSmar2=(LSmaritalXage2$coefficients$fixed[1]+LSmaritalXage2$coefficients$fixed[3]*x+LSmaritalXage2$coefficients$fixed[4]*x2)
LSemp1=(LSemployXage2$coefficients$fixed[1]+LSemployXage2$coefficients$fixed[2]+LSemployXage2$coefficients$fixed[3]*x+LSemployXage2$coefficients$fixed[4]*x2+LSemployXage2$coefficients$fixed[5]*x+LSemployXage2$coefficients$fixed[6]*x2)
LSemp2=(LSemployXage2$coefficients$fixed[1]+LSemployXage2$coefficients$fixed[3]*x+LSemployXage2$coefficients$fixed[4]*x2)
LSpro1=(LSprosocialXage2$coefficients$fixed[1]+LSprosocialXage2$coefficients$fixed[2]+LSprosocialXage2$coefficients$fixed[3]*x+LSprosocialXage2$coefficients$fixed[4]*x2+LSprosocialXage2$coefficients$fixed[5]*x+LSprosocialXage2$coefficients$fixed[6]*x2)
LSpro2=(LSprosocialXage2$coefficients$fixed[1]+LSprosocialXage2$coefficients$fixed[3]*x+LSprosocialXage2$coefficients$fixed[4]*x2)
LSmean1=(LSmeaningXage2$coefficients$fixed[1]+LSmeaningXage2$coefficients$fixed[2]+LSmeaningXage2$coefficients$fixed[3]*x+LSmeaningXage2$coefficients$fixed[4]*x2+LSmeaningXage2$coefficients$fixed[5]*x+LSmeaningXage2$coefficients$fixed[6]*x2)
LSmean2=(LSmeaningXage2$coefficients$fixed[1]+LSmeaningXage2$coefficients$fixed[3]*x+LSmeaningXage2$coefficients$fixed[4]*x2)
PAmar1=(PAmaritalXage2$coefficients$fixed[1]+PAmaritalXage2$coefficients$fixed[2]+PAmaritalXage2$coefficients$fixed[3]*x+PAmaritalXage2$coefficients$fixed[4]*x2+PAmaritalXage2$coefficients$fixed[5]*x+PAmaritalXage2$coefficients$fixed[6]*x2)
PAmar2=(PAmaritalXage2$coefficients$fixed[1]+PAmaritalXage2$coefficients$fixed[3]*x+PAmaritalXage2$coefficients$fixed[4]*x2)
PAemp1=(PAemployXage2$coefficients$fixed[1]+PAemployXage2$coefficients$fixed[2]+PAemployXage2$coefficients$fixed[3]*x+PAemployXage2$coefficients$fixed[4]*x2+PAemployXage2$coefficients$fixed[5]*x+PAemployXage2$coefficients$fixed[6]*x2)
PAemp2=(PAemployXage2$coefficients$fixed[1]+PAemployXage2$coefficients$fixed[3]*x+PAemployXage2$coefficients$fixed[4]*x2)
PApro1=(PAprosocialXage2$coefficients$fixed[1]+PAprosocialXage2$coefficients$fixed[2]+PAprosocialXage2$coefficients$fixed[3]*x+PAprosocialXage2$coefficients$fixed[4]*x2+PAprosocialXage2$coefficients$fixed[5]*x+PAprosocialXage2$coefficients$fixed[6]*x2)
PApro2=(PAprosocialXage2$coefficients$fixed[1]+PAprosocialXage2$coefficients$fixed[3]*x+PAprosocialXage2$coefficients$fixed[4]*x2)
PAmean1=(PAmeaningXage2$coefficients$fixed[1]+PAmeaningXage2$coefficients$fixed[2]+PAmeaningXage2$coefficients$fixed[3]*x+PAmeaningXage2$coefficients$fixed[4]*x2+PAmeaningXage2$coefficients$fixed[5]*x+PAmeaningXage2$coefficients$fixed[6]*x2)
PAmean2=(PAmeaningXage2$coefficients$fixed[1]+PAmeaningXage2$coefficients$fixed[3]*x+PAmeaningXage2$coefficients$fixed[4]*x2)
NAmar1=(NAmaritalXage2$coefficients$fixed[1]+NAmaritalXage2$coefficients$fixed[2]+NAmaritalXage2$coefficients$fixed[3]*x+NAmaritalXage2$coefficients$fixed[4]*x2+NAmaritalXage2$coefficients$fixed[5]*x+NAmaritalXage2$coefficients$fixed[6]*x2)
NAmar2=(NAmaritalXage2$coefficients$fixed[1]+NAmaritalXage2$coefficients$fixed[3]*x+NAmaritalXage2$coefficients$fixed[4]*x2)
NAemp1=(NAemployXage2$coefficients$fixed[1]+NAemployXage2$coefficients$fixed[2]+NAemployXage2$coefficients$fixed[3]*x+NAemployXage2$coefficients$fixed[4]*x2+NAemployXage2$coefficients$fixed[5]*x+NAemployXage2$coefficients$fixed[6]*x2)
NAemp2=(NAemployXage2$coefficients$fixed[1]+NAemployXage2$coefficients$fixed[3]*x+NAemployXage2$coefficients$fixed[4]*x2)
NApro1=(NAprosocialXage2$coefficients$fixed[1]+NAprosocialXage2$coefficients$fixed[2]+NAprosocialXage2$coefficients$fixed[3]*x+NAprosocialXage2$coefficients$fixed[4]*x2+NAprosocialXage2$coefficients$fixed[5]*x+NAprosocialXage2$coefficients$fixed[6]*x2)
NApro2=(NAprosocialXage2$coefficients$fixed[1]+NAprosocialXage2$coefficients$fixed[3]*x+NAprosocialXage2$coefficients$fixed[4]*x2)
NAmean1=(NAmeaningXage2$coefficients$fixed[1]+NAmeaningXage2$coefficients$fixed[2]+NAmeaningXage2$coefficients$fixed[3]*x+NAmeaningXage2$coefficients$fixed[4]*x2+NAmeaningXage2$coefficients$fixed[5]*x+NAmeaningXage2$coefficients$fixed[6]*x2)
NAmean2=(NAmeaningXage2$coefficients$fixed[1]+NAmeaningXage2$coefficients$fixed[3]*x+NAmeaningXage2$coefficients$fixed[4]*x2)
Outcomes=factor(rep(c(rep('Marriage - LS',6001),rep('Employment - LS',6001),rep('Prosociality - LS',6001),rep('Life Meaning - LS',6001),rep('Marriage - PA',6001),rep('Employment - PA',6001),rep('Prosociality - PA',6001),rep('Life Meaning - PA',6001),rep('Marriage - NA',6001),rep('Employment - NA',6001),rep('Prosociality - NA',6001),rep('Life Meaning - NA',6001)),2),levels=c("Marriage - LS","Employment - LS","Prosociality - LS","Life Meaning - LS","Marriage - PA","Employment - PA","Prosociality - PA","Life Meaning - PA","Marriage - NA","Employment - NA","Prosociality - NA","Life Meaning - NA"))
Lines=c(LSmar1,LSemp1,LSpro1,LSmean1,PAmar1,PAemp1,PApro1,PAmean1,NAmar1,NAemp1,NApro1,NAmean1,LSmar2,LSemp2,LSpro2,LSmean2,PAmar2,PAemp2,PApro2,PAmean2,NAmar2,NAemp2,NApro2,NAmean2)
Interactions=factor(c(rep(1,6001*12),rep(0,6001*12)),levels=c('1','0'))
Age=rep(x,24)
plotdata=data.frame(Outcomes,Lines,Age,Interactions)
#data for the fill
mins=pmin(Lines[1:72012],Lines[72013:144024])
maxs=pmax(Lines[1:72012],Lines[72013:144024])
differences=c(LSmar1-LSmar2,LSemp1-LSemp2,LSpro1-LSpro2,LSmean1-LSmean2,PAmar1-PAmar2,PAemp1-PAemp2,PApro1-PApro2,PAmean1-PAmean2,NAmar1-NAmar2,NAemp1-NAemp2,NApro1-NApro2,NAmean1-NAmean2)
size=rep(NA,72012);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,72012);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,72012);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,72012);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,72012);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,72012);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,72012);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,72012);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,72012);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,72012);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,72012);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,72012);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,72012);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,72012);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,72012);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,72012);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,72012);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,72012);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,72012);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Outcomes=factor(c(rep('Marriage - LS',6001),rep('Employment - LS',6001),rep('Prosociality - LS',6001),rep('Life Meaning - LS',6001),rep('Marriage - PA',6001),rep('Employment - PA',6001),rep('Prosociality - PA',6001),rep('Life Meaning - PA',6001),rep('Marriage - NA',6001),rep('Employment - NA',6001),rep('Prosociality - NA',6001),rep('Life Meaning - NA',6001)),levels=c("Marriage - LS","Employment - LS","Prosociality - LS","Life Meaning - LS","Marriage - PA","Employment - PA","Prosociality - PA","Life Meaning - PA","Marriage - NA","Employment - NA","Prosociality - NA","Life Meaning - NA"))
ribbondata=data.frame(Outcomes,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=Lines,linetype=Interactions),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(name="",limits=c(0,10))+
  facet_wrap(~Outcomes,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("1","0"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot


###Figures: Marriage
#LS
#get parameters
LSmaritalintercepts=LSmaritalXage2$coefficients$fixed[1]+LSmaritalXage2$coefficients$random$Region[,1]
LSmaritalcoefs1=LSmaritalXage2$coefficients$fixed[2]+LSmaritalXage2$coefficients$random$Region[,2]
LSmaritalcoefs2=LSmaritalXage2$coefficients$fixed[3]+LSmaritalXage2$coefficients$random$Region[,3]
LSmaritalcoefs3=LSmaritalXage2$coefficients$fixed[4]+LSmaritalXage2$coefficients$random$Region[,4]
LSmaritalcoefs4=LSmaritalXage2$coefficients$fixed[5]+LSmaritalXage2$coefficients$random$Region[,5]
LSmaritalcoefs5=LSmaritalXage2$coefficients$fixed[6]+LSmaritalXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(LSmaritalXage2$coefficients$fixed[1]+LSmaritalXage2$coefficients$fixed[2]+LSmaritalXage2$coefficients$fixed[3]*x+LSmaritalXage2$coefficients$fixed[4]*x2+LSmaritalXage2$coefficients$fixed[5]*x+LSmaritalXage2$coefficients$fixed[6]*x2)
Gl2=(LSmaritalXage2$coefficients$fixed[1]+LSmaritalXage2$coefficients$fixed[3]*x+LSmaritalXage2$coefficients$fixed[4]*x2)
A1=(LSmaritalintercepts[1]+LSmaritalcoefs1[1]+LSmaritalcoefs2[1]*x+LSmaritalcoefs3[1]*x2+LSmaritalcoefs4[1]*x+LSmaritalcoefs5[1]*x2)
A2=(LSmaritalintercepts[1]+LSmaritalcoefs2[1]*x+LSmaritalcoefs3[1]*x2)
LE1=(LSmaritalintercepts[2]+LSmaritalcoefs1[2]+LSmaritalcoefs2[2]*x+LSmaritalcoefs3[2]*x2+LSmaritalcoefs4[2]*x+LSmaritalcoefs5[2]*x2)
LE2=(LSmaritalintercepts[2]+LSmaritalcoefs2[2]*x+LSmaritalcoefs3[2]*x2)
GE1=(LSmaritalintercepts[3]+LSmaritalcoefs1[3]+LSmaritalcoefs2[3]*x+LSmaritalcoefs3[3]*x2+LSmaritalcoefs4[3]*x+LSmaritalcoefs5[3]*x2)
GE2=(LSmaritalintercepts[3]+LSmaritalcoefs2[3]*x+LSmaritalcoefs3[3]*x2)
NE1=(LSmaritalintercepts[4]+LSmaritalcoefs1[4]+LSmaritalcoefs2[4]*x+LSmaritalcoefs3[4]*x2+LSmaritalcoefs4[4]*x+LSmaritalcoefs5[4]*x2)
NE2=(LSmaritalintercepts[4]+LSmaritalcoefs2[4]*x+LSmaritalcoefs3[4]*x2)
EE1=(LSmaritalintercepts[5]+LSmaritalcoefs1[5]+LSmaritalcoefs2[5]*x+LSmaritalcoefs3[5]*x2+LSmaritalcoefs4[5]*x+LSmaritalcoefs5[5]*x2)
EE2=(LSmaritalintercepts[5]+LSmaritalcoefs2[5]*x+LSmaritalcoefs3[5]*x2)
CA1=(LSmaritalintercepts[6]+LSmaritalcoefs1[6]+LSmaritalcoefs2[6]*x+LSmaritalcoefs3[6]*x2+LSmaritalcoefs4[6]*x+LSmaritalcoefs5[6]*x2)
CA2=(LSmaritalintercepts[6]+LSmaritalcoefs2[6]*x+LSmaritalcoefs3[6]*x2)
LA1=(LSmaritalintercepts[7]+LSmaritalcoefs1[7]+LSmaritalcoefs2[7]*x+LSmaritalcoefs3[7]*x2+LSmaritalcoefs4[7]*x+LSmaritalcoefs5[7]*x2)
LA2=(LSmaritalintercepts[7]+LSmaritalcoefs2[7]*x+LSmaritalcoefs3[7]*x2)
SA1=(LSmaritalintercepts[8]+LSmaritalcoefs1[8]+LSmaritalcoefs2[8]*x+LSmaritalcoefs3[8]*x2+LSmaritalcoefs4[8]*x+LSmaritalcoefs5[8]*x2)
SA2=(LSmaritalintercepts[8]+LSmaritalcoefs2[8]*x+LSmaritalcoefs3[8]*x2)
Ar1=(LSmaritalintercepts[9]+LSmaritalcoefs1[9]+LSmaritalcoefs2[9]*x+LSmaritalcoefs3[9]*x2+LSmaritalcoefs4[9]*x+LSmaritalcoefs5[9]*x2)
Ar2=(LSmaritalintercepts[9]+LSmaritalcoefs2[9]*x+LSmaritalcoefs3[9]*x2)
Af1=(LSmaritalintercepts[10]+LSmaritalcoefs1[10]+LSmaritalcoefs2[10]*x+LSmaritalcoefs3[10]*x2+LSmaritalcoefs4[10]*x+LSmaritalcoefs5[10]*x2)
Af2=(LSmaritalintercepts[10]+LSmaritalcoefs2[10]*x+LSmaritalcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
LS=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Married=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(LS,Region,Age,Married)
#data for the fill
mins=pmin(LS[1:66011],LS[66012:132022])
maxs=pmax(LS[1:66011],LS[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=LS,linetype=Married),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Married","Unmarried"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

#PA
#get parameters
PAmaritalintercepts=PAmaritalXage2$coefficients$fixed[1]+PAmaritalXage2$coefficients$random$Region[,1]
PAmaritalcoefs1=PAmaritalXage2$coefficients$fixed[2]+PAmaritalXage2$coefficients$random$Region[,2]
PAmaritalcoefs2=PAmaritalXage2$coefficients$fixed[3]+PAmaritalXage2$coefficients$random$Region[,3]
PAmaritalcoefs3=PAmaritalXage2$coefficients$fixed[4]+PAmaritalXage2$coefficients$random$Region[,4]
PAmaritalcoefs4=PAmaritalXage2$coefficients$fixed[5]+PAmaritalXage2$coefficients$random$Region[,5]
PAmaritalcoefs5=PAmaritalXage2$coefficients$fixed[6]+PAmaritalXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(PAmaritalXage2$coefficients$fixed[1]+PAmaritalXage2$coefficients$fixed[2]+PAmaritalXage2$coefficients$fixed[3]*x+PAmaritalXage2$coefficients$fixed[4]*x2+PAmaritalXage2$coefficients$fixed[5]*x+PAmaritalXage2$coefficients$fixed[6]*x2)
Gl2=(PAmaritalXage2$coefficients$fixed[1]+PAmaritalXage2$coefficients$fixed[3]*x+PAmaritalXage2$coefficients$fixed[4]*x2)
A1=(PAmaritalintercepts[1]+PAmaritalcoefs1[1]+PAmaritalcoefs2[1]*x+PAmaritalcoefs3[1]*x2+PAmaritalcoefs4[1]*x+PAmaritalcoefs5[1]*x2)
A2=(PAmaritalintercepts[1]+PAmaritalcoefs2[1]*x+PAmaritalcoefs3[1]*x2)
LE1=(PAmaritalintercepts[2]+PAmaritalcoefs1[2]+PAmaritalcoefs2[2]*x+PAmaritalcoefs3[2]*x2+PAmaritalcoefs4[2]*x+PAmaritalcoefs5[2]*x2)
LE2=(PAmaritalintercepts[2]+PAmaritalcoefs2[2]*x+PAmaritalcoefs3[2]*x2)
GE1=(PAmaritalintercepts[3]+PAmaritalcoefs1[3]+PAmaritalcoefs2[3]*x+PAmaritalcoefs3[3]*x2+PAmaritalcoefs4[3]*x+PAmaritalcoefs5[3]*x2)
GE2=(PAmaritalintercepts[3]+PAmaritalcoefs2[3]*x+PAmaritalcoefs3[3]*x2)
NE1=(PAmaritalintercepts[4]+PAmaritalcoefs1[4]+PAmaritalcoefs2[4]*x+PAmaritalcoefs3[4]*x2+PAmaritalcoefs4[4]*x+PAmaritalcoefs5[4]*x2)
NE2=(PAmaritalintercepts[4]+PAmaritalcoefs2[4]*x+PAmaritalcoefs3[4]*x2)
EE1=(PAmaritalintercepts[5]+PAmaritalcoefs1[5]+PAmaritalcoefs2[5]*x+PAmaritalcoefs3[5]*x2+PAmaritalcoefs4[5]*x+PAmaritalcoefs5[5]*x2)
EE2=(PAmaritalintercepts[5]+PAmaritalcoefs2[5]*x+PAmaritalcoefs3[5]*x2)
CA1=(PAmaritalintercepts[6]+PAmaritalcoefs1[6]+PAmaritalcoefs2[6]*x+PAmaritalcoefs3[6]*x2+PAmaritalcoefs4[6]*x+PAmaritalcoefs5[6]*x2)
CA2=(PAmaritalintercepts[6]+PAmaritalcoefs2[6]*x+PAmaritalcoefs3[6]*x2)
LA1=(PAmaritalintercepts[7]+PAmaritalcoefs1[7]+PAmaritalcoefs2[7]*x+PAmaritalcoefs3[7]*x2+PAmaritalcoefs4[7]*x+PAmaritalcoefs5[7]*x2)
LA2=(PAmaritalintercepts[7]+PAmaritalcoefs2[7]*x+PAmaritalcoefs3[7]*x2)
SA1=(PAmaritalintercepts[8]+PAmaritalcoefs1[8]+PAmaritalcoefs2[8]*x+PAmaritalcoefs3[8]*x2+PAmaritalcoefs4[8]*x+PAmaritalcoefs5[8]*x2)
SA2=(PAmaritalintercepts[8]+PAmaritalcoefs2[8]*x+PAmaritalcoefs3[8]*x2)
Ar1=(PAmaritalintercepts[9]+PAmaritalcoefs1[9]+PAmaritalcoefs2[9]*x+PAmaritalcoefs3[9]*x2+PAmaritalcoefs4[9]*x+PAmaritalcoefs5[9]*x2)
Ar2=(PAmaritalintercepts[9]+PAmaritalcoefs2[9]*x+PAmaritalcoefs3[9]*x2)
Af1=(PAmaritalintercepts[10]+PAmaritalcoefs1[10]+PAmaritalcoefs2[10]*x+PAmaritalcoefs3[10]*x2+PAmaritalcoefs4[10]*x+PAmaritalcoefs5[10]*x2)
Af2=(PAmaritalintercepts[10]+PAmaritalcoefs2[10]*x+PAmaritalcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
PA=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Married=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(PA,Region,Age,Married)
#data for the fill
mins=pmin(PA[1:66011],PA[66012:132022])
maxs=pmax(PA[1:66011],PA[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=PA,linetype=Married),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Married","Unmarried"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

#NA
#get parameters
NAmaritalintercepts=NAmaritalXage2$coefficients$fixed[1]+NAmaritalXage2$coefficients$random$Region[,1]
NAmaritalcoefs1=NAmaritalXage2$coefficients$fixed[2]+NAmaritalXage2$coefficients$random$Region[,2]
NAmaritalcoefs2=NAmaritalXage2$coefficients$fixed[3]+NAmaritalXage2$coefficients$random$Region[,3]
NAmaritalcoefs3=NAmaritalXage2$coefficients$fixed[4]+NAmaritalXage2$coefficients$random$Region[,4]
NAmaritalcoefs4=NAmaritalXage2$coefficients$fixed[5]+NAmaritalXage2$coefficients$random$Region[,5]
NAmaritalcoefs5=NAmaritalXage2$coefficients$fixed[6]+NAmaritalXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(NAmaritalXage2$coefficients$fixed[1]+NAmaritalXage2$coefficients$fixed[2]+NAmaritalXage2$coefficients$fixed[3]*x+NAmaritalXage2$coefficients$fixed[4]*x2+NAmaritalXage2$coefficients$fixed[5]*x+NAmaritalXage2$coefficients$fixed[6]*x2)
Gl2=(NAmaritalXage2$coefficients$fixed[1]+NAmaritalXage2$coefficients$fixed[3]*x+NAmaritalXage2$coefficients$fixed[4]*x2)
A1=(NAmaritalintercepts[1]+NAmaritalcoefs1[1]+NAmaritalcoefs2[1]*x+NAmaritalcoefs3[1]*x2+NAmaritalcoefs4[1]*x+NAmaritalcoefs5[1]*x2)
A2=(NAmaritalintercepts[1]+NAmaritalcoefs2[1]*x+NAmaritalcoefs3[1]*x2)
LE1=(NAmaritalintercepts[2]+NAmaritalcoefs1[2]+NAmaritalcoefs2[2]*x+NAmaritalcoefs3[2]*x2+NAmaritalcoefs4[2]*x+NAmaritalcoefs5[2]*x2)
LE2=(NAmaritalintercepts[2]+NAmaritalcoefs2[2]*x+NAmaritalcoefs3[2]*x2)
GE1=(NAmaritalintercepts[3]+NAmaritalcoefs1[3]+NAmaritalcoefs2[3]*x+NAmaritalcoefs3[3]*x2+NAmaritalcoefs4[3]*x+NAmaritalcoefs5[3]*x2)
GE2=(NAmaritalintercepts[3]+NAmaritalcoefs2[3]*x+NAmaritalcoefs3[3]*x2)
NE1=(NAmaritalintercepts[4]+NAmaritalcoefs1[4]+NAmaritalcoefs2[4]*x+NAmaritalcoefs3[4]*x2+NAmaritalcoefs4[4]*x+NAmaritalcoefs5[4]*x2)
NE2=(NAmaritalintercepts[4]+NAmaritalcoefs2[4]*x+NAmaritalcoefs3[4]*x2)
EE1=(NAmaritalintercepts[5]+NAmaritalcoefs1[5]+NAmaritalcoefs2[5]*x+NAmaritalcoefs3[5]*x2+NAmaritalcoefs4[5]*x+NAmaritalcoefs5[5]*x2)
EE2=(NAmaritalintercepts[5]+NAmaritalcoefs2[5]*x+NAmaritalcoefs3[5]*x2)
CA1=(NAmaritalintercepts[6]+NAmaritalcoefs1[6]+NAmaritalcoefs2[6]*x+NAmaritalcoefs3[6]*x2+NAmaritalcoefs4[6]*x+NAmaritalcoefs5[6]*x2)
CA2=(NAmaritalintercepts[6]+NAmaritalcoefs2[6]*x+NAmaritalcoefs3[6]*x2)
LA1=(NAmaritalintercepts[7]+NAmaritalcoefs1[7]+NAmaritalcoefs2[7]*x+NAmaritalcoefs3[7]*x2+NAmaritalcoefs4[7]*x+NAmaritalcoefs5[7]*x2)
LA2=(NAmaritalintercepts[7]+NAmaritalcoefs2[7]*x+NAmaritalcoefs3[7]*x2)
SA1=(NAmaritalintercepts[8]+NAmaritalcoefs1[8]+NAmaritalcoefs2[8]*x+NAmaritalcoefs3[8]*x2+NAmaritalcoefs4[8]*x+NAmaritalcoefs5[8]*x2)
SA2=(NAmaritalintercepts[8]+NAmaritalcoefs2[8]*x+NAmaritalcoefs3[8]*x2)
Ar1=(NAmaritalintercepts[9]+NAmaritalcoefs1[9]+NAmaritalcoefs2[9]*x+NAmaritalcoefs3[9]*x2+NAmaritalcoefs4[9]*x+NAmaritalcoefs5[9]*x2)
Ar2=(NAmaritalintercepts[9]+NAmaritalcoefs2[9]*x+NAmaritalcoefs3[9]*x2)
Af1=(NAmaritalintercepts[10]+NAmaritalcoefs1[10]+NAmaritalcoefs2[10]*x+NAmaritalcoefs3[10]*x2+NAmaritalcoefs4[10]*x+NAmaritalcoefs5[10]*x2)
Af2=(NAmaritalintercepts[10]+NAmaritalcoefs2[10]*x+NAmaritalcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
Neg=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Married=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(Neg,Region,Age,Married)
#data for the fill
mins=pmin(Neg[1:66011],Neg[66012:132022])
maxs=pmax(Neg[1:66011],Neg[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=Neg,linetype=Married),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10),name='NA')+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Married","Unmarried"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot


###Figures: Employment
#LS
#get parameters
LSemployintercepts=LSemployXage2$coefficients$fixed[1]+LSemployXage2$coefficients$random$Region[,1]
LSemploycoefs1=LSemployXage2$coefficients$fixed[2]+LSemployXage2$coefficients$random$Region[,2]
LSemploycoefs2=LSemployXage2$coefficients$fixed[3]+LSemployXage2$coefficients$random$Region[,3]
LSemploycoefs3=LSemployXage2$coefficients$fixed[4]+LSemployXage2$coefficients$random$Region[,4]
LSemploycoefs4=LSemployXage2$coefficients$fixed[5]+LSemployXage2$coefficients$random$Region[,5]
LSemploycoefs5=LSemployXage2$coefficients$fixed[6]+LSemployXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(LSemployXage2$coefficients$fixed[1]+LSemployXage2$coefficients$fixed[2]+LSemployXage2$coefficients$fixed[3]*x+LSemployXage2$coefficients$fixed[4]*x2+LSemployXage2$coefficients$fixed[5]*x+LSemployXage2$coefficients$fixed[6]*x2)
Gl2=(LSemployXage2$coefficients$fixed[1]+LSemployXage2$coefficients$fixed[3]*x+LSemployXage2$coefficients$fixed[4]*x2)
A1=(LSemployintercepts[1]+LSemploycoefs1[1]+LSemploycoefs2[1]*x+LSemploycoefs3[1]*x2+LSemploycoefs4[1]*x+LSemploycoefs5[1]*x2)
A2=(LSemployintercepts[1]+LSemploycoefs2[1]*x+LSemploycoefs3[1]*x2)
LE1=(LSemployintercepts[2]+LSemploycoefs1[2]+LSemploycoefs2[2]*x+LSemploycoefs3[2]*x2+LSemploycoefs4[2]*x+LSemploycoefs5[2]*x2)
LE2=(LSemployintercepts[2]+LSemploycoefs2[2]*x+LSemploycoefs3[2]*x2)
GE1=(LSemployintercepts[3]+LSemploycoefs1[3]+LSemploycoefs2[3]*x+LSemploycoefs3[3]*x2+LSemploycoefs4[3]*x+LSemploycoefs5[3]*x2)
GE2=(LSemployintercepts[3]+LSemploycoefs2[3]*x+LSemploycoefs3[3]*x2)
NE1=(LSemployintercepts[4]+LSemploycoefs1[4]+LSemploycoefs2[4]*x+LSemploycoefs3[4]*x2+LSemploycoefs4[4]*x+LSemploycoefs5[4]*x2)
NE2=(LSemployintercepts[4]+LSemploycoefs2[4]*x+LSemploycoefs3[4]*x2)
EE1=(LSemployintercepts[5]+LSemploycoefs1[5]+LSemploycoefs2[5]*x+LSemploycoefs3[5]*x2+LSemploycoefs4[5]*x+LSemploycoefs5[5]*x2)
EE2=(LSemployintercepts[5]+LSemploycoefs2[5]*x+LSemploycoefs3[5]*x2)
CA1=(LSemployintercepts[6]+LSemploycoefs1[6]+LSemploycoefs2[6]*x+LSemploycoefs3[6]*x2+LSemploycoefs4[6]*x+LSemploycoefs5[6]*x2)
CA2=(LSemployintercepts[6]+LSemploycoefs2[6]*x+LSemploycoefs3[6]*x2)
LA1=(LSemployintercepts[7]+LSemploycoefs1[7]+LSemploycoefs2[7]*x+LSemploycoefs3[7]*x2+LSemploycoefs4[7]*x+LSemploycoefs5[7]*x2)
LA2=(LSemployintercepts[7]+LSemploycoefs2[7]*x+LSemploycoefs3[7]*x2)
SA1=(LSemployintercepts[8]+LSemploycoefs1[8]+LSemploycoefs2[8]*x+LSemploycoefs3[8]*x2+LSemploycoefs4[8]*x+LSemploycoefs5[8]*x2)
SA2=(LSemployintercepts[8]+LSemploycoefs2[8]*x+LSemploycoefs3[8]*x2)
Ar1=(LSemployintercepts[9]+LSemploycoefs1[9]+LSemploycoefs2[9]*x+LSemploycoefs3[9]*x2+LSemploycoefs4[9]*x+LSemploycoefs5[9]*x2)
Ar2=(LSemployintercepts[9]+LSemploycoefs2[9]*x+LSemploycoefs3[9]*x2)
Af1=(LSemployintercepts[10]+LSemploycoefs1[10]+LSemploycoefs2[10]*x+LSemploycoefs3[10]*x2+LSemploycoefs4[10]*x+LSemploycoefs5[10]*x2)
Af2=(LSemployintercepts[10]+LSemploycoefs2[10]*x+LSemploycoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
LS=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Employment=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(LS,Region,Age,Employment)
#data for the fill
mins=pmin(LS[1:66011],LS[66012:132022])
maxs=pmax(LS[1:66011],LS[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=LS,linetype=Employment),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Employed","Unemployed"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot


#PA
#get parameters
PAemployintercepts=PAemployXage2$coefficients$fixed[1]+PAemployXage2$coefficients$random$Region[,1]
PAemploycoefs1=PAemployXage2$coefficients$fixed[2]+PAemployXage2$coefficients$random$Region[,2]
PAemploycoefs2=PAemployXage2$coefficients$fixed[3]+PAemployXage2$coefficients$random$Region[,3]
PAemploycoefs3=PAemployXage2$coefficients$fixed[4]+PAemployXage2$coefficients$random$Region[,4]
PAemploycoefs4=PAemployXage2$coefficients$fixed[5]+PAemployXage2$coefficients$random$Region[,5]
PAemploycoefs5=PAemployXage2$coefficients$fixed[6]+PAemployXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(PAemployXage2$coefficients$fixed[1]+PAemployXage2$coefficients$fixed[2]+PAemployXage2$coefficients$fixed[3]*x+PAemployXage2$coefficients$fixed[4]*x2+PAemployXage2$coefficients$fixed[5]*x+PAemployXage2$coefficients$fixed[6]*x2)
Gl2=(PAemployXage2$coefficients$fixed[1]+PAemployXage2$coefficients$fixed[3]*x+PAemployXage2$coefficients$fixed[4]*x2)
A1=(PAemployintercepts[1]+PAemploycoefs1[1]+PAemploycoefs2[1]*x+PAemploycoefs3[1]*x2+PAemploycoefs4[1]*x+PAemploycoefs5[1]*x2)
A2=(PAemployintercepts[1]+PAemploycoefs2[1]*x+PAemploycoefs3[1]*x2)
LE1=(PAemployintercepts[2]+PAemploycoefs1[2]+PAemploycoefs2[2]*x+PAemploycoefs3[2]*x2+PAemploycoefs4[2]*x+PAemploycoefs5[2]*x2)
LE2=(PAemployintercepts[2]+PAemploycoefs2[2]*x+PAemploycoefs3[2]*x2)
GE1=(PAemployintercepts[3]+PAemploycoefs1[3]+PAemploycoefs2[3]*x+PAemploycoefs3[3]*x2+PAemploycoefs4[3]*x+PAemploycoefs5[3]*x2)
GE2=(PAemployintercepts[3]+PAemploycoefs2[3]*x+PAemploycoefs3[3]*x2)
NE1=(PAemployintercepts[4]+PAemploycoefs1[4]+PAemploycoefs2[4]*x+PAemploycoefs3[4]*x2+PAemploycoefs4[4]*x+PAemploycoefs5[4]*x2)
NE2=(PAemployintercepts[4]+PAemploycoefs2[4]*x+PAemploycoefs3[4]*x2)
EE1=(PAemployintercepts[5]+PAemploycoefs1[5]+PAemploycoefs2[5]*x+PAemploycoefs3[5]*x2+PAemploycoefs4[5]*x+PAemploycoefs5[5]*x2)
EE2=(PAemployintercepts[5]+PAemploycoefs2[5]*x+PAemploycoefs3[5]*x2)
CA1=(PAemployintercepts[6]+PAemploycoefs1[6]+PAemploycoefs2[6]*x+PAemploycoefs3[6]*x2+PAemploycoefs4[6]*x+PAemploycoefs5[6]*x2)
CA2=(PAemployintercepts[6]+PAemploycoefs2[6]*x+PAemploycoefs3[6]*x2)
LA1=(PAemployintercepts[7]+PAemploycoefs1[7]+PAemploycoefs2[7]*x+PAemploycoefs3[7]*x2+PAemploycoefs4[7]*x+PAemploycoefs5[7]*x2)
LA2=(PAemployintercepts[7]+PAemploycoefs2[7]*x+PAemploycoefs3[7]*x2)
SA1=(PAemployintercepts[8]+PAemploycoefs1[8]+PAemploycoefs2[8]*x+PAemploycoefs3[8]*x2+PAemploycoefs4[8]*x+PAemploycoefs5[8]*x2)
SA2=(PAemployintercepts[8]+PAemploycoefs2[8]*x+PAemploycoefs3[8]*x2)
Ar1=(PAemployintercepts[9]+PAemploycoefs1[9]+PAemploycoefs2[9]*x+PAemploycoefs3[9]*x2+PAemploycoefs4[9]*x+PAemploycoefs5[9]*x2)
Ar2=(PAemployintercepts[9]+PAemploycoefs2[9]*x+PAemploycoefs3[9]*x2)
Af1=(PAemployintercepts[10]+PAemploycoefs1[10]+PAemploycoefs2[10]*x+PAemploycoefs3[10]*x2+PAemploycoefs4[10]*x+PAemploycoefs5[10]*x2)
Af2=(PAemployintercepts[10]+PAemploycoefs2[10]*x+PAemploycoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
PA=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Employment=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(PA,Region,Age,Employment)
#data for the fill
mins=pmin(PA[1:66011],PA[66012:132022])
maxs=pmax(PA[1:66011],PA[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=PA,linetype=Employment),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Employed","Unemployed"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

#NA
#get parameters
NAemployintercepts=NAemployXage2$coefficients$fixed[1]+NAemployXage2$coefficients$random$Region[,1]
NAemploycoefs1=NAemployXage2$coefficients$fixed[2]+NAemployXage2$coefficients$random$Region[,2]
NAemploycoefs2=NAemployXage2$coefficients$fixed[3]+NAemployXage2$coefficients$random$Region[,3]
NAemploycoefs3=NAemployXage2$coefficients$fixed[4]+NAemployXage2$coefficients$random$Region[,4]
NAemploycoefs4=NAemployXage2$coefficients$fixed[5]+NAemployXage2$coefficients$random$Region[,5]
NAemploycoefs5=NAemployXage2$coefficients$fixed[6]+NAemployXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(NAemployXage2$coefficients$fixed[1]+NAemployXage2$coefficients$fixed[2]+NAemployXage2$coefficients$fixed[3]*x+NAemployXage2$coefficients$fixed[4]*x2+NAemployXage2$coefficients$fixed[5]*x+NAemployXage2$coefficients$fixed[6]*x2)
Gl2=(NAemployXage2$coefficients$fixed[1]+NAemployXage2$coefficients$fixed[3]*x+NAemployXage2$coefficients$fixed[4]*x2)
A1=(NAemployintercepts[1]+NAemploycoefs1[1]+NAemploycoefs2[1]*x+NAemploycoefs3[1]*x2+NAemploycoefs4[1]*x+NAemploycoefs5[1]*x2)
A2=(NAemployintercepts[1]+NAemploycoefs2[1]*x+NAemploycoefs3[1]*x2)
LE1=(NAemployintercepts[2]+NAemploycoefs1[2]+NAemploycoefs2[2]*x+NAemploycoefs3[2]*x2+NAemploycoefs4[2]*x+NAemploycoefs5[2]*x2)
LE2=(NAemployintercepts[2]+NAemploycoefs2[2]*x+NAemploycoefs3[2]*x2)
GE1=(NAemployintercepts[3]+NAemploycoefs1[3]+NAemploycoefs2[3]*x+NAemploycoefs3[3]*x2+NAemploycoefs4[3]*x+NAemploycoefs5[3]*x2)
GE2=(NAemployintercepts[3]+NAemploycoefs2[3]*x+NAemploycoefs3[3]*x2)
NE1=(NAemployintercepts[4]+NAemploycoefs1[4]+NAemploycoefs2[4]*x+NAemploycoefs3[4]*x2+NAemploycoefs4[4]*x+NAemploycoefs5[4]*x2)
NE2=(NAemployintercepts[4]+NAemploycoefs2[4]*x+NAemploycoefs3[4]*x2)
EE1=(NAemployintercepts[5]+NAemploycoefs1[5]+NAemploycoefs2[5]*x+NAemploycoefs3[5]*x2+NAemploycoefs4[5]*x+NAemploycoefs5[5]*x2)
EE2=(NAemployintercepts[5]+NAemploycoefs2[5]*x+NAemploycoefs3[5]*x2)
CA1=(NAemployintercepts[6]+NAemploycoefs1[6]+NAemploycoefs2[6]*x+NAemploycoefs3[6]*x2+NAemploycoefs4[6]*x+NAemploycoefs5[6]*x2)
CA2=(NAemployintercepts[6]+NAemploycoefs2[6]*x+NAemploycoefs3[6]*x2)
LA1=(NAemployintercepts[7]+NAemploycoefs1[7]+NAemploycoefs2[7]*x+NAemploycoefs3[7]*x2+NAemploycoefs4[7]*x+NAemploycoefs5[7]*x2)
LA2=(NAemployintercepts[7]+NAemploycoefs2[7]*x+NAemploycoefs3[7]*x2)
SA1=(NAemployintercepts[8]+NAemploycoefs1[8]+NAemploycoefs2[8]*x+NAemploycoefs3[8]*x2+NAemploycoefs4[8]*x+NAemploycoefs5[8]*x2)
SA2=(NAemployintercepts[8]+NAemploycoefs2[8]*x+NAemploycoefs3[8]*x2)
Ar1=(NAemployintercepts[9]+NAemploycoefs1[9]+NAemploycoefs2[9]*x+NAemploycoefs3[9]*x2+NAemploycoefs4[9]*x+NAemploycoefs5[9]*x2)
Ar2=(NAemployintercepts[9]+NAemploycoefs2[9]*x+NAemploycoefs3[9]*x2)
Af1=(NAemployintercepts[10]+NAemploycoefs1[10]+NAemploycoefs2[10]*x+NAemploycoefs3[10]*x2+NAemploycoefs4[10]*x+NAemploycoefs5[10]*x2)
Af2=(NAemployintercepts[10]+NAemploycoefs2[10]*x+NAemploycoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
Neg=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Employment=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(Neg,Region,Age,Employment)
#data for the fill
mins=pmin(Neg[1:66011],Neg[66012:132022])
maxs=pmax(Neg[1:66011],Neg[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=Neg,linetype=Employment),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10),name='NA')+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Employed","Unemployed"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

###Figures: Prosocial
#LS
#get parameters
LSprosocialintercepts=LSprosocialXage2$coefficients$fixed[1]+LSprosocialXage2$coefficients$random$Region[,1]
LSprosocialcoefs1=LSprosocialXage2$coefficients$fixed[2]+LSprosocialXage2$coefficients$random$Region[,2]
LSprosocialcoefs2=LSprosocialXage2$coefficients$fixed[3]+LSprosocialXage2$coefficients$random$Region[,3]
LSprosocialcoefs3=LSprosocialXage2$coefficients$fixed[4]+LSprosocialXage2$coefficients$random$Region[,4]
LSprosocialcoefs4=LSprosocialXage2$coefficients$fixed[5]+LSprosocialXage2$coefficients$random$Region[,5]
LSprosocialcoefs5=LSprosocialXage2$coefficients$fixed[6]+LSprosocialXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(LSprosocialXage2$coefficients$fixed[1]+LSprosocialXage2$coefficients$fixed[2]+LSprosocialXage2$coefficients$fixed[3]*x+LSprosocialXage2$coefficients$fixed[4]*x2+LSprosocialXage2$coefficients$fixed[5]*x+LSprosocialXage2$coefficients$fixed[6]*x2)
Gl2=(LSprosocialXage2$coefficients$fixed[1]+LSprosocialXage2$coefficients$fixed[3]*x+LSprosocialXage2$coefficients$fixed[4]*x2)
A1=(LSprosocialintercepts[1]+LSprosocialcoefs1[1]+LSprosocialcoefs2[1]*x+LSprosocialcoefs3[1]*x2+LSprosocialcoefs4[1]*x+LSprosocialcoefs5[1]*x2)
A2=(LSprosocialintercepts[1]+LSprosocialcoefs2[1]*x+LSprosocialcoefs3[1]*x2)
LE1=(LSprosocialintercepts[2]+LSprosocialcoefs1[2]+LSprosocialcoefs2[2]*x+LSprosocialcoefs3[2]*x2+LSprosocialcoefs4[2]*x+LSprosocialcoefs5[2]*x2)
LE2=(LSprosocialintercepts[2]+LSprosocialcoefs2[2]*x+LSprosocialcoefs3[2]*x2)
GE1=(LSprosocialintercepts[3]+LSprosocialcoefs1[3]+LSprosocialcoefs2[3]*x+LSprosocialcoefs3[3]*x2+LSprosocialcoefs4[3]*x+LSprosocialcoefs5[3]*x2)
GE2=(LSprosocialintercepts[3]+LSprosocialcoefs2[3]*x+LSprosocialcoefs3[3]*x2)
NE1=(LSprosocialintercepts[4]+LSprosocialcoefs1[4]+LSprosocialcoefs2[4]*x+LSprosocialcoefs3[4]*x2+LSprosocialcoefs4[4]*x+LSprosocialcoefs5[4]*x2)
NE2=(LSprosocialintercepts[4]+LSprosocialcoefs2[4]*x+LSprosocialcoefs3[4]*x2)
EE1=(LSprosocialintercepts[5]+LSprosocialcoefs1[5]+LSprosocialcoefs2[5]*x+LSprosocialcoefs3[5]*x2+LSprosocialcoefs4[5]*x+LSprosocialcoefs5[5]*x2)
EE2=(LSprosocialintercepts[5]+LSprosocialcoefs2[5]*x+LSprosocialcoefs3[5]*x2)
CA1=(LSprosocialintercepts[6]+LSprosocialcoefs1[6]+LSprosocialcoefs2[6]*x+LSprosocialcoefs3[6]*x2+LSprosocialcoefs4[6]*x+LSprosocialcoefs5[6]*x2)
CA2=(LSprosocialintercepts[6]+LSprosocialcoefs2[6]*x+LSprosocialcoefs3[6]*x2)
LA1=(LSprosocialintercepts[7]+LSprosocialcoefs1[7]+LSprosocialcoefs2[7]*x+LSprosocialcoefs3[7]*x2+LSprosocialcoefs4[7]*x+LSprosocialcoefs5[7]*x2)
LA2=(LSprosocialintercepts[7]+LSprosocialcoefs2[7]*x+LSprosocialcoefs3[7]*x2)
SA1=(LSprosocialintercepts[8]+LSprosocialcoefs1[8]+LSprosocialcoefs2[8]*x+LSprosocialcoefs3[8]*x2+LSprosocialcoefs4[8]*x+LSprosocialcoefs5[8]*x2)
SA2=(LSprosocialintercepts[8]+LSprosocialcoefs2[8]*x+LSprosocialcoefs3[8]*x2)
Ar1=(LSprosocialintercepts[9]+LSprosocialcoefs1[9]+LSprosocialcoefs2[9]*x+LSprosocialcoefs3[9]*x2+LSprosocialcoefs4[9]*x+LSprosocialcoefs5[9]*x2)
Ar2=(LSprosocialintercepts[9]+LSprosocialcoefs2[9]*x+LSprosocialcoefs3[9]*x2)
Af1=(LSprosocialintercepts[10]+LSprosocialcoefs1[10]+LSprosocialcoefs2[10]*x+LSprosocialcoefs3[10]*x2+LSprosocialcoefs4[10]*x+LSprosocialcoefs5[10]*x2)
Af2=(LSprosocialintercepts[10]+LSprosocialcoefs2[10]*x+LSprosocialcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
LS=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Prosocial=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(LS,Region,Age,Prosocial)
#data for the fill
mins=pmin(LS[1:66011],LS[66012:132022])
maxs=pmax(LS[1:66011],LS[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=LS,linetype=Prosocial),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Prosocial","No Prosocial"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

#PA
#get parameters
PAprosocialintercepts=PAprosocialXage2$coefficients$fixed[1]+PAprosocialXage2$coefficients$random$Region[,1]
PAprosocialcoefs1=PAprosocialXage2$coefficients$fixed[2]+PAprosocialXage2$coefficients$random$Region[,2]
PAprosocialcoefs2=PAprosocialXage2$coefficients$fixed[3]+PAprosocialXage2$coefficients$random$Region[,3]
PAprosocialcoefs3=PAprosocialXage2$coefficients$fixed[4]+PAprosocialXage2$coefficients$random$Region[,4]
PAprosocialcoefs4=PAprosocialXage2$coefficients$fixed[5]+PAprosocialXage2$coefficients$random$Region[,5]
PAprosocialcoefs5=PAprosocialXage2$coefficients$fixed[6]+PAprosocialXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(PAprosocialXage2$coefficients$fixed[1]+PAprosocialXage2$coefficients$fixed[2]+PAprosocialXage2$coefficients$fixed[3]*x+PAprosocialXage2$coefficients$fixed[4]*x2+PAprosocialXage2$coefficients$fixed[5]*x+PAprosocialXage2$coefficients$fixed[6]*x2)
Gl2=(PAprosocialXage2$coefficients$fixed[1]+PAprosocialXage2$coefficients$fixed[3]*x+PAprosocialXage2$coefficients$fixed[4]*x2)
A1=(PAprosocialintercepts[1]+PAprosocialcoefs1[1]+PAprosocialcoefs2[1]*x+PAprosocialcoefs3[1]*x2+PAprosocialcoefs4[1]*x+PAprosocialcoefs5[1]*x2)
A2=(PAprosocialintercepts[1]+PAprosocialcoefs2[1]*x+PAprosocialcoefs3[1]*x2)
LE1=(PAprosocialintercepts[2]+PAprosocialcoefs1[2]+PAprosocialcoefs2[2]*x+PAprosocialcoefs3[2]*x2+PAprosocialcoefs4[2]*x+PAprosocialcoefs5[2]*x2)
LE2=(PAprosocialintercepts[2]+PAprosocialcoefs2[2]*x+PAprosocialcoefs3[2]*x2)
GE1=(PAprosocialintercepts[3]+PAprosocialcoefs1[3]+PAprosocialcoefs2[3]*x+PAprosocialcoefs3[3]*x2+PAprosocialcoefs4[3]*x+PAprosocialcoefs5[3]*x2)
GE2=(PAprosocialintercepts[3]+PAprosocialcoefs2[3]*x+PAprosocialcoefs3[3]*x2)
NE1=(PAprosocialintercepts[4]+PAprosocialcoefs1[4]+PAprosocialcoefs2[4]*x+PAprosocialcoefs3[4]*x2+PAprosocialcoefs4[4]*x+PAprosocialcoefs5[4]*x2)
NE2=(PAprosocialintercepts[4]+PAprosocialcoefs2[4]*x+PAprosocialcoefs3[4]*x2)
EE1=(PAprosocialintercepts[5]+PAprosocialcoefs1[5]+PAprosocialcoefs2[5]*x+PAprosocialcoefs3[5]*x2+PAprosocialcoefs4[5]*x+PAprosocialcoefs5[5]*x2)
EE2=(PAprosocialintercepts[5]+PAprosocialcoefs2[5]*x+PAprosocialcoefs3[5]*x2)
CA1=(PAprosocialintercepts[6]+PAprosocialcoefs1[6]+PAprosocialcoefs2[6]*x+PAprosocialcoefs3[6]*x2+PAprosocialcoefs4[6]*x+PAprosocialcoefs5[6]*x2)
CA2=(PAprosocialintercepts[6]+PAprosocialcoefs2[6]*x+PAprosocialcoefs3[6]*x2)
LA1=(PAprosocialintercepts[7]+PAprosocialcoefs1[7]+PAprosocialcoefs2[7]*x+PAprosocialcoefs3[7]*x2+PAprosocialcoefs4[7]*x+PAprosocialcoefs5[7]*x2)
LA2=(PAprosocialintercepts[7]+PAprosocialcoefs2[7]*x+PAprosocialcoefs3[7]*x2)
SA1=(PAprosocialintercepts[8]+PAprosocialcoefs1[8]+PAprosocialcoefs2[8]*x+PAprosocialcoefs3[8]*x2+PAprosocialcoefs4[8]*x+PAprosocialcoefs5[8]*x2)
SA2=(PAprosocialintercepts[8]+PAprosocialcoefs2[8]*x+PAprosocialcoefs3[8]*x2)
Ar1=(PAprosocialintercepts[9]+PAprosocialcoefs1[9]+PAprosocialcoefs2[9]*x+PAprosocialcoefs3[9]*x2+PAprosocialcoefs4[9]*x+PAprosocialcoefs5[9]*x2)
Ar2=(PAprosocialintercepts[9]+PAprosocialcoefs2[9]*x+PAprosocialcoefs3[9]*x2)
Af1=(PAprosocialintercepts[10]+PAprosocialcoefs1[10]+PAprosocialcoefs2[10]*x+PAprosocialcoefs3[10]*x2+PAprosocialcoefs4[10]*x+PAprosocialcoefs5[10]*x2)
Af2=(PAprosocialintercepts[10]+PAprosocialcoefs2[10]*x+PAprosocialcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
PA=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Prosocial=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(PA,Region,Age,Prosocial)
#data for the fill
mins=pmin(PA[1:66011],PA[66012:132022])
maxs=pmax(PA[1:66011],PA[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=PA,linetype=Prosocial),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Prosocial","No Prosocial"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

#NA
#get parameters
NAprosocialintercepts=NAprosocialXage2$coefficients$fixed[1]+NAprosocialXage2$coefficients$random$Region[,1]
NAprosocialcoefs1=NAprosocialXage2$coefficients$fixed[2]+NAprosocialXage2$coefficients$random$Region[,2]
NAprosocialcoefs2=NAprosocialXage2$coefficients$fixed[3]+NAprosocialXage2$coefficients$random$Region[,3]
NAprosocialcoefs3=NAprosocialXage2$coefficients$fixed[4]+NAprosocialXage2$coefficients$random$Region[,4]
NAprosocialcoefs4=NAprosocialXage2$coefficients$fixed[5]+NAprosocialXage2$coefficients$random$Region[,5]
NAprosocialcoefs5=NAprosocialXage2$coefficients$fixed[6]+NAprosocialXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(NAprosocialXage2$coefficients$fixed[1]+NAprosocialXage2$coefficients$fixed[2]+NAprosocialXage2$coefficients$fixed[3]*x+NAprosocialXage2$coefficients$fixed[4]*x2+NAprosocialXage2$coefficients$fixed[5]*x+NAprosocialXage2$coefficients$fixed[6]*x2)
Gl2=(NAprosocialXage2$coefficients$fixed[1]+NAprosocialXage2$coefficients$fixed[3]*x+NAprosocialXage2$coefficients$fixed[4]*x2)
A1=(NAprosocialintercepts[1]+NAprosocialcoefs1[1]+NAprosocialcoefs2[1]*x+NAprosocialcoefs3[1]*x2+NAprosocialcoefs4[1]*x+NAprosocialcoefs5[1]*x2)
A2=(NAprosocialintercepts[1]+NAprosocialcoefs2[1]*x+NAprosocialcoefs3[1]*x2)
LE1=(NAprosocialintercepts[2]+NAprosocialcoefs1[2]+NAprosocialcoefs2[2]*x+NAprosocialcoefs3[2]*x2+NAprosocialcoefs4[2]*x+NAprosocialcoefs5[2]*x2)
LE2=(NAprosocialintercepts[2]+NAprosocialcoefs2[2]*x+NAprosocialcoefs3[2]*x2)
GE1=(NAprosocialintercepts[3]+NAprosocialcoefs1[3]+NAprosocialcoefs2[3]*x+NAprosocialcoefs3[3]*x2+NAprosocialcoefs4[3]*x+NAprosocialcoefs5[3]*x2)
GE2=(NAprosocialintercepts[3]+NAprosocialcoefs2[3]*x+NAprosocialcoefs3[3]*x2)
NE1=(NAprosocialintercepts[4]+NAprosocialcoefs1[4]+NAprosocialcoefs2[4]*x+NAprosocialcoefs3[4]*x2+NAprosocialcoefs4[4]*x+NAprosocialcoefs5[4]*x2)
NE2=(NAprosocialintercepts[4]+NAprosocialcoefs2[4]*x+NAprosocialcoefs3[4]*x2)
EE1=(NAprosocialintercepts[5]+NAprosocialcoefs1[5]+NAprosocialcoefs2[5]*x+NAprosocialcoefs3[5]*x2+NAprosocialcoefs4[5]*x+NAprosocialcoefs5[5]*x2)
EE2=(NAprosocialintercepts[5]+NAprosocialcoefs2[5]*x+NAprosocialcoefs3[5]*x2)
CA1=(NAprosocialintercepts[6]+NAprosocialcoefs1[6]+NAprosocialcoefs2[6]*x+NAprosocialcoefs3[6]*x2+NAprosocialcoefs4[6]*x+NAprosocialcoefs5[6]*x2)
CA2=(NAprosocialintercepts[6]+NAprosocialcoefs2[6]*x+NAprosocialcoefs3[6]*x2)
LA1=(NAprosocialintercepts[7]+NAprosocialcoefs1[7]+NAprosocialcoefs2[7]*x+NAprosocialcoefs3[7]*x2+NAprosocialcoefs4[7]*x+NAprosocialcoefs5[7]*x2)
LA2=(NAprosocialintercepts[7]+NAprosocialcoefs2[7]*x+NAprosocialcoefs3[7]*x2)
SA1=(NAprosocialintercepts[8]+NAprosocialcoefs1[8]+NAprosocialcoefs2[8]*x+NAprosocialcoefs3[8]*x2+NAprosocialcoefs4[8]*x+NAprosocialcoefs5[8]*x2)
SA2=(NAprosocialintercepts[8]+NAprosocialcoefs2[8]*x+NAprosocialcoefs3[8]*x2)
Ar1=(NAprosocialintercepts[9]+NAprosocialcoefs1[9]+NAprosocialcoefs2[9]*x+NAprosocialcoefs3[9]*x2+NAprosocialcoefs4[9]*x+NAprosocialcoefs5[9]*x2)
Ar2=(NAprosocialintercepts[9]+NAprosocialcoefs2[9]*x+NAprosocialcoefs3[9]*x2)
Af1=(NAprosocialintercepts[10]+NAprosocialcoefs1[10]+NAprosocialcoefs2[10]*x+NAprosocialcoefs3[10]*x2+NAprosocialcoefs4[10]*x+NAprosocialcoefs5[10]*x2)
Af2=(NAprosocialintercepts[10]+NAprosocialcoefs2[10]*x+NAprosocialcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
Neg=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Prosocial=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(Neg,Region,Age,Prosocial)
#data for the fill
mins=pmin(Neg[1:66011],Neg[66012:132022])
maxs=pmax(Neg[1:66011],Neg[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=Neg,linetype=Prosocial),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10),name='NA')+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Prosocial","No Prosocial"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

###Figures: Life Meaning
#LS
#get parameters
LSmeaningintercepts=LSmeaningXage2$coefficients$fixed[1]+LSmeaningXage2$coefficients$random$Region[,1]
LSmeaningcoefs1=LSmeaningXage2$coefficients$fixed[2]+LSmeaningXage2$coefficients$random$Region[,2]
LSmeaningcoefs2=LSmeaningXage2$coefficients$fixed[3]+LSmeaningXage2$coefficients$random$Region[,3]
LSmeaningcoefs3=LSmeaningXage2$coefficients$fixed[4]+LSmeaningXage2$coefficients$random$Region[,4]
LSmeaningcoefs4=LSmeaningXage2$coefficients$fixed[5]+LSmeaningXage2$coefficients$random$Region[,5]
LSmeaningcoefs5=LSmeaningXage2$coefficients$fixed[6]+LSmeaningXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(LSmeaningXage2$coefficients$fixed[1]+LSmeaningXage2$coefficients$fixed[2]+LSmeaningXage2$coefficients$fixed[3]*x+LSmeaningXage2$coefficients$fixed[4]*x2+LSmeaningXage2$coefficients$fixed[5]*x+LSmeaningXage2$coefficients$fixed[6]*x2)
Gl2=(LSmeaningXage2$coefficients$fixed[1]+LSmeaningXage2$coefficients$fixed[3]*x+LSmeaningXage2$coefficients$fixed[4]*x2)
A1=(LSmeaningintercepts[1]+LSmeaningcoefs1[1]+LSmeaningcoefs2[1]*x+LSmeaningcoefs3[1]*x2+LSmeaningcoefs4[1]*x+LSmeaningcoefs5[1]*x2)
A2=(LSmeaningintercepts[1]+LSmeaningcoefs2[1]*x+LSmeaningcoefs3[1]*x2)
LE1=(LSmeaningintercepts[2]+LSmeaningcoefs1[2]+LSmeaningcoefs2[2]*x+LSmeaningcoefs3[2]*x2+LSmeaningcoefs4[2]*x+LSmeaningcoefs5[2]*x2)
LE2=(LSmeaningintercepts[2]+LSmeaningcoefs2[2]*x+LSmeaningcoefs3[2]*x2)
GE1=(LSmeaningintercepts[3]+LSmeaningcoefs1[3]+LSmeaningcoefs2[3]*x+LSmeaningcoefs3[3]*x2+LSmeaningcoefs4[3]*x+LSmeaningcoefs5[3]*x2)
GE2=(LSmeaningintercepts[3]+LSmeaningcoefs2[3]*x+LSmeaningcoefs3[3]*x2)
NE1=(LSmeaningintercepts[4]+LSmeaningcoefs1[4]+LSmeaningcoefs2[4]*x+LSmeaningcoefs3[4]*x2+LSmeaningcoefs4[4]*x+LSmeaningcoefs5[4]*x2)
NE2=(LSmeaningintercepts[4]+LSmeaningcoefs2[4]*x+LSmeaningcoefs3[4]*x2)
EE1=(LSmeaningintercepts[5]+LSmeaningcoefs1[5]+LSmeaningcoefs2[5]*x+LSmeaningcoefs3[5]*x2+LSmeaningcoefs4[5]*x+LSmeaningcoefs5[5]*x2)
EE2=(LSmeaningintercepts[5]+LSmeaningcoefs2[5]*x+LSmeaningcoefs3[5]*x2)
CA1=(LSmeaningintercepts[6]+LSmeaningcoefs1[6]+LSmeaningcoefs2[6]*x+LSmeaningcoefs3[6]*x2+LSmeaningcoefs4[6]*x+LSmeaningcoefs5[6]*x2)
CA2=(LSmeaningintercepts[6]+LSmeaningcoefs2[6]*x+LSmeaningcoefs3[6]*x2)
LA1=(LSmeaningintercepts[7]+LSmeaningcoefs1[7]+LSmeaningcoefs2[7]*x+LSmeaningcoefs3[7]*x2+LSmeaningcoefs4[7]*x+LSmeaningcoefs5[7]*x2)
LA2=(LSmeaningintercepts[7]+LSmeaningcoefs2[7]*x+LSmeaningcoefs3[7]*x2)
SA1=(LSmeaningintercepts[8]+LSmeaningcoefs1[8]+LSmeaningcoefs2[8]*x+LSmeaningcoefs3[8]*x2+LSmeaningcoefs4[8]*x+LSmeaningcoefs5[8]*x2)
SA2=(LSmeaningintercepts[8]+LSmeaningcoefs2[8]*x+LSmeaningcoefs3[8]*x2)
Ar1=(LSmeaningintercepts[9]+LSmeaningcoefs1[9]+LSmeaningcoefs2[9]*x+LSmeaningcoefs3[9]*x2+LSmeaningcoefs4[9]*x+LSmeaningcoefs5[9]*x2)
Ar2=(LSmeaningintercepts[9]+LSmeaningcoefs2[9]*x+LSmeaningcoefs3[9]*x2)
Af1=(LSmeaningintercepts[10]+LSmeaningcoefs1[10]+LSmeaningcoefs2[10]*x+LSmeaningcoefs3[10]*x2+LSmeaningcoefs4[10]*x+LSmeaningcoefs5[10]*x2)
Af2=(LSmeaningintercepts[10]+LSmeaningcoefs2[10]*x+LSmeaningcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
LS=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Meaning=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(LS,Region,Age,Meaning)
#data for the fill
mins=pmin(LS[1:66011],LS[66012:132022])
maxs=pmax(LS[1:66011],LS[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=LS,linetype=Meaning),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Meaning","No Meaning"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

#PA
#get parameters
PAmeaningintercepts=PAmeaningXage2$coefficients$fixed[1]+PAmeaningXage2$coefficients$random$Region[,1]
PAmeaningcoefs1=PAmeaningXage2$coefficients$fixed[2]+PAmeaningXage2$coefficients$random$Region[,2]
PAmeaningcoefs2=PAmeaningXage2$coefficients$fixed[3]+PAmeaningXage2$coefficients$random$Region[,3]
PAmeaningcoefs3=PAmeaningXage2$coefficients$fixed[4]+PAmeaningXage2$coefficients$random$Region[,4]
PAmeaningcoefs4=PAmeaningXage2$coefficients$fixed[5]+PAmeaningXage2$coefficients$random$Region[,5]
PAmeaningcoefs5=PAmeaningXage2$coefficients$fixed[6]+PAmeaningXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(PAmeaningXage2$coefficients$fixed[1]+PAmeaningXage2$coefficients$fixed[2]+PAmeaningXage2$coefficients$fixed[3]*x+PAmeaningXage2$coefficients$fixed[4]*x2+PAmeaningXage2$coefficients$fixed[5]*x+PAmeaningXage2$coefficients$fixed[6]*x2)
Gl2=(PAmeaningXage2$coefficients$fixed[1]+PAmeaningXage2$coefficients$fixed[3]*x+PAmeaningXage2$coefficients$fixed[4]*x2)
A1=(PAmeaningintercepts[1]+PAmeaningcoefs1[1]+PAmeaningcoefs2[1]*x+PAmeaningcoefs3[1]*x2+PAmeaningcoefs4[1]*x+PAmeaningcoefs5[1]*x2)
A2=(PAmeaningintercepts[1]+PAmeaningcoefs2[1]*x+PAmeaningcoefs3[1]*x2)
LE1=(PAmeaningintercepts[2]+PAmeaningcoefs1[2]+PAmeaningcoefs2[2]*x+PAmeaningcoefs3[2]*x2+PAmeaningcoefs4[2]*x+PAmeaningcoefs5[2]*x2)
LE2=(PAmeaningintercepts[2]+PAmeaningcoefs2[2]*x+PAmeaningcoefs3[2]*x2)
GE1=(PAmeaningintercepts[3]+PAmeaningcoefs1[3]+PAmeaningcoefs2[3]*x+PAmeaningcoefs3[3]*x2+PAmeaningcoefs4[3]*x+PAmeaningcoefs5[3]*x2)
GE2=(PAmeaningintercepts[3]+PAmeaningcoefs2[3]*x+PAmeaningcoefs3[3]*x2)
NE1=(PAmeaningintercepts[4]+PAmeaningcoefs1[4]+PAmeaningcoefs2[4]*x+PAmeaningcoefs3[4]*x2+PAmeaningcoefs4[4]*x+PAmeaningcoefs5[4]*x2)
NE2=(PAmeaningintercepts[4]+PAmeaningcoefs2[4]*x+PAmeaningcoefs3[4]*x2)
EE1=(PAmeaningintercepts[5]+PAmeaningcoefs1[5]+PAmeaningcoefs2[5]*x+PAmeaningcoefs3[5]*x2+PAmeaningcoefs4[5]*x+PAmeaningcoefs5[5]*x2)
EE2=(PAmeaningintercepts[5]+PAmeaningcoefs2[5]*x+PAmeaningcoefs3[5]*x2)
CA1=(PAmeaningintercepts[6]+PAmeaningcoefs1[6]+PAmeaningcoefs2[6]*x+PAmeaningcoefs3[6]*x2+PAmeaningcoefs4[6]*x+PAmeaningcoefs5[6]*x2)
CA2=(PAmeaningintercepts[6]+PAmeaningcoefs2[6]*x+PAmeaningcoefs3[6]*x2)
LA1=(PAmeaningintercepts[7]+PAmeaningcoefs1[7]+PAmeaningcoefs2[7]*x+PAmeaningcoefs3[7]*x2+PAmeaningcoefs4[7]*x+PAmeaningcoefs5[7]*x2)
LA2=(PAmeaningintercepts[7]+PAmeaningcoefs2[7]*x+PAmeaningcoefs3[7]*x2)
SA1=(PAmeaningintercepts[8]+PAmeaningcoefs1[8]+PAmeaningcoefs2[8]*x+PAmeaningcoefs3[8]*x2+PAmeaningcoefs4[8]*x+PAmeaningcoefs5[8]*x2)
SA2=(PAmeaningintercepts[8]+PAmeaningcoefs2[8]*x+PAmeaningcoefs3[8]*x2)
Ar1=(PAmeaningintercepts[9]+PAmeaningcoefs1[9]+PAmeaningcoefs2[9]*x+PAmeaningcoefs3[9]*x2+PAmeaningcoefs4[9]*x+PAmeaningcoefs5[9]*x2)
Ar2=(PAmeaningintercepts[9]+PAmeaningcoefs2[9]*x+PAmeaningcoefs3[9]*x2)
Af1=(PAmeaningintercepts[10]+PAmeaningcoefs1[10]+PAmeaningcoefs2[10]*x+PAmeaningcoefs3[10]*x2+PAmeaningcoefs4[10]*x+PAmeaningcoefs5[10]*x2)
Af2=(PAmeaningintercepts[10]+PAmeaningcoefs2[10]*x+PAmeaningcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
PA=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Prosocial=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(PA,Region,Age,Prosocial)
#data for the fill
mins=pmin(PA[1:66011],PA[66012:132022])
maxs=pmax(PA[1:66011],PA[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=PA,linetype=Meaning),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Meaning","No Meaning"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

#NA
#get parameters
NAmeaningintercepts=NAmeaningXage2$coefficients$fixed[1]+NAmeaningXage2$coefficients$random$Region[,1]
NAmeaningcoefs1=NAmeaningXage2$coefficients$fixed[2]+NAmeaningXage2$coefficients$random$Region[,2]
NAmeaningcoefs2=NAmeaningXage2$coefficients$fixed[3]+NAmeaningXage2$coefficients$random$Region[,3]
NAmeaningcoefs3=NAmeaningXage2$coefficients$fixed[4]+NAmeaningXage2$coefficients$random$Region[,4]
NAmeaningcoefs4=NAmeaningXage2$coefficients$fixed[5]+NAmeaningXage2$coefficients$random$Region[,5]
NAmeaningcoefs5=NAmeaningXage2$coefficients$fixed[6]+NAmeaningXage2$coefficients$random$Region[,6]
#data for the lines
x=seq(20-Gallmean,80-Gallmean,.01)
x2=x^2
Gl1=(NAmeaningXage2$coefficients$fixed[1]+NAmeaningXage2$coefficients$fixed[2]+NAmeaningXage2$coefficients$fixed[3]*x+NAmeaningXage2$coefficients$fixed[4]*x2+NAmeaningXage2$coefficients$fixed[5]*x+NAmeaningXage2$coefficients$fixed[6]*x2)
Gl2=(NAmeaningXage2$coefficients$fixed[1]+NAmeaningXage2$coefficients$fixed[3]*x+NAmeaningXage2$coefficients$fixed[4]*x2)
A1=(NAmeaningintercepts[1]+NAmeaningcoefs1[1]+NAmeaningcoefs2[1]*x+NAmeaningcoefs3[1]*x2+NAmeaningcoefs4[1]*x+NAmeaningcoefs5[1]*x2)
A2=(NAmeaningintercepts[1]+NAmeaningcoefs2[1]*x+NAmeaningcoefs3[1]*x2)
LE1=(NAmeaningintercepts[2]+NAmeaningcoefs1[2]+NAmeaningcoefs2[2]*x+NAmeaningcoefs3[2]*x2+NAmeaningcoefs4[2]*x+NAmeaningcoefs5[2]*x2)
LE2=(NAmeaningintercepts[2]+NAmeaningcoefs2[2]*x+NAmeaningcoefs3[2]*x2)
GE1=(NAmeaningintercepts[3]+NAmeaningcoefs1[3]+NAmeaningcoefs2[3]*x+NAmeaningcoefs3[3]*x2+NAmeaningcoefs4[3]*x+NAmeaningcoefs5[3]*x2)
GE2=(NAmeaningintercepts[3]+NAmeaningcoefs2[3]*x+NAmeaningcoefs3[3]*x2)
NE1=(NAmeaningintercepts[4]+NAmeaningcoefs1[4]+NAmeaningcoefs2[4]*x+NAmeaningcoefs3[4]*x2+NAmeaningcoefs4[4]*x+NAmeaningcoefs5[4]*x2)
NE2=(NAmeaningintercepts[4]+NAmeaningcoefs2[4]*x+NAmeaningcoefs3[4]*x2)
EE1=(NAmeaningintercepts[5]+NAmeaningcoefs1[5]+NAmeaningcoefs2[5]*x+NAmeaningcoefs3[5]*x2+NAmeaningcoefs4[5]*x+NAmeaningcoefs5[5]*x2)
EE2=(NAmeaningintercepts[5]+NAmeaningcoefs2[5]*x+NAmeaningcoefs3[5]*x2)
CA1=(NAmeaningintercepts[6]+NAmeaningcoefs1[6]+NAmeaningcoefs2[6]*x+NAmeaningcoefs3[6]*x2+NAmeaningcoefs4[6]*x+NAmeaningcoefs5[6]*x2)
CA2=(NAmeaningintercepts[6]+NAmeaningcoefs2[6]*x+NAmeaningcoefs3[6]*x2)
LA1=(NAmeaningintercepts[7]+NAmeaningcoefs1[7]+NAmeaningcoefs2[7]*x+NAmeaningcoefs3[7]*x2+NAmeaningcoefs4[7]*x+NAmeaningcoefs5[7]*x2)
LA2=(NAmeaningintercepts[7]+NAmeaningcoefs2[7]*x+NAmeaningcoefs3[7]*x2)
SA1=(NAmeaningintercepts[8]+NAmeaningcoefs1[8]+NAmeaningcoefs2[8]*x+NAmeaningcoefs3[8]*x2+NAmeaningcoefs4[8]*x+NAmeaningcoefs5[8]*x2)
SA2=(NAmeaningintercepts[8]+NAmeaningcoefs2[8]*x+NAmeaningcoefs3[8]*x2)
Ar1=(NAmeaningintercepts[9]+NAmeaningcoefs1[9]+NAmeaningcoefs2[9]*x+NAmeaningcoefs3[9]*x2+NAmeaningcoefs4[9]*x+NAmeaningcoefs5[9]*x2)
Ar2=(NAmeaningintercepts[9]+NAmeaningcoefs2[9]*x+NAmeaningcoefs3[9]*x2)
Af1=(NAmeaningintercepts[10]+NAmeaningcoefs1[10]+NAmeaningcoefs2[10]*x+NAmeaningcoefs3[10]*x2+NAmeaningcoefs4[10]*x+NAmeaningcoefs5[10]*x2)
Af2=(NAmeaningintercepts[10]+NAmeaningcoefs2[10]*x+NAmeaningcoefs3[10]*x2)
Region=factor(rep(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),2),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
Neg=c(Gl1,A1,LE1,GE1,NE1,EE1,CA1,LA1,SA1,Ar1,Af1,Gl2,A2,LE2,GE2,NE2,EE2,CA2,LA2,SA2,Ar2,Af2)
Meaning=factor(c(rep(1,6001*11),rep(0,6001*11)),levels=c('1','0'))
Age=rep(x,22)
plotdata=data.frame(Neg,Region,Age,Meaning)
#data for the fill
mins=pmin(Neg[1:66011],Neg[66012:132022])
maxs=pmax(Neg[1:66011],Neg[66012:132022])
differences=c(Gl1-Gl2,A1-A2,LE1-LE2,GE1-GE2,NE1-NE2,EE1-EE2,CA1-CA2,LA1-LA2,SA1-SA2,Ar1-Ar2,Af1-Af2)
size=rep(NA,66011);size[which(differences>0&differences<.5)]=NA;size[which(differences<0&differences>-.4)]=NA;size[which(differences>.5&differences<1)]='smallpos';size[which(differences<(-.5)&differences>-1)]='smallneg';size[which(differences<2&differences>1)]='mediumpos';size[which(differences>-2&differences<(-1))]='mediumneg';size[which(differences>2)]='largepos';size[which(differences<(-2))]='largeneg'
smallpos=rep(NA,66011);smallpos[which(size=='smallpos')]=Age[which(size=='smallpos')]
smallposymin=rep(NA,66011);smallposymin[which(size=='smallpos')]=mins[which(size=='smallpos')]
smallposymax=rep(NA,66011);smallposymax[which(size=='smallpos')]=maxs[which(size=='smallpos')]
mediumpos=rep(NA,66011);mediumpos[which(size=='mediumpos')]=Age[which(size=='mediumpos')]
mediumposymin=rep(NA,66011);mediumposymin[which(size=='mediumpos')]=mins[which(size=='mediumpos')]
mediumposymax=rep(NA,66011);mediumposymax[which(size=='mediumpos')]=maxs[which(size=='mediumpos')]
largepos=rep(NA,66011);largepos[which(size=='largepos')]=Age[which(size=='largepos')]
largeposymin=rep(NA,66011);largeposymin[which(size=='largepos')]=mins[which(size=='largepos')]
largeposymax=rep(NA,66011);largeposymax[which(size=='largepos')]=maxs[which(size=='largepos')]
smallneg=rep(NA,66011);smallneg[which(size=='smallneg')]=Age[which(size=='smallneg')]
smallnegymin=rep(NA,66011);smallnegymin[which(size=='smallneg')]=mins[which(size=='smallneg')]
smallnegymax=rep(NA,66011);smallnegymax[which(size=='smallneg')]=maxs[which(size=='smallneg')]
mediumneg=rep(NA,66011);mediumneg[which(size=='mediumneg')]=Age[which(size=='mediumneg')]
mediumnegymin=rep(NA,66011);mediumnegymin[which(size=='mediumneg')]=mins[which(size=='mediumneg')]
mediumnegymax=rep(NA,66011);mediumnegymax[which(size=='mediumneg')]=maxs[which(size=='mediumneg')]
largeneg=rep(NA,66011);largeneg[which(size=='largeneg')]=Age[which(size=='largeneg')]
largenegymin=rep(NA,66011);largenegymin[which(size=='largeneg')]=mins[which(size=='largeneg')]
largenegymax=rep(NA,66011);largenegymax[which(size=='largeneg')]=maxs[which(size=='largeneg')]
Region=factor(c(rep('Global',6001),rep('Anglo',6001),rep('Latin Europe',6001),rep('Germanic Europe',6001),rep('Nordic Europe',6001),rep('Eastern Europe',6001),rep('Confucian Asia',6001),rep('Latin America',6001),rep('Southern Asia',6001),rep('Arab',6001),rep('Sub-Sahara Africa',6001)),levels=c("Global","Anglo","Latin Europe","Germanic Europe","Nordic Europe","Eastern Europe","Confucian Asia","Latin America","Southern Asia","Arab","Sub-Sahara Africa"))
ribbondata=data.frame(Region,smallpos,smallposymin,smallposymax,mediumpos,mediumposymin,mediumposymax,largepos,largeposymin,largeposymax,smallneg,smallnegymin,smallnegymax,mediumneg,mediumnegymin,mediumnegymax,largeneg,largenegymin,largenegymax)
#plot
plot=ggplot()+
  geom_ribbon(aes(x=smallpos,ymin=smallposymin,ymax=smallposymax,fill=".50-.99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumpos,ymin=mediumposymin,ymax=mediumposymax,fill="1.00-1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largepos,ymin=largeposymin,ymax=largeposymax,fill="2+"),data=ribbondata)+
  geom_ribbon(aes(x=smallneg,ymin=smallnegymin,ymax=smallnegymax,fill="-.50-[-].99"),data=ribbondata)+
  geom_ribbon(aes(x=mediumneg,ymin=mediumnegymin,ymax=mediumnegymax,fill="-1.00-[-]1.99"),data=ribbondata)+
  geom_ribbon(aes(x=largeneg,ymin=largenegymin,ymax=largenegymax,fill="-2+"),data=ribbondata)+
  geom_line(aes(x=Age,y=Neg,linetype=Meaning),data=plotdata,size=1)+
  scale_x_continuous(name="Age",limits=c(20-Gallmean,80-Gallmean),breaks=c(20-Gallmean,40-Gallmean,60-Gallmean,80-Gallmean),labels=c('20','40','60','80'))+
  scale_y_continuous(limits=c(0,10),name='NA')+
  facet_wrap(~Region,ncol=4)+
  theme_bw()+
  scale_linetype_manual(name="       ",values=c(1,2),labels=c("Meaning","No Meaning"))+
  scale_fill_manual(name='Differences',breaks=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'),values=c("#FEE0D2","#FCBBA1","#FC9272","#DEEBF7","#C6DBEF","#9ECAE1"),labels=c('.50-.99','1.00-1.99','2+','-.50-[-].99','-1.00-[-]1.99','-2+'))+
  guides(linetype=guide_legend(order=1),fill=guide_legend(order=2))+
  theme(plot.title=element_text(hjust=.5),legend.title=element_text(hjust=.5),panel.grid.minor.x=element_blank())
plot

