#==================================================#
# Education-IQ meta-analysis publication bias tests#
#==================================================#

#By Stuart Ritchie, October 2017

#Funnel plot ggplot2 code below is adapted from code written by John Sakaluk (2016): https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/

#==========================#
#Data and packages required#
#==========================#

df <- read.table("~/meta_dataset.txt", sep="\t", header=T)
names(df); dim(df); str(df)

library(metafor); library(ggplot2)

#Get the "multiplot" function from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#It's copied below for ease of reference

#Sort out which variables are factors

df$Study_ID <- as.factor(df$Study_ID)
df$Data_ID <- as.factor(df$Data_ID)
df$k <- as.factor(df$k)
df$outcome_test_cat <- as.factor(df$outcome_test_cat)
df$ses_funnel <- as.factor(df$ses_funnel)
df$published <- as.factor(df$published)

str(df)

#========================#
#Split into three subsets#
#========================#
#(Study design type: 1 = Control Prior Intelligence (cpiq); 2 = Policy Change (poli); 3 = School Age Cutoff (schl))

cpiq <- subset(df, subset=(Design=="Control Prior IQ"))
poli <- subset(df, subset=(Design=="Policy Change"))
schl <- subset(df, subset=(Design=="School Age Cutoff"))

#==================#
#"Control prior IQ"#
#==================#

#Basic meta-analysis - note that this differs from the Mplus meta-analysis (used for the main estimates in the paper), because it does not account for the clustering of within-study estimates
ma1 <- rma.uni(yi=Effect_size, sei=SE, data=cpiq, method="REML"); summary(ma1)

#Basic funnel plot
funnel(ma1, yaxis="sei", main="Control Prior IQ", xlab="Effect size (IQ points)")

#Funnel plot asymmetry
regtest.rma(ma1) #No significant asymmetry in funnel plot

#Funnel plot in ggplot

estimatec = 1.197; sec = 0.203 #Estimates from the Mplus model
se.seqc=seq(0, max(na.omit(cpiq$SE+.03)), .01)
ll95c <- estimatec-(1.96*se.seqc)
ul95c <- estimatec+(1.96*se.seqc)
dfCIc <- data.frame(ll95c, ul95c, se.seqc, estimatec)	

funn_cpiq <- ggplot(aes(x=SE, y=Effect_size), data=cpiq) +
		scale_x_reverse(expand=c(0.01,0.01)) +
		coord_flip() +
		xlab("Standard Error") + ylab("Effect size (IQ points)") +
		geom_line(aes(x=se.seqc, y=ll95c), linetype="dashed", colour="#666666", data=dfCIc) +
		geom_line(aes(x=se.seqc, y=ul95c), linetype="dashed", colour="#666666", data=dfCIc) +
		geom_segment(aes(y=estimatec, yend=estimatec, xend=0, x= .55), colour="#666666", size=.3) +
		geom_point(shape=16, size=3, colour="#00348E")+ 
		theme_bw() +
		ggtitle("Control Prior Intelligence")+ 
		theme(plot.title = element_text(hjust=0.5))

#PET-PEESE

cpiq$var <- (cpiq$SE)^2
mPET <- lm(Effect_size ~ SE, weights=1/var, data= cpiq); summary(mPET)
mPEESE <- lm(Effect_size ~ var, weights=1/var, data= cpiq); summary(mPEESE) #Both PET and PEESE intercepts significant

pet_cpiq <- ggplot(cpiq, aes(y = Effect_size, x = SE, weight=1/var)) + 
			geom_point(colour="#00348E") +
			geom_smooth(formula = y ~ x, method="lm", colour="#00348E", fill="#00348E", fullrange=TRUE) + 
			theme_bw() +
			ggtitle("Control Prior Intelligence - PET") + 
			theme(plot.title = element_text(hjust=0.5)) + 
			xlab("Standard error") +
			ylab("Effect size") + 
			scale_x_continuous(expand=c(0,0))

peese_cpiq <- ggplot(cpiq, aes(y = Effect_size, x = var, weight=1/var)) + 
			geom_point(colour="#00348E") +
			geom_smooth(formula = y ~ x, method="lm", colour="#00348E", fill="#00348E", fullrange=TRUE) + 
			theme_bw() + 
			ggtitle("Control Prior Intelligence - PEESE") + 
			theme(plot.title = element_text(hjust=0.5))+ 
			xlab("Variance") +
			ylab("Effect size") + 
			scale_x_continuous(expand=c(0,0))

multiplot(pet_cpiq, peese_cpiq, cols=2)

#p-curve

cpiq$z <- (cpiq$Effect_size/cpiq$SE)
cpiq$p <- 2*pnorm(-abs(cpiq$z))
cpiq_p <- ceiling(cpiq$p[cpiq$p<.05]*100)/100

blueline_cpiq=c() 
ksig= sum(cpiq_p<.05,na.rm=TRUE)
for (i in c(.01,.02,.03,.04,.05)) blueline_cpiq=c(blueline_cpiq, sum(cpiq_p==i,na.rm=TRUE)/ksig*100)

data_cpiq <- read.table("~/pcurve_graphdata_cpiq.txt", sep="\t", header=T) #Make p-curve file using data from above calculation - see attached

pcurve_cpiq <- ggplot(data_cpiq, aes(y = Percent, x = Point, colour=Line, linetype=Line)) +
			geom_point() +
			geom_line(size=1) +
			scale_color_manual(values=c("#999999", "#666666", "#00348E")) +
			scale_linetype_manual(values=c("dotted", "dashed","solid")) +
			theme_bw() +
			xlab("p-value") + 
			coord_cartesian(ylim=c(0,100)) + 
			ggtitle("Control Prior Intelligence")+ 
				theme(plot.title = element_text(hjust=0.5)) + theme(legend.title=element_blank())

#===============#
#"Policy Change"#
#===============#

#Basic meta-analysis
ma2 <- rma.uni(yi=Effect_size, sei=SE, data=poli, method="REML"); summary(ma2)

#Funnel plot
funnel(ma2, yaxis="sei", main="Policy Change", xlab="Effect size (IQ points)")

#Funnel plot asymmetry
regtest.rma(ma2) #No significant asymmetry in funnel plot

#Funnel plot in ggplot

estimatep = 2.185; sep = 0.578 #Estimates from the Mplus model
se.seqp=seq(0, max(poli$SE+.17), .01)
ll95p <- estimatep-(1.96*se.seqp)
ul95p <- estimatep+(1.96*se.seqp)
dfCIp <- data.frame(ll95p, ul95p, se.seqp, estimatep)	

funn_poli <- ggplot(aes(x=SE, y=Effect_size), data=poli) +
		scale_x_reverse(expand=c(0.01,0.01)) +
		coord_flip() +
		xlab("Standard Error") + ylab("Effect size (IQ points)") +
		geom_line(aes(x=se.seqp, y=ll95p), linetype="dashed", colour="#666666", data=dfCIp) +
		geom_line(aes(x=se.seqp, y=ul95p), linetype="dashed", colour="#666666", data=dfCIp) +
		geom_segment(aes(y=estimatep, yend=estimatep, xend=0, x=5), colour="#666666", size=.3) +
		geom_point(shape=16, size=3, colour="#BA1825")+ 
		theme_bw() +
		ggtitle("Policy Change")+ 
		theme(plot.title = element_text(hjust=0.5))

#PET-PEESE

poli$var <- (poli$SE)^2
mPET <- lm(Effect_size ~ SE, weights=1/var, data=poli); summary(mPET)
mPEESE <- lm(Effect_size ~ var, weights=1/var, data=poli); summary(mPEESE) #Both PET and PEESE intercepts significant

#Plot PET and PEESE

pet_poli <- ggplot(poli, aes(y = Effect_size, x = SE, weight=1/var)) + 
			geom_point(colour="#BA1825") +
			geom_smooth(formula = y ~ x, method="lm", colour="#BA1825", fill="#BA1825") + 
			theme_bw() +
			ggtitle("Policy Change - PET") + 
			theme(plot.title = element_text(hjust=0.5)) + 
			xlab("Standard error") +
			ylab("Effect size") + 
			scale_x_continuous(expand=c(0,0)) + 
			coord_cartesian(ylim=c(-12,20))

peese_poli <- ggplot(poli, aes(y = Effect_size, x = var, weight=1/var)) + 
			geom_point(colour="#BA1825") +
			geom_smooth(formula = y ~ x, method="lm", colour="#BA1825", fill="#BA1825") + 
			theme_bw() + 
			ggtitle("Policy Change - PEESE") + 
			theme(plot.title = element_text(hjust=0.5))+ 
			xlab("Variance") +
			ylab("Effect size") + 
			scale_x_continuous(expand=c(0,0)) + 
			coord_cartesian(ylim=c(-12,20))

multiplot(pet_poli, peese_poli, cols=2)

#p-curve

poli$z <- (poli$Effect_size/poli$SE)
poli$p <- 2*pnorm(-abs(poli$z))
poli_p <- ceiling(poli$p[poli$p<.05]*100)/100

blueline_poli=c() 
ksig= sum(poli_p<.05,na.rm=TRUE)
for (i in c(.01,.02,.03,.04,.05)) blueline_poli=c(blueline_poli, sum(poli_p==i,na.rm=TRUE)/ksig*100)

data_poli <- read.table("~/pcurve_graphdata_poli.txt", sep="\t", header=T) #Make p-curve file using data from above calculation - see attached

pcurve_poli <- ggplot(data_poli, aes(y = Percent, x = Point, colour=Line, linetype=Line)) +
			geom_point() +
			geom_line(size=1) +
			scale_color_manual(values=c("#999999", "#666666", "#BA1825")) +
			scale_linetype_manual(values=c("dotted", "dashed","solid")) +
			theme_bw() +
			xlab("p-value") + 
			coord_cartesian(ylim=c(0,100)) + 
			ggtitle("Policy Change")+ 
				theme(plot.title = element_text(hjust=0.5))+ theme(legend.title=element_blank())

#===================#
#"School age cutoff"#
#===================#

#Remove approximate and average SEs
schl_funn <- subset(schl, subset=(ses_funnel==1))

#Basic meta-analysis
ma3 <- rma.uni(yi=Effect_size, sei=SE, data=schl_funn, method="REML"); summary(ma3)

#Funnel plot
funnel(ma3, yaxis="sei", main="School age cutoff", xlab="Effect size (IQ points)")

#Funnel plot asymmetry
regtest.rma(ma3) # Significant asymmetry in funnel plot

#Funnel plot in ggplot

estimates = 5.229; ses = 0.530 #Estimates from the Mplus model
se.seqs=seq(0, max(na.omit(schl_funn$SE)+.05), .01)
ll95s <- estimates-(1.96*se.seqs)
ul95s <- estimates+(1.96*se.seqs)
dfCIs <- data.frame(ll95s, ul95s, se.seqs, estimates)

funn_schl <- ggplot(aes(x=SE, y=Effect_size), data=schl_funn) +
		scale_x_reverse(expand=c(0.01,0.01)) +
		coord_flip() +
		xlab("Standard Error") + ylab("Effect size (IQ points)") +
		geom_line(aes(x=se.seqs, y=ll95s), linetype="dashed", colour="#666666", data=dfCIs) +
		geom_line(aes(x=se.seqs, y=ul95s), linetype="dashed", colour="#666666", data=dfCIs) +
		geom_segment(aes(y=estimates, yend=estimates, xend=0, x=1.55), colour="#666666", size=.3) +
		geom_point(shape=16, size=3, colour="#367C36")+ 
		theme_bw() +
		ggtitle("School Age Cutoff")+ 
		theme(plot.title = element_text(hjust=0.5))
		
	
#Alternative funnel plot in ggplot - include approx. and average SEs

estimatesX = 5.229; sesX = 0.530 #Estimates from the Mplus model
se.seqsX=seq(0, max(na.omit(schl$SE)+.05), .01)
ll95sX <- estimates-(1.96*se.seqsX)
ul95sX <- estimates+(1.96*se.seqsX)
dfCIsX <- data.frame(ll95sX, ul95sX, se.seqsX, estimatesX)

funn_schlX <- ggplot(aes(x=SE, y=Effect_size), data=schl) +
		scale_x_reverse(expand=c(0.01,0.01)) +
		coord_flip() +
		xlab("Standard Error") + ylab("Effect size (IQ points)") +
		geom_line(aes(x=se.seqsX, y=ll95sX), linetype="dashed", colour="#666666", data=dfCIsX) +
		geom_line(aes(x=se.seqsX, y=ul95sX), linetype="dashed", colour="#666666", data=dfCIsX) +
		geom_segment(aes(y=estimatesX, yend=estimatesX, xend=0, x=3.3), colour="#666666", size=.3) +
		geom_point(shape=16, size=3, colour="#367C36")+ 
		theme_bw() +
		ggtitle("School Age Cutoff")+ 
		theme(plot.title = element_text(hjust=0.5))

#PET-PEESE

schl_funn$var <- (schl_funn$SE)^2
mPET <- lm(Effect_size ~ SE, weights=1/var, data=schl_funn); summary(mPET)
mPEESE <- lm(Effect_size ~ var, weights=1/var, data=schl_funn); summary(mPEESE) #Both estimates significant

#Plot PET and PEESE

pet_schl <- ggplot(schl_funn, aes(y = Effect_size, x = SE, weight=1/var)) + 
			geom_point(colour="#367C36") +
			stat_smooth(formula = y ~ x, method="lm", colour="#367C36", fill="#367C36", fullrange=TRUE) + 
			theme_bw() +
			ggtitle("School Age Cutoff - PET") + 
			theme(plot.title = element_text(hjust=0.5)) + 
			xlab("Standard error") +
			ylab("Effect size") + 
			scale_x_continuous(expand=c(0,0))

peese_schl <- ggplot(schl_funn, aes(y = Effect_size, x = var, weight=1/var)) + 
			geom_point(colour="#367C36") +
			geom_smooth(formula = y ~ x, method="lm", colour="#367C36", fill="#367C36", fullrange=T) + 
			theme_bw() + 
			ggtitle("School Age Cutoff - PEESE") + 
			theme(plot.title = element_text(hjust=0.5))+ 
			xlab("Variance") +
			ylab("Effect size") + 
			scale_x_continuous(expand=c(0,0))

multiplot(pet_schl, peese_schl, cols=2)

#p-curve

schl$z <- (schl$Effect_size/schl$SE)
schl$p <- 2*pnorm(-abs(schl$z))
schl_p <- ceiling(schl$p[schl$p<.05]*100)/100

blueline_schl=c() 
ksig= sum(schl_p<.05,na.rm=TRUE)
for (i in c(.01,.02,.03,.04,.05)) blueline_schl=c(blueline_schl, sum(schl_p==i,na.rm=TRUE)/ksig*100)

data_schl <- read.table("~/pcurve_graphdata_schl.txt", sep="\t", header=T) #Make p-curve file using data from above calculation - see attached

pcurve_schl <- ggplot(data_schl, aes(y = Percent, x = Point, colour=Line, linetype=Line)) +
			geom_point() +
			geom_line(size=1) +
			scale_color_manual(values=c("#999999", "#666666", "#367C36")) +
			scale_linetype_manual(values=c("dotted", "dashed","solid")) +
			theme_bw() +
			xlab("p-value") + 
			coord_cartesian(ylim=c(0,100)) + 
			ggtitle("School Age Cutoff")+ 
				theme(plot.title = element_text(hjust=0.5))+ theme(legend.title=element_blank())

#========================#
#Groups of plots together#
#========================#

#Funnel plots
multiplot(funn_cpiq, funn_poli, funn_schl, cols=3)

#Version with alternative funnel
multiplot(funn_cpiq, funn_poli, funn_schlX, cols=3)

#PET-PEESE plots
multiplot(pet_cpiq, pet_poli, pet_schl, peese_cpiq, peese_poli, peese_schl, cols=2)

#p-curve plots
multiplot(pcurve_cpiq, pcurve_poli, pcurve_schl, cols=3)

#===================================================================#
#Overall funnel plot - including all 3 designs, raw and residualised#
#===================================================================#

#Please see the R script "Overall_funnel_plot.R"

#========================================#
#multiplot function from "Cookbook for R"#
#========================================#

#Everything below is quoted from the following URL: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}