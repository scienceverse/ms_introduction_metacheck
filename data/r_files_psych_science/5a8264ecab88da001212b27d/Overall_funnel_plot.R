#========================================================#
# Education-IQ meta-analysis "all estimates" funnel plots#
#========================================================#

#By Elliot Tucker-Drob and Stuart Ritchie, October-November 2017
	#Updated February 2018

#Funnel plot ggplot2 code below is adapted from code written by John Sakaluk (2016): https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/

#=============================#
#Read in data and get packages#
#=============================#

df <- read.table("~/meta_dataset.txt", sep="\t", header=T)
names(df); dim(df); str(df)

library(ggplot2)

#Get the "multiplot" function from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#It's copied below for ease of reference

#=============================================#
#Clean data and create necessary new variables#
#=============================================#

#Sort out which variables are factors or numeric

df$Study_ID <- as.factor(df$Study_ID)
df$Data_ID <- as.factor(df$Data_ID)
df$k <- as.factor(df$k)
df$ses_funnel <- as.factor(df$ses_funnel)
df$published <- as.factor(df$published)

str(df)

#Contrast variables for moderator analysis 
	#(com = composite; flu = fluid; cry = crystallized)

df$com_vs_flu <- df$outcome_test_cat
df$com_vs_flu[as.numeric(df$g_vs_flu) %in% 2] <- 0

df$com_vs_cry <- df$outcome_test_cat
df$com_vs_cry[as.numeric(df$com_vs_cry) %in% 1] <- 0
df$com_vs_cry[as.numeric(df$com_vs_cry) %in% 2] <- 1

df$flu_vs_cry <- df$outcome_test_cat
df$flu_vs_cry[as.numeric(df$flu_vs_cry) %in% 0] <- NA
df$flu_vs_cry <- df$flu_vs_cry-1

df$com_vs_flucry <- df$outcome_test_cat
df$com_vs_flucry[as.numeric(df$com_vs_flucry) > 1] <- 1
df$com_vs_flucry <- df$com_vs_flucry-1

#===================================================================#
#Overall funnel plot - including all 3 designs, raw and residualised#
#===================================================================#

#Remove approximate and average SEs
df_funn <- subset(df, subset=(ses_funnel==1))

#Raw estimate funnel plot

estimateA = 3.428; seA = 0.739 #Estimates from the Mplus model
se.seqA=seq(0, max(na.omit(df_funn$SE+.5)), .01)
ll95A <- estimateA-(1.96*se.seqA)
ul95A <- estimateA+(1.96*se.seqA)
dfCIA <- data.frame(ll95A, ul95A, se.seqA, estimateA)

funnel_all_raw <- ggplot(aes(x=SE, y=Effect_size, colour = Design), data=df_funn) +
		scale_x_reverse(expand=c(0.01,0.01)) +
		coord_flip() +
		xlab("Standard Error") + ylab("Effect size (IQ points)") +
		geom_line(aes(x=se.seqA, y=ll95A), linetype="dashed", colour="#666666", data=dfCIA) +
		geom_line(aes(x=se.seqA, y=ul95A), linetype="dashed", colour="#666666", data=dfCIA) +
		geom_segment(aes(y=estimateA, yend=estimateA, xend=0, x=5.35), colour="#666666", size=.3) +
		geom_point(shape=16, size=3) + 
		scale_colour_manual(values=c("#00348E", "#BA1825", "#367C36")) +
		theme_bw() +
		ggtitle("Raw effect sizes")+ 
		theme(plot.title = element_text(hjust=0.5)) 

#Residualised funnel plot

#Create residual effect size using estimates from the Mplus model (shown in Table 2 of the paper)
df$Res[df$Design=="Control Prior IQ"]=df$Effect_size[df$Design=="Control Prior IQ"]-(2.079-0.011*df$cpiq_early[df$Design=="Control Prior IQ"]-0.020*df$Outcome_age[df$Design=="Control Prior IQ"]-0.689*df$com_vs_flucry[df$Design=="Control Prior IQ"]+0.874*df$Male_only[df$Design=="Control Prior IQ"])

df$Res[df$Design=="Policy Change"]=df$Effect_size[df$Design=="Policy Change"]-(-1.068+.137*df$quasi_age[df$Design=="Policy Change"]+.023*df$Outcome_age[df$Design=="Policy Change"]-1.479*df$com_vs_flucry[df$Design=="Policy Change"]-0.644*df$Male_only[df$Design=="Policy Change"])

df$Res[df$Design=="School Age Cutoff"]=df$Effect_size[df$Design=="School Age Cutoff"]-(2.603+.111*df$Outcome_age[df$Design=="School Age Cutoff"]+2.471*df$Achievement[df$Design=="School Age Cutoff"])

df_funn <- subset(df, subset=(ses_funnel==1))
df_funn=df_funn[!is.na(df_funn$Res),]

#Make the plot in ggplot

estimateAr = 0; seAr = 0.739 #Estimates from the Mplus model
se.seqAr=seq(0, max(na.omit(df_funn$SE+.5)), .01)
ll95Ar <- estimateAr-(1.96*se.seqA)
ul95Ar <- estimateAr+(1.96*se.seqA)
dfCIA <- data.frame(ll95Ar, ul95Ar, se.seqAr, estimateAr)

funnel_all_resid <- ggplot(aes(x=SE, y=Res, colour = Design), data=df_funn) +
  scale_x_reverse(expand=c(0.01,0.01)) +
  coord_flip() +
  xlab("Standard Error") + ylab("Residualised effect size") +
  geom_line(aes(x=se.seqAr, y=ll95Ar), linetype="dashed", colour="#666666", data=dfCIAr) +
  geom_line(aes(x=se.seqAr, y=ul95Ar), linetype="dashed", colour="#666666", data=dfCIAr) +
  geom_segment(aes(y=estimateAr, yend=estimateAr, xend=0, x=5.35), colour="#666666", size=.3) +
  geom_point(shape=16, size=3) + 
  scale_colour_manual(values=c("#00348E", "#BA1825", "#367C36")) +
  theme_bw() +
  ggtitle("Residualised effect sizes")+ 
  theme(plot.title = element_text(hjust=0.5)) 

multiplot(funnel_all_raw,funnel_all_resid,cols=2)

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



