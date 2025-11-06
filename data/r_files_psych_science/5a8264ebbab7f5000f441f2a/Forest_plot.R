#========================================#
# Education-IQ meta-analysis forest plots#
#========================================#

#By Stuart Ritchie, October 2017

#Forest plot ggplot2 code below is adapted from code written by John Sakaluk (2016): https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/

#============#
#Read in data#
#============#
#(Study design type: 1 = Control Prior Intelligence (cpiq); 2 = Policy Change (poli); 3 = School Age Cutoff (schl))

cpiq <- read.table("~/forest_data_cpiq.txt", header=T, sep="\t")
poli <- read.table("~/forest_data_poli.txt", header=T, sep="\t")
schl <- read.table("~/forest_data_schl.txt", header=T, sep="\t")

#Note that each dataset includes the overall estimate from the meta-analysis models run in Mplus

library(ggplot2)

#Get the "multiplot" function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#It's copied below for ease of reference

#=========================================#
#Get confidence intervals from mean and SE#
#=========================================#

#Lower limit
cpiq$ll <- ((cpiq$Effect_size) - (cpiq$SE*1.96))
poli$ll <- ((poli$Effect_size) - (poli$SE*1.96))
schl$ll <- ((schl$Effect_size) - (schl$SE*1.96))

#Upper limit
cpiq$ul <- ((cpiq$Effect_size) + (cpiq$SE*1.96))
poli$ul <- ((poli$Effect_size) + (poli$SE*1.96))
schl$ul <- ((schl$Effect_size) + (schl$SE*1.96))

#====================#
#Reorder the datasets#
#====================#
#In effect size order, but with the overall estimate (mean) at the bottom

cpiq$Name <- reorder(cpiq$Name, cpiq$Effect_size)
levels(cpiq$Name)
cpiq$Name <- factor(cpiq$Name, levels = as.matrix(levels(cpiq$Name))[c(16,1:15,17:27)])

poli$Name <- reorder(poli$Name, poli$Effect_size)
levels(poli$Name)
poli$Name <- factor(poli$Name, levels = as.matrix(levels(poli$Name))[c(13,1:12,14:31)])

schl$Name <- reorder(schl$Name, schl$Effect_size)
levels(schl$Name)
schl$Name <- factor(schl$Name, levels = as.matrix(levels(schl$Name))[c(56,1:55,57:87)])

#=================#
#Make forest plots#
#=================#

#Control Prior Intelligence

g1 <- ggplot(cpiq, aes(y=Name, x=Effect_size, xmin=ll, xmax=ul, shape=Design)) +
	geom_vline(xintercept=0, linetype="dashed", colour="grey") +
	geom_point(colour="#00348E", shape=20) +
	geom_point(data=subset(cpiq, subset=(Design=="Mean")), shape=18, size=4, colour="#00348E") +
	geom_errorbarh(height=.25, colour="#00348E") +
	theme_bw() +
	ylab("Study") + 
 	xlab("Effect size (IQ points for 1 additional year)") + 
	theme(legend.position="none") + 
	ggtitle("Control Prior Intelligence") + 
	theme(plot.title = element_text(hjust=.5))

#Policy Change
	
g2 <- ggplot(poli, aes(y=Name, x=Effect_size, xmin=ll, xmax=ul, shape=Design)) +
	geom_vline(xintercept=0, linetype="dashed", colour="grey") +
	geom_point(colour="#BA1825", shape=20) +
	geom_point(data=subset(poli, subset=(Design=="Mean")), shape=18, size=4, colour="#BA1825") +
	geom_errorbarh(height=.25, colour="#BA1825") +
	theme_bw() +
	ylab("Study") + 
 	xlab("Effect size (IQ points for 1 additional year)") + 
	theme(legend.position="none") + 
	ggtitle("Policy Change") + 
	theme(plot.title = element_text(hjust=.5))	

#School Age Cutoff

g3 <- ggplot(schl, aes(y=Name, x=Effect_size, xmin=ll, xmax=ul, shape=Design)) +
	geom_vline(xintercept=0, linetype="dashed", colour="grey") +
	geom_point(colour="#367C36", shape=20) +
	geom_point(data=subset(schl, subset=(Design=="Mean")), shape=18, size=4, colour="#367C36") +
	geom_errorbarh(height=.25, colour="#367C36") +
	theme_bw() +
	ylab("Study") + 
 	xlab("Effect size (IQ points for 1 additional year)") + 
	theme(legend.position="none") + 
	ggtitle("School Age Cutoff") + 
	theme(plot.title = element_text(hjust=.5))

#All plots together

multiplot(g1,g2,g3, cols=3)

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




