#====================================================#
# Education-IQ meta-analysis graphs of effects by age#
#====================================================#

#By Stuart Ritchie, October 2017

#==========================#
#Data and packages required#
#==========================#

df <- read.table("~/meta_dataset.txt", sep="\t", header=T)
names(df); dim(df); str(df)

library(ggplot2)

#Get the "multiplot" function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ - copied below

#======================#
#Split into two subsets#
#======================#
#Note that the "School Age Cutoff" design isn't relevant here because the age at outcome test doesn't really vary

cpiq <- subset(df, subset=(Design=="Control Prior IQ"))
poli <- subset(df, subset=(Design=="Policy Change"))

#Control Prior IQ

cpiq_age <- ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=cpiq) +
		geom_point(alpha=.55, colour="#00348E") +
		geom_hline(yintercept=0, linetype="dotted") + 
		theme_bw() + 
		scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
		scale_y_continuous(breaks=c(0,1,2,3),labels=c(" 0.0"," 1.0"," 2.0"," 3.0")) + 
		xlab("Age at outcome test (years)") +
		ylab("Gain for 1 year of education\n(IQ points)") +
		guides(size=F) +
		geom_smooth(method="lm", colour="#00348E",fill="#00348E",size=.5, alpha=.25) + ggtitle("Control Prior Intelligence") + 
		theme(plot.title = element_text(hjust=0.5)) +
		coord_cartesian(xlim=c(15,85))

#Policy Change

poli_age <- ggplot(aes(x=Outcome_age, y=Effect_size, size=1/(SE^2)), data=poli) +
		geom_point(alpha=.55, colour="#BA1825") +
		geom_hline(yintercept=0, linetype="dotted") + 
		theme_bw() + 
		scale_x_continuous(breaks=c(20,30,40,50,60,70,80)) +
		xlab("Age at outcome test (years)") +
		ylab("Gain for 1 year of education\n(IQ points)") +
		guides(size=F) +
		geom_smooth(method="lm", colour="#BA1825",fill="#BA1825",size=.5, alpha=.25) + ggtitle("Policy Change")+ 
		theme(plot.title = element_text(hjust=0.5))+
		coord_cartesian(xlim=c(15,85))

#Two graphs together
multiplot(cpiq_age,poli_age, cols=1)

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
