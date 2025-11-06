## Analysis Script for: "Mental imagery induces cross-modal sensory plasticity and changes future auditory perceptioon" 

#Experiment 2 EyeTracking Data Analysis

rm(list = setdiff(ls(), lsf.str()))

se <- function(x) sqrt(var(x)/length(x))

require("ggplot2")
require("plyr")
library(ggplot2)

mainDir<-"~/Exp4/"

setwd(mainDir)
working_dir<-"~/Exp4/EyeTrackingData"
setwd(working_dir)
library(data.table)
require("plyr")


s001<-read.table("~/Exp4/EyeTrackingData/s001_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s002<-read.table("~/Exp4/EyeTrackingData/s002_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s003<-read.table("~/Exp4/EyeTrackingData/s003_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s004<-read.table("~/Exp4/EyeTrackingData/s004_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s005<-read.table("~/Exp4/EyeTrackingData/s005_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s006<-read.table("~/Exp4/EyeTrackingData/s006_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s007<-read.table("~/Exp4/EyeTrackingData/s007_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s008<-read.table("~/Exp4/EyeTrackingData/s008_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s009<-read.table("~/Exp4/EyeTrackingData/s009_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s010<-read.table("~/Exp4/EyeTrackingData/s010_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s011<-read.table("~/Exp4/EyeTrackingData/s011_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s012<-read.table("~/Exp4/EyeTrackingData/s012_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s013<-read.table("~/Exp4/EyeTrackingData/s013_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s014<-read.table("~/Exp4/EyeTrackingData/s014_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s015<-read.table("~/Exp4/EyeTrackingData/s015_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s016<-read.table("~/Exp4/EyeTrackingData/s016_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s017<-read.table("~/Exp4/EyeTrackingData/s017_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s018<-read.table("~/Exp4/EyeTrackingData/s018_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s019<-read.table("~/Exp4/EyeTrackingData/s019_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s020<-read.table("~/Exp4/EyeTrackingData/s020_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s021<-read.table("~/Exp4/EyeTrackingData/s021_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s022<-read.table("~/Exp4/EyeTrackingData/s022_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s023<-read.table("~/Exp4/EyeTrackingData/s023_combined_eye_data.txt", header=TRUE,sep="", dec='.')
s024<-read.table("~/Exp4/EyeTrackingData/s024_combined_eye_data.txt", header=TRUE,sep="", dec='.')



eye_tracking_data<-ldply(c("s001", "s002", "s003", "s004", "s005", "s006", 
                           "s007", "s008", "s009", "s010", "s011", "s012", 
                           "s013", "s014", "s015", "s016", "s017", "s018", 
                           "s019", "s020", "s021", "s022", "s023", "s024"), 
                         function(dn) data.frame(Filename=dn, get(dn))) #this bit creates a dataframe that can be used to make calculations, plots, etc. from the .txt file created when running the experiment



class(eye_tracking_data$X_Deviation) 
range(eye_tracking_data$X_Deviation)
mean(eye_tracking_data$X_Deviation)

###make seperate files for the plot so that we can plat them later 
right_adapt_plot_data<-eye_tracking_data
right_adapt_plot_data$Condition[
        right_adapt_plot_data$Condition=="adpt_left"
        ]<-NA
right_adapt_plot_data<-right_adapt_plot_data[complete.cases(right_adapt_plot_data),]

left_adapt_plot_data<-eye_tracking_data
left_adapt_plot_data$Condition[
        left_adapt_plot_data$Condition=="adpt_right"#this has the legacy in which sound to riight (left adapt) is called adpt_right
        ]<-NA
left_adapt_plot_data<-left_adapt_plot_data[complete.cases(left_adapt_plot_data),]


same_adapt_plot_data<-eye_tracking_data
same_adapt_plot_data$Condition[
        same_adapt_plot_data$Condition=="adpt_center"
        ]<-NA
same_adapt_plot_data<-same_adapt_plot_data[complete.cases(same_adapt_plot_data),]


library("ggplot2")


#Make 3 seperate heatmpas--one for each condition
p1<-ggplot(same_adapt_plot_data, aes(x = X_Deviation, y = Y_Deviation)) + 
        ylim(-1, 1) +
        xlim(-1, 1) +
        stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
        geom_hline(aes(yintercept=0), colour="grey", linetype="dashed", size=.5) +
        geom_vline(aes(xintercept=0), colour="grey", linetype="dashed", size=.5) +
        scale_fill_gradient2(low = "white", high = "#666666") +
        ggtitle("Same Location Adaptaion Fixation Deviation")#grey
p2<-ggplot(left_adapt_plot_data, aes(x = X_Deviation, y = Y_Deviation)) +
        ylim(-1, 1) +
        xlim(-1, 1) +
        stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
        geom_hline(aes(yintercept=0), colour="grey", linetype="dashed", size=.5) +
        geom_vline(aes(xintercept=0), colour="grey", linetype="dashed", size=.5) +
        scale_fill_gradient2(low = "white", high = "#ff6666") + 
        ggtitle("Leftward Adaptaion Fixation Deviation")#red
p3<-ggplot(right_adapt_plot_data, aes(x = X_Deviation, y = Y_Deviation)) + 
        ylim(-1, 1) +
        xlim(-1, 1) +
        stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
        geom_hline(aes(yintercept=0), colour="grey", linetype="dashed", size=.5) +
        geom_vline(aes(xintercept=0), colour="grey", linetype="dashed", size=.5) +
        scale_fill_gradient2(low = "white", high = "#56B4E9") +
        ggtitle("Rightward Adaptaion Fixation Deviation")#blue


#make a function for plotting all these on one row.
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


#plot heatmaps
multiplot(p1, p2, p3, cols=3)

#analyze whether there were significnat difference in X_position accross conditions accross subs
Mean_X_deviation_df<-aggregate(eye_tracking_data$X_Deviation ~ eye_tracking_data$SubNum + eye_tracking_data$Condition, FUN=mean)
names(Mean_X_deviation_df)<-c("SubNum", "Condition", "X_Deviation")
Mean_X_deviation_df$X_Deviation<-round(Mean_X_deviation_df$X_Deviation, 2)
#anova 
anov_X<-aov(X_Deviation~Condition + Error(SubNum/Condition), data=Mean_X_deviation_df)
summary(anov_X)
library("ez")
aov_X2<-ezANOVA(data=Mean_X_deviation_df, dv=X_Deviation, wid=SubNum, within=.(Condition)) 
aov_X2

Mean_X_deviation_df_SD<-aggregate(eye_tracking_data$X_Deviation ~ eye_tracking_data$SubNum + eye_tracking_data$Condition, FUN=sd)

#same analysis for Y
Mean_Y_deviation_df<-aggregate(eye_tracking_data$Y_Deviation ~ eye_tracking_data$SubNum + eye_tracking_data$Condition, FUN=mean)
names(Mean_Y_deviation_df)<-c("SubNum", "Condition", "Y_Deviation")
Mean_Y_deviation_df$Y_Deviation<-round(Mean_Y_deviation_df$Y_Deviation, 2)
mean(Mean_Y_deviation_df$Y_Deviation)

aov_Y2<-ezANOVA(data=Mean_Y_deviation_df, dv=Y_Deviation, wid=SubNum, within=.(Condition)) 
aov_Y2

