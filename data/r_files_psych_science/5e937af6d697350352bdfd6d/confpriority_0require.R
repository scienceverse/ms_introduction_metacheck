####SET WORKING DIRECTORY###
#Before running the rest of the script, it is necessary to set the working directory to the folder where the files are. To do that, go to Session > Set Working Directory > To Source File Location

####LOAD R PACKAGES####
require(Rmisc)
require(MASS)
require(Rcpp)
require(Hmisc)
require(signal)
require(scales)
require(gtools)
require(signal)
require(Bolstad)
library(nortest)
library(reshape)
library(reshape2)
library(MethComp)
require(lme4)
require(lmerTest)
require(cowplot)
require(multcomp)
require(RColorBrewer)
require(BayesFactor)
require(tidyverse)

####LOAD CUSTOM R FUNCTIONS####
source('functions.R')

####DEFINE GRAPHICAL PARAMETERS FOR PLOTS####
axeswidth <- 0.5
genplotwidth <- 1
sizefont <- 12
sizeannot <- 4
smoothvalue <- 1/2

theme_set (theme_classic(12) + theme(axis.line.x = element_line(colour = 'black', size=axeswidth, linetype='solid'),
                                     axis.line.y = element_line(colour = 'black', size=axeswidth, linetype='solid'),
                                     axis.ticks = element_line(color="black", size=axeswidth),
                                     axis.text.x = element_text(color="black"),
                                     axis.text.y = element_text(color="black")))

hue_color <- "#FADA5E"
hue_letter <- "#78b065"

paltask <- scale_color_manual(name = "task",values = c(hue_color,hue_letter))
palfilltask <- scale_fill_manual(name = "task",values = c(hue_color,hue_letter))

limexperf <- scale_x_continuous(breaks=c(60,90),labels=c("60% (H)","90% (E)"))
limobsperf <- scale_y_continuous(breaks=seq(.4,1,len=7),labels=seq(.4,1,len=7),limits=c(.4,1))

exp_perf_xlab <- xlab("Expected performance")
obs_perf_ylab <- ylab("Observed performance (proportion)")

palshapeperf <- scale_shape_manual(name = "exp perf",values = c(16,17))

palposition <- scale_color_manual(values = c("#b5c992","#8d9c61","#7e874e","#5d7650"),guide=FALSE)
palfillposition <- scale_fill_manual(values = c("#b5c992","#8d9c61","#7e874e","#5d7650"),guide=FALSE)

palwitbet <- scale_color_manual(values = c("#32CD32","#4B0082"),guide=FALSE)
palfillwitbet <- scale_fill_manual(values = c("#32CD32","#4B0082"),guide=FALSE)

conf_xlab <- xlab("Confidence (%)")
conf_ylab <- ylab("Average confidence (%)")

palcorrect <- scale_color_manual(name = "correctness",values = c("red","blue"),labels = c("incorrect", "correct"))
palfillcorrect <- scale_fill_manual(name = "correctness",values = c("red","blue"),labels = c("incorrect", "correct"))

limblockfeed <- scale_x_continuous(breaks=seq(4,36,len=9),labels=seq(4,36,len=9))

block_xlab <- xlab("Block #")
perc_perf_ylab <- ylab("Observed performance (%)")

cleanxaxis <- theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

limyfullprop <- scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,1,len=5),limits=c(0,1))
limxfullprop <- scale_x_continuous(breaks=seq(0,1,len=5),labels=seq(0,1,len=5),limits=c(0,1))

linechance <-  geom_hline(yintercept=0.5,linetype="dashed")

palfillbefaf <- scale_fill_manual(values = c("#de2d26","#fc9272"),guide=FALSE)

palfillcheasy <- scale_fill_manual(name = expression(paste("Task first \n responded to")),values = c("#de2d26","#fc9272"),labels = c("Hard", "Easy"))

paldelays <- scale_color_manual(name = "Delay (s)",values = c("#FCE61C","#1D908B","#431953"))

palfillordchoice <- scale_fill_manual(name = "Choice order",values = c("#fc9272","#de2d26"),labels = c("First", "Second"))





palsamedifs <- scale_color_manual(name="trial type",values = c("#fc9272","#de2d26"))

hue_EE <-"#dc6b2f"
hue_EH <-"#779bc0"
hue_HE <-"#36573b"
hue_HH <-"#ac6e6e"


hue_EE <-"#E1CFCE"
hue_EH <-"#C09EB6"
hue_HE <-"#D3B68D"
hue_HH <-"#B38576"


hue_EE <-"#f2e7bf"
hue_EH <-"#d7b63e"
hue_HE <-"#cfb6dc"
hue_HH <-"#8e54ad"

hue_blue <- "#3c3cb4"
hue_orange <- "#ff8000"


palfillpapri_perf <- scale_fill_manual(name="task", labels= c("color","letter"), values = c("#e3cc77","#ab80c2")) #values = c("#aae69f","#ab80c2"))
papri_obs_perf_ylab <- ylab("Observed performance (%)")

####LOAD EXPERIMENTS' DATA####
load("exp1_raw.Rda")
load("exprep1_raw.Rda")
load("exp2_raw.Rda")
load("exp3_raw.Rda")
load("exp4_raw.Rda")
load("exp4_tcraw.Rda") #short version with info for each block
load("exp5_raw.Rda")
load("expsupp_raw.Rda")
