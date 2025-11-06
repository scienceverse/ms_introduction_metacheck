if(!require(xlsx)){install.packages('xlsx')}
if(!require(meta)){install.packages('meta')}

library(xlsx)
library(meta)

# a tweak to point RStudio to the current directory of the R file
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

# load source code for mini-meta
# adjusted from the Lakens workshop by Gilad Feldman
source("minimeta.R")

##########
# mini meta for escalation
##########

# read the csv file
idataset <- read.table("escalation-action-framing-with-stats.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# have a look
str(idataset)
idataset$Study

# Run the main mini-meta
odataset<-ExpMetaR(idataset$Study, idataset$N1, idataset$N2, idataset$M1, idataset$M2, idataset$SD1, idataset$SD2)

# Get the main results
PrintMetaResult(odataset)

# Psychological science wanted the plots exported to PNG/PDF
png(filename="Figure1_top_escalation.png", 
    units="in", 
    width=10, 
    height=4, 
    pointsize=12, 
    res=1000)
PrintPlotMetaR(odataset)
dev.off()
pdf(file="Figure1_top_escalation.pdf", 
    width=10, 
    height=4, 
    pointsize=12)    
PrintPlotMetaR(odataset)
dev.off()

# see some funnels
PrintFunnelMetaR(odataset)

# Export results, just so you have them
write.xlsx(odataset, "escalation-action-framing-meta.xlsx")

##########
# mini meta for action-orientation
##########

# read the csv file
idataset <- read.table("escalation-action-orientation-with-stats.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# have a look
str(idataset)
idataset$Study

# Run the main mini-meta
odataset<-ExpMetaR(idataset$Study, idataset$N1, idataset$N2, idataset$M1, idataset$M2, idataset$SD1, idataset$SD2)

# Get the main results
PrintMetaResult(odataset)

# Psychological science wanted the plots exported to PNG/PDF
png(filename="Figure1_bottom_action-orientation.png", 
    units="in", 
    width=10, 
    height=4, 
    pointsize=12, 
    res=1000)
PrintPlotMetaR(odataset)
dev.off()
pdf(file="Figure1_bottom_action-orientation.pdf", 
    width=10, 
    height=4, 
    pointsize=12)
PrintPlotMetaR(odataset)
dev.off()

# see some funnels
PrintFunnelMetaR(odataset)

# Export results, just so you have them
write.xlsx(odataset, "action-orientation-meta.xlsx")
