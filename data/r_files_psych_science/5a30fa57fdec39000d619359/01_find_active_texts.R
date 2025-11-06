# These analyses followed the first coding step
# TL;DR:
# Read the three coding files
# Computer inter-rater agreement
# Select active strategy texts for second coding step


#######################################
# Read the three coding files
#######################################


# Read the three data sets
# M, S & J are the three raters
code1 <- read.csv("coding_1_M.csv",
                  header = TRUE,
                  sep="\t",
                  stringsAsFactors = FALSE)
code2 <- read.csv("coding_1_S.csv",
                  header = TRUE,
                  sep="\t",
                  stringsAsFactors = FALSE)
code3 <- read.csv("coding_1_J.csv",
                  header = TRUE,
                  sep="\t",
                  stringsAsFactors = FALSE)

##############################
# Merge into one dataframe
##############################
colnames(code1) <- paste(colnames(code1), "_1", sep="")
colnames(code2) <- paste(colnames(code2), "_2", sep="")
colnames(code3) <- paste(colnames(code3), "_3", sep="")

comb <- cbind(code1, code2, code3)

##############################
# Check agreement 
##############################

###################
# Text contains idea y/n
###################
comb$idea_d1 <- 0
comb$idea_d2 <- 0
comb$idea_d3 <- 0
comb$idea_d1[comb$ideaCount_1 > 0] <- 1
comb$idea_d2[comb$ideaCount_2 > 0] <- 1
comb$idea_d3[comb$ideaCount_3 > 0] <- 1


library(irr)
kappam.fleiss(comb[,c("idea_d1", "idea_d2", "idea_d3")])
# Kappa = .940

# generate binary indicator
# if 0 or  1 raters rated idea -> 0
# if 2 or 3 raters rated idea -> 1
comb$idea <- apply(comb[,c("ideaCount_1", "ideaCount_2", "ideaCount_3")],
                   1,
                   function(x) sum(x > 0) > 1)

###################
# Text contains active strat y/n
###################

# Has any active strategy
comb$anyactive_1 <- apply(code1[,c("active1_1", "active2_1", "active3_1", "active4_1", "active5_1")], 1, function(x) any(x == 1))
comb$anyactive_2 <- apply(code2[,c("active1_2", "active2_2", "active3_2", "active4_2", "active5_2")], 1, function(x) any(x == 1))
comb$anyactive_3 <- apply(code3[,c("active1_3", "active2_3", "active3_3", "active4_3", "active5_3")], 1, function(x) any(x == 1))
comb$anyactive_1 <- as.numeric(comb$anyactive_1)
comb$anyactive_2 <- as.numeric(comb$anyactive_2)
comb$anyactive_3 <- as.numeric(comb$anyactive_3)


kappam.fleiss(comb[,c("anyactive_1", "anyactive_2", "anyactive_3")])
# kappa .792

# generate binary indicator
# if 0 or 1 raters rated active -> 0
# if 2 or 3 raters rated active -> 1
comb$active_vote <- rowSums(comb[,c("anyactive_1","anyactive_2","anyactive_3")]) > 1

###################
# Select active texts
###################

# Number of active versus non-active replies
table(comb$active_vote)


# de-indentified only indicates that one text has been slightly modified
# because one reply also contained the full names of the respondents
# these names have been removed without changing the rest of the reply

texts <- read.csv("texts_de-identified.tsv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)

# Frame only containing the active texts
texts_select <- texts[comb$active_vote,]

# Write in tsv
# This file was then sent out for the second coding step
write.table(texts_select, file = "active_texts.tsv", 
            row.names = FALSE, 
            col.names = FALSE,
            sep = "\t")


