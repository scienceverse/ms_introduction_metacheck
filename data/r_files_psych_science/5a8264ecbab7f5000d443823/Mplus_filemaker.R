#============================================#
# Education-IQ meta-analysis Mplus file maker#
#============================================#

#By Stuart Ritchie, October 2017
	#Updated February 2018

#============#
#Read in data#
#============#

df <- read.table("~/meta_dataset.txt", sep="\t", header=T)
names(df); dim(df); str(df)

#Sort out which variables are factors or numeric

df$Study_ID <- as.factor(df$Study_ID)
df$Data_ID <- as.factor(df$Data_ID)
df$k <- as.factor(df$k)
df$ses_funnel <- as.factor(df$ses_funnel)
df$published <- as.factor(df$published)

df$Design <- as.numeric(df$Design)

str(df)

#Contrast variables for moderator analysis
	#(com = composite; flu = fluid; cry = crystallized)

df$com_vs_flu <- df$outcome_test_cat
df$com_vs_flu[as.numeric(df$com_vs_flu) %in% 2] <- 0

df$com_vs_cry <- df$outcome_test_cat
df$com_vs_cry[as.numeric(df$com_vs_cry) %in% 1] <- 0
df$com_vs_cry[as.numeric(df$com_vs_cry) %in% 2] <- 1

df$flu_vs_cry <- df$outcome_test_cat
df$flu_vs_cry[as.numeric(df$flu_vs_cry) %in% 0] <- NA
df$flu_vs_cry <- df$flu_vs_cry-1

df$com_vs_flucry <- df$outcome_test_cat
df$com_vs_flucry[as.numeric(df$com_vs_flucry) > 1] <- 1
df$com_vs_flucry <- df$com_vs_flucry-1

df$cpiq <- df$Design
df$cpiq[as.numeric(df$cpiq) %in% 2] <- 0
df$cpiq[as.numeric(df$cpiq) %in% 3] <- 0

df$poli <- df$Design
df$poli[as.numeric(df$poli) %in% 1] <- 0
df$poli[as.numeric(df$poli) %in% 3] <- 0
df$poli[as.numeric(df$poli) %in% 2] <- 1

df$schl <- df$Design
df$schl[as.numeric(df$schl) %in% 1] <- 0
df$schl[as.numeric(df$schl) %in% 2] <- 0
df$schl[as.numeric(df$schl) %in% 3] <- 1

#Write the file to the locations where it's required

write.table(df, "~/metadata_mplus.dat", sep="\t", na="-9999", row.names=F, col.names=F)
