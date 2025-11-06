#S7
# load packages----
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(readr)) {install.packages("readr"); require(readr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(plotrix)) {install.packages("plotrix"); require(plotrix)}
if (!require(reshape)) {install.packages("reshape"); require(reshape)}
if (!require(yarrr)) {install_github("ndphillips/yarrr"); require(yarrr)} 
if (!require(VGAM)) {install.packages("VGAM"); require(VGAM)} #vglm for orginal w/ p values
if (!require(ordinal)) {install.packages("ordinal"); require(ordinal)} ## package for ordinal logistic regression
if (!require(effects)) {install.packages("effects"); require(effects)} 
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(car)) {install.packages("car"); require(car)}
if (!require(mosaic)) {install.packages("mosaic"); require(mosaic)}
if (!require(MASS)) {install.packages("MASS"); require(MASS)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)} # for gamma correlation
if (!require(plyr)) {install.packages("plyr"); require(plyr)}  #ddply
if (!require(effsize)) {install.packages("effsize"); require(effsize)}  ## for coheh.d
op <- par()
# load data----
load("S7workspace.RData") # loading the workspace will save a lot of computation time for the simulation
rawData <- read.csv("rawDataAnon.CSV", header = TRUE)
CleanData <- read.csv("CleanDataAnon.csv")

# Demographics----
mean(CleanData$age)
table(CleanData$gender)[[1]]/nrow(CleanData)
table(CleanData$condition)

# Confidence over time----
ctrl <- lmer(Confidence ~ confElicited + (1|QuestionID), data=CleanData[CleanData$condition == "control",])
summary(ctrl)
confint(ctrl)
plot(allEffects(ctrl))

treat <- lmer(Confidence ~ confElicited + (1|QuestionID), data=CleanData[CleanData$condition == "randomConf",])
summary(treat)
confint(treat)
plot(allEffects(treat))

Main1 <- lmer(Confidence ~ confElicited*condition + (1|QuestionID), data=CleanData)
summary(Main1)
confint(Main1)
plot(allEffects(Main1))

# question 1 difference anomoly
t.test(Q1conf ~ condition, data = subset(rawData, confElicited == 1)) # difference is significant when restricting the subset to the control group that was randomly selected for Q1
t.test(Q1conf ~ condition, data = rawData) # difference is marginally significant including all Q1 elicitations in control condition.  p = .1
with(subset(rawData, confElicited == 1), table(condition))

# bootstrap analysis-----
# to ensure the above result is not particular to the set of confidences assigned in control condition, we generate 10,000 random samples from the control condition and repeat the analysis
n_sim <- 10000 # set number of simulations you want to run
myModels <- list()
set.seed(435)
for (j in 1:n_sim) {  # loop for each sample in the simulation untill you have all the simulated samples at the desired size
    bootData <- rawData # each new sample starts with the original sample
    for (i in 1:nrow(bootData)){ # for each individual in the original sample
      if(bootData$condition[i] == "control") bootData$confElicited[i] <- sample(1:5,1) # assign a new randomly selected confidence question to all participants in the control condition
      # Create new variables "Confidence" and "QuestionID" that pull the confidence based on conElicited and label with the stimulous
      for (k in 1:5) { # for each possible confElicited
        if(grepl(pattern=k, x=bootData$confElicited[i])) { # if k is the Conf Elicited...
            bootData$Confidence[i] <- bootData[i, paste0("Q",k,"conf")] # Confidence is equal to the Qkconf
            bootData$QuestionID[i] <- bootData[i, paste0("Q",k)] # QuestionId is equal to Qk (ex: A1, A5, A8, etc.)
          } # close loop for creating new columns based on one possible conf elicited
        } # close loop for 5 possible conf elicited
      } # close loop for individuals
    # now that bootData consists of a new random assignment of conf elicitations, proceed with analysis
    bootData$QuestionID <- factor(bootData$QuestionID, levels = c(1:8), labels = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"))
    myModels[[j]] <- summary(lmer(Confidence ~ confElicited*condition + (1|QuestionID), data=bootData))
print(j)    
} # close loop for one simulation
myModels[[1]]

pVal_main <- c()
for (i in 1:n_sim) {
  pVal_main[i] <- myModels[[i]]$coefficients[4, 5]
}
hist(pVal_main)
mean(pVal_main<.05) # 23% of p values < .05

#pull interaction betas
b <- c()
for (i in 1:n_sim) {
  b[i] <- myModels[[i]]$coefficients[4, 1]
}
hist(b)
mean(b>0) #98% of estimates are positive

SE <- c()
for (i in 1:n_sim) {
  SE[i] <- myModels[[i]]$coefficients[4, 2]
}
hist(SE)

# compute CIs
CIs <- matrix(nrow = n_sim, ncol = 2)
for (i in 1:n_sim) {
  CIs[i, 1] <- b[i]-1.96*SE[i]
  CIs[i, 2] <- b[i]+1.96*SE[i]
}
CIsort <- CIs[order(CIs[, 1]), ]

apply(CIs, 1, function(x) x[1] < 0 & x[2] > 0) # TRUE if the CI contains zero
sum(apply(CIs, 1, function(x) x[1] < 0 & x[2] > 0))/n_sim # 76% of the CI's contain zero. 
b_bootstrap <- mean(b) #1.09

plot(c(-1, 1), xlim = c(-2, 4), ylim = c(0, n_sim), col = "transparent", xlab = "CI", ylab = "",
     main = "95-percent CIs")
for (i in 1:n_sim) {
  lines(x = c(CIsort[i, 1], CIsort[i, 2]), y = c(i, i), col = "lightgray")
}
abline(v = 0, col = "red")
abline(v = b_bootstrap, col = "blue")

# stats for the decline of confidence in the control condition in the above model
pVal_ctrl <- c()
for (i in 1:n_sim) {
  pVal_ctrl[i] <- myModels[[i]]$coefficients[2, 5]
}
hist(pVal_ctrl)
mean(pVal_ctrl<.05) # 79% of p values < .05

b_ctrl <- c()
for (i in 1:n_sim) {
  b_ctrl[i] <- myModels[[i]]$coefficients[2, 1]
}
hist(b_ctrl)
mean(b_ctrl) #-1.45
   
#figure----
CleanData$condition0 <- factor(CleanData$condition, labels = c("Control", "Single Confidence"))
ggplot(CleanData, aes(x=confElicited, y = Confidence)) +  facet_grid(~condition0) +
  stat_summary(fun.y = mean, geom = "point", alpha = 4/5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2, alpha = 4/5, position = position_dodge(.5)) + 
  labs(x = "\nQuestion Order", y = "Confidence (%)") +
  geom_smooth(method=lm) +
  theme_bw() + theme(axis.title.y = element_text(angle=0, vjust = .5), text = element_text(size = 16, family = "Times New Roman"),
                     strip.background = element_rect(fill="white")) +
  ggsave("SingleConf.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("SingleConf.jpeg", width=8, height=5, device="jpeg")
