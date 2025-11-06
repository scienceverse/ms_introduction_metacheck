#This file contains analysis scripts for the multinomial processing tree
#Models reported in Greene & Naveh-Benjamin "A Specificity Principle of
#Memory: Evidence from Aging and Associative Memory"


#Need package TreeBUGS; also load the following:

library("runjags")
library("TreeBUGS") #See Heck, Arnold, & Arnold (2018) for details
library("ggplot2") #Excellent plotting options
library("ggpubr") #Functions to concatenate plots into a single object (see forest plots)

#The EQN files store the equations of the MPT models
#These files are available in the folder EQN Files; set to same working directory

#As mentioned in the main text, we first specified the model as outlined
#In Stahl & Klauer (2008), which included a single a parameter for guessing
#"intact/old" both when gist is retrieved and when it is not retrieved
#This model is specified with the ftt.eqn script

readEQN("ftt.eqn") #Simplified Conjoint Recognition (Stahl & Klauer, 2008)

#Posterior predictive checks of that model were poor (see below)
#We adjusted the model to include an additional ab parameter for guessing
#"intact" when the gist is not retrieved (in state b), opposed to guessing
#"intact" when the gist is retrieved (modeled with parameter a)

readEQN("exp1MPT.eqn") #Model equations for MPT in Experiment 1
readEQN("exp2MPT.eqn") #Model equations for MPT in Experiment 2

#The Experiment 2 EQN file extends the model from Experiment 1
#To include an additional fuzzy retrieval parameter

###EXPERIMENT 1 

#Young Adults
ya1 <- read.csv("yaexperiment1.csv") #Young Adults

testHetChi(freq = "yaexperiment1.csv",
           tree = c("Intact", "Intact", "Intact", "Related", "Relate", "Relate",
                    "Unrela", "Unrela", "Unrela")) 
#Chi-square test of heterogeneity (e.g., Smith & Batchelder, 2008)


#Fit with the original Simplified Conjoint Recognition MPT (Stahl & Klauer, 2008)
ya.model1 <- traitMPT(eqnfile="ftt.eqn",
                      data=ya1,
                      modelfilename="ya.model1.jags",
                      parEstFile="ya.model1.txt",
                      mu=c(V_r="dnorm(0,4)", a="dnorm(0,1)", b="dnorm(0,1)",
                           V_t="dnorm(0,1)", G_r="dnorm(0,1)",
                           G_t="dnorm(0,1)"),
                      n.iter=80000, n.adapt=15000,
                      n.burnin=15000, ppp = 5000, dic = TRUE)
#Poor fit (see ppp value)
#We even adjusted the V_r parameter prior distribution to add more
#Regularization to the posterior, and this did not improve model fit


#The below code modifies Heck et al.'s 2018 posterior predictive plots
#To better capture the full range of responses in our design

plotFit2 <- function(fittedModel, M=1000, stat = "mean", ...){
  
  stat <- match.arg(stat, c("mean", "cov"))
  
  # get information about model:
  tree <- fittedModel$mptInfo$MPT$Tree
  cats <- fittedModel$mptInfo$MPT$Category
  dat <- fittedModel$mptInfo$dat[,cats]
  TreeNames <- unique(tree)
  
  # get posterior predictive:
  if(is.null(fittedModel$postpred) | M != 1000){
    freq.list <- posteriorPredictive(fittedModel, M=M)
  }else{
    freq.list <- fittedModel$postpred$freq.pred
  }
  
  if(stat == "mean"){
    # Plot mean frequencies:
    
    pred <- t(sapply(freq.list, colMeans))
    boxplot(pred[,cats], xaxt="n", col="gray",
            main="INSERT", las=1, ...)
    axis(1, 1:length(cats), labels = cats)
    xx <- by(1:length(tree), tree, mean)
    axis(1, xx,  TreeNames, tick=F, line=NA, mgp=c(3, 2.5, 0))
    points(1:length(cats), colMeans(dat)[cats], col="red", cex=1.4, pch=17)
    abline(v=cumsum(table(tree))[1:(length(TreeNames)-1)]+.5, col="gray")
    
  } else if (stat == "cov"){
    # Plot covariance of frequencies:
    
    nams <- outer(cats, cats, paste, sep="-")
    sel_cov <- nams[upper.tri(nams, diag = TRUE)]
    K <- length(sel_cov)
    
    # observed/predicted
    c.obs <- cov(dat[,cats])
    c.pred <- sapply(freq.list, function(xx){
      cc <- cov(xx[,cats])
      cc[upper.tri(cc, diag=TRUE)]
    })
    
    boxplot(t(c.pred), col="gray", ylab="Covariance",
            main="Observed (red) and predicted (gray) covariances",
            xaxt="n", las=1, ...)
    abline(h=0, lty=1, col="gray")
    axis(1, 1:K, labels = nams[upper.tri(nams, diag=TRUE)], las=2)
    points(1:K, c.obs[upper.tri(c.obs, diag=TRUE)], col=2, pch=17)
    abline(v = cumsum(seq(nrow(c.obs), 2, -1))+.5, col="lightgray")
  }
}

plotFit2(ya.model1) #Posterior predictive plot for the original SCR model


#Modified model with two a parameters
ya.model2 <- traitMPT(eqnfile="exp1MPT.eqn",
                      data=ya1, 
                      restrictions=list("Gi=Gr"), #Maintain identifiability
                      modelfilename="ya.model2.jags",
                      parEstFile="ya.model2.txt",
                      n.iter=60000, n.adapt=15000,
                      n.burnin=10000, ppp = 1000, dic = TRUE)

plotFit2(ya.model2) #Posterior predictive plot for modified model



#Old Adults
oa1 <- read.csv("oaexperiment1.csv")

testHetChi(freq = "oaexperiment1.csv",
           tree = c("Intact", "Intact", "Intact", "Relate", "Relate", "Relate",
                    "Unrela", "Unrela", "Unrela"))

#Simp Conj Recog model (Stahl & Klauer, 2008)
oa.model1 <- traitMPT(eqnfile="ftt.eqn",
                      data=oa1,
                      modelfilename="oa.model1.jags",
                      parEstFile="oa.model1.txt",
                      mu=c(V_r="dnorm(0,4)", a="dnorm(0,1)", b="dnorm(0,1)",
                           V_t="dnorm(0,1)", G_r="dnorm(0,1)",
                           G_t="dnorm(0,1)"),
                      n.iter=60000, n.adapt=15000,
                      n.burnin=10000, ppp = 1000, dic = TRUE)
plotFit2(oa.model1) #Poor fit

#Modified model with two a parameters
oa.model2 <- traitMPT(eqnfile="exp1MPT.eqn",
                      data=oa1, 
                      restrictions=list("Gi=Gr"), #For identifiability
                      modelfilename="oa.model2.jags",
                      parEstFile="oa.model2.txt",
                      n.iter=60000, n.adapt=15000,
                      n.burnin=10000, ppp = 1000, dic = TRUE)
plotFit2(oa.model2) #Good fit


##Conduct between-subject comparisons on each parameter by age

betweenSubjectMPT(ya.model2, oa.model2, par1="a")
betweenSubjectMPT(ya.model2, oa.model2, par1="ab")
betweenSubjectMPT(ya.model2, oa.model2, par1="b")
betweenSubjectMPT(ya.model2, oa.model2, par1="Gi")
betweenSubjectMPT(ya.model2, oa.model2, par1="Vi")
betweenSubjectMPT(ya.model2, oa.model2, par1="Vr")

##Forest plot of difference estimates

label <- c("a", "ab", "b", "Gi", "Gr", "Vi", "Vr")
estimate  <- c(-.04, -.01, -.05, .01, .01, .29, .31) 
lower <- c(-.18, -.19, -.13, -.04, -.04, .05, .15)
upper <- c(.08, .19, .04, .07, .07, .49, .45)

df <- data.frame(label, estimate, lower, upper)
df$label <- factor(df$label, levels=rev(df$label))

fig4a <- ggplot(data=df, aes(x=label, y=estimate, ymin=lower, ymax=upper)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Estimate (95% CI)") +
  labs(title="Experiment 1", tag="a") +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.line=element_line(size=0.5, linetype = "solid",
                               color="black")) +
  theme(text = element_text(family="serif", size=16, color="black")) + ylim(-0.20, 0.60) # use a white background
print(fig4a)

###Experiment 2 

#Young adults
ya2 <- read.csv("yaexperiment2all.csv")
testHetChi(freq = "yaexperiment2all.csv",
           tree = c("Intact", "Intact", "Intact", 
                    "Related", "Related", "Related",
                    "UnrelCC", "UnrelCC", "UnrelCC",
                    "UnrelWC", "UnrelWC", "UnrelWC"))

#Analyses below for the new MPT model (with a Fuzzy retrieval parameter)

##Equality restictions on gist parameters (overidentifed model)
ya2.model1 <- traitMPT(eqnfile="exp2MPT.eqn",
                   data=ya2, 
                   restrictions=list("Gi=Gr"),
                   modelfilename="ya2model1.jags",
                   parEstFile="ya2model1.txt",
                   n.iter=60000, n.adapt=15000,
                   n.burnin=10000, ppp = 1000, dic = TRUE)
plotFit2(ya2.model1)

#Free the gist parameters
ya2.g <- traitMPT(eqnfile="exp2MPT.eqn",
                  data=ya2, 
                  modelfilename="ya2freegist.jags",
                  parEstFile="ya2freegist.txt",
                  n.iter=60000, n.adapt=15000,
                  n.burnin=10000, ppp = 1000, dic = TRUE)
#Needs additional adaptation period, more sampling below
extendMPT(ya2.g, n.iter=30000, n.adapt=15000, n.burnin=5000)
extendMPT(ya2.g, n.iter=40000, n.adapt=20000, n.burnin=5000)
summary(ya2.g)
plotFit2(ya2.g)

#Set fuzzy retrieval to 0
ya2.nof <- traitMPT(eqnfile="exp2MPT.eqn",
                     data=ya2, 
                     restrictions=list("F=0"),
                     modelfilename="ya2nofuzzy.jags",
                     parEstFile="ya2nofuzzy.txt",
                     n.iter=60000, n.adapt=15000,
                     n.burnin=10000, ppp = 1000, dic = TRUE)
plotFit2(ya2.nof)


#Old Adults
oa2 <- read.csv("oaexperiment2all.csv")
testHetChi(freq = "oaexperiment2all.csv",
           tree = c("Intact", "Intact", "Intact", 
                    "Related", "Related", "Related",
                    "UnrelCC", "UnrelCC", "UnrelCC",
                    "UnrelWC", "UnrelWC", "UnrelWC"))

#Model with gist restrictions
oa2.model1 <- traitMPT(eqnfile="exp2MPT.eqn",
                   data=oa2, 
                   restrictions=list("Gi=Gr"),
                   modelfilename="oa2model1.jags",
                   parEstFile="oa2model1.txt",
                   n.iter=60000, n.adapt=15000,
                   n.burnin=10000, ppp = 1000, dic = TRUE)
plotFit2(oa2.model1)

#Free the gist parameters
oa2.g <- traitMPT(eqnfile="exp2MPT.eqn",
                  data=oa2, 
                  modelfilename="oa2freegist.jags",
                  parEstFile="oa2freegist.txt",
                  n.iter=60000, n.adapt=15000,
                  n.burnin=10000, ppp = 1000, dic = TRUE)
plotFit2(oa2.g)

#Set fuzzy retrieval to 0
oa2.nof <- traitMPT(eqnfile="exp2MPT.eqn",
                     data=oa2, 
                     restrictions=list("F=0"),
                     modelfilename="oa2nofuzzy.jags",
                     parEstFile="oa2nofuzzy.txt",
                     n.iter=60000, n.adapt=15000,
                     n.burnin=10000, ppp = 1000, dic = TRUE)
plotFit2(oa2.nof)

#Models with ab and gist parameter restrictions
betweenSubjectMPT(ya2.model1, oa2.model1, par1="a")
betweenSubjectMPT(ya2.model1, oa2.model1, par1="ab")
betweenSubjectMPT(ya2.model1, oa2.model1, par1="b")
betweenSubjectMPT(ya2.model1, oa2.model1, par1="F")
betweenSubjectMPT(ya2.model1, oa2.model1, par1="Gi")
betweenSubjectMPT(ya2.model1, oa2.model1, par1="Vi")
betweenSubjectMPT(ya2.model1, oa2.model1, par1="Vr")

##Forest plot of difference estimates

label2 <- c("a", "ab", "b", "F", "Gi", "Gr", "Vi", "Vr")
estimate2  <- c(.02, -.08, -.16, .02, .05, .05, .32, .21) 
lower2 <- c(-.14, -.27, -.25, -.07, -.05, -.05, .16, .06)
upper2 <- c(.18, .17, -.08, .10, .13, .13, .48, .34)


df2 <- data.frame(label2, estimate2, lower2, upper2)
df2$label <- factor(df2$label2, levels=rev(df2$label2))

fig4b <- ggplot(data=df2, aes(x=label2, y=estimate2, ymin=lower2, ymax=upper2)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Estimate (95% CI)") +
  labs(title="Experiment 2", tag="b") +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.line=element_line(size=0.5, linetype = "solid",
                               color="black")) +
  theme(text = element_text(family="serif", size=16, color="black")) + ylim(-0.30, 0.60)
print(fig4b)

#Combine plots from Exp 1 and Exp 2
fig4 <- ggarrange(fig4a, fig4b,
                  ncol=2, nrow=1) 
annotate_figure(fig4,
                top=text_grob("MPT Age Differences (Young minus Old)", size=18))

