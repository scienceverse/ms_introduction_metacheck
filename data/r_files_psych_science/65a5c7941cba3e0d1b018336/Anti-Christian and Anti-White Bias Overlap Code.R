# Packages and Functions ----
library(Hmisc)
library(Rmisc)
library(dplyr)
library(psych)
library(ggplot2)
library(stringr)

meas <- function(x) {
  a <- cor(x, use = "pairwise")
  b <- range(x, na.rm = T)
  c <- psych::alpha(x, check.keys = T)
  d <- psych::alpha(x, check.keys = T)$scores
  e <- shapiro.test(d)
  f <- data.frame(d)
  g <- ggplot2::ggplot(f, aes(x=d)) + 
    ggplot2::geom_histogram(aes(y=after_stat(density)), binwidth = (max(d, na.rm=T)-min(d, na.rm=T))/14) +
    geom_density(adjust=1,kernel="gaussian",na.rm=TRUE,
                 color="red" ,size=.5) + 
    stat_function(fun = dnorm, 
                  args = list(mean = mean(f$d,na.rm=TRUE),
                              sd = sd(f$d,na.rm=TRUE)), 
                  linewidth = 1, 
                  color = "blue") +
    xlab("Aggregate") +
    theme_bw()
  if(Hmisc::label(x[1]) != "") {
    h <- list("Item"=Hmisc::label(x), "Cor"=a, "ALpha"=c, "Range"=b, "Normality"=e, "Plot"=g, "Scores"=d)
    print(h[1:6])
    invisible(h)
  }
  else {
    h <- list("Cor"=a, "ALpha"=c, "Range"=b, "Normality"=e, "Plot"=g, "Scores"=d)
    print(h[1:5])
    invisible(h)
  }
}

stdcoef <- function(f) {
  x <- model.frame(f)[[1]]
  if(length(model.frame(f)) == 3) {
    y <- model.frame(f)[[2]]:model.frame(f)[[3]]
  }
  else {
    y <- model.frame(f)[[2]]
  }
  nalength <- function(x) { length(na.omit(x)) }
  psd <- sqrt(sum(tapply(x, y, var, na.rm=T)*(tapply(x, y, nalength)-1))/(nalength(x)-length(table(y))))
  mat <- cbind(coef(lm(f)), confint(lm(f)), lm(f)$coefficients/psd)
  colnames(mat) <- c("b", "2.5%CI", "97.5%CI", "d")
  mat
}

# Cleaned Data Analysis Code ----
## Experiment 1 ----
E1 <- read.csv("Experiment1Data.csv", header = F, skip = 2)
Hmisc::label(E1) <- read.csv("Experiment1Data.csv", header = F)[2,]
colnames(E1) <- read.csv("Experiment1Data.csv", header = F)[1,]
E1$Bias <- factor(E1$Bias, levels = c("None", "Christian", "White", "Black"))
# Anti-Christian Bias by Condition (Control coded as reference group)
summary(lm(E1$AntiChristian~E1$Bias))
stdcoef(E1$AntiChristian~E1$Bias)

# Anti-White Bias by Condition (Control coded as reference group)
summary(lm(E1$AntiWhite~E1$Bias))
stdcoef(E1$AntiWhite~E1$Bias)

# Anti-Black Bias by Condition (Control coded as reference group)
summary(lm(E1$AntiBlack~E1$Bias))
stdcoef(E1$AntiBlack~E1$Bias)


## Experiment 2 ----
E2 <- read.csv("Experiment2Data.csv", header = F, skip = 2)
Hmisc::label(E2) <- read.csv("Experiment2Data.csv", header = F)[2,]
colnames(E2) <- read.csv("Experiment2Data.csv", header = F)[1,]
E2$Black <- factor(E2$Black, levels = c("Black", "White"))
E2$White <- factor(E2$Black, levels = c("White", "Black"))
E2$Bias <- factor(E2$Bias, levels = c("None", "Christian", "White", "Black"))
# Perceptions of Anti-Christian Bias by Condition and Race (Control coded as reference group)
# White Participants coded as reference group
summary(lm(E2$AntiChristianBias~E2$Bias*E2$White))
stdcoef(E2$AntiChristianBias~E2$Bias*E2$White)
# Black Participants coded as reference group
summary(lm(E2$AntiChristianBias~E2$Bias*E2$Black))
stdcoef(E2$AntiChristianBias~E2$Bias*E2$Black)

# Perceptions of Anti-White Bias by Condition and Race (Control is reference group)
# White is reference group
summary(lm(E2$AntiWhiteBias~E2$Bias*E2$White))
stdcoef(E2$AntiWhiteBias~E2$Bias*E2$White)
# Black is reference group
summary(lm(E2$AntiWhiteBias~E2$Bias*E2$Black))
stdcoef(E2$AntiWhiteBias~E2$Bias*E2$Black)
# Perceptions of Anti-Black Bias by Condition and Race (Control is reference group)
# White is reference group
summary(lm(E2$AntiBlackBias~E2$Bias*E2$White))
stdcoef(E2$AntiBlackBias~E2$Bias*E2$White)
# Black is reference group
summary(lm(E2$AntiBlackBias~E2$Bias*E2$Black))
stdcoef(E2$AntiBlackBias~E2$Bias*E2$Black)

# Compare White participant's estimates of Christians who are White to PRRI 2021's
# 44% of U.S. being White Christians divided by 70% of U.S. being Christian = Percentage of Christians who are White
t.test(subset(E2, Black == "White")$ChristWhite_1, mu = 44/70*100)
(mean(subset(E2, Black == "White")$ChristWhite_1, na.rm=T)-(44/70*100))/sd(subset(E2, Black == "White")$ChristWhite_1, na.rm = T)
t.test(subset(E2, Black == "White")$ChristWhite_1, mu = 44/70*100)$conf-(44/70*100)

# Compare White participant's estimates of White people who are Christians to PRRI 2021's estimate of 70%
t.test(subset(E2, Black == "White")$WhiteChrist_1, mu = 71)
(mean(subset(E2, Black == "White")$WhiteChrist_1, na.rm = T)-(71))/sd(subset(E2, Black == "White")$WhiteChrist_1, na.rm = T)
t.test(subset(E2, Black == "White")$WhiteChrist_1, mu = 71)$conf-(71)

## Experiment 3 ----
E3 <- read.csv("Experiment3Data.csv", header = F, skip = 2)
Hmisc::label(E3) <- read.csv("Experiment3Data.csv", header = F)[2,]
colnames(E3) <- read.csv("Experiment3Data.csv", header = F)[1,]
E3$PolConservatism <- E3$AP_IDEO
E3$PolParty <- as.factor(E3$AP_POLID)
E3$Condition <- factor(E3$Condition, levels = c("Control", "Anti-Christian", "Anti-White", "Rel_Freedom"))
E3$Condition_ACB <- factor(E3$Condition, levels = c("Anti-Christian", "Anti-White", "Rel_Freedom", "Control"))
E3$Condition_AWB <- factor(E3$Condition, levels = c("Anti-White", "Anti-Christian", "Rel_Freedom", "Control"))

# Politician's Perceived Concern About Anti-Christian Bias by Condition
## Control coded as reference group
summary(lm(E3$ChristBias_Concern~E3$Condition))
stdcoef(E3$ChristBias_Concern~E3$Condition)
## controlling for politician conservatism and party
summary(lm(E3$ChristBias_Concern~E3$Condition + E3$PolConservatism + E3$PolParty))
stdcoef(E3$ChristBias_Concern~E3$Condition + E3$PolConservatism + E3$PolParty)

# Politician's Perceived Concern About Anti-White Bias by Condition
## Control coded as reference group
summary(lm(E3$WhiteBias_Concern~E3$Condition))
stdcoef(E3$WhiteBias_Concern~E3$Condition)
## controlling for politician conservatism and party
summary(lm(E3$WhiteBias_Concern~E3$Condition + E3$PolConservatism + E3$PolParty))
stdcoef(E3$WhiteBias_Concern~E3$Condition + E3$PolConservatism + E3$PolParty)

# Politician's Perceived Concern About Religious Freedom by Condition
## Control coded as reference group
summary(lm(E3$RelFreedom_Concern~E3$Condition))
stdcoef(E3$RelFreedom_Concern~E3$Condition)

# Politician's Perceived Controversiality
## Control coded as reference group
summary(lm(E3$PolControversial~E3$Condition))
stdcoef(E3$PolControversial~E3$Condition)
## Politician concerned with Anti-Christian Bias coded as reference group
summary(lm(E3$PolControversial~E3$Condition_ACB))
stdcoef(E3$PolControversial~E3$Condition_ACB)

# Politician's Speech's Perceived Offensiveness
## Control coded as reference group
summary(lm(E3$SpeechOffensive~E3$Condition))
stdcoef(E3$SpeechOffensive~E3$Condition)
## Politician concerned with Anti-Christian Bias coded as reference group
summary(lm(E3$SpeechOffensive~E3$Condition_ACB))
stdcoef(E3$SpeechOffensive~E3$Condition_ACB)

# Perceived Extent to Which Politician will Fight for Black People
## Control coded as reference group
summary(lm(E3$Fight4Black~E3$Condition))
stdcoef(E3$Fight4Black~E3$Condition)
## Politician concerned with Anti-Christian Bias coded as reference group
summary(lm(E3$Fight4Black~E3$Condition_ACB))
stdcoef(E3$Fight4Black~E3$Condition_ACB)

# Perceived Extent to Which Politician will Fight for Christians
## Control coded as reference group
summary(lm(E3$Fight4Christian~E3$Condition))
stdcoef(E3$Fight4Christian~E3$Condition)
## Politician concerned with Anti-Christian Bias coded as reference group
summary(lm(E3$Fight4Christian~E3$Condition_ACB))
stdcoef(E3$Fight4Christian~E3$Condition_ACB)

# Perceived Extent to Which Politician will Fight for White People
## Control coded as reference group
summary(lm(E3$Fight4White~E3$Condition))
stdcoef(E3$Fight4White~E3$Condition)
## Politician concerned with Anti-Christian Bias coded as reference group
summary(lm(E3$Fight4White~E3$Condition_ACB))
stdcoef(E3$Fight4White~E3$Condition_ACB)

# Perceived Extent to Which Politician will Support Racial Equality Policies
E3$RacePolicy <- meas(E3 |> dplyr::select(PQ_RACE_1, PQ_RACE_2, PQ_RACE_3))$Scores
## Control coded as reference group
summary(lm(E3$RacePolicy~E3$Condition))
stdcoef(E3$RacePolicy~E3$Condition)

## Experiment 4 ----
E4 <- read.csv("Experiment4Data.csv", header = F, skip = 2)
Hmisc::label(E4) <- read.csv("Experiment4Data.csv", header = F)[2,]
colnames(E4) <- read.csv("Experiment4Data.csv", header = F)[1,]

E4$PolConservatism <- E4$AP_2
E4$PolParty <- as.factor(E4$AP_3)
E4$Condition <- factor(E4$Condition, levels = c("Control", "Anti-Christian", "Anti-White"))
E4$Condition_ACB <- factor(E4$Condition, levels = c("Anti-Christian", "Anti-White", "Control"))
E4$Condition_AWB <- factor(E4$Condition, levels = c("Anti-White", "Anti-Christian", "Control"))

# Politician's Perceived Concern About Anti-Christian Bias by Condition
## Control coded as reference group
summary(lm(E4$ChristianBias_Concern~E4$Condition))
stdcoef(E4$ChristianBias_Concern~E4$Condition)
## controlling for politician conservatism and party
summary(lm(E4$ChristianBias_Concern~E4$Condition + E4$PolConservatism + E4$PolParty))
stdcoef(E4$ChristianBias_Concern~E4$Condition + E4$PolConservatism + E4$PolParty)

# Politician's Perceived Concern About Anti-White Bias by Condition
## Control coded as reference group
summary(lm(E4$WhiteBias_Concern~E4$Condition))
stdcoef(E4$WhiteBias_Concern~E4$Condition)
## controlling for politician conservatism and party
summary(lm(E4$WhiteBias_Concern~E4$Condition + E4$PolConservatism + E4$PolParty))
stdcoef(E4$WhiteBias_Concern~E4$Condition + E4$PolConservatism + E4$PolParty)

# Politician's Perceived Concern About Anti-Black Bias by Condition
## Control coded as reference group
summary(lm(E4$BlackBias_Concern~E4$Condition))
stdcoef(E4$BlackBias_Concern~E4$Condition)

# Politician's Speech's Perceived Offensiveness
## Control coded as reference group
summary(lm(E4$Offensive~E4$Condition))
stdcoef(E4$Offensive~E4$Condition)
## Politician concerned with Anti-Christian Bias coded as reference group
summary(lm(E4$Offensive~E4$Condition_ACB))
stdcoef(E4$Offensive~E4$Condition_ACB)
## controlling for politician conservatism and party
summary(lm(E4$Offensive~E4$Condition + E4$PolConservatism + E4$PolParty))
stdcoef(E4$Offensive~E4$Condition + E4$PolConservatism + E4$PolParty)

# Perceived Extent to Which Politician will Fight for Black People
## Control coded as reference group
summary(lm(E4$Fight4Black~E4$Condition))
stdcoef(E4$Fight4Black~E4$Condition)
## Politician concerned with Anti-Christian Bias coded as reference group
summary(lm(E4$Fight4Black~E4$Condition_ACB))
stdcoef(E4$Fight4Black~E4$Condition_ACB)
## controlling for politician conservatism and party
summary(lm(E4$Fight4Black~E4$Condition + E4$PolConservatism + E4$PolParty))
stdcoef(E4$Fight4Black~E4$Condition + E4$PolConservatism + E4$PolParty)

# E1-E2 Plots ----
library(Rmisc)
library(ggplot2)
source("https://raw.githubusercontent.com/easystats/see/5abfdf1224ed2309cabddf7322dafabb03814d82/R/geom_violinhalf.R")

viole12 <- function(vars, title) {
  if(is.list(vars)==F) {
    return("vars should be a list of variables")
  }
  else {
    plot <- data.frame("var"=c(vars[[1]], vars[[2]][E2$Black=="White"], vars[[2]][E2$Black=="Black"]),
                       "Condition"=c(E1$Bias, E2$Bias[E2$Black=="White"], E2$Bias[E2$Black=="Black"]),
                       "Study"=c(rep(c("Experiment 1 White Participants",
                                       "Experiment 2 White Participants", 
                                       "Experiment 2 Black Participants"), times=c(nrow(E1), nrow(subset(E2, Black == "White")), nrow(subset(E2, Black == "Black"))))))
    plot$Study <- factor(plot$Study, levels = c("Experiment 1 White Participants", "Experiment 2 White Participants", "Experiment 2 Black Participants"))
    plot$p <- plot$Study:plot$Condition
    mat <- c(0, ifelse(summary(lm(vars[[1]]~E1$Bias))$coefficients[2:4,4]<.05, 1, 0),
             0, ifelse(summary(lm(vars[[2]]~E2$Bias*E2$White))$coefficients[2:4,4]<.05, 1, 0),
             0, ifelse(summary(lm(vars[[2]]~E2$Bias*E2$Black))$coefficients[2:4,4]<.05, 1, 0))
    levels(plot$p) <- factor(mat)
    plot$Condition <- factor(plot$Condition, levels = c("None", "Christian", "White", "Black"))
    library(ggplot2)
    library(jtools)
    ggplot(plot, aes(x=Condition, y=var, group=Condition, fill=p)) +
      geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
      geom_jitter(alpha = .2, width = 0.15, color="dimgray") +
      stat_summary(fun.data = "mean_cl_normal", color="Black", size=.25, fun.args = list(conf.int=.99)) +
      coord_cartesian(ylim = c(1,7)) +
      scale_y_continuous(breaks = seq(1,7,1)) +
      xlab("Bias Target Condition") +
      ylab(title) +
      theme_bw()+
      jtools::theme_apa() +
      scale_fill_grey() +
      theme(text=element_text(family = "Times New Roman", size = 12), axis.text.x =element_text(size = 12)) +
      guides(fill="none") +
      facet_wrap(~Study, nrow = 3)
  }
}

viole12(vars = list(E1$AntiChristian, E2$AntiChristianBias), title = "Perceptions of Anti-Christian Bias")
viole12(vars = list(E1$AntiWhite, E2$AntiWhiteBias), title = "Perceptions of Anti-White Bias")
viole12(vars = list(E1$AntiBlack, E2$AntiBlackBias), title = "Perceptions of Anti-Black Bias")

# E3 Plots ----
library(Rmisc)
library(ggplot2)
source("https://raw.githubusercontent.com/easystats/see/5abfdf1224ed2309cabddf7322dafabb03814d82/R/geom_violinhalf.R")
viole3 <- function(vars, varnames, title=NULL) {
  if(is.list(vars)==F) {
    return("vars should be a list of variables")
  }
  else {
    plot <- data.frame("var"=unlist(vars),
                       "Condition"=rep(E3$Condition, times=length(vars)),
                       "varname"=c(rep(varnames, each=nrow(c))))
    plot$varname <- factor(plot$varname, levels = varnames)
    plot$p <- plot$varname:plot$Condition
    mat <- matrix(c(NA), nrow = 4*length(vars))
    for (i in 0:(length(vars)-1)) {
      mat[1:4+(4*i)] <- c(0, ifelse(summary(lm(vars[[i+1]]~E3$Condition))$coefficients[2:4,4]<.05, 1, 0))
    }
    levels(plot$p) <- factor(mat)
    plot$Condition <- factor(plot$Condition, levels = c("Control", "Anti-Christian", "Anti-White", "Rel_Freedom"))
    library(ggplot2)
    library(jtools)
    if(is.null(title))
      ggplot(plot, aes(x=Condition, y=var, group=Condition, fill=p)) +
      geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
      geom_jitter(alpha = .2, width = 0.15, color="dimgray") +
      stat_summary(fun.data = "mean_cl_normal", color="Black", size=.25, fun.args = list(conf.int=.99)) +
      coord_cartesian(ylim = c(1,7)) +
      scale_y_continuous(breaks = seq(1,7,1)) +
      xlab("Politician Concern Condition") +
      scale_x_discrete(labels=c("Control\n(Economy)", "Anti-Christian\nBias", "Anti-White\nBias", "Religious\nFreedom")) +
      theme_bw()+
      jtools::theme_apa() +
      scale_fill_grey() +
      guides(fill="none") +
      facet_wrap(~varname, nrow = length(vars), strip.position = "top") +
      theme(text=element_text(family = "Times New Roman", size = 12), axis.text.x =element_text(size = 12),
            plot.title = element_text(size = 12, face = "plain", hjust = .5), axis.title.y = element_blank())
    else {
      ggplot(plot, aes(x=Condition, y=var, group=Condition, fill=p)) +
        geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
        geom_jitter(alpha = .2, width = 0.15, color="dimgray") +
        stat_summary(fun.data = "mean_cl_normal", color="Black", size=.25, fun.args = list(conf.int=.99)) +
        coord_cartesian(ylim = c(1,7)) +
        scale_y_continuous(breaks = seq(1,7,1)) +
        xlab("Politician Concern Condition") +
        scale_x_discrete(labels=c("Control\n(Economy)", "Anti-Christian\nBias", "Anti-White\nBias", "Religious\nFreedom")) +
        theme_bw()+
        jtools::theme_apa() +
        scale_fill_grey() +
        guides(fill="none") +
        facet_wrap(~varname, nrow = length(vars), strip.position = "top") +
        ggtitle(title) +
        theme(text=element_text(family = "Times New Roman", size = 12), axis.text.x =element_text(size = 12),
              plot.title = element_text(size = 12, face = "plain", hjust = .5), axis.title.y = element_blank())
    }
  }
}

viole3(vars = list(E3$ChristBias_Concern, E3$WhiteBias_Concern, E3$RelFreedom_Concern), 
       varnames = c("Anti-Christian Bias", "Anti-White Bias", "Religious Freedom"),
       title = "Politician's Perceived Concern About:")

viole3(vars = list(E3$Fight4Black, E3$Fight4Christian, E3$Fight4Black), 
       varnames = c("Black People", "Christians", "White People"),
       title = "Perceptions that Politician Will Fight for:")

viole3(vars = list(E3$SpeechOffensive, E3$PolControversial), 
       varnames = c("Perceived Offensiveness of Politician's Claim", "Politician's Perceived Controversiality"))
viole3(vars = list(E3$AP_IDEO), 
       varnames = c("Politcian's Perceived Conservatism"))
viole3(vars = list(E3$RacePolicy), 
       varnames = c("Politcian's Perceived Support of Racial Equality Policies"))

# E4 Plots ----
viole4 <- function(vars, varnames, title=NULL) {
  if(is.list(vars)==F) {
    return("vars should be a list of variables")
  }
  else {
    plot <- data.frame("var"=unlist(vars),
                       "Condition"=rep(E4$Condition, times=length(vars)),
                       "varname"=c(rep(varnames, each=nrow(E4))))
    plot$varname <- factor(plot$varname, levels = varnames)
    plot$p <- plot$varname:plot$Condition
    mat <- matrix(c(NA), nrow = 3*length(vars))
    for (i in 0:(length(vars)-1)) {
      mat[1:3+(3*i)] <- c(0, ifelse(summary(lm(vars[[i+1]]~E4$Condition))$coefficients[2:3,4]<.05, 1, 0))
    }
    levels(plot$p) <- factor(mat)
    plot$Condition <- factor(plot$Condition, levels = c("Control", "Anti-Christian", "Anti-White"))
    library(ggplot2)
    library(jtools)
    if(is.null(title))
      p <- ggplot(plot, aes(x=Condition, y=var, group=Condition, fill=p)) +
      geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
      geom_jitter(alpha = .2, width = 0.15, color="dimgray") +
      stat_summary(fun.data = "mean_cl_normal", color="Black", size=.25, fun.args = list(conf.int=.99)) +
      coord_cartesian(ylim = c(1,7)) +
      scale_y_continuous(breaks = seq(1,7,1)) +
      xlab("Politician Concern Condition") +
      scale_x_discrete(labels=c("Control\n(Economy)", "Anti-Christian\nBias", "Anti-White\nBias")) +
      theme_bw()+
      jtools::theme_apa() +
      scale_fill_grey() +
      guides(fill="none") +
      theme(text=element_text(family = "Times New Roman", size = 12), axis.text.x =element_text(size = 12),
            plot.title = element_text(size = 12, face = "plain", hjust = .5), axis.title.y = element_blank()) +
      facet_wrap(~varname, nrow = length(vars), strip.position = "top")
    else {
      p <- ggplot(plot, aes(x=Condition, y=var, group=Condition, fill=p)) +
        geom_violinhalf(position = position_nudge(x = .2, y = 0)) +
        geom_jitter(alpha = .2, width = 0.15, color="dimgray") +
        stat_summary(fun.data = "mean_cl_normal", color="Black", size=.25, fun.args = list(conf.int=.99)) +
        coord_cartesian(ylim = c(1,7)) +
        scale_y_continuous(breaks = seq(1,7,1)) +
        xlab("Politician Concern Condition") +
        scale_x_discrete(labels=c("Control\n(Economy)", "Anti-Christian\nBias", "Anti-White\nBias")) +
        theme_bw()+
        jtools::theme_apa() +
        scale_fill_grey() +
        guides(fill="none") +
        ggtitle(title) +
        theme(text=element_text(family = "Times New Roman", size = 12), axis.text.x =element_text(size = 12),
              plot.title = element_text(size = 12, face = "plain", hjust = .5), axis.title.y = element_blank()) 
      
    }
  }
  if(length(vars)==1) {
    p
  }
  else {
    p + facet_wrap(~varname, nrow = length(vars), strip.position = "top") 
  }
}

viole4(vars = list(E4$ChristianBias_Concern, E4$WhiteBias_Concern), 
       varnames = c("Anti-Christian Bias", "Anti-White Bias"),
       title = "Politician's Perceived Concern About:")

viole4(vars = list(E4$Offensive), 
       varnames = c("Offensiveness of Claim"))
viole4(vars = list(E4$Fight4Black), 
       varnames = c("Fight for Black People"))
viole4(vars = list(E4$AP_2), 
       varnames = c("Politician's Perceived Conservatism"))

# Raw Data Cleaning Code ----
## Experiment 1 ----
E1r <- read.csv("Experiment1Data_raw.csv", header = F, skip = 2)
Hmisc::label(E1r) <- read.csv("Experiment1Data_raw.csv", header = F)[2,]
colnames(E1r) <- read.csv("Experiment1Data_raw.csv", header = F)[1,]

E1r$Bias <- relevel(as.factor(dplyr::recode(E1r$FL_31_DO, "FL_37"='Black', "FL_36"='White', "FL_35"='None', "FL_34"='Christian')), "None")
E1r$Biasn <- as.numeric(dplyr::recode(E1r$FL_31_DO, "FL_37"=3, "FL_36"=2, "FL_35"=0, "FL_34"=1))
E1r$Fundamentalism <- relevel(as.factor(dplyr::recode(as.numeric(E1r$F), "2"='No', "1"='Yes')), "No")
E1r$Evangelical <- relevel(as.factor(dplyr::recode(as.numeric(E1r$E), "2"='No', "1"='Yes')), "No")
table(E1r$Evangelical, E1r$Fundamentalism, E1r$Bias)

t <- meas(E1r |>  dplyr::select(ACB1, ACB2, ACB3, ACB4, ACB5, ACB6))
E1r$AntiChristian <- t$Scores

t <- meas(E1r |> dplyr::select(CN1, CN2, CN3, CN4, CN5, CN6))
E1r$ChristNationalism <- t$Scores

t <- meas(E1r |> dplyr::select(CR1, CR2, CR3, CR4, CR5, CR6))
E1r$ChristianRT <- t$Scores

t <- meas(E1r |> dplyr::select(AWB1, AWB2, AWB3, AWB4, AWB5, AWB6, AWB7, AWB8))
E1r$AntiWhite <- t$Scores

t <- meas(E1r |> dplyr::select(ABB1, ABB2, ABB3, ABB4, ABB5, ABB6))
E1r$AntiBlack <- t$Scores

t <- meas(E1r |> dplyr::select(RS1, RS2, RS3, RS4, RS5, RS6))
E1r$RaceST <- t$Scores

t <- meas(E1r |> dplyr::select(RR1, RR2, RR3, RR4, RR5))
E1r$RaceRT <- t$Scores

t <- meas(E1r |> dplyr::select(CS1, CS2, CS3, CS4, CS5, CS6))
E1r$ChristianST <- t$Scores

t <- meas(E1r |> dplyr::select(SDO1, SDO2, SDO3, SDO4))
E1r$SDO <- t$Scores

t <- meas(E1r |> dplyr::select(WID1, WID2, WID3, WID4, WID5))
E1r$WhiteID <- t$Scores

t <- meas(E1r |> dplyr::select(CID1, CID2, CID3, CID4, CID5))
E1r$ChristianID <- t$Scores

# Remove participants who did not report being (exclusively) White U.S. Christians
E1r_dem <- subset(E1r, RaceAll == 5 &
               RaceOne == 5 &
               Christian == 1 &
               US == 1
)
# Remove participants who failed any manipulation comprehension checks and/or an attention check
E1_clean <- subset(E1r, RaceAll == 5 &
               RaceOne == 5 &
               Christian == 1 &
               US == 1 & 
               Attention == 5 &
               ((ACBMC1 == 4 & ACBMC2 == 1) |    (AWBMC1 == 4 & AWBMC2 == 1) |    (ABBMC1 == 4 & ABBMC2 == 1) | Biasn == 0)
)

## Experiment 2 ----
E2r <- read.csv("Experiment2Data_raw.csv", header = F, skip = 2)
Hmisc::label(E2r) <- read.csv("Experiment2Data_raw.csv", header = F)[2,]
colnames(E2r) <- read.csv("Experiment2Data_raw.csv", header = F)[1,]


E2r$B <- ifelse(grepl("2", as.character(E2r$Race)), "Black", "non-Black")
E2r$W<- ifelse(E2r$Race == 1, "White", "non-exclusive-White")
E2r$Black <- relevel(as.factor(ifelse(E2r$B=="Black", E2r$B, E2r$W)), "White")
E2r$White <- relevel(E2r$Black, "Black")
E2r$Bias <- relevel(as.factor(dplyr::recode(E2r$FL_4_DO, "Anti-ChristianBlock"='Christian', "Anti-BlackBlock"='Black', 
                                              "Anti-WhiteBlock"='White', "ControlBlock"='None')), "None")
E2r$Biasn <- dplyr::recode(E2r$FL_4_DO, "Anti-ChristianBlock"=1, "Anti-BlackBlock"=3, 
                             "Anti-WhiteBlock"=2, "ControlBlock"=0)
E2r  |> dplyr::select(Bias, Biasn) |> table()
table(E2r$Race, E2r$Black)
# remove participants who did not report being Black or (exclusively) White U.S. Christians
E2r_dem <- subset(E2r , Religion == 1 &
                  US == 1 &
                  (Black == "Black" | Black == "White")
)
E2r_dem$Black <- droplevels(E2r_dem$Black)
E2r_dem$White <- droplevels(E2r_dem$White)
# remove participants who failed any manipulation comprehension questions, failed an attention check, and/or who qualtrics panels designated as giving bad data
E2r_clean <- subset(E2r_dem, 
            Attention == 5 &
              gc == 1 &
              ((Bias=="Christian" & ACBMC1 == 4 & ACBMC2==1) |
                 (Bias=="White" & AWBMC1 == 4 & AWBMC2==1) |
                 (Bias=="Black" & ABBMC1 == 4 & ABBMC2==1) |
                 Bias == "None") 
)
# white participants only data
E2r_clean_white <- subset(E2r_clean, White=="White")
# Black participants only data
E2r_clean_Black <- subset(E2r_clean, White=="Black")

t <- meas(E2r_clean|> dplyr::select(ACB1, ACB2, ACB3, ACB4, ACB5, ACB6, ACB7))
E2r_clean$AntiChristianBias <- t$Scores
meas(E2r_clean_white |> dplyr::select(ACB1, ACB2, ACB3, ACB4, ACB5, ACB6, ACB7))
meas(E2r_clean_Black |> dplyr::select(ACB1, ACB2, ACB3, ACB4, ACB5, ACB6, ACB7))

t <- meas(E2r_clean |> dplyr::select(AWB1, AWB2, AWB3, AWB4, AWB5, AWB6, AWB7))
E2r_clean$AntiWhiteBias <- t$Scores
meas(E2r_clean_white |> dplyr::select(AWB1, AWB2, AWB3, AWB4, AWB5, AWB6, AWB7))
meas(E2r_clean_Black |> dplyr::select(AWB1, AWB2, AWB3, AWB4, AWB5, AWB6, AWB7))

# ceiling effect
t <- meas(E2r_clean |> dplyr::select(ABB1, ABB2, ABB3, ABB4, ABB5, ABB6, ABB7))
E2r_clean$AntiBlackBias <- t$Scores
E2r_clean |> subset(ABB==7) |> dplyr::select(Black, ABB) |> table()
meas(E2r_clean_white |> dplyr::select(ABB1, ABB2, ABB3, ABB4, ABB5, ABB6, ABB7))
meas(E2r_clean_Black |> dplyr::select(ABB1, ABB2, ABB3, ABB4, ABB5, ABB6, ABB7))
t <- meas(E2r_clean |> dplyr::select(CN1, CN2, CN3, CN4, CN5, CN6, CN7))
E2r_clean$ChristianNationalism <- t$Scores

t <- meas(E2r_clean |> dplyr::select(LGBTQD1, LGBTQD2, LGBTQD3, LGBTQD4, LGBTQD5, LGBTQD6))
E2r_clean$LGBTQDiscrimination <- t$Scores

t <- meas(E2r_clean |> dplyr::select(VS1, VS2, VS3, VS4))
E2r_clean$VoterSupression <- t$Scores

# floor/ceiling
t <- meas(E2r_clean |> dplyr::select(SE1, SE2, SE3, SE4, SE5))
E2r_clean$SelfEsteem <- t$Scores
E2r_clean |>  subset(SE==1) |> dplyr::select(Black, SE) |> table()

t <- meas(E2r_clean |> dplyr::select(RaceTeaching1, RaceTeaching2, RaceTeaching3))
E2r_clean$AvoidRaceTeaching <- t$Scores
E2r_clean |>  subset(AvoidRaceTeaching==1) |> dplyr::select(Black, AvoidRaceTeaching) |> table()

t <- meas(E2r_clean |> dplyr::select(Surveil1, Surveil2, Surveil3))
E2r_clean$MuslimSurveillance <- t$Scores
E2r_clean |>  subset(MuslimSurveillance==1) |> dplyr::select(Black, MuslimSurveillance) |> table()

t <- meas(E2r_clean |> dplyr::select(Defund1, Defund2, Defund3, Defund4))
E2r_clean$DefundPolice <- t$Scores
E2r_clean |>  subset(DefundPolice==1) |> dplyr::select(Black, DefundPolice) |> table()

t <- meas(E2r_clean |> dplyr::select(CID1, CID2))
E2r_clean$ChristianID <- t$Scores
E2r_clean |>  subset(ChristianID==7) |> dplyr::select(Black, ChristianID) |> table()

t <- meas(E2r_clean |> dplyr::select(WID1, WID2))
E2r_clean$WhiteID <- t$Scores
E2r_clean |>  subset(WhiteID==7) |> dplyr::select(Black, WhiteID) |> table()
E2r_clean |>  subset(WhiteID==1) |> dplyr::select(Black, WhiteID) |> table()

t <- meas(E2r_clean |> dplyr::select(BID1, BID2))
E2r_clean$BlackID <- t$Scores
E2r_clean |>  subset(BlackID==7) |> dplyr::select(Black, BlackID) |> table()

## Experiment 3 ----
E3r <- read.csv("Experiment3Data_raw.csv", header = F, skip = 2)
Hmisc::label(E3r) <- read.csv("Experiment3Data_raw.csv", header = F)[2,]
colnames(E3r) <- read.csv("Experiment3Data_raw.csv", header = F)[1,]
# remove participants who did not report being (exclusively) White U.S. Christians
E3r_dem <- subset(E3r, race == 1 & usa == 1 & (rel_aff < 5 | rel_aff_13_TEXT == "Baptist"
                                               | rel_aff_13_TEXT == "Baspist"
                                               | rel_aff_13_TEXT == "Born Again"
                                               | rel_aff_13_TEXT == "Methodist"
                                               | rel_aff_13_TEXT == "Non deminstonap"
                                               | rel_aff_13_TEXT == "Pentecostal"
                                               | rel_aff_13_TEXT == "Ukrainian orthodox"
                                               | rel_aff_13_TEXT == "Unitarian Universalist"
                                               | rel_aff_13_TEXT == "Christian Spiritualist"))
table(E3r_dem$rel_aff_13_TEXT)
# remove participants who missed a manipulation comprehension check or an attention check
E3r_clean <- subset(E3r_dem, ATTCHK == 7 & manchk == 1)
var.labels <- label(E3r)
label(E3r_clean) <- as.list(var.labels[match(names(E3r_clean), names(var.labels))])
t <- meas(E3r_clean |> dplyr::select(PBOC_1, PBOC_2, PBOC_3, PBOC_4))
E3r_clean$PolControversial <- t$Scores
t <- meas(E3r_clean |> dplyr::select(OCI_1, OCI_2, OCI_3, OCI_4))
E3r_clean$SpeechOffensive <- t$Scores
t <- meas(E3r_clean |> dplyr::select(PQ_RACE_1, PQ_RACE_2, PQ_RACE_3))
E3r_clean$RacePolicy <- t$Scores
E3r_clean$Condition <- relevel(dplyr::recode(as.factor(stringr::str_trim(E3r_clean$FL_6_DO, "right")), 
                                     'Condition4:Economy(Control)' = 'Control', 
                                     "Condition3:ReligiousFreedom"='Rel_Freedom', 
                                     "Condition1:WhiteBias"='Anti-White', 
                                     "Condition2:ChristianBias"='Anti-Christian'), "Control")
E3r_clean$Condition_AWB <- relevel(dplyr::recode(as.factor(stringr::str_trim(E3r_clean$FL_6_DO, "right")), 
                                         'Condition4:Economy(Control)' = 'Control', 
                                         "Condition3:ReligiousFreedom"='Rel_Freedom', 
                                         "Condition1:WhiteBias"='Anti-White', 
                                         "Condition2:ChristianBias"='Anti-Christian'), "Anti-White")
E3r_clean$Condition_ACB <- relevel(dplyr::recode(as.factor(stringr::str_trim(E3r_clean$FL_6_DO, "right")), 
                                         'Condition4:Economy(Control)' = 'Control', 
                                         "Condition3:ReligiousFreedom"='Rel_Freedom', 
                                         "Condition1:WhiteBias"='Anti-White', 
                                         "Condition2:ChristianBias"='Anti-Christian'), "Anti-Christian")

## Experiment 4 ----
E4r <- read.csv("Experiment4Data_raw.csv", header = F, skip = 2)
Hmisc::label(E4r) <- read.csv("Experiment4Data_raw.csv", header = F)[2,]
colnames(E4r) <- read.csv("Experiment4Data_raw.csv", header = F)[1,]

E4r$Condition_Christ <- relevel(as.factor(E4r$FL_6_DO), ref = "Condition2:ChristianBias")
E4r$Condition_White <- relevel(as.factor(E4r$FL_6_DO), ref = "Condition1:WhiteBias")
E4r$Condition <- relevel(as.factor(E4r$FL_6_DO), ref = levels(as.factor(E4r$FL_6_DO))[3])
E4r$Condition <- factor(E4r$Condition, labels = c("Control", "Anti-White", "Anti-Christian"))
table(E4r$rel_aff_13_TEXT)
length(na.omit(E4r$race_10_TEXT))
# remove participants who did not report being Black U.S. Christians
E4r_dem <- subset(E4r, race==2 & (rel_aff==1 | rel_aff==2 | rel_aff==3 | 
                                    rel_aff== 4 | rel_aff_13_TEXT == "Unbaptised Publisher (Jehovah's Witnesses)" |
                                    rel_aff_13_TEXT == "Baptist") &
                  usa == 1)
table(E4r_dem$rel_aff_13_TEXT)
# remove participants who answered a manipulation comprehension check twice
E4r_clean <- subset(E4r_dem, ((manchk_wb == 1 | manchk2_wb == 1) | 
                                (manchk_cb == 1 | manchk2_cb == 1) | 
                                (manchk_ec == 1 | Q128 == 1)))

library(Hmisc)
var.labels <- label(E4r)
label(E4r_clean) <- as.list(var.labels[match(names(E4r_clean), names(var.labels))])

t <- meas(E4r_clean |>  dplyr::select(OCI_1, OCI_2, OCI_3, OCI_4))
E4r_clean$Offensive <- t$Scores

t <- meas(E4r_clean |>  dplyr::select(WhiteBias_Concern1, WhiteBias_Concern2))
E4r_clean$WhiteBias <- t$Scores

t <- meas(E4r_clean |>  dplyr::select(ChristianBias_Concern1, ChristianBias_Concern2))
E4r_clean$ChristianBias <- t$Scores

t <- meas(E4r_clean |>  dplyr::select(BlackBias_Concern1, BlackBias_Concern2))
E4r_clean$BlackBias <- t$Scores

# Power Analyses ----
library(doParallel)
library(parallel)
num_cores <- detectCores()
use_cores <- num_cores-1

## Experiment 1 ----
iterations <- 1e4
sequence <- seq(.2, .4, .01)
mat2 <- matrix(NA, ncol = 4)
cl<-makeCluster(use_cores)
registerDoParallel(cl)
mat2 <- foreach(i2=1:length(sequence), .combine=rbind
) %dopar%  {
  mat <- matrix(NA, nrow = iterations, ncol = 3)
  for(i in 1:iterations) {
    # randomly assign condition assignment
    x <- as.factor(sample(c(1,2,3,4), 543, replace = T))
    # simulate outcome variable. Mean of the value of the sequence for condition # 2 and 0 for other conditions
    # SD calculation ensures that the population standard deviation is 1 (as opposed to the SD being 1 for each group) 
    # Thus, the mean difference is standardized because x/1=x
    sd <- sqrt(1 - (1*3/(4^2)*(sequence[i2]^2)))
    y <- rnorm(543, c(0,sequence[i2],0,0)[x], sd = sd)
    mat[i,1] <- ifelse(summary(lm(y~x))$coefficients[2,4]<.05, 1, 0)
    mat[i,2] <- summary(lm(scale(y)~x))$coefficients[2,1]
    mat[i,3] <- sd(y, na.rm = T)
  }
  mat2[,1] <- mean(mat[,1])
  mat2[,2] <- mean(mat[,2])
  mat2[,3] <- mean(mat[,3])
  mat2[,4] <- sequence[i2]/sd
  mat2
}
stopCluster(cl)
rownames(mat2) <- sequence[1:nrow(mat2)]
colnames(mat2) <- c("Power", "Standardized_Mdiff", "SD_Y", "cohen_d")
matE1 <- mat2
matE1

## Experiment 2 ----
iterations <- 1e4
mat2 <- matrix(NA, ncol = 7)
xr <- c(rep(c(0), 1251), rep(c(1), 1344))
cl<-makeCluster(use_cores)
registerDoParallel(cl)
sequence <- seq(.15, .3, .01)
mat2 <- foreach(i2=1:length(sequence), .combine=rbind
) %dopar%  {
  mat <- matrix(NA, nrow = iterations, ncol = 6)
  for(i in 1:iterations) {
    # randomly assign condition assignment
    x <- as.factor(sample(c(1,2,3,4), 1251+1344, replace = T))
    # simulate outcome variable. Mean of the value of the sequence for condition # 2 and 0 for other conditions
    # SD calculation ensures that the population standard deviation is 1 (as opposed to the SD being 1 for each group) 
    # Thus, the mean difference is standardized because x/1=x
    sd <- sqrt(1 - (1*3/(4^2)*(sequence[i2]^2)))
    y <- rnorm(1251+1344, c(0,sequence[i2],0,0)[x], sd = sd)
    mat[i,1] <- ifelse(summary(lm(y~x*xr))$coefficients[2,4]<.05, 1, 0)
    mat[i,2] <- summary(lm(scale(y)~x*xr))$coefficients[2,1]
    mat[i,3] <- sd(y[xr==0], na.rm = T)
    mat[i,4] <- ifelse(summary(lm(y~x*I(abs(1-xr))))$coefficients[2,4]<.05, 1, 0)
    mat[i,5] <- summary(lm(scale(y)~x*I(abs(1-xr))))$coefficients[2,1]
    mat[i,6] <- sd(y[xr==1], na.rm = T)
  }
  mat2[,1] <- mean(mat[,1])
  mat2[,2] <- mean(mat[,2])
  mat2[,3] <- mean(mat[,3])
  mat2[,4] <- mean(mat[,4])
  mat2[,5] <- mean(mat[,5])
  mat2[,6] <- mean(mat[,6])
  mat2[,7] <- sequence[i2]/sd
  mat2
}
stopCluster(cl)
rownames(mat2) <- sequence[1:nrow(mat2)]
colnames(mat2) <- c("Power_b", "Standardized_Mdiff_b", "SD_Y_b","Power_w", "Standardized_Mdiff_w", "SD_Y_w", "cohen_d")
matE2 <- mat2
matE2

## Experiment 3 ----
iterations <- 1e4
cl<-makeCluster(use_cores)
registerDoParallel(cl)
sequence <- seq(.2, .4, .01)
mat2 <- matrix(NA, ncol = 4)
mat2 <- foreach(i2=1:length(sequence), .combine=rbind
) %dopar%  {
  mat <- matrix(NA, nrow = iterations, ncol = 3)
  for(i in 1:iterations) {
    # randomly assign condition assignment
    x <- as.factor(sample(c(1,2,3,4), 650, replace = T))
    # simulate outcome variable. Mean of the value of the sequence for condition # 2 and 0 for other conditions
    # SD calculation ensures that the population standard deviation is 1 (as opposed to the SD being 1 for each group) 
    # Thus, the mean difference is standardized because x/1=x
    sd <- sqrt(1 - (1*3/(4^2)*(sequence[i2]^2)))
    y <- rnorm(650, c(0,sequence[i2],0,0)[x], sd = sd)
    mat[i,1] <- ifelse(summary(lm(y~x))$coefficients[2,4]<.05, 1, 0)
    mat[i,2] <- summary(lm(scale(y)~x))$coefficients[2,1]
    mat[i,3] <- sd(y, na.rm = T)
  }
  mat2[,1] <- mean(mat[,1])
  mat2[,2] <- mean(mat[,2])
  mat2[,3] <- mean(mat[,3])
  mat2[,4] <- sequence[i2]/sd
  mat2
}
stopCluster(cl)
rownames(mat2) <- sequence[1:nrow(mat2)]
colnames(mat2) <- c("Power", "Standardized_Mdiff", "SD_Y", "cohen_d")
matE3 <- mat2
matE3

## Experiment 4 ----
iterations <- 1e4
cl <- makeCluster(use_cores)
registerDoParallel(cl)
sequence <- seq(.2, .4, .01)
mat2 <- matrix(NA, ncol = 4)
mat2 <- foreach(i2=1:length(sequence), .combine=rbind
) %dopar%  {
  mat <- matrix(NA, nrow = iterations, ncol = 3)
  for(i in 1:iterations) {
    # randomly assign condition assignment
    x <- as.factor(sample(c(1,2,3), 519, replace = T))
    # simulate outcome variable. Mean of the value of the sequence for condition # 2 and 0 for other conditions
    # SD calculation ensures that the population standard deviation is 1 (as opposed to the SD being 1 for each group) 
    # Thus, the mean difference is standardized because x/1=x
    sd <- sqrt(1 - (1*2/(3^2)*(sequence[i2]^2)))
    y <- rnorm(519, c(0,sequence[i2],0,0)[x], sd = sd)
    mat[i,1] <- ifelse(summary(lm(y~x))$coefficients[2,4]<.05, 1, 0)
    mat[i,2] <- summary(lm(scale(y)~x))$coefficients[2,1]
    mat[i,3] <- sd(y, na.rm = T)
  }
  mat2[,1] <- mean(mat[,1])
  mat2[,2] <- mean(mat[,2])
  mat2[,3] <- mean(mat[,3])
  mat2[,4] <- sequence[i2]/sd
  mat2
}
stopCluster(cl)
rownames(mat2) <- sequence[1:nrow(mat2)]
colnames(mat2) <- c("Power", "Standardized_Mdiff", "SD_Y", "cohen_d")
matE4 <- mat2
matE4

