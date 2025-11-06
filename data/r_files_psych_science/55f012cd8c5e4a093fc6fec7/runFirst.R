# libraries
for(pack in c("MBESS", "ez", "psych")) {
  if(!(pack %in% installed.packages()[,1])) {
    install.packages(pack)  
  }  
}
library("MBESS")
library("ez")
library("psych")

#-------------------------------------------------------------------------
# function for formating p-values
round_p <- function(x) {
  if(x > 0.01) return(round(x, 2))
  if(x < 0.0001) {
    return(x)
  } else {
    if(x < 0.001) {
      return(round(x, 4))
    } else {
      return(round(x, 3))
    }
  }
}

#-------------------------------------------------------------------------
# function that returns ouput of t-test
tTest <- function(group1, group2 = NULL, paired = T) {
  if(is.null(group2)) {
    t <- t.test(group1)
  } else {
    t <- t.test(group1, group2, paired = paired, var.equal = T)
  }
  if(paired) {
    cis <- ci.sm(ncp = t$statistic, N = t$parameter + 1)
  } else {
    cis <- ci.smd(ncp = t$statistic,
                  n.1 = length(group1),
                  n.2 = length(group2)
                  )
  }
  cat("t(", t$parameter, ") = ", sprintf("%.2f", t$statistic), 
      ", p = ", round_p(t$p.value),
      ", d = ", sprintf("%.2f", ifelse(paired, cis$Standardized, cis$smd)),
      ", 95% CI [", sprintf("%.2f", cis$Lower), 
      ", ", sprintf("%.2f", cis$Upper), "]",
      sep = "")  
}

# function that returns ouput of paired t-test
pairedTest <- function(group1, group2) {
  tTest(group1, group2, paired = T)
}

# function that returns output of one sample t-test
singleTest <- function(group) {tTest(group)}

# function that returns output of two sample un-paired t-test
unpairedTest <- function(group1, group2) {
  tTest(group1, group2, paired = F)
}

#-------------------------------------------------------------------------
# graph
makeGraph <- function(data, labels, groupedLabels) {
  par(las = 1)
  plot(0,0, axes = F, ann = F, type = "n", xlim = c(0,5), ylim = c(-1,1))
  
  axis(2, at = (-2:2)*0.5, labels = (-2:2)*0.5, pos = 0, cex.axis = 1.8)
  axis(1, labels = F, tick = T, pos = 0, lwd.ticks = 0)
  axis(1, at = c(0.7, 1.7, 3.2, 4.2), labels = labels, 
       tick = F, pos = -1, lwd.ticks = 0, cex.axis = 1.7)
  axis(1, at = c(1.2, 3.7), labels = groupedLabels, 
       tick = F, pos = -1.2, lwd.ticks = 0, cex.axis = 2)
 
  for(i in 1:4) {
    x <- i - 0.3 + (i > 2)*0.5
    color <- c("white", "grey")[-i%%2 + 1]
    rect(x-0.5, 0, x+0.5, mean(data[[i]]), col = color)
    arrows(x, t.test(data[[i]])$conf.int[1], 
           x, t.test(data[[i]])$conf.int[2], 
           angle = 90, code = 3, length = 0.1, lwd = 1.5)    
  }
  mtext("Difference in average ratings", side = 2, line = 2.5, las = 3, cex = 1.75)
}