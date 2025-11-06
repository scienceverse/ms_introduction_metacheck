#---------normalize a vector by deviding the vector by the square root of the sum of squares 
get.norm <- function(x) {
  dist <- x
  for(i in 1:nrow(x)){ dist[i,] <- x[i,]/sqrt(t(x[i,]) %*% x[i,]) }
  return(dist)
}


#----------------------  printP  ---------------------------------------
printP <- function(pvalue){
  if (pvalue != 0){
    printP <- paste("p = ", pvalue, sep = "")
  }  else {
    printP <- "p < .001"
  }
  return(printP)
}

#--------------------      mmlr    -----------------------------------
mmlr <- function(Y, X, subject_ids){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  #subid = 1:70
  similarity <- numeric(length(subject_ids))
  prediction <- matrix(0,length(subject_ids), ncol(Y))
  error <- matrix(0,length(subject_ids),ncol(Y))
  #fa <- numeric(length(subject_ids))
  for(i in 1:length(subject_ids)){
    
    X_cur_train <- cbind(1, X[-i,])
    Y_cur_train <- Y[-i,]
    
    X_cur_test <- c(1, X[i, ])
    Y_cur_test <- Y[i, ]
    
    # B is a K+1 x M matrix (weight?)
    B <- ((ginv(t(X_cur_train) %*% X_cur_train)) %*% t(X_cur_train)) %*% Y_cur_train
    # prediction is a 1xM row vector
    prediction[i, ] <- X_cur_test %*% B
    
    #-------------------------------------------------
    # get the errors and angular similarity of prediction and test
    # for errors: 10 values per body(on each shape dimension)
    # for similarity: 1 value per body
    
    error[i, ] <- Y_cur_test - prediction[i, ]
    
    #false_alarms <- norm_Y_train %*% t(prediction[i, ])
    #fa[i] <- mean(false_alarms)
    similarity[i] <- get.norm(matrix(Y_cur_test, nrow = 1)) %*% t(get.norm(matrix(prediction[i, ], nrow = 1)))
    
  }
  return(list(Y = Y, E = error, similarity = similarity))
}




# -----------------------fem.plot --------------------------
fem.biplot <- function (CAres){
  
  #because female traits are from positive to negative value, for ease of visualization, better flip it.
  CAres$ExPosition.Data$fj <- -CAres$ExPosition.Data$fj
  
  
  p1 <- prettyPlot(CAres$ExPosition.Data$fj[ ,1:2],contributionCircles = T,
                   contributions = CAres$ExPosition.Data$cj,
                   dev.new = T, new.plot = T, pch = 16, cex = 1.2,
                   constraints = lapply(CAres$Plotting.Data$constraints, "*", 1.3),
                   pos = c(4,3,1,3,3,
                           2,1,2,1,3,
                           1,2,4,3,2,
                           4,3,3,2,3,
                           3,3,4,4,3,
                           1,3,3,4,2),
                   col = c(ex,ex,ex,ex,ex,ex,
                           ag,ag,ag,ag,ag,ag,
                           con,con,con,con,con,con,
                           ne,ne,ne,ne,ne,ne,
                           op,op,op,op,op,op))
  # main="Female Biplot ") #xlab=paste('Component 1: ',
  #round(CAres$ExPosition.Data$t[1],2),'%', sep = ""),
  #ylab=paste('Component 2: ',
  #round(CAres$ExPosition.Data$t[2],2),'%', sep = ""))
  
  
  CAres$ExPosition.Data$fi <- -CAres$ExPosition.Data$fi
  
  #Plot the bodies, with the male and female bodies seperated
  p2 <- prettyPlot(CAres$ExPosition.Data$fi[ ,1:2],
                   contributionCircles = F,
                   contributions = CAres$ExPosition.Data$ci,
                   col = "grey", display_names = F, 
                   cex = 0.6, pch = 21,
                   dev.new = F, new.plot = F)
  #main='Male Body Space ',xlab=paste('Component 1: ',
  #                                   round(CAres$ExPosition.Data$t[1],2),'%'),
  #ylab=paste('Component 2: ',
  #           round(CAres$ExPosition.Data$t[2],2),'%'))
  
  #Add a legend to the plot
  #locator(1) gives you the right to point the location with your mouse.
  color=c('red','violetred','springgreen3','steelblue3','peru')
  leg <- legend('bottomright', 
                legend=c('Extravertion', 'Conscientiousness', 
                         'Agreeableness','Openess','Neuroticism'), 
                col=color,text.col = color, 
                pch =16,cex = 0.8,
                y.intersp=0.3,plot = FALSE)
  
  # adjust as desired
  leftx <- leg$rect$left
  # from the leftx to rightx, one could change the width of the box
  rightx <- (leg$rect$left + leg$rect$w) * 0.6
  # from the lefty to righty, one could change the height of the box
  topy <- leg$rect$top
  bottomy <- (leg$rect$top - leg$rect$h) * 1
  
  # use the new coordinates to define custom
  p3 <- legend(x = c(leftx, rightx), y = c(topy, bottomy) + 0.15,
               legend=c('Extravertion', 'Conscientiousness', 
                        'Agreeableness','Openess','Neuroticism'), 
               col=color,text.col = color, 
               pch =16,cex = 0.8,
               y.intersp=0.3)
  
}




#------------------       male.plot         ------------------------------
male.biplot <- function (CAres){
  
  p1 <- prettyPlot(CAres$ExPosition.Data$fj[ ,1:2],contributionCircles = T,
                   contributions = CAres$ExPosition.Data$cj,
                   dev.new = T, new.plot = T, pch = 16,cex = 1.2,
                   constraints = lapply(CAres$Plotting.Data$constraints, "*", 1.3),
                   pos = c(4,3,1,3,3,
                           2,1,2,3,3,
                           1,2,4,3,2,
                           4,3,3,2,3,
                           3,3,4,4,1,
                           2,3,3,3,2),
                   col = c(ex,ex,ex,ex,ex,ex,
                           ag,ag,ag,ag,ag,ag,
                           con,con,con,con,con,con,
                           ne,ne,ne,ne,ne,ne,
                           op,op,op,op,op,op))
  #main="Male Biplot ",xlab=paste('Component 1: ',
  #                                  round(CAres$ExPosition.Data$t[1],2),'%', sep = ""),
  #ylab=paste('Component 2: ',
  #            round(CAres$ExPosition.Data$t[2],2),'%', sep = ""))
  
  
  #Plot the bodies, with the male and female bodies seperated
  p2 <- prettyPlot(CAres$ExPosition.Data$fi[ ,1:2],
                   contributionCircles = F,
                   contributions = CAres$ExPosition.Data$ci,
                   col = "grey", display_names = F, 
                   cex = 0.5, pch = 21,
                   dev.new = F, new.plot = F)
  # main='Male Body Space ',xlab=paste('Component 1: ',
  #                                   round(CAres$ExPosition.Data$t[1],2),'%'),
  # ylab=paste('Component 2: ',
  #            round(CAres$ExPosition.Data$t[2],2),'%'))
  
  
  
  #Add a legend to the plot
  #locator(1) gives you the right to point the location with your mouse.
  color=c('red','violetred','springgreen3','steelblue3','peru')
  leg <- legend('bottomright', 
                legend=c('Extravertion', 'Conscientiousness', 
                         'Agreeableness','Openess','Neuroticism'), 
                col=color,text.col = color, 
                pch =16,cex = 0.8,
                y.intersp=0.3,plot = FALSE)
  
  # adjust as desired
  leftx <- leg$rect$left
  # from the leftx to rightx, one could change the width of the box
  rightx <- (leg$rect$left + leg$rect$w) * 0.55
  # from the lefty to righty, one could change the height of the box
  topy <- leg$rect$top
  bottomy <- (leg$rect$top - leg$rect$h) * 1
  
  # use the new coordinates to define custom
  p3 <- legend(x = c(leftx, rightx) + 0.2, y = c(topy, bottomy) + 0.05,
               legend=c('Extravertion', 'Conscientiousness', 
                        'Agreeableness','Openess','Neuroticism'), 
               col=color,text.col = color, 
               pch =16,cex = 0.8,
               y.intersp=0.3)
  
}