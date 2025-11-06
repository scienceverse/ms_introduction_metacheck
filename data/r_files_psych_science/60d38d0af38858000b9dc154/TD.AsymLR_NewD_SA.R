TD.AsymLR_NewD_SA <- function(trialN, feat, rew, weight, sigP=c(0,0), param){
  
  pos_lr = param[1]
  neg_lr = param[2]
  dec = param[3]
  pos_lr = as.numeric(pos_lr)
  neg_lr = as.numeric(neg_lr)
  dec = as.numeric(dec)
  if(sigP[1]==1){
   slo = param[4]
   slo = as.numeric(slo)
   slo = 1 / slo
  }else if(sigP[1]==0){
    slo = 1
  }
  if(sigP[2]==1){
    if(sigP[1]==1){
      shi = param[5]
      shi = as.numeric(shi)
    }else if(sigP[1]==0){
      shi = param[4]
      shi = as.numeric(shi)
    }
  }else if(sigP[2]==0){
    shi = 0
  }
  
  N <- length(feat) # rows based on trials
  D <- max(feat) # columns based on cues/features presented
  w <- matrix(weight, 1, D) # row of 0 for associative weights for each cue/feature
  
  # create output dataframe
  x = c('LoopN', 'trialInClust', 'trialNum', 'w1','w2','w3','w4','w5','RPE','Value')
  TD.Out <- matrix(ncol = length(x), nrow = N) # loop number, cue/feature number, total trial number, weights, prediction error, and value estimate
  colnames(TD.Out) <- x
  
  floorCeiling <- function(estimate){
    if(estimate <= 1){
      estimate <- 1
    }else if(estimate >= 7){
      estimate <- 7
    }
    return(estimate)
  }
  
  sigTrans <- function(feed, slope=1, shift=0){
    feed = feed - 4
    output = 1 / (1 + exp(slope * -(feed) - shift ) )
    output = (output * 6) + 1
    return(output)
  }
  
  for (n in 1:N) { # for each trial in all of the trials
    S = feat[n] # current feature/cue
    w <- w*(dec) + 4*(1-dec)
    V = w[S] # value estimate is the associative weight of the current feature
    V = floorCeiling(V)
    feed = sigTrans(rew[n],slope=slo,shift=shi)
    RPE = feed - V # reward prediction error is the reward of current feature minus the value estimate of current feature
    if (RPE <= 0) { # if the reward prediction error is negative, use negative learning rate
      w[S] = w[S] + neg_lr * RPE # the updated associative weight is the current feature's associative weight plus the prediction error weighted by learning rate
    } else { # if the reward prediction error is positive, use the positive learning rate
      w[S] = w[S] + pos_lr * RPE # the updated associative weight is the current feature's associative weight plus the prediction error weighted by learning rate
    }
    
    trialClust = sum(feat[1:n] == S) # get trial number for specific feature by summing all of it's appearances thus far
    trial = trialN[n]; # total trial number over all features
    
    # append row containing trial data to output dataframe
    TD.Out[n,] <- c(n, trialClust, trial, w[1], w[2], w[3], w[4], w[5], RPE, V)
  }
  TD.Out <- as.data.frame(TD.Out)
  colnames(TD.Out) <- x
  return(TD.Out)
}
