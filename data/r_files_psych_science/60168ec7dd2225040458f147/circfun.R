# Functions for circular data

deg2rad <- function(deg) {
  rad <- pi*deg/180
  return(rad)
}

rad2deg <- function(rad) {
  deg <- 180*rad/pi
  return(deg)
}

k2sd <- function (K) {
  S <- matrix(0,1,length(K))
  for (j in 1:length(K)) {
    if (K[j]==0) S[j] = Inf
    if (is.infinite(K[j])) S[j] = 0    
    if (K[j] >= 0 & !is.infinite(K[j])) {
      S[j] = sqrt(-2*log(besselI(K[j],1)/besselI(K[j],0)));    
    }
  }
  return(S)
}


sd2k <- function (S) {
  #kappa <- matrix(0,1,length(S))
  #for (j in 1:length(S)) {
  R = exp(-S^2/2)
  K = 1/(R^3 - 4 * R^2 + 3 * R)
  K[R < 0.85] = -0.4 + 1.39 * R[R < 0.85] + 0.43/(1 - R[R < 0.85])
  K[R < 0.53] = 2 * R[R < 0.53] + R[R < 0.53]^3 + (5 * R[R < 0.53]^5)/6    
  #kappa[j] <- K
  #}
  #kappa
  return(K)
}

wrap = function(angle, bound=pi) {
  wangle <- ( (angle + bound) %% (2*bound) ) - bound
  return(wangle)
}


