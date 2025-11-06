# 2-parameter mixture model
MM2 <- function(parms, error) {
  x <- deg2rad(-179:180)
  Likelihood <- suppressWarnings(parms[1]*dvonmises(x,0,max(0,parms[2])) + (1-parms[1])*(1/(2*pi)))
  L <- Likelihood[error] # we can use all the errors as indices into Likelihood!
  Dev <- sum(-2*log(L))
  return(Dev)
}
