library(geoR)
library(brms) 
library(dplyr)
library(bayesboot)
library(bayestestR)
library(ggplot2)
library(emmeans)
library(patchwork)
### CREATE LINGUISTIC COVARIANCE MATRIX

# load distance matrix
lingDist <- as.matrix(read.csv("....", row.names = 1, na.strings = NULL))
# from distance to covariance
A_ling <- varcov.spatial(dists.lowertri = lingDist[lower.tri(lingDist)], cov.pars = c(1, 0.5))$varcov
#Fixing non-positive definite
cholStatus <- try(u <- chol(A_ling), silent = FALSE)
cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
newMat <- A_ling
iter <- 0
while (cholError) {
  iter <- iter + 1
  cat("iteration ", iter, "\n")
  # replace -ve eigen values with small +ve number
  newEig <- eigen(newMat)
  newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)
  # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
  # eig vectors
  newMat <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)
  # normalize modified matrix eqn 6 from Brissette et al 2007
  newMat <- newMat/sqrt(diag(newMat) %*% t(diag(newMat)))
  # try chol again
  cholStatus <- try(u <- chol(newMat), silent = TRUE)
  cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}
A_ling <- newMat
rownames(A_ling) <- colnames(A_ling) <- colnames(lingDist)
save(A_ling, file = "A_ling.rda")

### CREATE geographic distance COVARIANCE MATRIX
# load distance matrix
geoDist <- as.matrix(read.csv("...", row.names = 1, na.strings = NULL))
# convert to distance
geoDist <- geoDist/1000
diag(geoDist) <- 0
# from distance to covariance
A_geo <- varcov.spatial(dists.lowertri = geoDist[lower.tri(geoDist)],cov.pars = c(1,0.5))$varcov
# give A_geo row and col names
rownames(A_geo) <- colnames(A_geo) <- colnames(geoDist)
save(A_geo, file = "A_geo.rda")
