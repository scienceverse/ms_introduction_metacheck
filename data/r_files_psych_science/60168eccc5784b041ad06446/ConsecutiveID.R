ConsecutiveID <- function(id1) {
  idlist <- sort(unique(id1))
  id <- sapply(id1, FUN=function(x) {x <- which(idlist==x)})  # assigns subjects consecutive ids
  return(id)
}