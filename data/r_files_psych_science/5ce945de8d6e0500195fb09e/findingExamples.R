# Loading data
# Preparing
subNo                <- 1:6
N                    <- length(subNo)
numberObjects        <- 20
kitchenOjects        <- 1:12
numberKitchenObjects <- length(kitchenOjects)
nonKitchenObjets     <- 13:30
locationRatings      <- array(data = NA, dim = c(numberObjects, numberObjects, N))

# Sequently loading data
for(i in 1:N){
  locationRatings[,,i] <- matrix(scan(paste('U:/Projects/schemaVR/schemaVR3/preparation/schemaVR3_stimulusSelection/data/locationRatings_', as.character(subNo[i]) ,'.dat', sep = '')), byrow = TRUE, ncol = 20)
}

# Calculating mean rank per object/ location
meanRatings <- apply(locationRatings, 1:2, mean)
meanRatings <- round(meanRatings)
which(meanRatings == max(meanRatings), arr.ind = T)
which(meanRatings == min(meanRatings), arr.ind = T)
which(meanRatings == 0, arr.ind = T)
meanRatings[, 13]
