# Comparing the correlation coefficients ----------------------------------
source("Analysis/SvD Analysis_1b.R", echo  = TRUE)

# Copy the data from "SvDvA Analysis_mec.R" to work on this independently
dcor <- dlong

# Create the correlation variables of the signed errors
dcor$guess_1_cor <- dcor$guess_1 - dcor$answer
dcor$guess_2_cor <- dcor$guess_2 - dcor$answer

# Generate a subset list of each participant
subsets <- split(dcor, dcor$id)

# Create an r index for each participant 
results_r <- as.data.frame(matrix(ncol=1,nrow=length(subsets)))#where to save the r index
colnames(results_r) <- c("r")

# Get the r for each participant
for (i in 1:length(subsets)){ 
  m1 <- cor.test(subsets[[ c(i, 19) ]],
                 subsets[[ c(i, 20) ]],
                 method = "pearson")#the model
  results_r[i,] <- m1$estimate #extract r estimate
}

# Add the r's to the wide dataset 
dcorwide <- d
dcorwide <- bind_cols(dcorwide, results_r["r"])

# DO the t-test 
t.test(r ~ condition, data = dcorwide)
14.63*sqrt((1/445)+(1/449))

