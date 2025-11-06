# Comparing the correlation coefficients ----------------------------------

# Copy the data from "SvDvA Analysis_4.R" to work on this independently
source("Analysis/SvDvA Analysis_4.R", echo  = TRUE)

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
  m1 <- cor.test(subsets[[ c(i, 16) ]],
                 subsets[[ c(i, 17) ]],
                 method = "pearson")#the model
  results_r[i,] <- m1$estimate #extract r estimate
}

# Add the r's to the wide dataset 
dcorwide <- dat
dcorwide <- bind_cols(dcorwide, results_r["r"])

# Do the t-test 
# Self vs. Agree 
t.test(r ~ condition, data = subset(dcorwide, dcorwide$condition != "Disagree"))
2.74*sqrt((1/611)+(1/620))

# Self vs. Disagree 
t.test(r ~ condition, data = subset(dcorwide, dcorwide$condition != "Agree"))
14.48*sqrt((1/605)+(1/620))

# Agree vs. Disagree 
t.test(r ~ condition, data = subset(dcorwide, dcorwide$condition != "Self"))
16.82*sqrt((1/605)+(1/611))
