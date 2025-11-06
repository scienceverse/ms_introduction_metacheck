# Comparing the correlation coefficients ----------------------------------
source("Analysis/SvDvA Analysis_3.R", echo  = TRUE)

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

# Do the t-test 
# Self vs. Agree 
t.test(r ~ condition, data = subset(dcorwide, dcorwide$condition != "Disagree"))
1.13*sqrt((1/477)+(1/468))

# Self vs. Disagree 
t.test(r ~ condition, data = subset(dcorwide, dcorwide$condition != "Agree"))
12.08*sqrt((1/481)+(1/468))

# Agree vs. Disagree 
t.test(r ~ condition, data = subset(dcorwide, dcorwide$condition != "Self"))
11.01*sqrt((1/481)+(1/477))
