# Summary: Create Mplus version of Kok et al. (2013) dataset

# Define directories
inpdir <- "./"

# Import data
ps2013 <- read.csv(file = "2013_psych_science.csv", header = T)

# Output data in Mplus-readable format:
ps2013[is.na(ps2013) == T] <- -999 # Missing value flag
write.table(ps2013, file = "2013_psych_science.dat", quote = F,
            row.names = F, sep = "\t")