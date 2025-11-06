# Summary: Create Mplus-ready version of Kok et al. (2013) dataset with
# winsorized change score variable, after removing T2 HF-HRV values for 557004
# and 557027 and correcting the T2 HF-HRV value for 557055

# Define directories
datadir <- "../raw_data/"
outdir <- "../analysis_data/"

# Load required packages/files
library(psych)

# Import data
ps2013 <- read.csv(file = paste0(datadir, "2013_psych_science.csv"), header = T)

# Remove T2 values for PID #557004, #557027
ps2013[ps2013$ID %in% c(557004, 557027), "hf_HRV_t2"]
ps2013$hf_HRV_t2[ps2013$ID %in% c(557004, 557027)] <- NA
ps2013[ps2013$ID %in% c(557004, 557027), "hf_HRV_t2"] # Good.

# Change PID 557055 T2 value (ectopic beat)
ps2013[ps2013$ID == 557055, "hf_HRV_t2"]
ps2013[ps2013$ID == 557055, "hf_HRV_t2"] <- 0.00016682
ps2013[ps2013$ID == 557055, "hf_HRV_t2"] # Good.

# Recalculate HRV_T2_SQRT_Z
ps2013$HRV_T2_SQRT_Z <- scale(sqrt(ps2013$hf_HRV_t2))

# Create change score variable with full dataset, as in Kok et al. (2013)
ps2013$HRV_Z <- NA
HRV_Z <- rstandard(lm(HRV_T2_SQRT_Z ~ HRV_T1_SQRT_Z, data = ps2013))
ps2013[names(HRV_Z), "HRV_Z"] <- HRV_Z

# Winsorize change score variable (95%)
ps2013$HRV_Z_WIN <- winsor(ps2013$HRV_Z, trim = .05)

# Output data in Mplus-readable format:
write.table(ps2013,
            file = paste0(outdir, "2013_psych_science_winsorized.dat"),
            quote = F, row.names = F, col.names = F, sep = "\t", na = "-999")

# Output data in csv format (for creation of histogram):
# Output data in Mplus-readable format:
write.table(ps2013,
            file = paste0(outdir, "2013_psych_science_winsorized.csv"),
            quote = F, row.names = F, na = "", sep = ",")