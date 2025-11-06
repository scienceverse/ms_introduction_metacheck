mplus <- final[final$active == 1, c("social", "ls14", "ls15", "affil15")]
# Recode affil15 so that higher values mean more socializing
mplus$affil15 <- 5 - mplus$affil15

write.table(mplus, 
            file = "mplus.csv", 
            col.names = FALSE, row.names = FALSE,
            sep = ",")
