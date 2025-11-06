#########################################
# Generate Indices
#########################################

final$domains14 <- rowMeans(final[,c("satis_health14", "satis_sleep14", "satis_income14",
                                     "satis_dwelling14", "satis_leisure14", 
                                     "satis_family14")])

final$domains15 <- rowMeans(final[,c("satis_health15", "satis_sleep15", "satis_income15",
                                     "satis_dwelling15", "satis_leisure15", 
                                     "satis_family15")])

final$negative14 <- rowMeans(final[, c("irritated14", "anxious14", "sad14")])

final$negative15 <- rowMeans(final[, c("irritated15", "anxious15", "sad15")])

##################
# Single items
summary(lm(domains15 ~ domains14 + social, data = final[final$active == 1,]))
summary(lm(negative15 ~ negative14 + social, data = final[final$active == 1,]))
summary(lm(happy15 ~ happy14 + social, data = final[final$active == 1,]))
