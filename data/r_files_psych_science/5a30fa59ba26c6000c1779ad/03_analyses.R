# Central analyses
# TL;DR:
# Bring variables together
# Central pre-registered comparison socia vs non-social
# Analysis comparing active strats vs. no active strat


#########################################
# Prepare Data
#########################################

# The following part requires the Innovation Sample data and the
# Link file provided by David. This documents the way I ran
# the analyses on the regular SOEP-IS data distribution. 
# However, you can just skip this part and continue
# with the analyses based on the shared files which can be found
# below in this file
#############
# library("foreign")
# inno_sample <- read.dta("p.dta",
#                      convert.factors=FALSE)
# Only keep relevant waves
# inno_sample <- inno_sample[inno_sample$syear == 2014 | inno_sample$syear == 2015,]
# Only keep relevant measures
# inno_sample <- inno_sample[, c("syear", "pid", "plh0182",
                               "pli0094", # frequency of socializing
                               "plh0171", "plh0172", "plh0175",
                               "plh0177", "plh0178", "plh0180",
                               "plh0184", "plh0185", "plh0186",
                               "plh0187")]

# names(inno_sample) <- c("syear", "pid", "ls",
#                        "affil", "satis_health", "satis_sleep",
#                        "satis_income", "satis_dwelling", "satis_leisure",
#                        "satis_family", "irritated", "anxious",
#                        "happy", "sad")
# Decode missings
# inno_sample[inno_sample < 0] <- NA

#############
# Linking file
# link <- read.csv("rated_data_linked.csv", sep = ";")

# final <- merge(link, inno_sample[inno_sample$syear == 2014,], all.x = TRUE)
# final$syear <- NULL
# final$affil <- NULL
# names(final)[4:14] <- paste(names(final)[4:14], "14", sep = "")
# final <- merge(final, inno_sample[inno_sample$syear == 2015,], all.x = TRUE)
# final$syear <- NULL
# names(final)[15:26] <- paste(names(final)[15:26], "15", sep = "")

# remove PID so that it is impossible to merge shared data to SOEP IS
# this step is just an additional precaution to comply with the
# data sharing policies of the SOEP.
# Please contact the first author (Julia Rohrer) if you want to link
# the ratings to the SOEP IS files.
# final <- subset(final, select = -pid)
# final <- final[complete.cases(final[, c("ls14", "ls15")]), ]

# write.table(final, file = "final_data.csv", 
#            row.names = FALSE, 
#            col.names = TRUE,
#            sep = ",")

##################################################################################
# Main Analyses
##################################################################################

final <- read.csv("final_data.csv")

#########################################
# Baseline differences
#########################################

t.test(final$ls14[final$active == 1 & final$social == 0], 
       final$ls14[final$active == 1 & final$social == 1])
# non-social 7.29 vs. social 7.57
# p = .043


library(effsize)
cohen.d(final$ls14[final$active == 1],
        as.factor(final$social[final$active == 1]),
        pooled = TRUE)

#########################################
# Paired t-tests within groups
#########################################
# non-social
t.test(final$ls15[final$active == 1 & final$social == 0], 
       final$ls14[final$active == 1 & final$social == 0],
       paired = TRUE)
# difference -0.02, p = .792
cohen.d(final$ls14[final$social == 0 & final$active == 1],
        final$ls15[final$social == 0 & final$active == 1],
        pooled = TRUE)

# social

t.test(final$ls15[final$active == 1 & final$social == 1], 
       final$ls14[final$active == 1 & final$social == 1],
       paired = TRUE)
cohen.d(final$ls14[final$social == 1],
        final$ls15[final$social == 1],
        pooled = TRUE)

# difference 0.19, p = .048

#########################################
# Comparison of difference scores
#########################################
final$diff <- final$ls15 - final$ls14

t.test(final$diff[final$active == 1 & final$social == 0],
       final$diff[final$active == 1 & final$social == 1])
# comparing -0.02 with 0.19, p = .085

cohen.d(final$diff[final$social == 1 & final$active == 1],
        final$diff[final$social == 0 & final$active == 1],
        pooled = TRUE)
#########################################
# ANCOVA
#########################################

summary(lm(ls15 ~ ls14 + social,
           data = final[final$active == 1,]))
# Coefficient of social is 0.33. p = .005

# Express coefficient in Units of SD
summary(lm(ls15 ~ ls14 + social,
           data = final[final$active == 1,]))$coef/sd(c(final$ls15[final$active == 1],
                                                        final$ls14[final$active == 1]))

confint(lm(ls15 ~ ls14 + social,
                   data = final[final$active == 1,]))/sd(c(final$ls15[final$active == 1],
                                                           final$ls14[final$active == 1]))



##################################################################################
# Additional: active vs no active strat
##################################################################################

#########################################
# Baseline differences
#########################################

t.test(final$ls14[final$active == 0], 
       final$ls14[final$active == 1])
# non-active 7.49 vs. active 7.38
# p = .219


cohen.d(final$ls14,
        as.factor(final$active),
        pooled = TRUE)

#########################################
# Paired t-tests within groups
#########################################
# non-active
t.test(final$ls15[final$active == 0], 
       final$ls14[final$active == 0],
       paired = TRUE)
# difference .10, p = .063
cohen.d(final$ls15[final$active == 0],
        final$ls14[final$active == 0],
        pooled = TRUE)
# d = .07

# active
t.test(final$ls15[final$active == 1], 
       final$ls14[final$active == 1],
       paired = TRUE)

cohen.d(final$ls15[final$active == 1],
        final$ls14[final$active == 1],
        pooled = TRUE)

# difference -.03, p = .441

#########################################
# Comparison of difference scores
#########################################

t.test(final$diff[final$active == 0],
       final$diff[final$active == 1])
# comparing .10 with 0.05, p = .493

cohen.d(final$diff[final$active == 1],
        final$diff[final$active == 0],
        pooled = TRUE)
# d = -.04
#########################################
# ANCOVA
#########################################

summary(lm(ls15 ~ ls14 + active,
           data = final))

# Coefficient of active is -.10 p = .177
summary(lm(ls15 ~ ls14 + active,
           data = final))$coef/sd(c(final$ls15,
                                    final$ls14))

confint(lm(ls15 ~ ls14 + active,
           data = final), level = .95)/sd(c(final$ls15,
             final$ls14))
# -.06 SD units





