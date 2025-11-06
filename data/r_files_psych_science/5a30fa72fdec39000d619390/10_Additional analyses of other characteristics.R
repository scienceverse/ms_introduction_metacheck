# Additional exploratory analyses

# Notice that these analyses were not preregistered and not initially planned for.
# During the review process, a reviewer suggested that we additionally explore
# other potentially relevant dimensions of a successful pursuit of happiness,
# which is why we once again coded the active texts for various types of
# pursuits

#######################################
# Rater agreement
#######################################

library(readxl)
explore_j <- read_excel("Revision_Texts_to_be_coded_JMR.xlsx")
explore_s <- read_excel("Revision_Texts_to_be_coded_Sabine.xlsx")
explore_m <- read_excel("Revision_Texts_to_be_coded_Michael.xlsx")
explore_comb <- explore_j[, 1:2]
names(explore_comb) <- c("id", "text")

kappam.fleiss(cbind(
  explore_j[,"Material"],
  explore_s[,"Material"],
  explore_m[,"Material"]
  ))
# .606
explore_comb$material <- rowSums(cbind(explore_j[,"Material"], explore_s[,"Material"], explore_m[,"Material"]))
explore_comb$material_d <- ifelse(explore_comb$material > 1, 1, 0)

kappam.fleiss(cbind(
  explore_j[,"Experiential"],
  explore_s[,"Experiential"],
  explore_m[,"Experiential"]
  ))
# .62

explore_comb$experiential <- rowSums(cbind(explore_j[,"Experiential"], explore_s[,"Experiential"], explore_m[,"Experiential"]))
explore_comb$experiential_d <- ifelse(explore_comb$experiential > 1, 1, 0)


kappam.fleiss(cbind(
  explore_j[,"Pro-Social Pursuits"],
  explore_s[,"Pro-Social Pursuits"],
  explore_m[,"Pro-Social Pursuits"]
  ))
# .67
explore_comb$prosoc <- rowSums(cbind(explore_j[,"Pro-Social Pursuits"], explore_s[,"Pro-Social Pursuits"], explore_m[,"Pro-Social Pursuits"]))
explore_comb$prosoc_d <- ifelse(explore_comb$prosoc > 1, 1, 0)

kappam.fleiss(cbind(
  explore_j[,"Personal Growth"],
  explore_s[,"Personal Growth"],
  explore_m[,"Personal Growth"]
  ))
# .09
# Makes no sense to work with this



#######################################
# Linking
#######################################
# This again requires linking from the unlinked
# Text files to the newly generated variables

final_explore <- read.csv("final_explore.csv")

summary(lm(ls15 ~ ls14 + material_d,
           data = final_explore))
# -0.48, p = .09
-0.48102/sd(c(final_explore$ls15[final_explore$active == 1],
              final_explore$ls14[final_explore$active == 1]), na.rm = TRUE)


summary(lm(ls15 ~ ls14 + experiential_d,
           data = final_explore))
# .15, p = .223
0.14594/sd(c(final_explore$ls15,
             final_explore$ls14))


summary(lm(ls15 ~ ls14 + prosoc_d,
           data = final_explore))
# .28, p = .236
0.27930/sd(c(final_explore$ls15,
             final_explore$ls14))


summary(lm(ls15 ~ ls14 + prosoc_d + social,
           data = final_explore))
# social still significant at .31, p = .001

summary(lm(ls15 ~ ls14 + material_d + experiential_d + prosoc_d + social,
           data = final_explore))
# Marginally significant negative effect of material pursuits
# Still significant effect of social pursuits
-0.55586/sd(c(final_explore$ls15,
             final_explore$ls14))

0.34145/sd(c(final_explore$ls15,
              final_explore$ls14))

0.01317/sd(c(final_explore$ls15,
             final_explore$ls14))

0.04598/sd(c(final_explore$ls15,
             final_explore$ls14))