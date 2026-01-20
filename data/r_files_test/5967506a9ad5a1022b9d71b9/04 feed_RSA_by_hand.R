library(RSA)

#first run "03 multilevel models"
#note: export with 900 x 500 resolution

#extraversion
plotRSA(x = r.inc_e2@beta[2], y = r.inc_e2@beta[3], x2 = r.inc_e2@beta[4], y2 = r.inc_e2@beta[5], xy = r.inc_e2@beta[6], b0 = r.inc_e2@beta[1], zlim=c(-.5,.5), xlab = "Actual Personality", ylab = "Demanded Personality", zlab = "Log income (residual)", main = "Extraversion", legend=FALSE)

#agreeableness
plotRSA(x = r.inc_a2@beta[2], y = r.inc_a2@beta[3], x2 = r.inc_a2@beta[4], y2 = r.inc_a2@beta[5], xy = r.inc_a2@beta[6], b0 = r.inc_a2@beta[1], zlim=c(-.5,.5), xlab = "Actual Personality", ylab = "Demanded Personality", zlab = "Log income (residual)", main = "Agreeableness", legend=FALSE)

#conscientiousness
plotRSA(x = r.inc_c2@beta[2], y = r.inc_c2@beta[3], x2 = r.inc_c2@beta[4], y2 = r.inc_c2@beta[5], xy = r.inc_c2@beta[6], b0 = r.inc_c2@beta[1], zlim=c(-.5,.5), xlab = "Actual Personality", ylab = "Demanded Personality", zlab = "Log income (residual)", main = "Conscientiousness", legend=FALSE)

#stability
plotRSA(x = r.inc_s2@beta[2], y = r.inc_s2@beta[3], x2 = r.inc_s2@beta[4], y2 = r.inc_s2@beta[5], xy = r.inc_s2@beta[6], b0 = r.inc_s2@beta[1], zlim=c(-.5,.5), xlab = "Actual Personality", ylab = "Demanded Personality", zlab = "Log income (residual)", main = "Emotional stability", legend=FALSE)

#openness
plotRSA(x = r.inc_o2@beta[2], y = r.inc_o2@beta[3], x2 = r.inc_o2@beta[4], y2 = r.inc_o2@beta[5], xy = r.inc_o2@beta[6], b0 = r.inc_o2@beta[1], zlim=c(-.5,.5), xlab = "Actual Personality", ylab = "Demanded Personality", zlab = "Log income (residual)", main = "Openness to experience", legend=FALSE)
