###################################
### PERSON-JOB-FIT AND INCOME #####
###################################

##########################
#### FIT ANALYSES     ####
##########################


### LOAD PACKAGES AND DATA 

#input.1 = "C:/Users/mluhmann/Dropbox/SOEP_fit"
input.data = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data"
input.results = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Results"


library(ggplot2)
library(RSA)
library(lmerTest)
library(psych)

setwd(input.data)

valid <- read.delim("/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data/valid.csv")

# scale Big Five variables (to make them more conductive to RSA logic)
names(valid)
valid[,20:29] <- as.data.frame(scale(valid[,20:29]))
describe(valid[,20:29])

######### FIT ANALYSES: EXPERT RATINGS AS REFERENCE ################

attach(valid)

###############
##multilevel
###############

valid$per_e2 <- valid$per_e*valid$per_e
valid$per_a2 <- valid$per_a*valid$per_a
valid$per_c2 <- valid$per_c*valid$per_c
valid$per_s2 <- valid$per_s*valid$per_s
valid$per_o2 <- valid$per_o*valid$per_o

valid$ba2_e2 <- valid$ba2_e*valid$ba2_e
valid$ba2_a2 <- valid$ba2_a*valid$ba2_a
valid$ba2_c2 <- valid$ba2_c*valid$ba2_c
valid$ba2_s2 <- valid$ba2_s*valid$ba2_s
valid$ba2_o2 <- valid$ba2_o*valid$ba2_o

library(lme4)


### DO NOT CHANGE ORDER OF MODEL TERMS!!!

# Extraversion: random slopes for both linear and quadratic trend
r.inc_e2 = lmer(loginc.res ~  per_e + ba2_e + per_e*ba2_e + per_e2 + ba2_e2 
                 + (per_e + per_e2 | ISCO), data = valid)
summary(r.inc_e2)

# Agreeableness: random slopes for both linear and quadratic trend
r.inc_a2 = lmer(loginc.res ~  per_a + ba2_a + per_a*ba2_a + per_a2 + ba2_a2 
                 + (per_a + per_a2 | ISCO), data = valid)
summary(r.inc_a2)

# Conscientiousness: random slopes for only linear trend
r.inc_c2 = lmer(loginc.res ~  per_c + ba2_c + per_c*ba2_c + per_c2 + ba2_c2 
                + (per_c  | ISCO), data = valid)
summary(r.inc_c2)

# Emotional stability: random slopes for both linear and quadratic trend
r.inc_s2 = lmer(loginc.res ~  per_s + ba2_s + per_s*ba2_s + per_s2 + ba2_s2 
                + (per_s + per_s2 | ISCO), data = valid)
summary(r.inc_s2)

# Openness: random slopes for both linear and quadratic trend ##ATTENTION: I had to remove the quadratic trend
r.inc_o2 = lmer(loginc.res ~  per_o + ba2_o + per_o*ba2_o + per_o2 + ba2_o2 
                + (per_o | ISCO), data = valid)
summary(r.inc_o2)

#switch folder to results
setwd(input.results)

write.csv(coef(summary(r.inc_e2)), file = "Multilevel regression coefficients_E.csv")
write.csv(coef(summary(r.inc_a2)), file = "Multilevel regression coefficients_A.csv")
write.csv(coef(summary(r.inc_c2)), file = "Multilevel regression coefficients_C.csv")
write.csv(coef(summary(r.inc_s2)), file = "Multilevel regression coefficients_S.csv")
write.csv(coef(summary(r.inc_o2)), file = "Multilevel regression coefficients_O.csv")

save(r.inc_e2, file = "r.inc_e2.rdata")
save(r.inc_a2, file = "r.inc_a2.rdata")
save(r.inc_c2, file = "r.inc_c2.rdata")
save(r.inc_s2, file = "r.inc_s2.rdata")
save(r.inc_o2, file = "r.inc_o2.rdata")


### PLOT RANDOM SLOPES

pdf("/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Plots/Dotplots for random coefficients.pdf")
dotplot(ranef(r.inc_e2))
dotplot(ranef(r.inc_a2))
dotplot(ranef(r.inc_c2))
dotplot(ranef(r.inc_s2))
dotplot(ranef(r.inc_o2))
dev.off()


pdf("/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Plots/Spaghetti plots for random coefficients.pdf")

spag.e = coef(r.inc_e2)$ISCO
spag.a = coef(r.inc_a2)$ISCO
spag.c = coef(r.inc_c2)$ISCO
spag.s = coef(r.inc_s2)$ISCO
spag.o = coef(r.inc_o2)$ISCO


plot(NULL, xlim = c(-2,2), ylim = c(-1,1), xlab = "Extraversion", ylab = "Log-Income")
for(i in 1:nrow(spag.e)) {
  curve(spag.e[i,1] + spag.e[i,2]*x + spag.e[i,4]*x*x, add = T)
}


plot(NULL, xlim = c(-2,2), ylim = c(-1,1), xlab = "Agreeableness", ylab = "Log-Income")
for(i in 1:nrow(spag.a)) {
  curve(spag.a[i,1] + spag.a[i,2]*x + spag.a[i,4]*x*x, add = T)
}


plot(NULL, xlim = c(-2,2), ylim = c(-1,1), xlab = "conscientiousness", ylab = "Log-Income")
for(i in 1:nrow(spag.c)) {
  curve(spag.c[i,1] + spag.c[i,2]*x + spag.c[i,4]*x*x, add = T)
}

plot(NULL, xlim = c(-2,2), ylim = c(-1,1), xlab = "Emotional stability", ylab = "Log-Income")
for(i in 1:nrow(spag.s)) {
  curve(spag.s[i,1] + spag.s[i,2]*x + spag.s[i,4]*x*x, add = T)
}


plot(NULL, xlim = c(-2,2), ylim = c(-1,1), xlab = "Openness", ylab = "Log-Income")
for(i in 1:nrow(spag.o)) {
  curve(spag.o[i,1] + spag.o[i,2]*x + spag.o[i,4]*x*x, add = T)
}

dev.off()



### COMPUTE A1 to A4

# Input 1: Regression coefficients

beta.e = r.inc_e2@beta
beta.a = r.inc_a2@beta
beta.c = r.inc_c2@beta
beta.s = r.inc_s2@beta
beta.o = r.inc_o2@beta


# Input 2: Standard errors for regression coefficients

se.e = summary(r.inc_e2)$coef[,2]
se.a = summary(r.inc_a2)$coef[,2]
se.c = summary(r.inc_c2)$coef[,2]
se.s = summary(r.inc_s2)$coef[,2]
se.o = summary(r.inc_o2)$coef[,2]



# Input 3: Covariances between fixed effects

cov.e = summary(r.inc_e2)$vcov
cov.a = summary(r.inc_a2)$vcov
cov.c = summary(r.inc_c2)$vcov
cov.s = summary(r.inc_s2)$vcov
cov.o = summary(r.inc_o2)$vcov


# Create empty tables

e.rsa = data.frame(est = rep(NA, 4), se = NA)
row.names(e.rsa) = c("a1", "a2", "a3", "a4")

a.rsa = e.rsa
c.rsa = e.rsa
s.rsa = e.rsa
o.rsa = e.rsa


# Compute coefficients: extraversion

e.rsa$est[1] = beta.e[2] + beta.e[3]
e.rsa$est[2] = beta.e[4] + beta.e[5] + beta.e[6]
e.rsa$est[3] = beta.e[2] - beta.e[3]
e.rsa$est[4] = beta.e[4] + beta.e[5] - beta.e[6]

e.rsa$se[1] = sqrt(se.e[2]^2 + se.e[3]^2 + 2*cov.e[2,3])
e.rsa$se[2] = sqrt(se.e[4]^2 + se.e[5]^2 + se.e[6]^2 + 2*cov.e[4,5] + 2*cov.e[4,6] + 2*cov.e[5,6])
e.rsa$se[3] = sqrt(se.e[2]^2 + se.e[3]^2 - 2*cov.e[2,3])
e.rsa$se[4] = sqrt(se.e[4]^2 + se.e[5]^2 + se.e[6]^2 + 2*cov.e[4,5] - 2*cov.e[4,6] - 2*cov.e[5,6])


# Compute coefficients: Agreeableness

a.rsa$est[1] = beta.a[2] + beta.a[3]
a.rsa$est[2] = beta.a[4] + beta.a[5] + beta.a[6]
a.rsa$est[3] = beta.a[2] - beta.a[3]
a.rsa$est[4] = beta.a[4] + beta.a[5] - beta.a[6]

a.rsa$se[1] = sqrt(se.a[2]^2 + se.a[3]^2 + 2*cov.a[2,3])
a.rsa$se[2] = sqrt(se.a[4]^2 + se.a[5]^2 + se.a[6]^2 + 2*cov.a[4,5] + 2*cov.a[4,6] + 2*cov.a[5,6])
a.rsa$se[3] = sqrt(se.a[2]^2 + se.a[3]^2 - 2*cov.a[2,3])
a.rsa$se[4] = sqrt(se.a[4]^2 + se.a[5]^2 + se.a[6]^2 + 2*cov.a[4,5] - 2*cov.a[4,6] - 2*cov.a[5,6])


# Compute coefficients: conscientiousness

c.rsa$est[1] = beta.c[2] + beta.c[3]
c.rsa$est[2] = beta.c[4] + beta.c[5] + beta.c[6]
c.rsa$est[3] = beta.c[2] - beta.c[3]
c.rsa$est[4] = beta.c[4] + beta.c[5] - beta.c[6]

c.rsa$se[1] = sqrt(se.c[2]^2 + se.c[3]^2 + 2*cov.c[2,3])
c.rsa$se[2] = sqrt(se.c[4]^2 + se.c[5]^2 + se.c[6]^2 + 2*cov.c[4,5] + 2*cov.c[4,6] + 2*cov.c[5,6])
c.rsa$se[3] = sqrt(se.c[2]^2 + se.c[3]^2 - 2*cov.c[2,3])
c.rsa$se[4] = sqrt(se.c[4]^2 + se.c[5]^2 + se.c[6]^2 + 2*cov.c[4,5] - 2*cov.c[4,6] - 2*cov.c[5,6])


# Compute coefficients: emotional stability

s.rsa$est[1] = beta.s[2] + beta.s[3]
s.rsa$est[2] = beta.s[4] + beta.s[5] + beta.s[6]
s.rsa$est[3] = beta.s[2] - beta.s[3]
s.rsa$est[4] = beta.s[4] + beta.s[5] - beta.s[6]

s.rsa$se[1] = sqrt(se.s[2]^2 + se.s[3]^2 + 2*cov.s[2,3])
s.rsa$se[2] = sqrt(se.s[4]^2 + se.s[5]^2 + se.s[6]^2 + 2*cov.s[4,5] + 2*cov.s[4,6] + 2*cov.s[5,6])
s.rsa$se[3] = sqrt(se.s[2]^2 + se.s[3]^2 - 2*cov.s[2,3])
s.rsa$se[4] = sqrt(se.s[4]^2 + se.s[5]^2 + se.s[6]^2 + 2*cov.s[4,5] - 2*cov.s[4,6] - 2*cov.s[5,6])



# Compute coefficients: openness

o.rsa$est[1] = beta.o[2] + beta.o[3]
o.rsa$est[2] = beta.o[4] + beta.o[5] + beta.o[6]
o.rsa$est[3] = beta.o[2] - beta.o[3]
o.rsa$est[4] = beta.o[4] + beta.o[5] - beta.o[6]

o.rsa$se[1] = sqrt(se.o[2]^2 + se.o[3]^2 + 2*cov.o[2,3])
o.rsa$se[2] = sqrt(se.o[4]^2 + se.o[5]^2 + se.o[6]^2 + 2*cov.o[4,5] + 2*cov.o[4,6] + 2*cov.o[5,6])
o.rsa$se[3] = sqrt(se.o[2]^2 + se.o[3]^2 - 2*cov.o[2,3])
o.rsa$se[4] = sqrt(se.o[4]^2 + se.o[5]^2 + se.o[6]^2 + 2*cov.o[4,5] - 2*cov.o[4,6] - 2*cov.o[5,6])


# Compute t values and p values

e.rsa$t = e.rsa$est / e.rsa$se
a.rsa$t = a.rsa$est / a.rsa$se
c.rsa$t = c.rsa$est / c.rsa$se
s.rsa$t = s.rsa$est / s.rsa$se
o.rsa$t = o.rsa$est / o.rsa$se

#note that x2 is necessary, otherwise the p-values are computed for one-sided test
e.rsa$p = 2*(1 - pt(abs(e.rsa$t), df = 175))
a.rsa$p = 2*(1 - pt(abs(a.rsa$t), df = 175))
c.rsa$p = 2*(1 - pt(abs(c.rsa$t), df = 175))
s.rsa$p = 2*(1 - pt(abs(s.rsa$t), df = 175))
o.rsa$p = 2*(1 - pt(abs(o.rsa$t), df = 175))

write.csv(e.rsa, file = "RSA coefficients_E.csv")
write.csv(a.rsa, file = "RSA coefficients_A.csv")
write.csv(c.rsa, file = "RSA coefficients_C.csv")
write.csv(s.rsa, file = "RSA coefficients_S.csv")
write.csv(o.rsa, file = "RSA coefficients_O.csv")

#overall table

big5_rsa <- cbind(e.rsa[,c(1,4)], a.rsa[,c(1,4)], c.rsa[,c(1,4)], s.rsa[,c(1,4)], o.rsa[,c(1,4)])