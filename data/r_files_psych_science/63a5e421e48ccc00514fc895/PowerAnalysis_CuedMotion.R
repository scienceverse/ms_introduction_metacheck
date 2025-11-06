library("pwr")
library("effsize")
library("dplyr")

Expt1 <- read.csv("ttest_expt1.csv", stringsAsFactors = F)

# Experiment 1, CuedMotion
Expt1 <- filter(Expt1, Coherence == 0)

# using pilot data to estimate sample size
pilot <- filter(Expt1, Type == "Pilot")

cohen.d(c(pilot$X0, pilot$X180), rep(c("Treatment","Control"), each = 10), paired = T)

d = -2.094109

p = 0.9

pwr.t.test(d = d, power = p, type = "paired", alternative = "two.sided")
# Paired t test power calculation 
# 
# n = 4.692581
# d = 2.094109
# sig.level = 0.05
# power = 0.9
# alternative = two.sided

###

# Experiment 2

f2 = 1.994012
pwr.anova.test(k = 7, f = f2, sig.level = 0.05, n = 20)
# Balanced one-way analysis of variance power calculation 
# 
# k = 7
# n = 20
# f = 1.994012
# sig.level = 0.05
# power = 1
#

# Expt 1 vs 2, t.test at Coherence of 0%
d = 0.86
pwr.t.test(d = d, n = 20, type = "paired", alternative = "two.sided")
# Paired t test power calculation 
# 
# n = 20
# d = 0.86
# sig.level = 0.05
# power = 0.954038
# alternative = two.sided

# Experiment 3

# Expt 1 vs 3, t.test at Coherence of 0%
d = 0.96
pwr.t.test(d = d, n = 20, type = "paired", alternative = "two.sided")
# Paired t test power calculation 
# 
# n = 20
# d = 0.96
# sig.level = 0.05
# power = 0.9824691
# alternative = two.sided