# Whorf numbers stats
library(lme4)
library(psych) # masks "alpha" in ggplot2?

# Descriptive ####

length(unique(d$id)) # 30 good.
range(d$sample)

# Age
describeBy(P$age, group = P$group)

# Education
table(P$schooling, P$group)
describeBy(P$schooling, group = P$group)
unique(P$id[is.na(P$schooling)]) # 2127 has NA schooling...

# Mean number of trials
aggregate(trial ~ id * group, subset(d, type=="critical"), FUN = length)

# Model w estimates
exp(mean(fit.dat$logW)) # overall weber fraction = 0.13

# Switchpoints (and hicounts)
mean(P$cutoff[P$group=="exp"]) # 6.4
median(P$cutoff[P$group=="exp"]) # 6
table(P$cutoff[P$group=="exp"]) 
range(P$cutoff[P$group=="exp"]) # Max = 11
mean(P$hicount[P$group=="exp"]) # 12.6
mean(P$hicount[P$group=="exp"]) - mean(P$cutoff[P$group=="exp"]) # 6.2
range(P$hicount[P$group=="exp"] - P$cutoff[P$group=="exp"]) # -1 - 12

mean(P$cutoff[P$group=="ctrl"]) # 28.8
range(P$cutoff[P$group=="ctrl"]) # Max = 32
  
  # Compare cutoffs across groups:
t.test(cutoff ~ group, P) # t(17.47) = 11.01, p < .0001

# Hicounts
mean(P$hicount[P$group=="exp"]) # 12.6
median(P$hicount[P$group=="exp"]) # 10
range(P$hicount[P$group=="exp"]) # 6 - 20
mean(P$gap[P$group=="exp"]) # 6.3333
range(P$gap[P$group=="exp"]) # 0 - 12
range(P$gap[P$group=="ctrl"]) # 8 - 35
basics$count_diff <- as.numeric(as.character(basics$count_diff))
mean(abs(na.exclude(basics$count_diff[P$group=="exp"]))) # 2.0 = mean diff in count

# 3x failures
mean(na.exclude(P$fail_point_2[P$group=="exp"])) # Mean = 7.84
max(na.exclude(P$fail_point_2[P$group=="exp"])) # Max = 14

# Comparison of highest match v. switchpoints
mean(na.exclude(P$fail_point_2 - P$cutoff)) # 1.77
cor.test(~cutoff + fail_point_2, P) # R2 = 0.67, t(11) = 2.97, p = .01

# Probability of cutoff exceeding hicount
pover <- mean(P$pabove[P$group=="exp"]) # Mean pabove = .12
(1-pover)/pover # 7.36 times more likely using approx system than exact system above


# 0. # Parallel match performance

prop.table(xtabs(~correct, para.dat)) # 93% accuracy overall (all ppts)
prop.table(xtabs(~correct, para.high)) # 100% accuracy in high-counters
# low counters
prop.table(xtabs(~correct, para.low)) # 87% accuracy overall
prop.table(xtabs(~correct, subset(para.low, attempt==1))) # 85% accuracy on first attempts
prop.table(xtabs(~correct, subset(para.low, attempt==1 & sample > 5))) # 70% accuracy on first attempts larger than 5 (10 and 15)
table(para.low$deviation) # deviation from correct cardinality
table(para.low$attempt, para.low$id)
# >> The maximum number of errors on a parallel match sample was one (i.e. all 2nd attempts were correct)

prop.table(table(d$Accuracy)) # 51% correct in ortho task

# 1. Correlation between switchpoint and hicount ####

# 1a. Low counters only
str(P) 
summary(lm(fail_point ~  hicount, subset(P, group=="exp"))) # p = .032
summary(lm(cutoff ~  hicount, subset(P, group=="exp"))) # p = .16

# 1b. All ppts
summary(lm(cutoff ~  hicount + schooling, P)) # B = 2.07, SEM = 0.60, t = 3.43, p = .002 

# 1c. Just those educated 0 years
schooling0.dat <- P$sid[P$schooling==0]
summary(lm(cutoff ~  hicount, subset(P, sid %in% schooling0.dat))) # β = 0.39, SEM = 0.15, t = 2.59, p = .02


# 2. Were points really below the line?

# 2a. Ratio above:below the line for model results
prop.test(x = table(P$above)) # Chi2 = 24.3, p = .000008
prop.test(x = table(P$above[P$group=="exp"])) # Chi2 = 9.60, p = .002

# 2b. Ratio above:below the line for 3x failure results (low counters only)
prop.test(x = table(P$above.3x)) # Chi2 = 7.69, p = .00


# 3. Can location of switchpoints be explained simply by generally poor performance on the match task?
# Permutation tests: permute switchpoints and hicounts ####
PermuteTest <- function(dat, DV, observed.sum){
   # 2 for cutoff and happens to be 1 for 3x (which is correct)
  sum.above <- data.frame(sum.above=numeric())
  pmut <- data.frame(sid=dat$sid, switchpoint=dat[, DV])
  
  for(i in 1:10000){ # Shuffle 10k times
    pmut$hicount.perm <- sample(x = dat$hicount) # make shuffled order
    pmut$above <- ifelse(pmut$switchpoint > pmut$hicount.perm, 1, 0)
    sum.above[i, "sum.above"] <- sum(pmut$above)
  }
  # Plot histogram of permutations and observed value
  plt <- ggplot(sum.above, aes(sum.above)) + geom_histogram() +
          geom_vline(xintercept = observed.sum, col = "red")
  # Print numeric results
  print(paste("p = ", sum(sum.above <= observed.sum) / nrow(sum.above))) # < or <= ?
  print(paste("mean dots above:", mean(sum.above$sum.above)))
  print(table(sum.above)) # NEVER did it get 2 (or fewer) points above the 
  return(plt)
}

# All ppts (high and low)
PermuteTest(P, "cutoff", sum(P$above)) # p = 0, 1 vs. 7.54

# Highjump
mean(P$pabove) # low and high counters
mean(P$pabove[P$group=="exp"])
range(P$pabove[P$group=="exp"])




