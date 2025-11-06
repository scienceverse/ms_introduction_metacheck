library(lme4)


d6 <- read.table("study6_harm.txt", head = T, sep = "\t", as.is = T)
d6_novelty <- read.table("study6_novelty.txt", head = T, sep = "\t", as.is = T)
d6_fluency <- read.table("study6_pronounceability.txt", head = T, 
                         sep = "\t", as.is = T)
new_additives <- unlist(read.table("study6_new_additives.txt", as.is = T))
old_additives <- unlist(read.table("study6_old_additives.txt", as.is = T))

# pronounceability
pron <- sapply(d6$name, function(x) {mean(d6_fluency$disfluency[d6_fluency$name == x])})
pron.nov <- sapply(d6_novelty$name, 
                   function(x) {mean(d6_fluency$disfluency[d6_fluency$name == x])})

# new/old
original <- d6$name %in% old_additives
original.nov <- d6_novelty$name %in% old_additives

d6 <- cbind(d6, fluency = pron, original = original)
d6_novelty <- cbind(d6_novelty, fluency = pron.nov, original = original.nov)

# exclusion of participants with 12 or more same ratings
retain <- sapply(d6$id, function(x) {max(table(d6[d6$id == x, "harmful"])) < 12})
d6 <- d6[retain, ]

retain.nov <- sapply(d6_novelty$id, 
                     function(x) {max(table(d6_novelty[d6_novelty$id == x, 
                                                       "novel"])) < 12})
d6_novelty <- d6_novelty[retain.nov, ]


# results
#-------------------------------------------------------------------------------
# harmfulness
# interaction
m6a <- lmer(harmful ~ fluency*original + (1|id) + (1|name), data = d6)
summary(m6a)
# original
m6a.old <- lmer(harmful ~ fluency + (1|id) + (1|name), data = d6[d6$original,])
summary(m6a.old)
# new
m6a.new <- lmer(harmful ~ fluency + (1|id) + (1|name), data = d6[!d6$original,])
summary(m6a.new)

# novelty
# interaction
m6b <- lmer(novel ~ fluency*original + (1|id) + (1|name), data = d6_novelty)
summary(m6b)
# original
m6b.old <- lmer(novel ~ fluency + (1|id) + (1|name), 
                data = d6_novelty[d6_novelty$original,])
summary(m6b.old)
# new
m6b.new <- lmer(novel ~ fluency + (1|id) + (1|name), 
                data = d6_novelty[!d6_novelty$original,])
summary(m6b.new)