rm(list=ls())

library(ggplot2)
library(rstan)
library(analysisHelpers)

# Import data
basics <- read.csv("./basics.csv") # demographic info

para.dat <- read.csv("./para_dat.csv") # Parallel match task data

d <- read.csv("./d.csv") # Orthogonal match task data
d <- d[order(d$sid),] # sort by subject
data <- list(NROWS=nrow(d), NSUBJ=max(d$sid), N=max(d$sample), shown=d$sample, response=d$response, hicount=d$hicount_max, subject=d$sid )

# Run model in Stan
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
fit <- stan(file="model21.stan", data=data, iter=10000, chains=4, cores=4, control=list(adapt_delta=0.9999))
saveRDS(fit, file = "fit10k.rds") # save fit
#fit10k <- readRDS("fit10k.rds")
#print(fit10k)
print(fit)

# Extract probabilities from model
P <- NULL
hicounts <- basics[order(basics$sid), 'hicount_max'] # to ensure its in order of sid
for(s in 1:length(unique(d$sid))) {
    hc <- hicounts[s]
    subject_cutoff   <- extract(fit, paste("subj_cutoff[",s,"]"))[[1]]

    P <- rbind(P, data.frame(sid=s,
                             cutoff=median(subject_cutoff),
                             switch.mean=mean(subject_cutoff),
                             q05=quantile(subject_cutoff, 0.05),
                             q25=quantile(subject_cutoff, 0.25),
                             q50=quantile(subject_cutoff, 0.50),
                             q75=quantile(subject_cutoff, 0.75),
                             q95=quantile(subject_cutoff, 0.95),
                             pabove=mean(subject_cutoff > hc),
                             hicount=hc,
                             sd=sd(subject_cutoff)))
}
str(P)
P$sid
P$hicount

# Add simple criterion and demo data to P
P <- merge(P, basics[, c('id', 'group', "fail_point", 'sid', 'age', 'gender', 'schooling', 'hicount_max', 'para.acc', 'para.acc.1')], by = "sid", all.x = T)

P$above <- ifelse(P$cutoff > P$hicount_max, 1, 0) # P(cutoff > hicount)

P$fail_point_2 <- P$fail_point-2 # Highest match point based on 3x failures
P$above.3x <- ifelse(P$fail_point_2 > P$hicount_max, 1, 0)
P$gap <- abs(P$hicount - P$cutoff)

# Add cutoff etc to d
d <- merge(d, P[, c('sid', 'cutoff', 'q25', 'q75')], by = 'sid', all.x = T)
d$inrange <- ifelse(d$sample > d$hicount_max, 0, 1)
d$deviation <- abs(d$response - d$sample)

# And add stuff to para.dat
para.dat <- merge(para.dat, P[, c('id', 'sid', 'cutoff', 'hicount_max')], by = 'id', all.x = T)
head(para.dat)
para.low <- subset(para.dat, group=="exp")
para.high <- subset(para.dat, group=="ctrl")
length(unique(para.low$id)) # 15
length(unique(para.high$id)) # 15

# Full model fit ####

# Make stanfit object parts into a dataframes
w.dat <- as.data.frame(fit, pars = c("subj_w")) # Is this the right way?
head(w.dat)
names(w.dat) <- c(1:30)

w.long <- stack(w.dat)
head(w.long)
names(w.long) <- c("w", "sid")
w.long <- merge(w.long, P[, c("sid", "hicount", "schooling", "group")])
w.long$hicount <- as.numeric(w.long$hicount)
w.long <- fix.factors(w.long)
str(w.long)

cutoff.dat <- as.data.frame(fit, pars = "subj_cutoff")
names(cutoff.dat) <- c(1:30)

cutoff.long <- stack(cutoff.dat)
head(cutoff.long)
names(cutoff.long) <- c("estimate", "sid")
cutoff.long <- merge(cutoff.long, P[, c("sid", "hicount", "schooling", "group")])
cutoff.long$hicount <- as.numeric(cutoff.long$hicount)
cutoff.long <- fix.factors(cutoff.long)
str(cutoff.long)

fit.dat <- extract(fit)
names(fit.dat)
mean(fit.dat$subj_w) # .141 -> -.01
mean(fit.dat$low_mu) # -.16 -> -.14
mean(fit.dat$low_sd) # 0.14 -> .14


