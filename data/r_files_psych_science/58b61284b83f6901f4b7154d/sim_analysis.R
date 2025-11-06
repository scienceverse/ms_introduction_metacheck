load("sim_results.Rda")
conds$nstudies <- factor(conds$nstudies)
conds$prior_scale_tau <- paste0(
  "prior_scale_tau = ", round(conds$sdTau, 3)
)

for (i in seq_len(nrow(conds))) {
  conds$pct_evid_fixed[i] <- 
    sum(conds$results[[i]]$BF_fixed_random > 3) / 
    nrow(conds$results[[i]])
  conds$pct_evid_random[i] <- 
    sum(conds$results[[i]]$BF_fixed_random < 1/3) / 
    nrow(conds$results[[i]])
  conds$mean_logBF_fixed_random[i] <- 
    mean(log(conds$results[[i]]$BF_fixed_random))
}

library(ggplot2)
ggplot(conds) +
  aes(x = tau, y = pct_evid_fixed, color = nstudies) + 
  facet_wrap("prior_scale_tau") +
  geom_line()
ggsave(file = "pct_evid_fixed.pdf")

ggplot(conds) +
  aes(x = tau, y = pct_evid_random, color = nstudies) + 
  facet_wrap("prior_scale_tau") +
  geom_line()
ggsave(file = "pct_evid_random.pdf")

ggplot(conds) +
  aes(x = tau, y = mean_logBF_fixed_random, color = nstudies) + 
  facet_wrap("prior_scale_tau") +
  geom_line()
ggsave(file = "mean_logBF_fixed_random.pdf")
