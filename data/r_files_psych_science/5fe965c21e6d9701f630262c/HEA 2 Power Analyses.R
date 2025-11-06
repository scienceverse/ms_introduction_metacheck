################################
# Study 2 Power Simulations
################################

library(tidyverse)
library(simr)

set.seed(17)

power_extend <- function(fit, nsim, along, within, new_size, ...) {
  power_lst <- vector("list", length(new_size))
  for (j in seq_along(new_size)) {
    sim_m <- extend(fit, along = along, within = within, n = new_size[j])
    power_lst[[j]] <- powerSim(sim_m, nsim = nsim, seed = 1, ...)
  }
  setNames(power_lst, new_size)
}

# Add a plotting function to get power curve
plot_power_extend <- function(object) {
  new_size <- as.integer(names(object))
  nsim <- object[[1]]$n
  tibble(n = new_size, 
         power = object %>% 
           map_dbl(~ .x$x / nsim)) %>% 
    mutate(ll = binom::binom.agresti.coull(power * nsim, nsim)$lower, 
           ul = binom::binom.agresti.coull(power * nsim, nsim)$upper) %>% 
    ggplot(aes(x = n, y = power)) + 
    geom_line() + 
    geom_pointrange(aes(ymin = ll, ymax = ul))
}

# Participant ID 
id <- 1:100

# Cluster size (EMA prompts per participant)
nj <- 30

# Continuous predictor at level 2
N_w <- length(id)  # sample size
w <- rnorm(N_w)

# standardize w:
w <- (w - mean(w)) / sd(w) * (N_w - 1) / N_w

# Continuous predictor at level 1
N_x <- length(id) * nj
x <- rnorm(N_x)

# standardize w:
x <- (x - mean(x)) / sd(x) * (N_x - 1) / N_x

# Combine all to form a data frame
(sim_dat <- data.frame(w = rep(w, 
                               each = length(id) / length(w) * nj), 
                       x = x, 
                       id = rep(id, each = nj)))


gams <- c(0, 0.3, 0.2)  # gamma_00, gamma_01, gamma_10
taus <- matrix(0.3)  # tau00 = 0.3
sigma <- sqrt(.96)  # sigma, reflecting that x would account for (0.2)^2 = 0.04 variance

sim_m4 <- makeLmer(y ~ w + x + (1 | id), 
                   fixef = gams, VarCorr = taus, sigma = sigma, 
                   data = sim_dat)

# Uncomment the linw below to run power simulations for the between-subject effect

# system.time(powers <- power_extend(sim_m4, nsim = 1000, new_size = c(50, 75, 100), 
#                           along = "id"))

powers

plot_power_extend(powers)

# Uncomment the linw below to run power simulations for the within-subject effect

# system.time(powers_2 <- power_extend(sim_m4, nsim = 1000, new_size = 50, 
#                           within = "id + w"))

powers_2

plot_power_extend(powers_2)

