# this functions is adapted by Peter Moseley from the one provided by the psycho package (dprime function),
# but removes the Hautus adjustment for 0 values, replacing it with the one recommended by Stanislaw & Todorov (1999)
# in which 0 values are replaced with 0.5, and 100% hits or FAs replaced with n_targets - 0.5
# user must either provide hits, FAs, misses, and CRs *OR* hits, FA, n_targets and n_distractors

calc_sdt <- function (n_hit, n_fa, n_miss = NULL, n_cr = NULL, n_targets = NULL, 
                      n_distractors = NULL) 
{
  n_hit[n_hit == 0] <- 0.5 # replaces 0 hit values with 0.5
  n_fa[n_fa == 0] <- 0.5 # replaces 0 FA values with 0.5
  n_hit[n_hit == n_targets] <- (n_targets - 0.5) # adjusts 100% hit rate
  n_fa[n_fa == n_distractors] <- (n_distractors - 0.5) # adjusts 100% FA rate
  if (is.null(n_targets)) {
    n_targets <- n_hit + n_miss
  }
  if (is.null(n_distractors)) {
    n_distractors <- n_fa + n_cr
  }
     
    hit_rate <- n_hit/n_targets
    fa_rate <- n_fa/n_distractors
    dprime <- qnorm(hit_rate) - qnorm(fa_rate)
    zhr <- qnorm(hit_rate)
    zfar <- qnorm(fa_rate)
    beta <- exp(-zhr * zhr/2 + zfar * zfar/2)
    c <- -(qnorm(hit_rate) + qnorm(fa_rate))/2
  
  if (any(n_distractors == 0)) {
    warning("Several rows have 0 distractors. Please remove these rows to compute non-parametric indices.")
    return(list(dprime = dprime, beta = beta, aprime = NA, 
                bppd = NA, c = c))
  }
  hit_rate <- n_hit/n_targets
  fa_rate <- n_fa/n_distractors
  a <- 1/2 + ((hit_rate - fa_rate) * (1 + hit_rate - fa_rate)/(4 * 
                                                                 hit_rate * (1 - fa_rate)))
  b <- 1/2 - ((fa_rate - hit_rate) * (1 + fa_rate - hit_rate)/(4 * 
                                                                 fa_rate * (1 - hit_rate)))
  a[fa_rate > hit_rate] <- b[fa_rate > hit_rate]
  a[fa_rate == hit_rate] <- 0.5
  aprime <- a
  bppd <- (hit_rate * (1 - hit_rate) - fa_rate * (1 - fa_rate))/(hit_rate * 
                                                                   (1 - hit_rate) + fa_rate * (1 - fa_rate))
  bppd_b <- (fa_rate * (1 - fa_rate) - hit_rate * (1 - hit_rate))/(fa_rate * 
                                                                     (1 - fa_rate) + hit_rate * (1 - hit_rate))
  bppd[fa_rate > hit_rate] <- bppd_b[fa_rate > hit_rate]
  return(list(dprime = dprime, beta = beta, aprime = aprime, 
              bppd = bppd, c = c))
}
