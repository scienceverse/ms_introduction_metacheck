# simulation of the data
#-----------------------
# word effects
words <- data.frame(word = 1:200,
                    length = rep(6:13, each = 25)
                    )
words$pronounceability <- runif(200, 2.75, 5.25) + (words$length - 9.5)/2
words$effect <- rnorm(200, 0, 1)

# simulated data
sim <- data.frame(id = rep(1:1000, each = 22),
                  word = as.numeric(replicate(1000, sample(1:200, 22))),
                  scenario = rep(as.numeric(replicate(1000, sample(c(-0.5, 0.5), 2))), 
                                 each = 11)
                  )
sim$pronounceability <- words$pronounceability[sim$word]
sim$length <- words$length[sim$word]
sim$word.effect <- words$effect[sim$word]
sim$order <- rep(sim$scenario[(0:999)*22 + 1], each = 22)

# participant effects
sim$part.effect <- rep(rnorm(1000), each = 22)
sim$part.pron.effect <- rep(rnorm(1000, 0.4, 0.4), each = 22)
sim$part.len.effect <- rep(rnorm(1000, 0.2, 0.3), each = 22)
sim$part.scen.effect <- rep(rnorm(1000, 0.2, 0.25), each = 22)
sim$part.order.effect <- rep(rnorm(1000, 0.5, 0.2), each = 22)

# ratings simulations
sim$ratings <- (rnorm(22000, 0, 2) + 
                  sim$word.effect + 
                  sim$part.effect + 
                  sim$part.pron.effect * sim$pronounceability + 
                  sim$part.len.effect * sim$length + 
                  sim$part.scen.effect * sim$scenario + 
                  sim$part.order.effect * sim$order
                )
sim$ratings <- round((sim$ratings + 9) / 2)
sim$ratings <- ifelse(sim$ratings < 1, 1, sim$ratings)
sim$ratings <- ifelse(sim$ratings > 7, 7, sim$ratings)

# centering
sim$pronounceability <- (sim$pronounceability - 4)/6
sim$length <- (sim$length - 9.5)/7


#-------------------------------------------------------------------------------
# analysis
#---------
library(lme4)
m <- lmer(ratings ~ scenario*pronounceability + scenario*order + pronounceability*length + 
            (pronounceability + length + scenario|id) + (1|word),
          data = sim)
summary(m)