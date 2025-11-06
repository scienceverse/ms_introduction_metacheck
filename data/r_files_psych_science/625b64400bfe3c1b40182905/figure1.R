print('Running figure1.R ...')
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(jtools)
###############
## Load data ##
###############
fig_data <-
  read_csv('Data Analysis/Results/results_noRound.csv') %>%
  bind_rows(
    read_csv('Data Analysis/Results/results_log_noRound.csv'))
##################
## Build Figure ##
##################
figure1 <- 
  ggplot(
    fig_data, 
    aes(x = change.mean)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = .0005,
    color = 1,
    fill = 'white') +
  geom_density() +
  scale_y_continuous(
    limits = c(0, 650), 
    breaks = seq(0, 650, 100)) +
  scale_x_continuous(
    breaks = seq(0, .02, .005)) +
  labs(
    x = expression(paste('Mean Change in ', italic(R) ^ 2)),
    y = 'Number of Tests') +
  theme_apa()
#################
## Save Figure ##
#################
ggsave(
  plot = figure1, 
  filename = "Data Analysis/Results/figure1.png")
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print('figure1.R finished running!')
Sys.sleep(5)  