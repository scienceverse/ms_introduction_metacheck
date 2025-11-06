# Plot the two panel figure


library(ggplot2)

apatheme=theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        legend.position="none",
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'),
        text  = element_text(size=16),
        axis.text  = element_text(size=14))


exp_plot <- data.frame(matrix(NA, nrow = 4, ncol = 5))
names(exp_plot) <- c("time", "group", "mean", "lb", "ub")
exp_plot$time <- c(14, 15, 14, 15)
exp_plot$group <- c(1, 1, 2, 2)

return_stats <- function(x) {
  mean <- mean(x)
  lb <- mean - qnorm(0.975) * sd(x)/sqrt(length(x))
  ub <- mean + qnorm(0.975) * sd(x)/sqrt(length(x))
  return(c(mean, lb, ub))
}

exp_plot[1, 3:5] <- return_stats(final$ls14[final$social == 0 & final$active == 1])
exp_plot[2, 3:5] <- return_stats(final$ls15[final$social == 0 & final$active == 1])
exp_plot[3, 3:5] <- return_stats(final$ls14[final$social == 1 & final$active == 1])
exp_plot[4, 3:5] <- return_stats(final$ls15[final$social == 1 & final$active == 1])


color_social <- "#D55E00"
color_not_social <- "#0072B2"

library(ggplot2)
limits <- aes(ymax = ub, ymin = lb)
exp_plot$time <- c(14, 15, 14.1, 15.1)

ggplot(data = exp_plot, aes(x = time, 
                            y = mean, 
                            color = as.factor(group))) +
  geom_hline(aes(yintercept = mean(c(final$ls14, final$ls15))),
             color = "#E0E0E0",
             #linetype = "dotted", 
             size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_errorbar(limits, position = "dodge", size = 1) +
  scale_color_manual(values = c(color_not_social, color_social)) +
  xlab("Year") +
  ylab("Life Satisfaction") +
  scale_x_continuous(breaks = c(14.1, 15.1), labels = c("2014", "2015"), limits = c(13.7, 15.5)) +
  scale_y_continuous(breaks = seq(7, 8, 0.25), limits = c(6.9, 8.1)) +
  apatheme
ggsave("soc_vs_nonsoc.png", width = 5, height = 4)

#################
exp_plot2 <- data.frame(matrix(NA, nrow = 4, ncol = 5))
names(exp_plot2) <- c("time", "group", "mean", "lb", "ub")
exp_plot2$time <- c(14, 15, 14, 15)
exp_plot2$group <- c(1, 1, 2, 2)

exp_plot2[1, 3:5] <- return_stats(final$ls14[final$active == 0])
exp_plot2[2, 3:5] <- return_stats(final$ls15[final$active == 0])
exp_plot2[3, 3:5] <- return_stats(final$ls14[final$active == 1])
exp_plot2[4, 3:5] <- return_stats(final$ls15[final$active == 1])


color_active <- "#333333"
color_not_active <- "#AAAAAA"

library(ggplot2)
theme_set(theme_classic())
limits <- aes(ymax = ub, ymin = lb)
exp_plot2$time <- c(14, 15, 14.1, 15.1)

ggplot(data = exp_plot2, aes(x = time, 
                            y = mean, 
                            color = as.factor(group))) +
  geom_hline(aes(yintercept = mean(c(final$ls14, final$ls15))),
             color = "#E0E0E0",
             #linetype = "dotted", 
             size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_errorbar(limits, position = "dodge", size = 1) +
  scale_size_manual(values = c(0.5, 0.8)) +
  scale_color_manual(values = c(color_not_active, color_active)) +
  xlab("Year") +
  ylab("Life Satisfaction") +
  scale_x_continuous(breaks = c(14.1, 15.1), labels = c("2014", "2015"), limits = c(13.7, 15.5)) +
  scale_y_continuous(breaks = seq(7, 8, 0.25), limits = c(6.9, 8.1)) +
  apatheme
ggsave("active_vs_nonact.png", width = 5, height = 4)
