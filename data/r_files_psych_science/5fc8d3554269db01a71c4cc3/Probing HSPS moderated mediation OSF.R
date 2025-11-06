# Probe HSPS moderated mediation

library(ggplot2)

# ggplot for conditional indirect effect
Sensitivity <- as.numeric(c(-1.0080, -0.0470, 1.1123))
Indirect_effect <- as.numeric(c(0.0325, -0.0082, -0.0573))
Conditional_indirect <- as.data.frame(cbind(Sensitivity, Indirect_effect))

Conditional_indirect_plot <- ggplot(Conditional_indirect, aes(x=Sensitivity, y=Indirect_effect)) + geom_point(size=3) + geom_line(size=1) +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Sensory processing sensitivity",
       y = "Indirect effect of age on fatigue via auditory attention") + geom_hline(yintercept=0, linetype="dashed", size=1)


# ggplot2 for conditional
Auditory_attention <- as.numeric(c(-1.3159, 0.4366, 1.30801,-1.3159, 0.4366, 1.30801, -1.3159, 0.4366, 1.30801))
SPS <- as.character(c("Low", "Low", "Low", "Moderate", "Moderate", "Moderate", "High", "High", "High"))
Fatigue <- as.numeric(c(-0.2852, -0.4087, -0.4701, -0.0553, -0.0243, -0.0089, 0.2220, 0.4393, 0.5473))
Conditional <- as.data.frame(cbind(Auditory_attention, Sensitivity, Fatigue))
Conditional$Auditory_attention <- as.numeric(Conditional$Auditory_attention)
Conditional$Fatigue <- as.numeric(Conditional$Fatigue)
Conditional$SPS <- factor(Conditional$SPS, levels = c("High", "Moderate", "Low"))
Conditional_plot <- ggplot(Conditional, aes(x=Auditory_attention, y=Fatigue, shape=SPS, linetype=SPS)) + geom_point(size=3) + geom_line(size=1) +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = c(0.8, 0.65)) +
  labs(x = "Auditory attention ability",
       y = "Listening-related fatigue")



# plot together

require(gridExtra)
library(cowplot)

plot_grid(Conditional_indirect_plot, Conditional_plot)
