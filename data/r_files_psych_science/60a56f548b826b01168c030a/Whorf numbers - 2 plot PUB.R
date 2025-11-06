# Plots for Whorf paper
# Run Prep script first

# Parllel vs. Orthogonal match
# Parallel match
ggplot(para.low, aes(x=sample, y=response, colour = Accuracy)) + 
  geom_abline(slope = 1, intercept = 0, linetype="solid", color = "black", size=.1) + 
  geom_point(aes(shape = Accuracy, colour=Accuracy, alpha=Accuracy), size = 2, position=position_jitter(0,.5)) + 
  xlab("Sample set size") + ylab("Response set size") +   xlim(3,20) + ylim(3,20) + theme_bw() +
  theme(legend.position="top", legend.title = element_blank()) +
  scale_color_manual(values = c("blue", "red")) + scale_shape_manual(values=c(19, 4)) + scale_alpha_manual(values=c(.4,.8)) 

# Orthogonal match
ggplot(subset(d, group=="exp"), aes(x=sample, y=response, group=Accuracy)) + 
  geom_abline(slope = 1, intercept = 0, linetype="solid", color = "black", size=.1) +
  geom_point(aes(shape = Accuracy, colour=Accuracy, alpha=Accuracy), size = 2, position = position_jitter(0,.5)) + 
  xlab("Sample set size") + ylab("Response set size") +   xlim(3,20) + ylim(3,20) + theme_bw() +
  theme(legend.position="top", legend.title = element_blank()) +
  scale_color_manual(values = c("blue", "red")) + scale_shape_manual(values=c(19, 4)) + scale_alpha_manual(values=c(.4,.8))


# Diamonds and Dots: low and high counters
ggplot(P, aes(x=hicount, y=fail_point_2)) + # or -1?
  # Plot 3x points
  geom_point(shape = 1, color="brown3", size = 4, alpha=.3, stroke = .5, 
             position=position_jitter(width = 0, height = .5)) +
  geom_abline(slope = 1, intercept = 0, linetype="dotted") +
  # Plot low counters
  geom_errorbar(data = subset(P, sid %in% c(1:15)), aes(x=hicount, ymin=q25, ymax=q75, width=0), colour="brown3") +
  geom_point(data = subset(P, sid %in% c(1:15)), aes(x=hicount, y=cutoff), 
             shape = 23, color="brown3", size=2, fill = "white", stroke = 1) +
  # Shaded area
  geom_rect(aes(xmin=40, xmax=Inf, ymin=-Inf, ymax=Inf), alpha=.005) +
  # Plot high counters
  geom_errorbar(data = subset(P, sid %in% c(16:30)), aes(x=hicount+c(1:15), ymin=q25, ymax=q75, width=0), colour="blue3") +
  geom_point(data = subset(P, sid %in% c(16:30)), aes(x=hicount+c(1:15), y=cutoff), 
             shape = 23, color="blue3", size=2, fill = "white", stroke = 1) + 
  scale_x_continuous(limits=c(0,56), breaks = seq(0,40,10), minor_breaks = seq(0, 40, 5),
                     labels=c("0","10","20", "30", "               40 or above")) +
  xlab("Highest verbal count") + ylab("Highest numerical match") +
  theme_bw() + ylim(0, 38)
#ggsave(filename = "./Plots/Diamonds_and_Dots_ALL.png", width = 7, height = 5, scale = .8)



# Highjump
ggplot(subset(P, group=="exp"), aes(x=hicount, y=pabove)) +
  geom_point(shape = 23, color="brown3", size=2, stroke = 1, alpha = 1, 
             position=position_jitter(width = 0.0, height = 0.01)) + 
  geom_hline(yintercept=0.5, linetype="dotted") +
  xlab("Highest verbal count") + ylab("Probability that switchpoint exceeds highest count") +
  theme_bw() + ylim(-0.01,1) + xlim(5,20) #+ theme(legend.position="none")
#ggsave(filename = "./Plots/Highjump.png", width = 3, height = 5, scale = .8)



# Plot by subject plots
str(d)

ggplot(subset(d, group=="exp")) + 
  geom_rect(data = subset(P, group=="exp"), aes(xmin=hicount, xmax=Inf, ymin=-Inf, ymax=Inf), alpha = .2, color = "grey") +
  geom_abline(slope = 1, intercept = 0, linetype="solid", color = "black", size=.1) +
  geom_vline(aes(xintercept = hicount_max), color = "darkgrey") +
  geom_vline(aes(xintercept = fail_point_2), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = cutoff), color = "red", linetype = "solid") + theme_bw() +
  geom_point(aes(x=sample, y=response, shape=Accuracy, colour=Accuracy, alpha = Accuracy), size = 2) + 
  facet_wrap(. ~ hicount_max*sid, nrow = 3) + # was 5
  #geom_point(data=para.low, aes(x=sample, y=response, colour = "black"), shape=4, size = 2, alpha = .33) +
  theme(strip.text.x = element_blank(), panel.border=element_rect(colour="black", size=.8)) +
  scale_color_manual(values=c("blue", "red")) + scale_shape_manual(values=c(19, 4)) + scale_alpha_manual(values=c(.4,.8)) +
  scale_y_continuous(breaks = seq(0, 20, 5), minor_breaks = NULL, limits = c(3, 21)) +
  scale_x_continuous(breaks = seq(0, 20, 5), minor_breaks = NULL, limits = c(3, 21)) +
  xlab("Sample set size") + ylab("Response set size") +
  theme(legend.position="top", legend.title = element_blank()) +
  ggtitle("Low counters") + theme(plot.title = element_text(hjust = 0.5)) +
#ggsave("./Plots/By_Subject_LOW.png", height = 6, width = 9) # was 9 x 6

# High-counters: control group
ggplot(subset(d, group=="ctrl")) + 
  geom_abline(slope = 1, intercept = 0, linetype="solid", color = "black", size=.1) +
  geom_vline(aes(xintercept = cutoff), color = "red", linetype = "solid") +
  geom_point(aes(x=sample, y=response, shape=Accuracy, colour=Accuracy, alpha = Accuracy), size = 1.5) + 
  facet_wrap(. ~ sid, nrow = 3) + theme_bw() +
  theme(strip.text.x = element_blank(), panel.border=element_rect(colour="black", size=.8)) +
  scale_color_manual(values = c("blue", "red")) + scale_shape_manual(values=c(19, 4)) + scale_alpha_manual(values=c(.4,.8)) +
  scale_y_continuous(breaks = seq(0, 30, 5), minor_breaks = NULL, limits = c(3, 33)) +
  scale_x_continuous(breaks = seq(0, 30, 5), minor_breaks = NULL, limits = c(3, 33)) +
  xlab("Sample set size") + ylab("Response set size") +
  theme(legend.position="top", legend.title = element_blank()) +
  ggtitle("High counters") + theme(plot.title = element_text(hjust = 0.5)) +
#ggsave("./Plots/By_Subject_HIGH.png", height = 6, width = 9)
