# Making Figure SX (Inferiority Test)

inf.df <- bind_rows(mutate(Study1.EmotT1.CIs, Study = "Study 1"), 
                    mutate(Study2.EmotT1.CIs, Study = "Study 2"))

FigS1 <- ggplot(inf.df, aes(x = fct_rev(Study), y = Estimate)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = .10, size = .75) +
  geom_hline(yintercept = 0, alpha = .6, size = 1) +
  geom_hline(yintercept = -.05, linetype = "dotted", size = 1) +
  apatheme + scale_y_continuous(limits =  c(-.5, .5),
                                breaks = round(seq(-.5, .5, by = .1), 2)) +
  coord_flip() +
  theme(text = element_text(family = "Times New Roman", size = 16),
        axis.text.y = element_text(size = 18)) +
  labs(y = "\nFixed Effect of Emotionality on Predicted Stability\n(90% Confidence Intervals)",
       x = "")
FigS1
ggsave(plot = FigS1, "FigureS1.png", type = "cairo-png", 
       dpi = 300, width = 8, height = 3.5, units = "in") #Save graph
