
# First run the "SvD_Analysis_1a.R" script
# Make sure that the working directory is correctly assigned

source("Analysis/SvD Analysis_1a.R", echo  = TRUE)

# Histograms of all questions ---------------------------------------------
dhist <- dlong
dhist$question <- factor(dhist$question, 
                         levels = c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10"))

library(scales)
# First estimate
h1 <- ggplot(dhist,
             aes(x = guess_1))+
  geom_histogram(bins = 50,
                 color = "black",
                 fill = "white")+
  theme_classic()+
  facet_wrap(~question, scales = "free")+
  labs(x = "First Estimate",
       y = "Count")
h1

cowplot::ggsave2("h1.png",
                 width = 10,
                 height = 10,
                 units = "in",
                 dpi = 300)

# Second estimate
h2 <- ggplot(dhist,
             aes(x = guess_2))+
  geom_histogram(bins = 50,
                 color = "black",
                 fill = "white")+
  theme_classic()+
  facet_wrap(~question, scales = "free")+
  labs(x = "Second Estimate",
       y = "Count")
h2

cowplot::ggsave2("h2.png",
                 width = 10,
                 height = 10,
                 units = "in",
                 dpi = 300)
