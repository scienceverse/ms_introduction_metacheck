
# First run the "SvD_Analysis_1b.R" script

source("Analysis/SvD Analysis_1b.R", echo = TRUE)

# Scatterplots for each question individually -----------------------------

# Let's create the dataset 
dscat <- dlong

# Create the correlation variables of the signed errors
dscat$guess_1_cor <- dscat$guess_1 - dscat$answer
dscat$guess_2_cor <- dscat$guess_2 - dscat$answer

dscat$question <- factor(dscat$question, 
                         levels = c("1","2","3","4","5","6"),
                         labels = c("q1","q2","q3","q4","q5","q6"))

names(dscat)[names(dscat) == "condition"] <- "Perspective"

scat <- ggplot(dscat,aes(x = guess_1_cor, 
                         y = guess_2_cor, 
                         color = Perspective,
                         shape = Perspective))+ 
  geom_point(size = 2, alpha = 0.3, show.legend = T)+
  geom_smooth(method = lm, se = FALSE, show.legend = F)+
  labs(x = "",
       y = "")+
  theme_classic()+ 
  scale_color_manual(values=c('#999999','#56B4E9'))+
  scale_y_continuous(label = comma)+
  scale_x_continuous(label = comma)+
  facet_wrap(~question, scales = "free")+
  theme(legend.position = "top")
scat

cowplot::ggsave2("s1.png",
                width = 10,
                height = 10,
                units = "in",
                dpi = 300)
