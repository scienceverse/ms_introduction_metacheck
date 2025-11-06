
# First run the "SvDvA Analysis_4.R" script

source("Analysis/SvDvA Analysis_4.R", echo  = TRUE)
library(scales)
library(cowplot)
 
# Scatterplots for each question individually -----------------------------

dscat <- dlong

# Create the correlation variables of the signed errors
dscat$guess_1_cor <- dscat$guess_1 - dscat$answer
dscat$guess_2_cor <- dscat$guess_2 - dscat$answer

dscat$question <- factor(dscat$question, 
                         levels = c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12"),
                         labels = c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12"))

names(dscat)[names(dscat) == "condition"] <- "Perspective"

order <- c("Disagree","Self","Agree")#Re-order factors so that they are shown correctly on the graph
dscat$Perspective <- ordered(dscat$Perspective, order)

scat <- ggplot(dscat,aes(x = guess_1_cor, 
                         y = guess_2_cor, 
                         color = Perspective,
                         shape = Perspective))+ 
  geom_point(size = 2, alpha = 0.3, show.legend = T)+
  geom_smooth(method = lm, se = FALSE, show.legend = F)+
  labs(x = "",
       y = "")+
  theme_classic()+ 
  scale_color_manual(values=c('#999999','#56B4E9','#E69F00'))+
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
