library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# real data - familiar trials
Israel_prop = read.csv("df_serial_Israel.csv")
German_prop = read.csv("df_serial_German.csv")

# permutated data - for chance level
Israel_chance = read.csv("df_serial_Israel_parm_mean.csv")
German_chance = read.csv("df_serial_German_parm_mean.csv")

# compare real data to parmutated one
t.test(Israel_prop$mean_prop,Israel_chance$mean_score, paired = T)
cohens_d(Israel_prop$mean_prop,Israel_chance$mean_score, paired = T)

t.test(German_prop$mean_prop,German_chance$mean_score, paired = T)
cohens_d(German_prop$mean_prop,German_chance$mean_score, paired = T)

t.test(Israel_prop$mean_prop,German_prop$mean_prop, var.equal = T)
cohens_d(Israel_prop$mean_prop,German_prop$mean_prop)

# for graph - familiar trials only
data_graph_perm = as.data.frame(cbind("prop_serial" = c(Israel_prop$mean_prop, 
                                                   German_prop$mean_prop,
                                                   c(Israel_chance$mean_score,
                                                   German_chance$mean_score)),
                                 "condition" = c(rep("Israel",35), rep("Germany", 33),
                                                 rep("chance", 33+35))))
data_graph_perm$prop_serial = as.numeric(as.character(data_graph_perm$prop_serial))
data_graph_perm$condition = as.factor(data_graph_perm$condition)

data_avg = data.frame("condition" = c("Israel", "Germany"),
                      "avg" = c(mean(Israel_prop$mean_prop),
                                mean(German_prop$mean_prop)))
mean_chance = mean(c(mean(Israel_chance$mean_score),
                     mean(German_chance$mean_score)))

ggplot(data_graph_perm, aes(x=prop_serial, color=condition, fill=condition)) + 
  geom_density(alpha=0.1) +
  scale_color_manual(values = c("white","white", "white")) +
  scale_fill_manual(values = c("gray62","deepskyblue2","deeppink2", "darkorange"))+
  theme_minimal() +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.text =  element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = "top",
        legend.spacing.x = unit(1.0, 'cm'))+
  xlab ("Serial scanning score")+
  xlim(0.4,1)+
  geom_vline(xintercept=mean_chance, color = "gray", size = 1.5)+
  geom_vline(data=data_avg,aes(xintercept=avg),
                               color = c("deeppink2","deepskyblue2"), size = 1.5, alpha = 0.2)
  