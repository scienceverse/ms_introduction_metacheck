
options(scipen=999)

library(haven)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(cowplot)
library(ggpubr)
library(ggfortify)
library(psych)
library(summarytools)
library(correlation)

######## JORDAN DATA

Jordan.data <- read_sav("JORDAN Data OSF.sav")

Jordan.data <- Jordan.data %>%
  mutate(islam2.rev = 8 - islam2)

islam.ideology.Jordan <- Jordan.data %>%
  select(.,c(islam1,islam4,islam5,islam6,islam7,islam8,islam9,islam10,islam11,islam12,islam13,islam14,islam2.rev))
alpha(islam.ideology.Jordan)

sacrif.relig.Jordan <- Jordan.data %>%
  select(.,c(sacrif.rel1, sacrif.rel2, sacrif.rel3, sacrif.rel4, sacrif.rel5, sacrif.rel6))
alpha(sacrif.relig.Jordan)

sacrif.west.Jordan <- Jordan.data %>%
  select(.,c(sacrif.west1, sacrif.west2, sacrif.west3, sacrif.west4, sacrif.west5, sacrif.west6))
alpha(sacrif.west.Jordan)

##### COMPUTE MEANS

Jordan.data <- Jordan.data %>%
  mutate(islam.ideology.MEAN1 = rowMeans(select(.,c(islam1,islam4,islam5,islam6,islam7,
                                                   islam8,islam9,islam10,islam11,islam12,islam13,islam14,islam2.rev)),
                                        na.rm = TRUE)) %>%
  mutate(sacrif.relig.MEAN = rowMeans(select(.,c(sacrif.rel1, sacrif.rel2, sacrif.rel3, 
                                                 sacrif.rel4, sacrif.rel5, sacrif.rel6)),na.rm = TRUE)) %>%
  mutate(sacrif.immigrant.MEAN1 = rowMeans(select(.,c(sacrif.west1, sacrif.west2, sacrif.west3, sacrif.west4, 
                                                     sacrif.west5, sacrif.west6)),na.rm = TRUE)) %>%
  mutate(go.west.2ITEMS.MEAN = rowMeans(select(.,c(go.west1, go.west3)),na.rm = TRUE)) %>%
  mutate(back.Syria.2ITEMS.MEAN = rowMeans(select(.,c(back.syria1,back.syria3)),na.rm = TRUE)) 


###### remove values for ps who answered fewer than 50% ITEMS ON A GIVEN SCALE 

Jordan.data <- Jordan.data %>% 
  mutate(islam.na_count = rowSums(!is.na(select(.,c(islam1,islam4,islam5,islam6,islam7,
                                                    islam8,islam9,islam10,islam11,
                                                    islam12,islam13,islam14,islam2.rev)))))%>% 
  mutate(sacrif.relig.na_count = rowSums(!is.na(select(.,c(sacrif.rel1, sacrif.rel2, sacrif.rel3, 
                                                           sacrif.rel4, sacrif.rel5, sacrif.rel6)))))%>% 
  mutate(sacrif.immigrant.na_count = rowSums(!is.na(select(.,c(sacrif.west1, sacrif.west2, sacrif.west3, 
                                                               sacrif.west4, sacrif.west5, sacrif.west6)))))

Jordan.data <- Jordan.data %>% 
  mutate(islam.ideology.MEAN = ifelse(islam.na_count < 6, NA, islam.ideology.MEAN1))%>%
  mutate(sacrif.immigrant.MEAN = ifelse(sacrif.immigrant.na_count < 3, NA, sacrif.immigrant.MEAN1))
 

## Descriptives and correlations Jordan

describe(Jordan.data$go.west.2ITEMS.MEAN)
describe(Jordan.data$back.Syria.2ITEMS.MEAN)
describe(Jordan.data$islam.ideology.MEAN)
describe(Jordan.data$sacrif.relig.MEAN)
describe(Jordan.data$sacrif.immigrant.MEAN)


corrs.spearman.Jordan <- Jordan.data %>% 
  select(back.Syria.2ITEMS.MEAN, go.west.2ITEMS.MEAN, islam.ideology.MEAN, 
         sacrif.relig.MEAN, sacrif.immigrant.MEAN) %>% 
  correlation(method = "spearman")

##### Plots JORDAN

plot1.J <- ggplot(Jordan.data, aes(x=islam.ideology.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7))+
  labs(x = "Islamist Ideology", y = "Migrate West")+
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p1.J <- ggMarginal(plot1.J, type="histogram", fill = "salmon", binwidth = 0.5)

plot2.J <- ggplot(Jordan.data, aes(x=sacrif.relig.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Religion", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7))+
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p2.J <- ggMarginal(plot2.J, type="histogram", fill = "salmon")
plot2.J.b <- ggMarginal(plot2.J, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot3.J <- ggplot(Jordan.data, aes(x=sacrif.immigrant.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Political Rights", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7))+
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p3.J <- ggMarginal(plot3.J, type="histogram", fill = "salmon")

plot4.J <- ggplot(Jordan.data, aes(x=islam.ideology.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Islamist Ideology", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7))+
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p4.J <- ggMarginal(plot4.J, type="histogram", fill = "salmon3", binwidth = 0.5)
p4.J.b <- ggMarginal(plot4.J, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot5.J <- ggplot(Jordan.data, aes(x=sacrif.relig.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Religion", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7))+
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p5.J <- ggMarginal(plot5.J, type="histogram", fill = "salmon3")
p5.J.b <- ggMarginal(plot5.J, margins = "y", type="histogram", fill = "salmon3")

plot6.J <- ggplot(Jordan.data, aes(x=sacrif.immigrant.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Political Rights", y = "Go back to Syria") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7))+
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p6.J <- ggMarginal(plot6.J, type="histogram", fill = "salmon3")
p6.J.b <- ggMarginal(plot6.J, margins = "y", type="histogram", fill = "salmon3")


##### Lebanon Turkey Iraq

LebTurIrq.data <- read_sav("LEB TUR IRAQ Data OSF.sav")


LebTurIrq.data <- LebTurIrq.data %>%
  mutate(islam.extremism.ALL.MEAN = rowMeans(select(.,c(islam.viol1, islam.viol2, islam.viol3, islam.viol4, 
                                                        islam.viol5, islam.viol6, islam.viol7, islam1, 
                                                        islam2, islam3, islam4, islam5, islam6, islam7, 
                                                        islam8, islam9, islam10, islam11)),
                                             na.rm = TRUE)) %>%
  mutate(sacrif.relig.MEAN = rowMeans(select(.,c(sacrif.rel1:sacrif.rel7)),na.rm = TRUE)) %>%
  mutate(sacrif.refugees.MEAN = rowMeans(select(.,c(sacrif.refugees1:sacrif.refugees7)),na.rm = TRUE)) %>%
  mutate(go.west.2ITEMS.MEAN = rowMeans(select(.,c(go.west1, go.west3)),na.rm = TRUE)) %>%
  mutate(back.Syria.2ITEMS.MEAN = rowMeans(select(.,c(back.syria1,back.syria3)),na.rm = TRUE)) %>%
  mutate(fight.west.MEAN = rowMeans(select(.,c(fight.west1:fight.west5)),na.rm = TRUE)) %>%
  mutate(att.west.MEAN = rowMeans(select(.,c(att.west1:att.west5)),na.rm = TRUE)) 


Lebanon.data <- LebTurIrq.data %>%
  filter(country.recoded == 1)
Turkey.data <- LebTurIrq.data %>%
  filter(country.recoded == 2)
Iraq.data <- LebTurIrq.data %>%
  filter(country.recoded == 3)

## Descriptives and correlations LEBANON

describe(Lebanon.data$go.west.2ITEMS.MEAN)
describe(Lebanon.data$back.Syria.2ITEMS.MEAN)
describe(Lebanon.data$islam.extremism.ALL.MEAN)
describe(Lebanon.data$sacrif.relig.MEAN)
describe(Lebanon.data$sacrif.refugees.MEAN)
describe(Lebanon.data$fight.west.MEAN)
describe(Lebanon.data$att.west.MEAN)

corrs.spearman.Lebanon <- Lebanon.data %>% 
  select(back.Syria.2ITEMS.MEAN, go.west.2ITEMS.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

summary(corrs.spearman.Lebanon)

##### Plots LEBANON

plot1.L <- ggplot(Lebanon.data, aes(x=islam.extremism.ALL.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Islamist Ideology", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p1.L <- ggMarginal(plot1.L, type="histogram", fill = "salmon", binwidth = 0.5)

plot2.L <- ggplot(Lebanon.data, aes(x=sacrif.relig.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Religion", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p2.L <- ggMarginal(plot2.L, type="histogram", fill = "salmon")

plot3.L <- ggplot(Lebanon.data, aes(x=sacrif.refugees.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Political Rights", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p3.L <- ggMarginal(plot3.L, type="histogram", fill = "salmon")

plot4.L <- ggplot(Lebanon.data, aes(x=fight.west.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Fighting West", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p4.L <- ggMarginal(plot4.L, type="histogram", fill = "salmon")

plot5.L <- ggplot(Lebanon.data, aes(x=att.west.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Negative Attitudes West", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p5.L <- ggMarginal(plot5.L, type="histogram", fill = "salmon")

plot6.L <- ggplot(Lebanon.data, aes(x=islam.extremism.ALL.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Islamist Ideology", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p6.L <- ggMarginal(plot6.L, type="histogram", fill = "salmon3", binwidth = 0.5)
p6.L.b <- ggMarginal(plot6.L, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot7.L <- ggplot(Lebanon.data, aes(x=sacrif.relig.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Religion", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p7.L <- ggMarginal(plot7.L, type="histogram", fill = "salmon3")
p7.L.b <- ggMarginal(plot7.L, margins = "y", type="histogram", fill = "salmon3")

plot8.L <- ggplot(Lebanon.data, aes(x=sacrif.refugees.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Political Rights", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p8.L <- ggMarginal(plot8.L, type="histogram", fill = "salmon3")
p8.L.b <- ggMarginal(plot8.L, margins = "y", type="histogram", fill = "salmon3")

plot9.L <- ggplot(Lebanon.data, aes(x=fight.west.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Fighting West", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p9.L <- ggMarginal(plot9.L, type="histogram", fill = "salmon3")
p9.L.b <- ggMarginal(plot9.L, margins = "y", type="histogram", fill = "salmon3")

plot10.L <- ggplot(Lebanon.data, aes(x=att.west.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Negative Attitudes West", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p10.L <- ggMarginal(plot10.L, type="histogram", fill = "salmon3")
p10.L.b <- ggMarginal(plot10.L, margins = "y", type="histogram", fill = "salmon3")

## Additional correlations Lebanon inside and outside camps

Lebanon.data$camp <- as.factor(Lebanon.data$camp)
Lebanon.data$camp <- revalue(Lebanon.data$camp, c("1" = "Yes", "2" = "No"))

corrs.spearman.camp.Lebanon <- Lebanon.data %>% 
  filter(camp == "Yes") %>%
  select(go.west.2ITEMS.MEAN, back.Syria.2ITEMS.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

corrs.spearman.outside.Lebanon <- Lebanon.data %>% 
  filter(camp == "No") %>%
  select(go.west.2ITEMS.MEAN, back.Syria.2ITEMS.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")


## Descriptives and correlations TURKEY

Turkey.data <- LebTurIrq.data %>%
  filter(country.recoded == 2)

describe(Turkey.data$go.west.2ITEMS.MEAN)
describe(Turkey.data$back.Syria.2ITEMS.MEAN)
describe(Turkey.data$islam.extremism.ALL.MEAN)
describe(Turkey.data$sacrif.relig.MEAN)
describe(Turkey.data$sacrif.refugees.MEAN)
describe(Turkey.data$fight.west.MEAN)
describe(Turkey.data$att.west.MEAN)

corrs.spearman.Turkey <- Turkey.data %>% 
  select(back.Syria.2ITEMS.MEAN, go.west.2ITEMS.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")


## plots TURKEY

plot1.T <- ggplot(Turkey.data, aes(x=islam.extremism.ALL.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Islamist Ideology", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p1.T <- ggMarginal(plot1.T, type="histogram", fill = "salmon", binwidth = 0.5)

plot2.T <- ggplot(Turkey.data, aes(x=sacrif.relig.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Religion", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p2.T <- ggMarginal(plot2.T, type="histogram", fill = "salmon", binwidth = 0.5)

plot3.T <- ggplot(Turkey.data, aes(x=sacrif.refugees.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Political Rights", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p3.T <- ggMarginal(plot3.T, type="histogram", fill = "salmon", binwidth = 0.5)

plot4.T <- ggplot(Turkey.data, aes(x=fight.west.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "salmon", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Fighting West", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p4.T <- ggMarginal(plot4.T, type="histogram", fill = "salmon", binwidth = 0.5)

plot5.T <- ggplot(Turkey.data, aes(x=att.west.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Negative Attitudes West", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p5.T <- ggMarginal(plot5.T, type="histogram", fill = "salmon", binwidth = 0.5)

plot6.T <- ggplot(Turkey.data, aes(x=islam.extremism.ALL.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Islamist Ideology", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p6.T <- ggMarginal(plot6.T, type="histogram", fill = "salmon3", binwidth = 0.5)
p6.T.b <- ggMarginal(plot6.T, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot7.T <- ggplot(Turkey.data, aes(x=sacrif.relig.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Religion", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p7.T <- ggMarginal(plot7.T, type="histogram", fill = "salmon3", binwidth = 0.5)
p7.T.b <- ggMarginal(plot7.T, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot8.T <- ggplot(Turkey.data, aes(x=sacrif.refugees.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Sacrifice for Political Rights", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p8.T <- ggMarginal(plot8.T, type="histogram", fill = "salmon3", binwidth = 0.5)
p8.T.b <- ggMarginal(plot8.T, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot9.T <- ggplot(Turkey.data, aes(x=fight.west.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Fighting West", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p9.T <- ggMarginal(plot9.T, type="histogram", fill = "salmon3", binwidth = 0.5)
p9.T.b <- ggMarginal(plot9.T, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot10.T <- ggplot(Turkey.data, aes(x=att.west.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "saddlebrown", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Negative Attitudes West", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p10.T <- ggMarginal(plot10.T, type="histogram", fill = "salmon3", binwidth = 0.5)
p10.T.b <- ggMarginal(plot10.T, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)



###### IRAQ

Iraq.data <- LebTurIrq.data %>%
  filter(country.recoded == 3)
Iraq.data$camp <- as.factor(Iraq.data$camp)
Iraq.data$camp <- revalue(Iraq.data$camp, c("1" = "Yes", "2" = "No"))

corrs.spearman.Iraq <- Iraq.data %>% 
  select(back.Syria.2ITEMS.MEAN, go.west.2ITEMS.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

corrs.spearman.camp.Iraq <- Iraq.data %>% 
  filter(camp == "Yes") %>%
  select(go.west.2ITEMS.MEAN, back.Syria.2ITEMS.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

corrs.spearman.outside.Iraq <- Iraq.data %>% 
  filter(camp == "No") %>%
  select(go.west.2ITEMS.MEAN, back.Syria.2ITEMS.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
   correlation(method = "spearman")
  

describe(Iraq.data$go.west.2ITEMS.MEAN)
describe(Iraq.data$back.Syria.2ITEMS.MEAN)
describe(Iraq.data$islam.extremism.ALL.MEAN)
describe(Iraq.data$sacrif.relig.MEAN)
describe(Iraq.data$sacrif.refugees.MEAN)
describe(Iraq.data$fight.west.MEAN)
describe(Iraq.data$att.west.MEAN)


###### plots IRAQ

plot1.I <- ggplot(Iraq.data, aes(x=islam.extremism.ALL.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) +
  scale_fill_manual(values = c("salmon", "salmon1")) +
  labs(x = "Islamist Ideology", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 
  
p1.I <- ggMarginal(plot1.I, type="histogram", fill = "salmon", binwidth = 0.5)

plot2.I <- ggplot(Iraq.data, aes(x=sacrif.relig.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon", "salmon1")) +
  labs(x = "Sacrifice for Religion", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p2.I <- ggMarginal(plot2.I, type="histogram", fill = "salmon", binwidth = 0.5)

plot3.I <- ggplot(Iraq.data, aes(x=sacrif.refugees.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon", "salmon1")) +
  labs(x = "Sacrifice for Political Rights", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p3.I <- ggMarginal(plot3.I, type="histogram", fill = "salmon", binwidth = 0.5)

plot4.I <- ggplot(Iraq.data, aes(x=fight.west.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon", "salmon1")) +
  labs(x = "Fighting West", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p4.I <- ggMarginal(plot4.I, type="histogram", fill = "salmon", binwidth = 0.5)

plot5.I <- ggplot(Iraq.data, aes(x=att.west.MEAN, y=go.west.2ITEMS.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon", "salmon1")) +
  labs(x = "Negative Attitudes West", y = "Migrate West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p5.I <- ggMarginal(plot5.I, type="histogram", fill = "salmon", binwidth = 0.5)


plot6.I <- ggplot(Iraq.data, aes(x=islam.extremism.ALL.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon3", "salmon3")) +
  labs(x = "Islamist Ideology", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p6.I <- ggMarginal(plot6.I, type="histogram", fill = "salmon3", binwidth = 0.5)
p6.I.b <- ggMarginal(plot6.I, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot7.I <- ggplot(Iraq.data, aes(x=sacrif.relig.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon3", "salmon3")) +
  labs(x = "Sacrifice for Religion", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p7.I <- ggMarginal(plot7.I, type="histogram", fill = "salmon3", binwidth = 0.5)
p7.I.b <- ggMarginal(plot7.I, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot8.I <- ggplot(Iraq.data, aes(x=sacrif.refugees.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon3", "salmon3")) +
  labs(x = "Sacrifice for Political Rights", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p8.I <- ggMarginal(plot8.I, type="histogram", fill = "salmon3", binwidth = 0.5)
p8.I.b <- ggMarginal(plot8.I, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot9.I <- ggplot(Iraq.data, aes(x=fight.west.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon3", "salmon3")) +
  labs(x = "Fighting West", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p9.I <- ggMarginal(plot9.I, type="histogram", fill = "salmon3", binwidth = 0.5)
p9.I.b <- ggMarginal(plot9.I, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot10.I <- ggplot(Iraq.data, aes(x=att.west.MEAN, y=back.Syria.2ITEMS.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("saddlebrown", "salmon2")) + 
  scale_fill_manual(values = c("salmon3", "salmon3")) +
  labs(x = "Negative Attitudes West", y = "Go back to Syria")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p10.I <- ggMarginal(plot10.I, type="histogram", fill = "salmon3", binwidth = 0.5)
p10.I.b <- ggMarginal(plot10.I, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)


##### PLOTS ISLAMIST IDEOLOGY

plot_grid(p1.J, 
          p1.L,
          p1.T,
          p1.I, 
          p4.J.b, 
          p6.L.b,
          p6.T.b,
          p6.I.b,
          nrow=2,
          ncol=4, labels = c("Jordan", "Lebanon", "Turkey", "Iraq"))


##### PLOTS SACRIFICE FOR RELIGION

plot_grid(p2.J, 
          p2.L,
          p2.T,
          p2.I, 
          p5.J.b, 
          p7.L.b,
          p7.T.b,
          p7.I.b,
          nrow=2,
          ncol=4, labels = c("Jordan", "Lebanon", "Turkey", "Iraq"))


##### PLOTS SACRIFICE FOR POLITICAL RIGHTS

plot_grid(p3.J, 
          p3.L,
          p3.T,
          p3.I, 
          p6.J.b, 
          p8.L.b,
          p8.T.b,
          p8.I.b,
          nrow=2,
          ncol=4, labels = c("Jordan", "Lebanon", "Turkey", "Iraq"))

##### PLOTS FIGHTING WEST

plot_grid(p4.L,
          p4.T,
          p4.I, 
          p9.L.b,
          p9.T.b,
          p9.I.b,
          nrow=2,
          ncol=3, labels = c("Lebanon", "Turkey", "Iraq"))

##### PLOTS ATTITUDES WEST

plot_grid(p5.L,
          p5.T,
          p5.I, 
          p10.L.b,
          p10.T.b,
          p10.I.b,
          nrow=2,
          ncol=3, labels = c("Lebanon", "Turkey", "Iraq"))



####### Violent vs. non-violent sacrifice

relig.sacrifice.Jordan.data <- Jordan.data %>%
  select(sacrif.rel1, sacrif.rel2, sacrif.rel5, sacrif.rel6, sacrif.rel3, sacrif.rel4) %>%
  mutate(sacrifice.rel.nviol = rowMeans(select(.,sacrif.rel5, sacrif.rel6, sacrif.rel3, sacrif.rel4), na.rm = TRUE)) %>%
  mutate(sacrifice.rel.viol = rowMeans(select(.,sacrif.rel1, sacrif.rel2), na.rm = TRUE)) %>%
  mutate(country = "Jordan")

relig.sacrifice.Lebanon.data <- Lebanon.data %>%
  select(sacrif.rel1, sacrif.rel2, sacrif.rel5, sacrif.rel6, sacrif.rel3, sacrif.rel4, sacrif.rel7) %>%
  mutate(sacrifice.rel.nviol = rowMeans(select(.,sacrif.rel1, sacrif.rel2, sacrif.rel3, sacrif.rel5), na.rm = TRUE)) %>%
  mutate(sacrifice.rel.viol = rowMeans(select(.,sacrif.rel4, sacrif.rel6, sacrif.rel7), na.rm = TRUE))%>%
  mutate(country = "Lebanon")

relig.sacrifice.Turkey.data <- Turkey.data %>%
  select(sacrif.rel1, sacrif.rel2, sacrif.rel5, sacrif.rel6, sacrif.rel3, sacrif.rel4, sacrif.rel7) %>%
  mutate(sacrifice.rel.nviol = rowMeans(select(.,sacrif.rel1, sacrif.rel2, sacrif.rel3, sacrif.rel5), na.rm = TRUE)) %>%
  mutate(sacrifice.rel.viol = rowMeans(select(.,sacrif.rel4, sacrif.rel6, sacrif.rel7), na.rm = TRUE))%>%
  mutate(country = "Turkey")

relig.sacrifice.Iraq.data <- Iraq.data %>%
  select(sacrif.rel1, sacrif.rel2, sacrif.rel5, sacrif.rel6, sacrif.rel3, sacrif.rel4, sacrif.rel7) %>%
  mutate(sacrifice.rel.nviol = rowMeans(select(.,sacrif.rel1, sacrif.rel2, sacrif.rel3, sacrif.rel5), na.rm = TRUE)) %>%
  mutate(sacrifice.rel.viol = rowMeans(select(.,sacrif.rel4, sacrif.rel6, sacrif.rel7), na.rm = TRUE))%>%
  mutate(country = "Iraq")

relig.sacrifice.Jordan.data <- relig.sacrifice.Jordan.data%>%
  select(sacrifice.rel.nviol,sacrifice.rel.viol, country)
relig.sacrifice.Lebanon.data <- relig.sacrifice.Lebanon.data%>%
  select(sacrifice.rel.nviol,sacrifice.rel.viol, country)
relig.sacrifice.Turkey.data <- relig.sacrifice.Turkey.data%>%
  select(sacrifice.rel.nviol,sacrifice.rel.viol, country)
relig.sacrifice.Iraq.data <- relig.sacrifice.Iraq.data%>%
  select(sacrifice.rel.nviol,sacrifice.rel.viol,country)


relig.sacrifice.all.data <- rbind(relig.sacrifice.Jordan.data, relig.sacrifice.Lebanon.data, 
                                  relig.sacrifice.Turkey.data, relig.sacrifice.Iraq.data)

relig.sacrifice.all.data$country <- factor(relig.sacrifice.all.data$country, 
                                           levels=c("Jordan", "Lebanon", "Turkey", "Iraq"))

viol.sacrifice.plot <- ggplot(relig.sacrifice.all.data, aes(x = country, y = sacrifice.rel.viol), 
                              show.legend = FALSE) +
  geom_jitter(aes(color = country), show.legend = FALSE, alpha = 0.2) + 
  geom_boxplot(aes(fill = country), show.legend = FALSE, alpha = 0.8) + 
  scale_fill_manual(values=c(" salmon", "salmon1", "salmon3", "salmon4")) +
  scale_color_manual(values=c("salmon", "salmon1", "salmon3", "salmon4")) +
  theme_classic() +
  labs(x = "Country", y = "Violent Sacrifice for Religion") + 
  theme(axis.title.x = element_blank(), axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15))+
  scale_y_continuous(limits = c(0.9, 7.1), breaks = c(1,2,3,4,5,6,7)) 

non.viol.sacrifice.plot <- ggplot(relig.sacrifice.all.data, aes(x = country, y = sacrifice.rel.nviol),
                                  show.legend = FALSE) +
  geom_jitter(aes(color = country), show.legend = FALSE, alpha = 0.2) + 
  geom_boxplot(aes(fill = country), show.legend = FALSE, alpha = 0.8) + 
  scale_fill_manual(values=c(" salmon", "salmon1", "salmon3", "salmon4")) +
  scale_color_manual(values=c("salmon", "salmon1", "salmon3", "salmon4")) +
  theme_classic() +
  labs(x = "Country", y = "Non-violent Sacrifice for Religion")+ 
  theme(axis.title.x = element_blank(), axis.title = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 15))+
  scale_y_continuous(limits = c(0.9, 7.1), breaks = c(1,2,3,4,5,6,7)) 

plot_grid(viol.sacrifice.plot, 
  non.viol.sacrifice.plot)


######### Expectations toward the West

## Jordan

Jordan.data <- Jordan.data %>%
  mutate(los.west3.rev=8-los.west3,
         los.west8.rev=8-los.west8,
         los.west10.rev=8-los.west10,
         los.west11.rev=8-los.west11,
         west.exp1.rev=8-west.exp1,
         west.exp2.rev=8-west.exp2,
         west.exp4.rev=8-west.exp4,
         west.exp6.rev=8-west.exp6,
         west.exp9.rev=8-west.exp9,
         west.exp10.rev=8-west.exp10)

Jordan.data <- Jordan.data %>%
  mutate(west.basic.needs.all.MEAN=rowMeans(select(.,c(west.exp11, west.exp12, west.exp13, 
                                         west.exp14, west.exp3, west.exp5, 
                                         west.exp8, west.exp1.rev, west.exp2.rev, 
                                         west.exp4.rev, west.exp6.rev, west.exp9.rev, 
                                         west.exp10.rev)), na.rm = TRUE))%>%
  mutate(west.frustr.basic.needs.MEAN=8-west.basic.needs.all.MEAN)%>%
  mutate(los.west.MEAN=rowMeans(select(.,c(los.west1, los.west2, los.west4, los.west5, 
                                           los.west6, los.west9, los.west3.rev, los.west8.rev, 
                                           los.west10.rev)), na.rm = TRUE)) #### los.west11 = proud was not included due to low correlations 
         
neg.expectations.Jordan.data <- Jordan.data %>%
  select(islam.ideology.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN, 
         sacrif.relig.MEAN, sacrif.immigrant.MEAN) %>%
  mutate(neg.expect.MEAN = rowMeans(select(., west.frustr.basic.needs.MEAN,los.west.MEAN), na.rm = TRUE))

corrs.spearman.expect.Jordan <- neg.expectations.Jordan.data %>% 
  select(neg.expect.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN, islam.ideology.MEAN, sacrif.relig.MEAN, sacrif.immigrant.MEAN) %>% 
  correlation(method = "spearman")

## Lebanon

Lebanon.data <- Lebanon.data %>%
  mutate(los.west3.rev=8-los.west3,
         los.west8.rev=8-los.west8,
         los.west10.rev=8-los.west10,
         los.west11.rev=8-los.west11,
         west.exp1.rev=8-west.exp1,
         west.exp2.rev=8-west.exp2,
         west.exp8.rev=8-west.exp8)

Lebanon.data <- Lebanon.data %>%
  mutate(west.basic.needs.all.MEAN=rowMeans(select(.,c(west.exp1.rev, west.exp2.rev, west.exp3, west.exp4, 
                                                       west.exp5, west.exp6, 
                                                       west.exp7, west.exp8.rev, west.exp9, west.exp10, 
                                                       west.exp11, west.exp12)), na.rm = TRUE))%>%
  mutate(west.frustr.basic.needs.MEAN=8-west.basic.needs.all.MEAN)%>%
  mutate(los.west.MEAN=rowMeans(select(.,c(los.west1, los.west2, los.west4, los.west5, los.west6, los.west9, 
                                           los.west3.rev, los.west8.rev, los.west10.rev, los.west11.rev)), na.rm = TRUE))


neg.expectations.Lebanon.data <- Lebanon.data %>%
  select(islam.extremism.ALL.MEAN, sacrif.relig.MEAN, sacrif.refugees.MEAN, fight.west.MEAN, 
         att.west.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN) %>%
  mutate(neg.expect.MEAN = rowMeans(select(., west.frustr.basic.needs.MEAN,los.west.MEAN), na.rm = TRUE))


corrs.spearman.expect.Lebanon <- neg.expectations.Lebanon.data %>% 
  select(neg.expect.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

## Turkey

Turkey.data <- Turkey.data %>%
  mutate(los.west3.rev=8-los.west3,
         los.west8.rev=8-los.west8,
         los.west10.rev=8-los.west10,
         los.west11.rev=8-los.west11,
         west.exp1.rev=8-west.exp1,
         west.exp2.rev=8-west.exp2,
         west.exp8.rev=8-west.exp8)

Turkey.data <- Turkey.data %>%
  mutate(west.basic.needs.all.MEAN=rowMeans(select(.,c(west.exp1.rev, west.exp2.rev, west.exp3, west.exp4, 
                                                       west.exp5, west.exp6, 
                                                       west.exp7, west.exp8.rev, west.exp9, west.exp10, 
                                                       west.exp11, west.exp12)), na.rm = TRUE))%>%
  mutate(west.frustr.basic.needs.MEAN=8-west.basic.needs.all.MEAN)%>%
  mutate(los.west.MEAN=rowMeans(select(.,c(los.west1, los.west2, los.west4, los.west5, los.west6, los.west9, 
                                           los.west3.rev, los.west8.rev, los.west10.rev, los.west11.rev)), na.rm = TRUE))

neg.expectations.Turkey.data <- Turkey.data %>%
  select(islam.extremism.ALL.MEAN, sacrif.relig.MEAN, sacrif.refugees.MEAN, fight.west.MEAN, 
         att.west.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN) %>%
  mutate(neg.expect.MEAN = rowMeans(select(., west.frustr.basic.needs.MEAN,los.west.MEAN), na.rm = TRUE))


corrs.spearman.expect.Turkey <- neg.expectations.Turkey.data %>% 
  select(neg.expect.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

## Iraq

Iraq.data <- Iraq.data %>%
  mutate(los.west3.rev=8-los.west3,
         los.west8.rev=8-los.west8,
         los.west10.rev=8-los.west10,
         los.west11.rev=8-los.west11,
         west.exp1.rev=8-west.exp1,
         west.exp2.rev=8-west.exp2,
         west.exp8.rev=8-west.exp8)

Iraq.data <- Iraq.data %>%
  mutate(west.basic.needs.all.MEAN=rowMeans(select(.,c(west.exp1.rev, west.exp2.rev, west.exp3, west.exp4, 
                                                       west.exp5, west.exp6, 
                                                       west.exp7, west.exp8.rev, west.exp9, west.exp10, 
                                                       west.exp11, west.exp12)), na.rm = TRUE))%>%
  mutate(west.frustr.basic.needs.MEAN=8-west.basic.needs.all.MEAN)%>%
  mutate(los.west.MEAN=rowMeans(select(.,c(los.west1, los.west2, los.west4, los.west5, los.west6, los.west9, 
                                           los.west3.rev, los.west8.rev, los.west10.rev, los.west11.rev)), na.rm = TRUE))

neg.expectations.Iraq.data <- Iraq.data %>%
  select(islam.extremism.ALL.MEAN, sacrif.relig.MEAN, sacrif.refugees.MEAN, fight.west.MEAN, 
         att.west.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN, camp) %>%
  mutate(neg.expect.MEAN = rowMeans(select(., west.frustr.basic.needs.MEAN,los.west.MEAN), na.rm = TRUE))


corrs.spearman.expect.Iraq <- neg.expectations.Iraq.data %>% 
  select(neg.expect.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

corrs.spearman.expect.camp.Iraq <- neg.expectations.Iraq.data %>% 
  filter(camp == "Yes") %>%
  select(neg.expect.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

corrs.spearman.expect.outside.Iraq <- neg.expectations.Iraq.data %>% 
  filter(camp == "No") %>%
  select(neg.expect.MEAN, west.frustr.basic.needs.MEAN,los.west.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, 
         sacrif.refugees.MEAN, fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

## PLOTS

plot1.needs <- ggplot(neg.expectations.Jordan.data, aes(x=islam.ideology.MEAN, y=neg.expect.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "salmon", fill = "salmon", alpha=0.15) +
  theme_classic() +
  labs(x = "Islamist Ideology", y = "Negative expectations toward the West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p1.needs <- ggMarginal(plot1.needs, margins = "y", type="histogram", fill = "salmon", binwidth = 0.5)


plot1.L.needs <- ggplot(neg.expectations.Lebanon.data, aes(x=islam.extremism.ALL.MEAN, y=neg.expect.MEAN)) +
  geom_point(color = "salmon1", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "salmon1", fill = "salmon1", alpha=0.15) +
  theme_classic() +
  labs(x = "Islamist Ideology", y = "Negative expectations toward the West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p1.L.needs <- ggMarginal(plot1.L.needs, margins = "y", type="histogram", fill = "salmon1", binwidth = 0.5)

plot1.T.needs <- ggplot(neg.expectations.Turkey.data, aes(x=islam.extremism.ALL.MEAN, y=neg.expect.MEAN)) +
  geom_point(color = "salmon3", size = 2, alpha = .50) + 
  geom_smooth(method=lm, color = "salmon3", fill = "salmon3", alpha=0.15) +
  theme_classic() +
  labs(x = "Islamist Ideology", y = "Negative expectations toward the West")+
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p1.T.needs <- ggMarginal(plot1.T.needs, margins = "y", type="histogram", fill = "salmon3", binwidth = 0.5)

plot1.I.needs <- ggplot(neg.expectations.Iraq.data, aes(x=islam.extremism.ALL.MEAN, y=neg.expect.MEAN)) +
  geom_point(color = "salmon", size = 2, alpha = .50) + 
  geom_smooth(aes(color=camp, fill = camp), 
              method=lm, alpha=0.15) +
  theme_classic() +
  scale_color_manual(values = c("salmon3", "salmon4")) + 
  scale_fill_manual(values = c("salmon3", "salmon4")) +
  labs(x = "Islamist Ideology", y = "Negative expectations toward the West") +
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) 

p1.I.needs <- ggMarginal(plot1.I.needs, margins = "y", type="histogram", fill = "salmon4", binwidth = 0.5)


plot_grid(p1.needs, 
          p1.L.needs,
          p1.T.needs,
          p1.I.needs,
          nrow=2,
          ncol=2, labels = c("Jordan", "Lebanon", "Turkey", "   Iraq"), 
          hjust = -1)



###### Supplemental analyses ###### 

## Correlations with NFC

Jordan.data <- Jordan.data %>% 
  mutate(nfc.na_count = rowSums(!is.na(select(.,c(nfc1:nfc15)))))
table(Jordan.data$nfc.na_count)

Jordan.data <- Jordan.data %>%
  mutate(nfc.MEAN = rowMeans(select(.,c(nfc1:nfc15)), na.rm = TRUE))
  
corrs.NFC.Jordan <- Jordan.data %>% 
  select(nfc.MEAN, islam.ideology.MEAN, sacrif.relig.MEAN, sacrif.immigrant.MEAN) %>% 
  correlation(method = "spearman")

Lebanon.data <- Lebanon.data %>%
  mutate(nfc.MEAN = rowMeans(select(.,c(nfc1:nfc15)), na.rm = TRUE))

corrs.NFC.Lebanon <- Lebanon.data %>% 
  select(nfc.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, sacrif.refugees.MEAN, 
         fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")


Turkey.data <- Turkey.data %>%
  mutate(nfc.MEAN = rowMeans(select(.,c(nfc1:nfc15)), na.rm = TRUE))

corrs.NFC.Turkey <- Turkey.data %>% 
  select(nfc.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, sacrif.refugees.MEAN, 
         fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

Iraq.data$camp <- as.factor(Iraq.data$camp)
Iraq.data$camp <- revalue(Iraq.data$camp, c("1" = "Yes", "2" = "No"))

Iraq.data <- Iraq.data %>%
  mutate(nfc.MEAN = rowMeans(select(.,c(nfc1:nfc15)), na.rm = TRUE))

corrs.NFC.camp.Iraq <- Iraq.data %>% 
  filter(camp == "Yes") %>%
  select(nfc.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, sacrif.refugees.MEAN, 
         fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")

corrs.NFC.outside.Iraq <- Iraq.data %>% 
  filter(camp == "No") %>%
  select(nfc.MEAN, islam.extremism.ALL.MEAN, sacrif.relig.MEAN, sacrif.refugees.MEAN, 
         fight.west.MEAN, att.west.MEAN) %>% 
  correlation(method = "spearman")



###### Sacrifice for religion vs. Sacrifice for safety

## JORDAN
Jordan.data <- Jordan.data %>%
  mutate(sacrifice.safety.mean = rowMeans(select(.,sacrif.safe1, sacrif.safe2, sacrif.safe3, sacrif.safe4, sacrif.safe5), na.rm = TRUE)) 

corrs.safety.Jordan <- Jordan.data %>% 
  select(sacrif.relig.MEAN, sacrif.immigrant.MEAN, sacrifice.safety.mean) %>% 
  correlation(method = "spearman")

sacrifice.Jordan.lm <- lm(go.west.2ITEMS.MEAN ~ sacrif.relig.MEAN + sacrifice.safety.mean, Jordan.data)
summary(sacrifice.Jordan.lm)
beta(sacrifice.Jordan.lm)

## LEBANON
Lebanon.data <- Lebanon.data %>%
  mutate(sacrifice.safety.mean = rowMeans(select(.,sacrif.safe1, sacrif.safe2, sacrif.safe3, sacrif.safe4, sacrif.safe5, sacrif.safe6), na.rm = TRUE)) 

corrs.safety.Lebanon <- Lebanon.data %>% 
  select(sacrif.relig.MEAN, sacrif.refugees.MEAN, sacrifice.safety.mean) %>% 
  correlation(method = "spearman")

sacrifice.Lebanon.lm <- lm(go.west.2ITEMS.MEAN ~ sacrif.relig.MEAN + sacrifice.safety.mean, Lebanon.data)
summary(sacrifice.Lebanon.lm)
beta(sacrifice.Lebanon.lm)

## TURKEY

Turkey.data <- Turkey.data %>%
  mutate(sacrifice.safety.mean = rowMeans(select(.,sacrif.safe1, sacrif.safe2, sacrif.safe3, sacrif.safe4, sacrif.safe5, sacrif.safe6), na.rm = TRUE)) 

corrs.safety.Turkey <- Turkey.data %>% 
  select(sacrif.relig.MEAN, sacrif.refugees.MEAN, sacrifice.safety.mean) %>% 
  correlation(method = "spearman")

sacrifice.Turkey.lm <- lm(go.west.2ITEMS.MEAN ~ sacrif.relig.MEAN + sacrifice.safety.mean, Turkey.data)
summary(sacrifice.Turkey.lm)
beta(sacrifice.Turkey.lm)
plot(sacrifice.Turkey.lm, which=1:4)

## IRAQ

Iraq.data$camp <- as.factor(Iraq.data$camp)
Iraq.data$camp <- revalue(Iraq.data$camp, c("1" = "Yes", "2" = "No"))

Iraq.data <- Iraq.data %>%
  mutate(sacrifice.safety.mean = rowMeans(select(.,sacrif.safe1, sacrif.safe2, sacrif.safe3, sacrif.safe4, sacrif.safe5, sacrif.safe6), na.rm = TRUE)) 

corrs.safety.Iraq <- Iraq.data %>% 
  select(sacrif.relig.MEAN, sacrif.refugees.MEAN, sacrifice.safety.mean) %>% 
  correlation(method = "spearman")

sacrifice.Iraq.lm <- lm(go.west.2ITEMS.MEAN ~ sacrif.relig.MEAN + sacrifice.safety.mean + camp, Iraq.data)
summary(sacrifice.Iraq.lm)
beta(sacrifice.Iraq.lm)

plot(sacrifice.Iraq.lm, which=1:4)


####### Violent vs. non-violent sacrifice

Jordan.data <- Jordan.data %>%
  mutate(sacrifice.rel.nviol.mean = rowMeans(select(.,sacrif.rel5, sacrif.rel6, sacrif.rel3, sacrif.rel4), na.rm = TRUE)) %>%
  mutate(sacrifice.rel.viol.mean = rowMeans(select(.,sacrif.rel1, sacrif.rel2), na.rm = TRUE))

corrs.n.viol.Jordan <- Jordan.data %>% 
  select(sacrifice.rel.nviol.mean, sacrifice.rel.viol.mean, go.west.2ITEMS.MEAN) %>% 
  correlation(method = "spearman")

Lebanon.data <- Lebanon.data %>%
  mutate(sacrifice.rel.nviol.mean = rowMeans(select(.,sacrif.rel1, sacrif.rel2, sacrif.rel3, sacrif.rel5), na.rm = TRUE)) %>%
  mutate(sacrifice.rel.viol.mean = rowMeans(select(.,sacrif.rel4, sacrif.rel6, sacrif.rel7), na.rm = TRUE))

corrs.n.viol.Lebanon <- Lebanon.data %>% 
  select(sacrifice.rel.nviol.mean, sacrifice.rel.viol.mean, go.west.2ITEMS.MEAN) %>% 
  correlation(method = "spearman")


Turkey.data <- Turkey.data %>%
  mutate(sacrifice.rel.nviol.mean = rowMeans(select(.,sacrif.rel1, sacrif.rel2, sacrif.rel3, sacrif.rel5), na.rm = TRUE)) %>%
  mutate(sacrifice.rel.viol.mean = rowMeans(select(.,sacrif.rel4, sacrif.rel6, sacrif.rel7), na.rm = TRUE))

corrs.n.viol.Turkey <- Turkey.data %>% 
  select(sacrifice.rel.nviol.mean, sacrifice.rel.viol.mean, go.west.2ITEMS.MEAN) %>% 
  correlation(method = "spearman")


Iraq.data <- Iraq.data %>%
  mutate(sacrifice.rel.nviol.mean = rowMeans(select(.,sacrif.rel1, sacrif.rel2, sacrif.rel3, sacrif.rel5), na.rm = TRUE)) %>%
  mutate(sacrifice.rel.viol.mean = rowMeans(select(.,sacrif.rel4, sacrif.rel6, sacrif.rel7), na.rm = TRUE))

corrs.nviol.camp.Iraq <- Iraq.data %>% 
  filter(camp == "Yes") %>%
  select(sacrifice.rel.nviol.mean, sacrifice.rel.viol.mean, go.west.2ITEMS.MEAN) %>% 
  correlation(method = "spearman")

corrs.nviol.outside.Iraq <- Iraq.data %>% 
  filter(camp == "No") %>%
  select(sacrifice.rel.nviol.mean, sacrifice.rel.viol.mean, go.west.2ITEMS.MEAN) %>% 
  correlation(method = "spearman")



