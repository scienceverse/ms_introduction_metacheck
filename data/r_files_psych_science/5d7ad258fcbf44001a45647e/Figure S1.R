D0 = read_excel("/Users/All Studies SOI SE.xlsx", sheet = "F", col_names = TRUE) 
D0$Sex = factor(D0$Sex, levels = c(2, 1), labels = c("Female", "Male"))

ggarrange(
  ggplot(D0, aes(x = SOI_B, y = Own.SE, group = Sex, color=Sex))+
    stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(linetype = "Quadratic"))+
    stat_smooth(method = "lm", se=F, aes(linetype = "Linear"), size =.8, alpha = .8)+
    geom_jitter(alpha = .2)+ 
    labs(title = "", x = "Own Sexual Behavior", y = "Own Self-Esteem")+
    theme_bw()+
    theme(legend.position="bottom")+ coord_cartesian(ylim=c(1, 4))+
    scale_color_brewer(palette = "Set1")+
    scale_linetype_manual(values=c("twodash", "solid")), 
  ggplot(D0, aes(x = SOI_D, y = Own.SE, group = Sex, color=Sex))+
    stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(linetype = "Quadratic"))+
    stat_smooth(method = "lm", se=F, aes(linetype = "Linear"), size =.8, alpha = .8)+
    geom_jitter(alpha = .2)+ 
    labs(title = "", x = "Own Sexual Attitude/Desire", y = "Own Self-Esteem")+
    theme_bw()+
    theme(legend.position="bottom")+ coord_cartesian(ylim=c(1, 4))+
    scale_color_brewer(palette = "Set1")+
    scale_linetype_manual(values=c("twodash", "solid")), 
  common.legend = T, labels = c("a)", "b)"))
