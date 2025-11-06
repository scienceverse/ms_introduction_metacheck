require(ggplot2)
require(cowplot)

load("plot_data.Rda")

Times <- windowsFonts(Times=windowsFont("Times New Roman"))

plot1 <- ggplot(data=dat, aes(x=dat$tau_prior, y=dat$bf, 
                              group=dat$int_prior, shape=dat$int_prior, 
                              colour=dat$int_prior)) + 
  geom_hline(yintercept = 3, linetype = 2)+
  geom_line(aes(color = int_prior), size = .75) +     
  geom_point(size = 2.5, fill = "white", color = "black") +         
  scale_colour_grey(name = expression(paste("Prior on", ~mu)), 
                    start = .5, end = .1) +                 
  scale_shape_manual(name = expression(paste("Prior on", ~mu)), 
                     values=c(22, 21, 20)) +      
  ylab(bquote('Bayes Factor')) + 
  scale_y_continuous(limits = c(0, 13), breaks = 0:13) +
  xlab(expression(paste("Scale of Half Cauchy Prior on ", tau))) +
  theme_cowplot(font_size = 17, font_family = "Times") +
  theme(legend.position=c(.80, .75), 
        axis.line = element_line(),
        plot.margin = unit(c(.25, 0, 0, 0), "cm"))
 

plot2 <- ggplot(data=dat, aes(x=dat$tau_prior, y=dat$estimate, 
                              group=dat$int_prior, shape=dat$int_prior, 
                              colour=dat$int_prior))  +     
  geom_errorbar(aes(ymin=dat$l_95, ymax=dat$u_95, color = int_prior), 
                width =.25, size = .75, position = position_dodge(.5), show.legend = FALSE) +
  geom_point(color = "black", size = 2.5, fill = "white", 
             position = position_dodge(.5), show.legend = FALSE) +         
  geom_hline(yintercept = 0, linetype = 2) +
  scale_colour_grey(name = expression(paste("Prior on", ~mu)), 
                    start = .5, end = .1 ) +                 
  scale_shape_manual(name = expression(paste("Prior on", ~mu)), 
                     values=c(22, 21, 20)) +      
  scale_y_continuous(limits = c(-.2, .5), 
                     breaks = c(-.2, -.2, -.1, 0, .1, .2, .3, .4, .5)) +
  xlab(expression(paste("Scale of Cauchy Prior on ", tau)))+
  ylab("Log Odds Ratio") +
  theme_cowplot(font_size = 17, font_family = "Times") +
  theme(legend.position=c(.15, .15), 
        axis.line = element_line(),
        axis.title.x=element_blank(),
        plot.margin = unit(c(.25,0,0,0), "cm"))
 
tiff("OR_BF_600.tif", height=6, width=8, compression = "lzw", units = "in", res = 600)
plot_grid(plot2, plot1, labels = c("A", "B"), nrow = 2, align = "v",
          rel_heights = c(1, 1.25))
dev.off()
  