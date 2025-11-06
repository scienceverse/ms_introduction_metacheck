
setwd("~/Documents/Scholarship/Manuscripts/Georgetown/Weekly paper/ANALYSES/UBC_UVA")

options(buildtools.check = function(action) TRUE)
devtools::install_version("haven", version = "2.2.0")
devtools::install_version("sjPlot", version = "2.8.1")

library(haven)


###########Self-REport########

d<-read.csv("UBCUVA-long-main-Computes&Health.csv")
names(d)
d$time
d$time<-d$time-3
d$time


# TRANSFORMATIONS
# d$COND <- as.factor(d$COND) 
# d$sample.A1<-as.factor(d$sample.A1)



library(lme4)
library(nlme)

#Number of sick days
mod.s <-glmer(BRFFS20 ~ time * COND + (1| SUB),
              d, na.action = na.omit,
              glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
              family="poisson") 
summary(mod.s)

#number of days sickness prevents activity
mod.s <-glmer(BRFFS40 ~ time * COND + (1| SUB),
              d, na.action = na.omit,
              glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
              family="poisson") 
summary(mod.s)

#number of days feeling energetic
mod.s <-glmer(BRFFS80 ~ time * COND + (1| SUB),
              d, na.action = na.omit,
              glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
              family="poisson") 
summary(mod.s)


require(sjPlot)
tab_model(mod.s,
          show.stat = T, show.df = T, show.std = "std2", show.se=T, show.ci = F)


#COHEN-D
library(EMAtools)
#Effect size
dd<-as.data.frame(d)
library(nlme)
mod <-lme(BRFFS80 ~ time * COND, random = ~ 1|SUB, data=dd, na.action = na.omit)
summary(mod)
lme.dscore(mod, dd, "nlme")




########BIOLOGICAL ANALYSES######

biol<-read.csv("UBCUVA-long-bio.csv")



# d$COND<-as.numeric(d$COND)
# d$COND <- cut(d$COND, 2, labels=c("Control", "ENHANCE"))
# is.factor(d$COND)
# 
# d$sample.A1<-as.numeric(d$sample.A1)
# d$sample.A1 <- cut(d$sample.A1, 2, labels=c("Online", "InPerson"))
# is.factor(d$sample.A1)

biol$time
biol$time<-d$time-3

names(d)

library(nlme)
library(EMAtools)


fm2 <- lme(BMI ~ time*COND, random = ~1|id,
           data = biol, na.action = na.omit)
summary(fm2)
lme.dscore(fm2,d, "nlme")


fm4 <- lme(Systolic ~ time * COND, random = ~1|id,
           data = biol, na.action = na.omit)
summary(fm4)
lme.dscore(fm4,d, "nlme")

fm5 <- lme(Diastolic ~ time*COND,  random = ~1|id,
           data = biol, na.action = na.omit)
summary(fm5)
lme.dscore(fm5,d, "nlme")



###########PLOTTING########
require(ggplot2)
library(hmisc)

#Value Lables
d$Condition<-d$COND
d$Condition <- factor(d$Condition,
                      levels = c(0,1),
                      labels = c("Control", "Treatment"))

d$time3<-d$time+3
d$time3
d$time3 <- factor(d$time3,
                      levels = c(1,2,3),
                      labels = c("Baseline", "Post-Treat", "Follow-up"))





#RAINCLOUD
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)
library(tidyverse)


raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 0, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))





#Summary Stats
lb <- function(x) mean(x,na.rm=TRUE) - sd(x,na.rm=TRUE)
ub <- function(x) mean(x,na.rm=TRUE) + sd(x,na.rm=TRUE)

sumld <- d %>% 
  select(-SUB) %>% 
  group_by(time3, Condition) %>% 
   summarise (mean = mean(BRFFS80,na.rm=TRUE ), 
              median = median(BRFFS80, na.rm=TRUE), 
              lower = lb(BRFFS80), upper = ub(BRFFS80))
sumld

# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r

library(ggplot2)
library(dplyr)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

#Raincloud Plot
g <- ggplot(data = d, aes(y = BRFFS20, x = time3, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = BRFFS40, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # expand_limits(x = 5.25) +
  # guides(fill = FALSE) +
  # guides(color = FALSE) +
  scale_color_manual(values=c("blue", "red")) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(y="Days Feeling Sick", x="Time") + 
  # coord_flip() +
  theme_bw() +
  raincloud_theme
g
g <- ggplot(data = d, aes(y = BRFFS40, x = time3, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = BRFFS40, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # expand_limits(x = 5.25) +
  # guides(fill = FALSE) +
  # guides(color = FALSE) +
  scale_color_manual(values=c("blue", "red")) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(y="Days Sickness Prevent Activity", x="Time") + 
  # coord_flip() +
  theme_bw() +
  raincloud_theme
g
g <- ggplot(data = d, aes(y = BRFFS80, x = time3, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = BRFFS80, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # expand_limits(x = 5.25) +
  # guides(fill = FALSE) +
  # guides(color = FALSE) +
  scale_color_manual(values=c("blue", "red")) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(y="Days Feeling Healthy", x="Time") + 
  # coord_flip() +
  theme_bw() +
  raincloud_theme
g


biol$Condition<-biol$COND
biol$Condition <- factor(biol$Condition,
                      levels = c(0,1),
                      labels = c("Control", "Treatment"))


biol$time3<-biol$time
biol$time3 <- factor(biol$time3,
                  levels = c(1,2,3),
                  labels = c("Baseline", "Post-Treat", "Follow-up"))

g <- ggplot(data = biol, aes(y = Diastolic, x = time3, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Diastolic, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # expand_limits(x = 5.25) +
  # guides(fill = FALSE) +
  # guides(color = FALSE) +
  scale_color_manual(values=c("blue", "red")) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(y="Diastolic Blood Pressure", x="Time") + 
  # coord_flip() +
  theme_bw() +
  raincloud_theme
g

g <- ggplot(data = biol, aes(y = Systolic, x = time3, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Systolic, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # expand_limits(x = 5.25) +
  # guides(fill = FALSE) +
  # guides(color = FALSE) +
  scale_color_manual(values=c("blue", "red")) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(y="Systolic Blood Pressure", x="Time") + 
  # coord_flip() +
  theme_bw() +
  raincloud_theme
g

g <- ggplot(data = biol, aes(y = BMI, x = time3, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = BMI, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  # expand_limits(x = 5.25) +
  # guides(fill = FALSE) +
  # guides(color = FALSE) +
  scale_color_manual(values=c("blue", "red")) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(y="Body Mass Index", x="Time") + 
  # coord_flip() +
  theme_bw() +
  raincloud_theme
g








