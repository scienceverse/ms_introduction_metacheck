###set seed 
set.seed(72684)

###Grab data
pbpFull <- pbpFullCleaned

############################### Plot models for different counts ############################
### RHB vs LHP - model and predict
pbpFull1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$count.reel == "0-0")

gam1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
                             method="GCV.Cp", data = pbpFull1, family=binomial(link="logit"), cluster=cl)


df_p1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                      pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p1$lp <- predict(gam1, df_p1)
df_p1$Probability <- exp(df_p1$lp) / (1 + exp(df_p1$lp))


pbpFull1.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$count.reel == "0-2")

gam1.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
            method="GCV.Cp", data = pbpFull1.1, family=binomial(link="logit"), cluster=cl)


df_p1.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                     pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p1.1$lp <- predict(gam1.1, df_p1.1)
df_p1.1$Probability <- exp(df_p1.1$lp) / (1 + exp(df_p1.1$lp))


pbpFull1.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$count.reel == "3-0")

gam1.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull1.2, family=binomial(link="logit"), cluster=cl)


df_p1.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p1.2$lp <- predict(gam1.2, df_p1.2)
df_p1.2$Probability <- exp(df_p1.2$lp) / (1 + exp(df_p1.2$lp))


### RHB vs RHP - model and predict
pbpFull2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$count.reel == "0-0")

gam2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
            method="GCV.Cp", data = pbpFull2, family=binomial(link="logit"), cluster=cl)


df_p2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                     pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p2$lp <- predict(gam2, df_p2)
df_p2$Probability <- exp(df_p2$lp) / (1 + exp(df_p2$lp))


pbpFull2.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$count.reel == "0-2")

gam2.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
            method="GCV.Cp", data = pbpFull2.1, family=binomial(link="logit"), cluster=cl)


df_p2.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                     pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p2.1$lp <- predict(gam2.1, df_p2.1)
df_p2.1$Probability <- exp(df_p2.1$lp) / (1 + exp(df_p2.1$lp))


pbpFull2.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$count.reel == "3-0")

gam2.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull2.2, family=binomial(link="logit"), cluster=cl)


df_p2.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p2.2$lp <- predict(gam2.2, df_p2.2)
df_p2.2$Probability <- exp(df_p2.2$lp) / (1 + exp(df_p2.2$lp))

### LHB vs LHP - model and predict

pbpFull3 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$count.reel == "0-0")

gam3 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
            method="GCV.Cp", data = pbpFull3, family=binomial(link="logit"), cluster=cl)


df_p3 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                     pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p3$lp <- predict(gam3, df_p3)
df_p3$Probability <- exp(df_p3$lp) / (1 + exp(df_p3$lp))


pbpFull3.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$count.reel == "0-2")

gam3.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
            method="GCV.Cp", data = pbpFull3.1, family=binomial(link="logit"), cluster=cl)


df_p3.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                     pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p3.1$lp <- predict(gam3.1, df_p3.1)
df_p3.1$Probability <- exp(df_p3.1$lp) / (1 + exp(df_p3.1$lp))


pbpFull3.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$count.reel == "3-0")

gam3.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull3.2, family=binomial(link="logit"), cluster=cl)


df_p3.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p3.2$lp <- predict(gam3.2, df_p3.2)
df_p3.2$Probability <- exp(df_p3.2$lp) / (1 + exp(df_p3.2$lp))

### LHB vs RHP - model and predict

pbpFull4 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$count.reel == "0-0")

gam4 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
            method="GCV.Cp", data = pbpFull4, family=binomial(link="logit"), cluster=cl)


df_p4 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                     pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p4$lp <- predict(gam4, df_p4)
df_p4$Probability <- exp(df_p4$lp) / (1 + exp(df_p4$lp))


pbpFull4.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$count.reel == "0-2")

gam4.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
            method="GCV.Cp", data = pbpFull4.1, family=binomial(link="logit"), cluster=cl)


df_p4.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                     pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p4.1$lp <- predict(gam4.1, df_p4.1)
df_p4.1$Probability <- exp(df_p4.1$lp) / (1 + exp(df_p4.1$lp))


pbpFull4.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$count.reel == "3-0")

gam4.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull4.2, family=binomial(link="logit"), cluster=cl)


df_p4.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p4.2$lp <- predict(gam4.2, df_p4.2)
df_p4.2$Probability <- exp(df_p4.2$lp) / (1 + exp(df_p4.2$lp))


############ Plot everything with Geom_contour_filled

###Create custom palette
blrd_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 11, direction = -1))(21)
blrd_interpolated %>% scales::show_col()

#RHB vs LHP
plot1 <- ggplot(df_p1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
    geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() +
  theme(text=element_text(size=11))
  
plot1 <- plot1 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot1 <- plot1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot1 <- plot1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot1 <- plot1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot1.1 <- ggplot(df_p1.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() +
  theme(text=element_text(size=11))

plot1.1 <- plot1.1 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot1.1 <- plot1.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot1.1 <- plot1.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot1.1 <- plot1.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot1.2 <- ggplot(df_p1.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() +
  theme(text=element_text(size=11))

plot1.2 <- plot1.2 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot1.2 <- plot1.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot1.2 <- plot1.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot1.2 <- plot1.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

#RHB vs RHP
plot2 <- ggplot(df_p2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot2 <- plot2 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot2 <- plot2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot2 <- plot2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot2 <- plot2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot2.1 <- ggplot(df_p2.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot2.1 <- plot2.1 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot2.1 <- plot2.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot2.1 <- plot2.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot2.1 <- plot2.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot2.2 <- ggplot(df_p2.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot2.2 <- plot2.2 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot2.2 <- plot2.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot2.2 <- plot2.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot2.2 <- plot2.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


###LHB vs LHP

plot3 <- ggplot(df_p3, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot3 <- plot3 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot3 <- plot3 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot3 <- plot3 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot3 <- plot3 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot3.1 <- ggplot(df_p3.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot3.1 <- plot3.1 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot3.1 <- plot3.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot3.1 <- plot3.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot3.1 <- plot3.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot3.2 <- ggplot(df_p3.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot3.2 <- plot3.2 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot3.2 <- plot3.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot3.2 <- plot3.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot3.2 <- plot3.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

###LHB vs RHP

plot4 <- ggplot(df_p4, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot4 <- plot4 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot4 <- plot4 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot4 <- plot4 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot4 <- plot4 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot4.1 <- ggplot(df_p4.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot4.1 <- plot4.1 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot4.1 <- plot4.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot4.1 <- plot4.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot4.1 <- plot4.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot4.2 <- ggplot(df_p4.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot4.2 <- plot4.2 + geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                                  xend = 0.708335, yend = mean(called$pitchData.strikeZoneBottom)), 
                              linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(called$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(called$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(called$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot4.2 <- plot4.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot4.2 <- plot4.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot4.2 <- plot4.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

###Paste all on one page
library(ggpubr)

####option 2x2
####ggarrange(plot1, plot3, plot2, plot4, 
#          labels = c("RHB/LHP", "LHB/LHP", 
#                     "RHB/RHP", "LHB/RHP"),
#          label.x = 0.34, label.y = 0.98, font.label = list(color = "white"),
#          ncol = 2, nrow = 2, legend = "right", common.legend = TRUE)

####option 1x3
#RHB vs LHP
ggarrange(plot1.1, plot1, plot1.2, 
          labels = c("0-2", "0-0","3-0"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 3, nrow = 1, legend = "right", common.legend = TRUE)
#RHB vs RHP
ggarrange(plot2.1, plot2, plot2.2, 
          labels = c("0-2", "0-0","3-0"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 3, nrow = 1, legend = "right", common.legend = TRUE)

#LHB vs LHP
ggarrange(plot3.1, plot3, plot3.2, 
          labels = c("0-2", "0-0","3-0"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 3, nrow = 1, legend = "right", common.legend = TRUE)

#LHB vs RHP
ggarrange(plot4.1, plot4, plot4.2, 
          labels = c("0-2", "0-0","3-0"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 3, nrow = 1, legend = "right", common.legend = TRUE)

############################### Plot models for different Year ############################
### RHB vs LHP - model and predict

pbpFull5.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$Year == "2010")

gam5.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull5.1, family=binomial(link="logit"), cluster=cl)


df_p5.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p5.1$lp <- predict(gam5.1, df_p5.1)
df_p5.1$Probability <- exp(df_p5.1$lp) / (1 + exp(df_p5.1$lp))


pbpFull5.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$Year == "2019")

gam5.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull5.2, family=binomial(link="logit"), cluster=cl)


df_p5.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p5.2$lp <- predict(gam5.2, df_p5.2)
df_p5.2$Probability <- exp(df_p5.2$lp) / (1 + exp(df_p5.2$lp))

### RHB vs RHP - model and predict
pbpFull6.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$Year == "2010")

gam6.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull6.1, family=binomial(link="logit"), cluster=cl)


df_p6.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p6.1$lp <- predict(gam6.1, df_p6.1)
df_p6.1$Probability <- exp(df_p6.1$lp) / (1 + exp(df_p6.1$lp))


pbpFull6.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$Year == "2019")

gam6.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull6.2, family=binomial(link="logit"), cluster=cl)


df_p6.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p6.2$lp <- predict(gam6.2, df_p6.2)
df_p6.2$Probability <- exp(df_p6.2$lp) / (1 + exp(df_p6.2$lp))

### LHB vs LHP - model and predict

pbpFull7.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$Year == "2010")

gam7.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull7.1, family=binomial(link="logit"), cluster=cl)


df_p7.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p7.1$lp <- predict(gam7.1, df_p7.1)
df_p7.1$Probability <- exp(df_p7.1$lp) / (1 + exp(df_p7.1$lp))


pbpFull7.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$Year == "2019")

gam7.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull7.2, family=binomial(link="logit"), cluster=cl)


df_p7.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p7.2$lp <- predict(gam7.2, df_p7.2)
df_p7.2$Probability <- exp(df_p7.2$lp) / (1 + exp(df_p7.2$lp))

### LHB vs RHP - model and predict

pbpFull8.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$Year == "2010")

gam8.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull8.1, family=binomial(link="logit"), cluster=cl)


df_p8.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p8.1$lp <- predict(gam8.1, df_p8.1)
df_p8.1$Probability <- exp(df_p8.1$lp) / (1 + exp(df_p8.1$lp))


pbpFull8.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$Year == "2019")

gam8.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull8.2, family=binomial(link="logit"), cluster=cl)


df_p8.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p8.2$lp <- predict(gam8.2, df_p8.2)
df_p8.2$Probability <- exp(df_p8.2$lp) / (1 + exp(df_p8.2$lp))

############ Plot everything with Geom_contour_filled

#RHB vs LHP
plot5.1 <- ggplot(df_p5.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() +
  theme(text=element_text(size=11))

plot5.1 <- plot5.1 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot5.1 <- plot5.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot5.1 <- plot5.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot5.1 <- plot5.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot5.2 <- ggplot(df_p5.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() +
  theme(text=element_text(size=11))

plot5.2 <- plot5.2 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot5.2 <- plot5.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot5.2 <- plot5.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot5.2 <- plot5.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

#RHB vs RHP
plot6.1 <- ggplot(df_p6.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot6.1 <- plot6.1 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot6.1 <- plot6.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot6.1 <- plot6.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot6.1 <- plot6.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot6.2 <- ggplot(df_p6.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot6.2 <- plot6.2 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot6.2 <- plot6.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot6.2 <- plot6.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot6.2 <- plot6.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


###LHB vs LHP
plot7.1 <- ggplot(df_p7.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot7.1 <- plot7.1 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot7.1 <- plot7.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot7.1 <- plot7.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot7.1 <- plot7.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot7.2 <- ggplot(df_p7.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot7.2 <- plot7.2 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot7.2 <- plot7.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot7.2 <- plot7.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot7.2 <- plot7.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

###LHB vs RHP
plot8.1 <- ggplot(df_p8.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot8.1 <- plot8.1 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot8.1 <- plot8.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot8.1 <- plot8.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot8.1 <- plot8.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot8.2 <- ggplot(df_p8.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot8.2 <- plot8.2 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot8.2 <- plot8.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot8.2 <- plot8.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot8.2 <- plot8.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

###Paste all on one page

####option 1x2
#RHB vs LHP
ggarrange(plot5.1, plot5.2, 
          labels = c("2010", "2019"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 2, nrow = 1, legend = "right", common.legend = TRUE)
#RHB vs RHP
ggarrange(plot6.1, plot6.2, 
          labels = c("2010", "2019"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 2, nrow = 1, legend = "right", common.legend = TRUE)

#LHB vs LHP
ggarrange(plot7.1,plot7.2, 
          labels = c("2010", "2019"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 2, nrow = 1, legend = "right", common.legend = TRUE)

#LHB vs RHP
ggarrange(plot8.1, plot8.2, 
          labels = c("2010", "2019"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 2, nrow = 1, legend = "right", common.legend = TRUE)

############################### Plot models for different pitch type ############################
### RHB vs LHP - model and predict

pbpFull9.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$details.type.code == "FF")

gam9.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull9.1, family=binomial(link="logit"), cluster=cl)


df_p9.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p9.1$lp <- predict(gam9.1, df_p9.1)
df_p9.1$Probability <- exp(df_p9.1$lp) / (1 + exp(df_p9.1$lp))


pbpFull9.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$details.type.code == "CU")

gam9.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull9.2, family=binomial(link="logit"), cluster=cl)


df_p9.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p9.2$lp <- predict(gam9.2, df_p9.2)
df_p9.2$Probability <- exp(df_p9.2$lp) / (1 + exp(df_p9.2$lp))

### RHB vs RHP - model and predict
pbpFull10.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$details.type.code == "FF")

gam10.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull10.1, family=binomial(link="logit"), cluster=cl)


df_p10.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p10.1$lp <- predict(gam10.1, df_p10.1)
df_p10.1$Probability <- exp(df_p10.1$lp) / (1 + exp(df_p10.1$lp))


pbpFull10.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "R" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$details.type.code == "CU")

gam10.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull10.2, family=binomial(link="logit"), cluster=cl)


df_p10.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p10.2$lp <- predict(gam10.2, df_p10.2)
df_p10.2$Probability <- exp(df_p10.2$lp) / (1 + exp(df_p10.2$lp))

### LHB vs LHP - model and predict

pbpFull11.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$details.type.code == "FF")

gam11.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
               method="GCV.Cp", data = pbpFull11.1, family=binomial(link="logit"), cluster=cl)


df_p11.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                        pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p11.1$lp <- predict(gam11.1, df_p11.1)
df_p11.1$Probability <- exp(df_p11.1$lp) / (1 + exp(df_p11.1$lp))


pbpFull11.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$details.type.code == "CU")

gam11.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
               method="GCV.Cp", data = pbpFull11.2, family=binomial(link="logit"), cluster=cl)


df_p11.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                        pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p11.2$lp <- predict(gam11.2, df_p11.2)
df_p11.2$Probability <- exp(df_p11.2$lp) / (1 + exp(df_p11.2$lp))

### LHB vs RHP - model and predict

pbpFull12.1 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "R" & pbpFull$details.type.code == "FF")

gam12.1 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull12.1, family=binomial(link="logit"), cluster=cl)


df_p12.1 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p12.1$lp <- predict(gam12.1, df_p12.1)
df_p12.1$Probability <- exp(df_p12.1$lp) / (1 + exp(df_p12.1$lp))


pbpFull12.2 <- subset(pbpFull, pbpFull$matchup.batSide.code == "L" & pbpFull$matchup.pitchHand.code == "L" & pbpFull$details.type.code == "CU")

gam12.2 <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50), 
              method="GCV.Cp", data = pbpFull12.2, family=binomial(link="logit"), cluster=cl)


df_p12.2 <- expand.grid(pitchData.coordinates.pX = seq(-1.5, 1.5, length=100),
                       pitchData.coordinates.pZ = seq(1, 4, length=100))
df_p12.2$lp <- predict(gam12.2, df_p12.2)
df_p12.2$Probability <- exp(df_p12.2$lp) / (1 + exp(df_p12.2$lp))

############ Plot everything with Geom_contour_filled

#RHB vs LHP
plot9.1 <- ggplot(df_p9.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() +
  theme(text=element_text(size=11))

plot9.1 <- plot9.1 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot9.1 <- plot9.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot9.1 <- plot9.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot9.1 <- plot9.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot9.2 <- ggplot(df_p9.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() +
  theme(text=element_text(size=11))

plot9.2 <- plot9.2 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot9.2 <- plot9.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot9.2 <- plot9.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot9.2 <- plot9.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

#RHB vs RHP
plot10.1 <- ggplot(df_p10.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot10.1 <- plot10.1 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot10.1 <- plot10.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot10.1 <- plot10.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot10.1 <- plot10.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot10.2 <- ggplot(df_p10.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot10.2 <- plot10.2 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot10.2 <- plot10.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot10.2 <- plot10.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot10.2 <- plot10.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


###LHB vs LHP
plot11.1 <- ggplot(df_p11.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot11.1 <- plot11.1 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot11.1 <- plot11.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot11.1 <- plot11.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot11.1 <- plot11.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot11.2 <- ggplot(df_p11.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot11.2 <- plot11.2 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot11.2 <- plot11.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot11.2 <- plot11.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot11.2 <- plot11.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

###LHB vs RHP
plot12.1 <- ggplot(df_p12.1, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot12.1 <- plot12.1 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot12.1 <- plot12.1 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot12.1 <- plot12.1 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot12.1 <- plot12.1 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))


plot12.2 <- ggplot(df_p12.2, aes(pitchData.coordinates.pX, pitchData.coordinates.pZ, z= Probability)) + 
  geom_contour_filled(binwidth = .05) + scale_fill_manual(values = c(blrd_interpolated)) + theme_classic() 

plot12.2 <- plot12.2 + geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                                      xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneBottom)), 
                                  linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = -0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = -0.708335, y = mean(pbpSZsize$pitchData.strikeZoneTop), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed") +
  geom_segment(aes(x = 0.708335, y = mean(pbpSZsize$pitchData.strikeZoneBottom), 
                   xend = 0.708335, yend = mean(pbpSZsize$pitchData.strikeZoneTop)), 
               linetype = "dashed")

plot12.2 <- plot12.2 + theme(axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

plot12.2 <- plot12.2 + scale_x_continuous(limits = c(-2,2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0, 0))

plot12.2 <- plot12.2 + guides(fill = guide_legend(title = "Called strike probability", reverse=TRUE)) + coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(1, 4))

###Paste all on one page

####option 1x2
#RHB vs LHP
ggarrange(plot9.1, plot9.2, 
          labels = c("FF", "CU"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 2, nrow = 1, legend = "right", common.legend = TRUE)
#RHB vs RHP
ggarrange(plot10.1, plot10.2, 
          labels = c("FF", "CU"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 2, nrow = 1, legend = "right", common.legend = TRUE)

#LHB vs LHP
ggarrange(plot11.1,plot11.2, 
          labels = c("FF", "CU"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 2, nrow = 1, legend = "right", common.legend = TRUE)

#LHB vs RHP
ggarrange(plot12.1, plot12.2, 
          labels = c("FF", "CU"),
          label.x = 0.05, label.y = 0.98, font.label = list(color = "white"),
          ncol = 2, nrow = 1, legend = "right", common.legend = TRUE)



