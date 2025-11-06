library(tidyverse)

setwd("insertlocaldirectoryhere") # Change working directory based on local folders

study2_data <- read_csv("study 2 final data.csv")


#########################################
# Prepare data for changepoint analyses
########################################

changepoint <- study2_data %>%
  filter(daysfromevent_new >= -42 & daysfromevent_new <= 42) %>%
  group_by(daysfromevent_new) %>%
  mutate(eclipse_mean = mean(eclipse_s)) %>%
  ungroup() %>%
  dplyr::select(id, daysfromevent_new, eclipse_mean, awewords)

# Take mean eclipse value for each day
eclipse_values <- changepoint %>%
  distinct(daysfromevent_new, eclipse_mean) %>%
  arrange(daysfromevent_new)

# Run changepoint
library(changepoint)
mvalues <- cpt.meanvar(eclipse_values$eclipse_mean, method="PELT")
cpts(mvalues)

# Plot Figure 1
plot(mvalues, xlab="6 Weeks Before and After Eclipse", ylab="% of Eclipse-Related Words per Day", 
     main="", axes = FALSE)
abline(v=43, col="blue")
axis(2, at = seq(0, .30, by = .05))
axis(side=1, at=c(1, 8, 15, 22, 29, 36, 43, 50, 57, 64, 71, 78, 85), 
     labels=c("-6","-5","-4","-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6"), cex.axis= .90)

#######################################################################################################################
# Chunk users' awe words into panels of time around the eclipse (daysfromevent_new = 0) based on changepoint results.
#
# (Note: The small changepoints that fall within the before and post eclipse periods do not reflect substative shifts
# in eclipse words (reflected in the changepoint figure), and chunking time into additional time frames
# based on these minor fluctiations does not impact the interpretation of the results.)
######################################################################################################################

before_eclipse <- changepoint %>%
  filter(daysfromevent_new <= -8) %>%
  group_by(id) %>%
  mutate(time1 = mean(awewords)) %>%
  ungroup() %>%
  dplyr::select(id, time1) %>%
  na.omit() %>%
  distinct(id, .keep_all = T)

leadup_eclipse <- changepoint %>%
  filter(daysfromevent_new >= -7 & daysfromevent_new <= -1) %>%
  group_by(id) %>%
  mutate(time2 = mean(awewords)) %>%
  ungroup() %>%
  dplyr::select(id, time2) %>%
  na.omit() %>%
  distinct(id, .keep_all = T)

eclipse_period <- changepoint %>%
  filter(daysfromevent_new >= 0 & daysfromevent_new <= 1) %>%
  group_by(id) %>%
  mutate(time3 = mean(awewords)) %>%
  ungroup() %>%
  dplyr::select(id, time3) %>%
  na.omit() %>%
  distinct(id, .keep_all = T)

post_eclipse <- changepoint %>%
  filter(daysfromevent_new >= 2) %>%
  group_by(id) %>%
  mutate(time4 = mean(awewords)) %>%
  ungroup() %>%
  dplyr::select(id, time4) %>%
  na.omit() %>%
  distinct(id, .keep_all = T)


# Merge panels
user_panel1 <- merge(before_eclipse, leadup_eclipse, by = "id")
user_panel2 <- merge(user_panel1, eclipse_period, by = "id")
user_panel3 <- merge(user_panel2, post_eclipse, by = "id")


# Scale/standardize the data prior to running clustering
user_panel.scaled <- user_panel3 %>%
  mutate(time1z = scale(time1)) %>%
  mutate(time2z = scale(time2)) %>%
  mutate(time3z = scale(time3)) %>%
  mutate(time4z = scale(time4)) %>%
  dplyr::select(id, time1z, time2z, time3z, time4z)

#######################################################
# Clustering
#######################################################

# Determine optimum # of clusters using ALL indices
library(NbClust)

res<-NbClust(user_panel.scaled[-c(1)], diss=NULL, distance = "euclidean", max.nc=20, 
             method = "kmeans", index = "all")

res$Best.nc

# Run kmeans clustering
set.seed(16)
km.res <- kmeans(user_panel.scaled[-c(1)], 3, nstart = 10)


# Fold clusters into merged panels
user_panel4 <- user_panel3 
user_panel4$cluster <- as.factor(km.res$cluster)

# RUN GRAPH BELOW FIRST, then assign names to their respective clusters, then re-run graph
levels(user_panel4$cluster)[levels(user_panel4$cluster) == 1] <- "Elevated Awe"
levels(user_panel4$cluster)[levels(user_panel4$cluster) == 2] <- "Reduced Awe" 
levels(user_panel4$cluster)[levels(user_panel4$cluster) == 3] <- "No Awe" 

set.seed(16)
user_clusters <- user_panel4 %>%
  gather(time, awewords, -id, -cluster) %>%
  arrange(time, cluster) %>%
  group_by(time, cluster) %>%
  dplyr::summarize(n=dplyr::n(),
                   cluster_mean=mean(awewords),
                   se=FSA::se(awewords)) %>%
  dplyr::mutate(lci=cluster_mean-qt(0.975,df=n-1)*se,
                uci=cluster_mean+qt(0.975,df=n-1)*se) %>% 
  distinct(cluster, .keep_all = T) %>%
  ungroup()

plot <- ggplot(user_clusters, aes(x= time, y= cluster_mean, group = cluster, color=cluster))

plot + 
  geom_line(aes(group=factor(cluster)),size=1.5, show.legend = T) +
  geom_pointrange(aes(ymin=lci,ymax=uci), size = .8, fatten = .5) +
  geom_point(size=2) + 
  theme(legend.position = "none") +
  labs(title = "Awe Over Time by Cluster", 
       x = "\nTime", 
       y = "% of Tweets with Awe Words\n", 
       color = "Cluster") + 
  #caption = "N = 7,736") +
  scale_x_discrete(labels=c("Before\nEclipse", "Eclipse\nLead-up", 
                            "Eclipse\nPeriod", "Post\nEclipse")) +
  viridis::scale_color_viridis(discrete = TRUE, option = "D") +
  theme_light(base_size=14) +
  theme(plot.title = element_text(hjust = 0.5))

# n per group
user_panel4 %>% 
  group_by(cluster) %>% 
  summarise(count = n_distinct(id))

# Fold clusters back into main data
clustermerge_awe <- merge(study2_data, user_panel4, by = "id", all=FALSE)


################################################################
# Analyses
################################################################

panel <- clustermerge_awe %>% 
  mutate(timegroup = case_when(daysfromevent_new <= -8 ~ "time 1",
                               daysfromevent_new >= -7 & daysfromevent_new <= -1 ~ "time 2",
                               daysfromevent_new >= 0 & daysfromevent_new <= 1 ~ "time 3", 
                               daysfromevent_new >= 2 ~ "time 4")) 


library(rcompanion)
library(lsmeans)
library(forcats)
library(glmmTMB)

lme_panel <- panel %>%
  arrange(timegroup) %>% 
  mutate(timegroup = as_factor(timegroup)) %>% 
  mutate(tg_value = case_when(timegroup == "time 1" ~ 1,
                              timegroup == "time 2" ~ 2,
                              timegroup == "time 3" ~ 3,
                              timegroup == "time 4" ~ 4,)) %>% 
  mutate(timegroup = fct_reorder(timegroup, tg_value)) # Reorder timegroup based on its numerical ordering




# Set number of cores available if using parallel processing
cl <- min(parallel::detectCores(),11)

# Awe
awe_model <- glmmTMB(awewords ~ timegroup * cluster + (1|id),
                     family = binomial,
                     data = lme_panel,
                     control = glmmTMBControl(parallel = cl))
summary(awe_model)

# Eclipse 
eclipse_model <- glmmTMB(eclipse_s ~ timegroup * cluster + (1|id),
                         family = binomial,
                         data = lme_panel,
                         control = glmmTMBControl(parallel = cl))

summary(eclipse_model)

# I words
i_model_new <- glmmTMB(i_new ~ timegroup * cluster + (1|id),
                       family = binomial,
                       data = lme_panel,
                       control = glmmTMBControl(parallel = cl))

summary(i_model_new)

# Prosocial
proso_model <- glmmTMB(proso_grati ~ timegroup * cluster + (1|id),
                       family = binomial,
                       data = lme_panel,
                       control = glmmTMBControl(parallel = cl))

summary(proso_model)

# Tentative
tenta_model <- glmmTMB(tenta_full ~ timegroup * cluster + (1|id),
                       family = binomial,
                       data = lme_panel,
                       control = glmmTMBControl(parallel = cl))

summary(tenta_model)

# We words
we_new <- glmmTMB::glmmTMB(we_new ~ timegroup * cluster + (1|id),
                           family = binomial,
                           data = lme_panel,
                           control = glmmTMBControl(parallel = cl))
summary(we_new)

# Affiliation
affili_full <- glmmTMB(affiliation_full ~ timegroup * cluster + (1|id),
                       family = binomial,
                       data = lme_panel,
                       control = glmmTMBControl(parallel = cl))


# Compute marginal means -- type = "response" command gives probability estimates
# Probability estimates are number of successes/total number of trials -- so number of awe tweets/total tweets, a proprotion of awe tweets
awe_marginal <- emmeans(awe_model, ~ timegroup * cluster, type = "response") 
awe_marginal #contrast commands will give odds ratios

eclipse_marginal <- emmeans(eclipse_model, ~ timegroup * cluster, type = "response") 
i_marginal <- emmeans(i_model_new, ~ timegroup * cluster, type = "response")
proso_marginal <- emmeans(proso_model, ~ timegroup * cluster, type = "response") 
tenta_marginal <- emmeans(tenta_model, ~ timegroup * cluster, type = "response") 
we_marginal <- emmeans(we_new, ~ timegroup * cluster, type = "response") 
affili_marginal <- emmeans(affili_full, ~ timegroup * cluster, type = "response") 


# Coding for specific contrasts
awetime3_vs_awetime2 <- list("awe3 vs. awe2" = c(0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0))
awetime3_vs_reducedtime3 <- list("awe3 vs. reduced3" = c(0, 0, 1, 0, 0, 0, -1, 0, 0 ,0 ,0, 0))
awetime3_vs_noawetime3 <- list("awe3 vs. noawe3" = c(0, 0, 1, 0, 0, 0, 0, 0, 0 ,0 ,-1, 0))

# Reduced and no awe within-cluster contrasts
reducedtime3_vs_reducedtime2 <- list("reduced3 vs. reduced2" = c(0, 0, 0, 0, 0, -1, 1, 0, 0 ,0 ,0, 0))
noawetime3_vs_noawetime2 <- list("noawe3 vs. noawe2" = c(0, 0, 0, 0, 0, 0, 0, 0, 0 , -1, 1, 0))


# Contrasts
confint(contrast(awe_marginal, awetime3_vs_awetime2))
confint(contrast(awe_marginal, awetime3_vs_reducedtime3))
confint(contrast(awe_marginal, awetime3_vs_noawetime3))

confint(contrast(eclipse_marginal, awetime3_vs_awetime2))
confint(contrast(eclipse_marginal, awetime3_vs_reducedtime3))
confint(contrast(eclipse_marginal, awetime3_vs_noawetime3))

confint(contrast(i_marginal, awetime3_vs_awetime2))
confint(contrast(i_marginal, awetime3_vs_reducedtime3))
confint(contrast(i_marginal, awetime3_vs_noawetime3))

confint(contrast(proso_marginal, awetime3_vs_awetime2))
confint(contrast(proso_marginal, awetime3_vs_reducedtime3))
confint(contrast(proso_marginal, awetime3_vs_noawetime3))

confint(contrast(tenta_marginal, awetime3_vs_awetime2))
confint(contrast(tenta_marginal, awetime3_vs_reducedtime3))
confint(contrast(tenta_marginal, awetime3_vs_noawetime3))

confint(contrast(we_marginal, awetime3_vs_awetime2))
confint(contrast(we_marginal, awetime3_vs_reducedtime3))
confint(contrast(we_marginal, awetime3_vs_noawetime3))

confint(contrast(affili_marginal, awetime3_vs_awetime2))
confint(contrast(affili_marginal, awetime3_vs_reducedtime3))
confint(contrast(affili_marginal, awetime3_vs_noawetime3))

# Reduced and no awe within-cluster contrasts
confint(contrast(i_marginal, reducedtime3_vs_reducedtime2))
confint(contrast(proso_marginal, reducedtime3_vs_reducedtime2))
confint(contrast(tenta_marginal, reducedtime3_vs_reducedtime2))
confint(contrast(we_marginal, reducedtime3_vs_reducedtime2))
confint(contrast(affili_marginal, reducedtime3_vs_reducedtime2))

confint(contrast(i_marginal, noawetime3_vs_noawetime2))
confint(contrast(proso_marginal, noawetime3_vs_noawetime2))
confint(contrast(tenta_marginal, noawetime3_vs_noawetime2))
confint(contrast(we_marginal, noawetime3_vs_noawetime2))
confint(contrast(affili_marginal, noawetime3_vs_noawetime2))

###########################################################
# Graphing results
###########################################################

# Before graphing, put things back into log odds, so that we can exponentiate them in ggplot into odds

awe_marginal <- emmeans::emmeans(awe_model, ~ cluster*timegroup)
eclipse_marginal <- emmeans::emmeans(eclipse_model, ~ cluster*timegroup)
i_marginal <- emmeans::emmeans(i_model_new, ~ cluster*timegroup)
proso_marginal <- emmeans::emmeans(proso_model, ~ cluster*timegroup)
tenta_marginal <- emmeans::emmeans(tenta_model, ~ cluster*timegroup)
we_marginal <- emmeans::emmeans(we_new, ~ cluster*timegroup)
affili_marginal <- emmeans::emmeans(affili_full, ~ cluster*timegroup)

library(viridis)

# Plot odds at each timepoint by cluster

# I words
i_plot <- emmip(i_marginal, cluster ~ timegroup, plotit = FALSE, CIs = TRUE) %>% 
  ggplot() +
  # Exponentiate the y variable to convert log odds to ODDS
  aes(x = timegroup, y = exp(yvar), color = cluster, group = cluster) +
  geom_line(size = 1.2) +
  geom_point(size = 1.7) +
  geom_linerange(aes(ymin = exp(LCL), ymax = exp(UCL)), size = 1, alpha = .4) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = "Odds", breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(name = "Time", 
                   labels=c("Before\nEclipse", "Eclipse\nLead-up", 
                            "Eclipse\nPeriod", "Post\nEclipse")) +
  labs(color = "Cluster") +
  ggtitle("Self Focus") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.title=element_blank(),
        legend.text = element_text(size = 12))


# Tentative words
tenta_plot <- emmip(tenta_marginal, cluster ~ timegroup, plotit = FALSE, CIs = TRUE) %>% 
  ggplot() +
  aes(x = timegroup, y = exp(yvar), color = cluster, group = cluster) +
  geom_line(size = 1.2) +
  geom_point(size = 1.7) +
  geom_linerange(aes(ymin = exp(LCL), ymax = exp(UCL)), size = 1, alpha = .4) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = "Odds", breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(name = "Time", 
                   labels=c("Before\nEclipse", "Eclipse\nLead-up", 
                            "Eclipse\nPeriod", "Post\nEclipse")) +
  labs(color = "Cluster") +
  ggtitle("Tentativeness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.title=element_blank(),
        legend.text = element_text(size = 12))


# Prosocial words
proso_plot <- emmip(proso_marginal, cluster ~ timegroup, plotit = FALSE, CIs = TRUE) %>% 
  ggplot() +
  aes(x = timegroup, y = exp(yvar), color = cluster, group = cluster) +
  geom_line(size = 1.2) +
  geom_point(size = 1.7) +
  geom_linerange(aes(ymin = exp(LCL), ymax = exp(UCL)), size = 1, alpha = .4) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = "Odds", breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(name = "Time", 
                   labels=c("Before\nEclipse", "Eclipse\nLead-up", 
                            "Eclipse\nPeriod", "Post\nEclipse")) +
  labs(color = "Cluster") +
  ggtitle("Prosociality") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.title=element_blank(),
        legend.text = element_text(size = 12))

# We words
we_plot <- emmip(we_marginal, cluster ~ timegroup, plotit = FALSE, CIs = TRUE) %>% 
  ggplot() +
  aes(x = timegroup, y = exp(yvar), color = cluster, group = cluster) +
  geom_line(size = 1.2) +
  geom_point(size = 1.7) +
  geom_linerange(aes(ymin = exp(LCL), ymax = exp(UCL)), size = 1, alpha = .4) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = "Odds", breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(name = "Time", 
                   labels=c("Before\nEclipse", "Eclipse\nLead-up", 
                            "Eclipse\nPeriod", "Post\nEclipse")) +
  labs(color = "Cluster") +
  ggtitle("Collective Focus") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.title=element_blank(),
        legend.text = element_text(size = 12))

# Affiliation words
affili_plot <- emmip(affili_marginal, cluster ~ timegroup, plotit = FALSE, CIs = TRUE) %>% 
  ggplot() +
  aes(x = timegroup, y = exp(yvar), color = cluster, group = cluster) +
  geom_line(size = 1.2) +
  geom_point(size = 1.7) +
  geom_linerange(aes(ymin = exp(LCL), ymax = exp(UCL)), size = 1, alpha = .4) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = "Odds", breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(name = "Time", 
                   labels=c("Before\nEclipse", "Eclipse\nLead-up", 
                            "Eclipse\nPeriod", "Post\nEclipse")) +
  labs(color = "Cluster") +
  ggtitle("Affiliation") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.title=element_blank(),
        legend.text = element_text(size = 12))

library(ggpubr)
library(gridExtra)


ggarrange(affili_plot, i_plot, tenta_plot, proso_plot, we_plot, ncol=2, nrow=3, common.legend = TRUE, legend = "bottom")


#####################################################################
### Supplemental 
#####################################################################

# Present Focus
present_log_model <- glmmTMB(presentfocus ~ timegroup * cluster + (1|id),
                             family = binomial,
                             data = lme_panel,
                             control = glmmTMBControl(parallel = cl))
summary(present_log_model)

confint(contrast(present_marginal, awetime3_vs_awetime2))
confint(contrast(present_marginal, awetime3_vs_reducedtime3))
confint(contrast(present_marginal, awetime3_vs_noawetime3))


# Certainty
cert_log_model <- glmmTMB(certain_full ~ timegroup * cluster + (1|id),
                          family = binomial,
                          data = lme_panel,
                          control = glmmTMBControl(parallel = cl))
summary(cert_log_model)


present_marginal <- emmeans(present_log_model, ~ timegroup * cluster, type = "response") 
cert_marginal <- emmeans(cert_log_model, ~ timegroup * cluster, type = "response")


confint(contrast(cert_marginal, awetime3_vs_awetime2))
confint(contrast(cert_marginal, awetime3_vs_reducedtime3))
confint(contrast(cert_marginal, awetime3_vs_noawetime3))

confint(contrast(present_marginal, awetime3_vs_awetime2))
confint(contrast(present_marginal, awetime3_vs_reducedtime3))
confint(contrast(present_marginal, awetime3_vs_noawetime3))


# Graphing
marginal <- emmeans::emmeans(cert_log_model, ~ cluster*timegroup)

p <- emmip(marginal, cluster ~ timegroup, plotit = FALSE, CIs = TRUE)

ggplot(p) +
  aes(x = timegroup, y = exp(yvar), color = cluster, group = cluster) +
  geom_line(size = 1.2) +
  geom_point(size = 1.7) +
  geom_linerange(aes(ymin = exp(LCL), ymax = exp(UCL)), size = 1, alpha = .4) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = "Odds", breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(name = "Time", 
                   labels=c("Before\nEclipse", "Eclipse\nLead-up",
                            "Day of\nEclipse", "After\nEclipse")) +
  labs(color = "Cluster") +
  ggtitle("Certainty by Cluster") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))



###
marginal <- emmeans::emmeans(present_log_model, ~ cluster*timegroup)

p <- emmip(marginal, cluster ~ timegroup, plotit = FALSE, CIs = TRUE)

ggplot(p) +
  aes(x = timegroup, y = exp(yvar), color = cluster, group = cluster) +
  geom_line(size = 1.2) +
  geom_point(size = 1.7) +
  geom_linerange(aes(ymin = exp(LCL), ymax = exp(UCL)), size = 1, alpha = .4) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_y_continuous(name = "Odds", breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(name = "Time", 
                   labels=c("Before\nEclipse", "Eclipse\nLead-up",
                            "Day of\nEclipse", "After\nEclipse")) +
  labs(color = "Cluster") +
  ggtitle("Present Focus by Cluster") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))


# Non-overlapping affiliation and prosocial 
affili_short <- glmmTMB(affiliation_s ~ timegroup * cluster + (1|id),
                        family = binomial,
                        data = lme_panel,
                        control = glmmTMBControl(parallel = cl))

proso_no_model <- glmmTMB(proso_no_ovlp ~ timegroup * cluster + (1|id),
                          family = binomial,
                          data = lme_panel,
                          control = glmmTMBControl(parallel = cl))

affili_proso <- glmmTMB(affili_proso_ovlp ~ timegroup * cluster + (1|id),
                        family = binomial,
                        data = lme_panel,
                        control = glmmTMBControl(parallel = cl))

affili_marginal_short <- emmeans(affili_short, ~ timegroup * cluster, type = "response") 
proso_no_marginal <- emmeans(proso_no_model, ~ timegroup * cluster, type = "response") 
affili_proso_marginal <- emmeans(affili_proso, ~ timegroup * cluster, type = "response") 


confint(contrast(affili_marginal_short, awetime3_vs_awetime2))
confint(contrast(affili_marginal_short, awetime3_vs_reducedtime3))
confint(contrast(affili_marginal_short, awetime3_vs_noawetime3))

confint(contrast(affili_marginal_short, reducedtime3_vs_reducedtime2))
confint(contrast(affili_marginal_short, noawetime3_vs_noawetime2))


confint(contrast(proso_no_marginal, awetime3_vs_awetime2))
confint(contrast(proso_no_marginal, awetime3_vs_reducedtime3))
confint(contrast(proso_no_marginal, awetime3_vs_noawetime3))

confint(contrast(proso_no_marginal, reducedtime3_vs_reducedtime2))
confint(contrast(proso_no_marginal, noawetime3_vs_noawetime2))


confint(contrast(affili_proso_marginal, awetime3_vs_awetime2))
confint(contrast(affili_proso_marginal, awetime3_vs_reducedtime3))
confint(contrast(affili_proso_marginal, awetime3_vs_noawetime3))

confint(contrast(affili_proso_marginal, reducedtime3_vs_reducedtime2))
confint(contrast(affili_proso_marginal, noawetime3_vs_noawetime2))
