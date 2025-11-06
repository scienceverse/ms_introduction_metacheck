# Script 3: Networks ----------------------------------------------------------------------------------
# This script implements a network approach to model and analyse the data.

# The script performs these tasks:
# 1. Loading libraries, setting parameters, and loading data
# 2. Constructing and analysing a network model of adversity variables
# 3. Constructing and analysing a network model of outcome variables
# 4. Constructing and analysing an overall network model
# 5. Implementing model-based recursive partitioning
# 6. Comparing the resultant subnetworks of the network tree
# 7. Making additional figures for publication

input = ""
output = ""

# 1. Loading libraries, setting parameters, and loading data --------------------------------------------
# 1a. Load libraries ----

library(mgm)
library(qgraph)
library(igraph)
library(bootnet) 
library(networktools)
library(NetworkComparisonTest)
library(networktree)
library(smacof)
library(permute)
library(ggplot2) 
library(ggpubr)
library(viridisLite) 


# 1b. Set parameters ----
bootstraps <- 2000
permutations <- 1000

# 1c. Load data and make data frames ----

proj_data_imp <- read.csv(file=paste(input,"/proj_data_imp.csv",sep=""))
proj_data_imp <- subset(proj_data_imp, select = -X)

# Standardise continuous variables to the sample mean
proj_data_imp[,c("fin_diff.1","fin_diff.2","fin_diff.3","fin_diff.4","fin_diff.5","interact_mum.1","interact_mum.2",
                 "interact_mum.3","interact_mum.4","interact_mum.5","interact_dad.1","interact_dad.2","interact_dad.3",
                 "interact_dad.4","interact_dad.5","extern","intern","wasi","stopsig",
                 "fin_diff_cu","interact_mum_cu","interact_dad_cu")] <-
  scale(proj_data_imp[,c("fin_diff.1","fin_diff.2","fin_diff.3","fin_diff.4","fin_diff.5","interact_mum.1","interact_mum.2",
                         "interact_mum.3","interact_mum.4","interact_mum.5","interact_dad.1","interact_dad.2","interact_dad.3",
                         "interact_dad.4","interact_dad.5","extern","intern","wasi","stopsig",
                         "fin_diff_cu","interact_mum_cu","interact_dad_cu")])

# Reshape data into long format by age for recursive partitioning
proj_data_imp_long <- proj_data_imp
proj_data_imp_long <- reshape(proj_data_imp_long,
                              varying =list(c("dom_viol_phys.1","dom_viol_phys.2","dom_viol_phys.3","dom_viol_phys.4","dom_viol_phys.5"),
                                            c("dom_viol_emo.1","dom_viol_emo.2","dom_viol_emo.3","dom_viol_emo.4","dom_viol_emo.5"),
                                            c("phys_cruel.1","phys_cruel.2","phys_cruel.3","phys_cruel.4","phys_cruel.5"),
                                            c("emo_cruel.1","emo_cruel.2","emo_cruel.3","emo_cruel.4","emo_cruel.5"),
                                            c("phys_abuse.1","phys_abuse.2","phys_abuse.3","phys_abuse.4","phys_abuse.5"),c("sex_abuse.1","sex_abuse.2","sex_abuse.3","sex_abuse.4","sex_abuse.5"),
                                            c("care_change.1","care_change.2","care_change.3","care_change.4","care_change.5"),c("separate.1","separate.2","separate.3","separate.4","separate.5"),
                                            c("fin_diff.1","fin_diff.2","fin_diff.3","fin_diff.4","fin_diff.5"),c("interact_mum.1","interact_mum.2","interact_mum.3","interact_mum.4","interact_mum.5"),
                                            c("interact_dad.1","interact_dad.2","interact_dad.3","interact_dad.4","interact_dad.5")),
                              direction = "long", idvar = "pid",
                              v.names = c("dom_viol_emo","dom_viol_phys","dom_viol","phys_cruel",
                                          "emo_cruel","phys_abuse","sex_abuse","care_change",
                                          "separate","fin_diff","interact_mum","interact_dad"),
                              times = c("1","2","3","4","5"))
proj_data_imp_long$time <- as.numeric(proj_data_imp_long$time)

# Generate lists for overall mgm
types <- c("c","c","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g") 
levels <- c(2050,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) 
colnames <- c("pid","sex","extern","intern","stopsig","wasi",
              "dom_viol_phys_cu","dom_viol_emo_cu","phys_cruel_cu",
              "emo_cruel_cu","phys_abuse_cu","sex_abuse_cu",
              "care_change_cu","separate_cu",
              "fin_diff_cu","interact_mum_cu","interact_dad_cu") 
descriptions <- c("participant","sex","externalising (SDQ)","internalising (SDQ)","inhibition (SST)","IQ (WASI)",
                  "physical domestic violence","emotional domestic violence","physical cruelty",
                  "emotional cruelty","physical abuse","sexual abuse",
                  "caregiver change","parental separation",
                  "financial difficulties","maternal neglect","paternal neglect") 
mgm_data <- list(data = as.matrix(proj_data_imp[,c("pid","sex","extern","intern","stopsig","wasi",
                                                   "dom_viol_phys_cu","dom_viol_emo_cu","phys_cruel_cu",
                                                   "emo_cruel_cu","phys_abuse_cu","sex_abuse_cu",
                                                   "care_change_cu","separate_cu",
                                                   "fin_diff_cu","interact_mum_cu","interact_dad_cu")]), 
                 types = types, levels=levels, colnames=colnames, descriptions=descriptions)


# Generate list for partitioning mgm
types_long <- c("c","c","g","g","g","g","c","c","c","c","c","c","c","c","c","g","g","g")
levels_long <- c(2050,2,1,1,1,1,5,2,2,2,2,2,2,2,2,1,1,1) # continuous = 1, categorical > 1
colnames_long <- c("pid","sex","extern","intern","stopsig","wasi","time",
                   "dom_viol_phys","dom_viol_emo","phys_cruel",
                   "emo_cruel","phys_abuse","sex_abuse",
                   "care_change","separate","fin_diff","interact_mum","interact_dad") 
descriptions_long <- c("participant","sex","externalising (SDQ)","internalising (SDQ)","inhibition (SST)","IQ (WASI)","timepoint",
                       "physical domestic violence","emotional domestic violence","physical cruelty",
                       "emotional cruelty","physical abuse","sexual abuse",
                       "caregiver change","parental separation","financial difficulties","maternal neglect","paternal neglect") 
mgm_data_long <- list(data = as.matrix(proj_data_imp_long[,c("pid","sex","extern","intern","stopsig","wasi","time",
                                                             "dom_viol_phys","dom_viol_emo","phys_cruel",
                                                             "emo_cruel","phys_abuse","sex_abuse",
                                                             "care_change","separate","fin_diff","interact_mum","interact_dad")]), 
                      types = types_long, levels=levels_long, colnames=colnames_long, descriptions=descriptions_long)


# 2. Network model A: adversity measures alone --------------------------------------------
# 2a. Define node variables ----
nodevars_a <- c("dom_viol_phys_cu","dom_viol_emo_cu","phys_cruel_cu",
                "emo_cruel_cu","phys_abuse_cu","care_change_cu","separate_cu",
                "fin_diff_cu","interact_mum_cu","interact_dad_cu")
index_a <- match(nodevars_a,colnames)

# 2b. Fit network ----
fit_a <- mgm(data = mgm_data$data[,nodevars_a],
             type = mgm_data$type[index_a],
             level = mgm_data$level[index_a],
             ruleReg = "AND",
             k = 2, 
             lambdaSel = 'EBIC',
             lambdaGam = 0.25, 
             pbar = FALSE)

# 2c. Plot network ----
network_a <- qgraph(fit_a$pairwise$wadj,
                    layout = "spring",
                    edge.color = fit_a$pairwise$edgecolor,
                    nodeNames = mgm_data$descriptions[index_a])

# 2d. Perform community detection ----
# Detect communities using walktrap from igraph
igraph_a <- as.igraph(network_a)
communities_a <- cluster_walktrap(igraph_a,
                                  weights = E(igraph_a)$weight,
                                  steps = 7, 
                                  merges = TRUE,
                                  modularity = TRUE,
                                  membership = TRUE)
modularity_a <- modularity(igraph_a,communities_a$membership,weights=E(igraph_a)$weight)

# Define groups and re-do original plot
groups_a <- vector(mode = "list", length = length(unique(communities_a$membership)))
for (i in 1:length(unique(communities_a$membership)))
{
  groups_a[[i]] = which(communities_a$membership %in% i);
}

network_a <- qgraph(fit_a$pairwise$wadj,
                    layout = "spring",
                    edge.color = fit_a$pairwise$edgecolor,
                    nodeNames = mgm_data$descriptions[index_a],
                    groups = groups_a,
                    legend = TRUE)

tiff(file = paste(output,"/network_a.tiff",sep = ""),units="in",width=11,height=7,res=300)
plot(network_a)
dev.off()

# 2e. Calculate strength ----
centrality_a <- centrality_auto(network_a)
bridge_a <- bridge(network_a, communities = communities_a)
centralities_a <- data.frame("strength" = scale(centrality_a$node.centrality$Strength), 
                             "bridge.strength" = scale(bridge_a$`Bridge Strength`),
                             "nodes" = c(1:length(nodevars_a)),
                             "node.names" = mgm_data$colnames[index_a])

central_plot_a <- ggplot(centralities_a, aes(nodes)) + 
  geom_line(aes(y = bridge.strength, colour = "bridge.strength")) + geom_point(aes(y = bridge.strength, colour = "bridge.strength")) +
  geom_line(aes(y = strength, colour = "strength")) + geom_point(aes(y = strength, colour = "strength")) +
  labs( x = "nodes", y = "z-score",title = "Centrality Indices: Adversity Network") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1),plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  scale_x_continuous(breaks = centralities_a$nodes, labels = mgm_data$descriptions[index_a]) +
  scale_colour_manual("", breaks = c('bridge.strength',"strength"),values = c('red',"orange"))

plot(central_plot_a)

tiff(file = paste(output,"/centrality_a.tiff",sep = ""),units="in",width=6,height=5,res=300)
plot(central_plot_a)
dev.off()

# 2f. Assess stability of parameters ----
boots_a <- bootnet(data = mgm_data$data[,nodevars_a],
                   nBoots = bootstraps, 
                   default = "mgm",
                   type = "case",
                   statistics = c("bridgeStrength","strength"),
                   communities = communities_a$membership,
                   verbose = TRUE,
                   caseMin=0.025,
                   caseMax=0.975,
                   caseN=100)

boots_a_plot <- plot(boots_a,statistics =c("bridgeStrength","strength"))
tiff(file = paste(output,"/stability_a.tiff",sep = ""),units="in",width=6,height=4,res=300)
plot(boots_a_plot)+ ggtitle("Stability of Centrality Indices: Adversities Network")
dev.off()

stability_a <- corStability(boots_a)

# 2g. Assess robustness of clusters ----
perms_a <- shuffleSet(n = length(nodevars_a),nset = permutations,
                      control = how(within = Within(type ="free")))

null_effects_a <-apply(X = perms_a,MARGIN = 1,FUN = function(x){
  communities_temp <- communities_a$membership[t(x)]
  modularity_temp <- modularity(igraph_a,communities_temp,weights=E(igraph_a)$weight)
  
  return(modularity_temp)
}
)

null_effects_a <- as.data.frame(null_effects_a)
colMeans(null_effects_a)
perm_pval_a = (sum(null_effects_a > modularity_a)*1)/permutations

# 3: Network model B: outcomes alone --------------------------------------------
# 3a. Define node variables ----
nodevars_b <- c("intern","extern","wasi","stopsig")
index_b <- match(nodevars_b,colnames)

# 3b. Fit network ----
fit_b <- mgm(data = mgm_data$data[,nodevars_b],
             type = mgm_data$type[index_b],
             level = mgm_data$level[index_b],
             k = 2,
             lambdaSel = 'EBIC',  
             lambdaGam = 0.25,
             pbar = FALSE)

# 3c. Plot network ----
network_b <- qgraph(fit_b$pairwise$wadj,
                    layout = "spring",
                    edge.color = fit_b$pairwise$edgecolor,
                    nodeNames = mgm_data$descriptions[index_b])

# 3d. Perform community detection ----
# Detect communities using walktrap from igraph
igraph_b <- as.igraph(network_b)
communities_b <- cluster_walktrap(igraph_b,
                                  weights = E(igraph_b)$weight,
                                  steps = 7,
                                  merges = TRUE,
                                  modularity = TRUE,
                                  membership = TRUE)

# Define groups and re-do original plot
groups_b <- vector(mode = "list", length = length(unique(communities_b$membership)))
for (i in 1:length(unique(communities_b$membership)))
{
  groups_b[[i]] = which(communities_b$membership %in% i);
}

network_b <- qgraph(fit_b$pairwise$wadj,
                    layout = "spring",
                    edge.color = fit_b$pairwise$edgecolor,
                    nodeNames = mgm_data$descriptions[index_b],
                    groups = groups_b)

tiff(file = paste(output,"/network_b.tiff",sep = ""),units="in",width=7,height=5,res=300)
plot(network_b)
dev.off()

# 3e. Calculate node strength ----
centrality_b <- centrality_auto(network_b)
bridge_b <- bridge(network_b, communities = communities_b$membership)
centralities_b <- data.frame("strength" = scale(centrality_b$node.centrality$Strength), 
                             "nodes" = c(1:length(nodevars_b)),
                             "node.names" = mgm_data$colnames[index_b])

central_plot_b <- ggplot(centralities_b, aes(nodes)) + 
  geom_line(aes(y = strength, colour = "strength")) + geom_point(aes(y = strength, colour = "strength")) +
  labs( x = "nodes", y = "z-score",title = "Centrality Indices: Mental Health and Cognition Network") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1),plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  scale_x_continuous(breaks = centralities_b$nodes, labels = mgm_data$descriptions[index_b]) +
  scale_colour_manual("", breaks = "strength",values = "orange")

plot(central_plot_b)

tiff(file = paste(output,"/centrality_b.tiff",sep = ""),units="in",width=6,height=5,res=300)
plot(central_plot_b)
dev.off()

# 3f. Assess stability of parameters ----
boots_b <- bootnet(data = mgm_data$data[,nodevars_b],
                   nBoots = bootstraps, 
                   default = "mgm",
                   type = "case",
                   statistics = "strength",
                   verbose = TRUE,
                   caseMin=0.025,
                   caseMax=0.925,
                   caseN=100)

boots_b_plot <- plot(boots_b)
tiff(file = paste(output,"/stability_b.tiff",sep = ""),units="in",width=6,height=4,res=300)
plot(boots_b_plot)
dev.off()

stability_b <- corStability(boots_b)

# 4: Network model C: all measures  --------------------------------------------
# 4a. Define node variables ----
nodevars_c <- c("intern","extern","stopsig","wasi",
                "dom_viol_phys_cu","dom_viol_emo_cu","phys_cruel_cu",
                "emo_cruel_cu","phys_abuse_cu","care_change_cu","separate_cu",
                "fin_diff_cu","interact_mum_cu","interact_dad_cu")
index_c <- match(nodevars_c,colnames)

# 4b. Fit model ----
fit_c <- mgm(data = mgm_data$data[,nodevars_c],
             type = mgm_data$type[index_c],
             level = mgm_data$level[index_c],
             ruleReg = "AND",
             k = 2,        
             lambdaSel = 'EBIC',    
             lambdaGam = 0.25, 
             pbar = FALSE)

# 4b. Plot network ----
network_c <- qgraph(fit_c$pairwise$wadj,
                    layout = "spring",
                    edge.color = fit_c$pairwise$edgecolor,
                    nodeNames = mgm_data$descriptions[index_c])

# 4c. Perform community detection ----
# Detect communities using walktrap from igraph
igraph_c <- as.igraph(network_c)
communities_c <- cluster_walktrap(igraph_c,
                                  weights = E(igraph_c)$weight,
                                  steps = 7,
                                  merges = TRUE,
                                  modularity = TRUE,
                                  membership = TRUE)

modularity_c <- modularity(igraph_c,communities_c$membership,weights=E(igraph_c)$weight)

# Define groups and re-do original plot
groups_c <- vector(mode = "list", length = length(unique(communities_c$membership)))
names(groups_c) <- c(1:length(unique(communities_c$membership)))
for (i in 1:length(unique(communities_c$membership)))
{
  groups_c[[i]] = which(communities_c$membership %in% i);
}

network_c <- qgraph(fit_c$pairwise$wadj,
                    layout = "spring",
                    edge.color = fit_c$pairwise$edgecolor,
                    nodeNames = mgm_data$descriptions[index_c],
                    groups = groups_c)

tiff(file = paste(output,"/network_c.tiff",sep = ""),units="in",width=11,height=7,res=300)
plot(network_c)
dev.off()

# 4d. Calculate centrality and bridge strength ----
centrality_c <- centrality_auto(network_c)
bridge_c <- bridge(network_c, communities = groups_c)
centralities_c <- data.frame("strength" = scale(centrality_c$node.centrality$Strength), 
                             "bridge.strength" = scale(bridge_c$`Bridge Strength`),
                             "nodes" = c(1:length(nodevars_c)),
                             "node.names" = mgm_data$colnames[index_c])

central_plot_c <- ggplot(centralities_c, aes(nodes)) + 
  geom_line(aes(y = bridge.strength, colour = "bridge.strength")) + geom_point(aes(y = bridge.strength, colour = "bridge.strength")) +
  geom_line(aes(y = strength, colour = "strength")) + geom_point(aes(y = strength, colour = "strength")) +
  labs( x = "nodes", y = "z-score",title = "Centrality Indices: Overall Network") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1),plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  scale_x_continuous(breaks = centralities_c$nodes, labels = mgm_data$descriptions[index_c]) +
  scale_colour_manual("", breaks = c("bridge.strength", "strength"),values = c("red", "orange"))

plot(central_plot_c)

tiff(file = paste(output,"/centrality_c.tiff",sep = ""),units="in",width=6,height=5,res=300)
plot(central_plot_c)
dev.off()

# 4e. Test stability  of estimated parameters ----
boots_c <- bootnet(data = mgm_data$data[,nodevars_c],
                   nBoots = bootstraps, 
                   default = "mgm",
                   type = "case",
                   statistics = c("strength","bridgeStrength"),
                   communities = communities_c$membership,
                   verbose = TRUE,
                   caseMin=0.025,
                   caseMax=0.975,
                   caseN=100)
boots_c_plot <- plot(boots_c,statistics =c("bridgeStrength","strength"))
tiff(file = paste(output,"/stability_c.tiff",sep = ""),units="in",width=6,height=4,res=300)
plot(boots_c_plot) + ggtitle("Stability of Centrality Indices: Overall Network")
dev.off()

stability_c <- corStability(boots_c)

# 4f. Assess robustness of clusters ----
perms_c <- shuffleSet(n = length(nodevars_c),nset = permutations,
                      control = how(within = Within(type ="free")))

null_effects_c <-apply(X = perms_c,MARGIN = 1,FUN = function(x){
  communities_temp <- communities_c$membership[t(x)]
  modularity_temp <- modularity(igraph_c,communities_temp,weights=E(igraph_c)$weight)
  
  return(modularity_temp)
}
)

null_effects_c <- as.data.frame(null_effects_c)
colMeans(null_effects_c)
perm_pval_c = (sum(null_effects_c > modularity_c)*1)/permutations


# 5. Perform model-based recursive partitioning by age --------------------------------------------
# Define partitioning and node variables
nodevars_split <- c("intern","extern","stopsig","wasi",
                    "dom_viol_emo","dom_viol_phys","phys_cruel",
                    "emo_cruel","phys_abuse",
                    "care_change","separate","fin_diff","interact_mum","interact_dad")
index_split <- match(nodevars_split,colnames_long)
splitvars_split <- "time"
# Run model-based partitioning
network_split <- networktree(nodevars = mgm_data_long$data[,nodevars_split],
                             splitvars = mgm_data_long$data[,splitvars_split],
                             model = "correlation",
                             method = "mob",
                             transform ="glasso",
                             maxdepth = 5,
                             na.action=na.omit)
network_split

tiff(file = paste(output,"/network_tree.tiff",sep = ""),units="in",width=15,height=11,res=200)
plot(network_split,
     transform = "glasso",
     layout = "spring")
dev.off()

# Split the data frame based on where the partition was detected
leaf1 = c(1)
leaf2 = c(2:4)
leaf3 = c(5)

mgm_data_leaf1 <- list(data=as.matrix(proj_data_imp_long[proj_data_imp_long$time %in% leaf1,colnames_long]), 
                       types = types_long, levels=levels_long, colnames=colnames_long, descriptions=descriptions_long)
mgm_data_leaf2 <- list(data=as.matrix(proj_data_imp_long[proj_data_imp_long$time %in% leaf2,colnames_long]),
                       types = types_long, levels=levels_long, colnames=colnames_long, descriptions=descriptions_long)
mgm_data_leaf3 <- list(data=as.matrix(proj_data_imp_long[proj_data_imp_long$time %in% leaf3,colnames_long]),
                       types = types_long, levels=levels_long, colnames=colnames_long, descriptions=descriptions_long)

# 6. Analyse partitioned data  --------------------------------------------
# 6a. Fit mixed graphical model and plot first node ----
fit_leaf1 <- mgm(data = mgm_data_leaf1$data[,nodevars_split],
                 type = mgm_data_leaf1$type[index_split],
                 level = mgm_data_leaf1$level[index_split],
                 ruleReg = "AND",
                 k = 2,                               
                 lambdaSel = 'EBIC',                  
                 lambdaGam = 0.25,                    
                 pbar = FALSE)

network_leaf1 <- qgraph(fit_leaf1$pairwise$wadj,
                        layout = "spring",
                        edge.color = fit_leaf1$pairwise$edgecolor,
                        nodeNames = mgm_data_leaf1$descriptions[index_split])

# 6b. Fit mixed graphical model and plot second node ----
fit_leaf2 <- mgm(data = mgm_data_leaf2$data[,nodevars_split],
                 type = mgm_data_leaf2$type[index_split],
                 level = mgm_data_leaf2$level[index_split],
                 ruleReg = "AND",
                 k = 2,                               
                 lambdaSel = 'EBIC',                  
                 lambdaGam = 0.25,                    
                 pbar = FALSE)

network_leaf2 <- qgraph(fit_leaf2$pairwise$wadj,
                        layout = "spring",
                        edge.color = fit_leaf2$pairwise$edgecolor,
                        nodeNames = mgm_data_leaf2$descriptions[index_split])

# 6c. Fit mixed graphical model and plot third node ----
fit_leaf3 <- mgm(data = mgm_data_leaf3$data[,nodevars_split],
                 type = mgm_data_leaf3$type[index_split],
                 level = mgm_data_leaf3$level[index_split],
                 ruleReg = "AND",
                 k = 2,                               
                 lambdaSel = 'EBIC',                  
                 lambdaGam = 0.25,                    
                 pbar = FALSE)

network_leaf3 <- qgraph(fit_leaf3$pairwise$wadj,
                        layout = "spring",
                        edge.color = fit_leaf3$pairwise$edgecolor,
                        nodeNames = mgm_data_leaf3$descriptions[index_split])

# 6d. Perform community detection on each node ----
# Detect communities on leaf 1
igraph_leaf1 <- as.igraph(network_leaf1)
communities_leaf1 <- cluster_walktrap(igraph_leaf1,
                                      weights = E(igraph_leaf1)$weight,
                                      steps = 7, 
                                      merges = TRUE,
                                      modularity = TRUE,
                                      membership = TRUE)

modularity_leaf1<- modularity(igraph_leaf1,communities_leaf1$membership,weights=E(igraph_leaf1)$weight)

# Define groups and re-do original plot for leaf 1
groups_leaf1 <- vector(mode = "list", length = length(unique(communities_leaf1$membership)))
for (i in 1:length(unique(communities_leaf1$membership)))
{
  groups_leaf1[[i]] = which(communities_leaf1$membership %in% i);
}
network_leaf1 <- qgraph(fit_leaf1$pairwise$wadj,
                        layout = "spring",
                        edge.color = fit_leaf1$pairwise$edgecolor,
                        nodeNames = mgm_data_leaf1$descriptions[index_split],
                        groups = groups_leaf1)

tiff(file = paste(output,"/network_leaf1.tiff",sep = ""),units="in",width=11,height=7,res=300)
plot(network_leaf1)
dev.off()

# Detect communities on leaf 2
igraph_leaf2 <- as.igraph(network_leaf2)
communities_leaf2 <- cluster_walktrap(igraph_leaf2,
                                      weights = E(igraph_leaf2)$weight,
                                      steps = 7, 
                                      merges = TRUE,
                                      modularity = TRUE,
                                      membership = TRUE)

modularity_leaf2 <- modularity(igraph_leaf2,communities_leaf2$membership,weights=E(igraph_leaf2)$weight)

# Define groups and re-do original plot for leaf 2
groups_leaf2 <- vector(mode = "list", length = length(unique(communities_leaf2$membership)))
for (i in 1:length(unique(communities_leaf2$membership)))
{
  groups_leaf2[[i]] = which(communities_leaf2$membership %in% i);
}

network_leaf2 <- qgraph(fit_leaf2$pairwise$wadj,
                        layout = "spring",
                        edge.color = fit_leaf2$pairwise$edgecolor,
                        nodeNames = mgm_data_leaf2$descriptions[index_split],
                        groups = groups_leaf2)

tiff(file = paste(output,"/network_leaf2.tiff",sep = ""),units="in",width=11,height=7,res=300)
plot(network_leaf2)
dev.off()

# Detect communities on leaf 3
igraph_leaf3 <- as.igraph(network_leaf3)
communities_leaf3 <- cluster_walktrap(igraph_leaf3,
                                      weights = E(igraph_leaf3)$weight,
                                      steps = 7, 
                                      merges = TRUE,
                                      modularity = TRUE,
                                      membership = TRUE)

modularity_leaf3 <- modularity(igraph_leaf3,communities_leaf3$membership,weights=E(igraph_leaf3)$weight)

# Define groups and re-do original plot for leaf 3
groups_leaf3 <- vector(mode = "list", length = length(unique(communities_leaf3$membership)))
for (i in 1:length(unique(communities_leaf3$membership)))
{
  groups_leaf3[[i]] = which(communities_leaf3$membership %in% i);
}
network_leaf3 <- qgraph(fit_leaf3$pairwise$wadj,
                        layout = "spring",
                        edge.color = fit_leaf3$pairwise$edgecolor,
                        nodeNames = mgm_data_leaf3$descriptions[index_split],
                        groups = groups_leaf3)

tiff(file = paste(output,"/network_leaf3.tiff",sep = ""),units="in",width=11,height=7,res=300)
plot(network_leaf3)
dev.off()

# 6e. Calculate centrality and bridge strength for each node ----
centrality_leaf1 <- centrality_auto(network_leaf1)
bridge_leaf1 <- bridge(network_leaf1, communities = communities_leaf1$membership)
centralities_leaf1 <- data.frame("strength" = scale(centrality_leaf1$node.centrality$Strength), 
                                 "bridge.strength" = scale(bridge_leaf1$`Bridge Strength`),
                                 "nodes" = c(1:length(nodevars_split)),
                                 "node.names" = mgm_data_leaf1$colnames[index_split])

central_plot_leaf1 <- ggplot(centralities_leaf1, aes(nodes)) + 
  geom_line(aes(y = bridge.strength, colour = "bridge.strength")) + geom_point(aes(y = bridge.strength, colour = "bridge.strength")) +
  geom_line(aes(y = strength, colour = "strength")) + geom_point(aes(y = strength, colour = "strength")) +
  labs( x = "nodes", y = "z-score",title = "Centrality Indices: First Subnetwork") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1),plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  scale_x_continuous(breaks = centralities_leaf1$nodes, labels = mgm_data_leaf1$descriptions[index_split]) +
  scale_colour_manual("", breaks = c("bridge.strength", "strength"),values = c("red", "orange"))

plot(central_plot_leaf1)
tiff(file = paste(output,"/centrality_leaf1.tiff",sep = ""),units="in",width=6,height=5,res=300)
plot(central_plot_leaf1)
dev.off()

centrality_leaf2 <- centrality_auto(network_leaf2)
bridge_leaf2 <- bridge(network_leaf2, communities = communities_leaf2$membership)
centralities_leaf2 <- data.frame("strength" = scale(centrality_leaf2$node.centrality$Strength), 
                                 "bridge.strength" = scale(bridge_leaf2$`Bridge Strength`),
                                 "nodes" = c(1:length(nodevars_split)),
                                 "node.names" = mgm_data_leaf2$colnames[index_split])

central_plot_leaf2 <- ggplot(centralities_leaf2, aes(nodes)) + 
  geom_line(aes(y = bridge.strength, colour = "bridge.strength")) + geom_point(aes(y = bridge.strength, colour = "bridge.strength")) +
  geom_line(aes(y = strength, colour = "strength")) + geom_point(aes(y = strength, colour = "strength")) +
  labs( x = "nodes", y = "z-score",title = "Centrality Indices: Second Subnetwork") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1),plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  scale_x_continuous(breaks = centralities_leaf2$nodes, labels = mgm_data_leaf2$descriptions[index_split]) +
  scale_colour_manual("", breaks = c("bridge.strength", "strength"),values = c("red", "orange"))

plot(central_plot_leaf2)
tiff(file = paste(output,"/centrality_leaf2.tiff",sep = ""),units="in",width=6,height=5,res=300)
plot(central_plot_leaf2)
dev.off()

centrality_leaf3 <- centrality_auto(network_leaf3)
bridge_leaf3 <- bridge(network_leaf3, communities = communities_leaf3$membership)
centralities_leaf3 <- data.frame("strength" = scale(centrality_leaf3$node.centrality$Strength), 
                                 "bridge.strength" = scale(bridge_leaf3$`Bridge Strength`),
                                 "nodes" = c(1:length(nodevars_split)),
                                 "node.names" = mgm_data_leaf3$colnames[index_split])

central_plot_leaf3 <- ggplot(centralities_leaf3, aes(nodes)) + 
  geom_line(aes(y = bridge.strength, colour = "bridge.strength")) + geom_point(aes(y = bridge.strength, colour = "bridge.strength")) +
  geom_line(aes(y = strength, colour = "strength")) + geom_point(aes(y = strength, colour = "strength")) +
  labs( x = "nodes", y = "z-score",title = "Centrality Indices: Third Subnetwork") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1),plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  scale_x_continuous(breaks = centralities_leaf3$nodes, labels = mgm_data_leaf3$descriptions[index_split]) +
  scale_colour_manual("", breaks = c("bridge.strength", "strength"),values = c("red", "orange"))

plot(central_plot_leaf3)
tiff(file = paste(output,"/centrality_leaf3.tiff",sep = ""),units="in",width=6,height=5,res=300)
plot(central_plot_leaf3)
dev.off()

# 6f. Test stability  of estimated parameters ----
boots_leaf1 <- bootnet(data = mgm_data_leaf1$data[,nodevars_split],
                       nBoots = bootstraps, 
                       default = "mgm",
                       type = "case",
                       statistics = c("strength","bridgeStrength"),
                       communities = communities_leaf1$membership,
                       verbose = TRUE,
                       caseMin=0.025,
                       caseMax=0.975,
                       caseN=100)
boots_leaf1_plot <- plot(boots_leaf1, statistics = c("strength","bridgeStrength"))
tiff(file = paste(output,"/stability_leaf1.tiff",sep = ""),units="in",width=6,height=4,res=300)
plot(boots_leaf1_plot) + ggtitle("Stability of Centrality Indices: First Subnetwork")
dev.off()

stability_leaf1 <- corStability(boots_leaf1)


boots_leaf2 <- bootnet(data = mgm_data_leaf2$data[,nodevars_split],
                       nBoots = bootstraps, 
                       default = "mgm",
                       type = "case",
                       statistics = c("strength","bridgeStrength"),
                       communities = communities_leaf2$membership,
                       verbose = TRUE,
                       caseMin=0.025,
                       caseMax=0.975,
                       caseN=100)
boots_leaf2_plot <- plot(boots_leaf2, statistics = c("strength","bridgeStrength"))
tiff(file = paste(output,"/stability_leaf2.tiff",sep = ""),units="in",width=6,height=4,res=300)
plot(boots_leaf2_plot) + ggtitle("Stability of Centrality Indices: Second Subnetwork")
dev.off()

stability_leaf2 <- corStability(boots_leaf2)

boots_leaf3 <- bootnet(data = mgm_data_leaf3$data[,nodevars_split],
                       nBoots = bootstraps, 
                       default = "mgm",
                       type = "case",
                       statistics = c("strength","bridgeStrength"),
                       communities = communities_leaf3$membership,
                       verbose = TRUE,
                       caseMin=0.025,
                       caseMax=0.975,
                       caseN=100)
boots_leaf3_plot <- plot(boots_node3, statistics = c("strength","bridgeStrength"))
tiff(file = paste(output,"/stability_leaf3.tiff",sep = ""),units="in",width=6,height=4,res=300)
plot(boots_leaf3_plot) + ggtitle("Stability of Centrality Indices: Third Subnetwork")
dev.off()

stability_leaf3 <- corStability(boots_leaf3)

# 6g. Test robustness of clusters ----
perms_leaf1 <- shuffleSet(n = length(nodevars_split),nset = permutations,
                          control = how(within = Within(type ="free")))

null_effects_leaf1 <-apply(X = perms_leaf1,MARGIN = 1,FUN = function(x){
  communities_temp <- communities_leaf1$membership[t(x)]
  modularity_temp <- modularity(igraph_leaf1,communities_temp,weights=E(igraph_leaf1)$weight)
  
  return(modularity_temp)
}
)

null_effects_leaf1 <- as.data.frame(null_effects_leaf1)
colMeans(null_effects_leaf1)
perm_pval_leaf1 = (sum(null_effects_leaf1 > modularity_leaf1)*1)/permutations


perms_leaf2 <- shuffleSet(n = length(nodevars_split),nset = permutations,
                          control = how(within = Within(type ="free")))

null_effects_leaf2 <-apply(X = perms_leaf2,MARGIN = 1,FUN = function(x){
  communities_temp <- communities_leaf2$membership[t(x)]
  modularity_temp <- modularity(igraph_leaf2,communities_temp,weights=E(igraph_leaf2)$weight)
  
  return(modularity_temp)
}
)

null_effects_leaf2 <- as.data.frame(null_effects_leaf2)
colMeans(null_effects_leaf2)
perm_pval_leaf2 = (sum(null_effects_leaf2 > modularity_leaf2)*1)/permutations


perms_leaf3 <- shuffleSet(n = length(nodevars_split),nset = permutations,
                          control = how(within = Within(type ="free")))

null_effects_leaf3 <-apply(X = perms_leaf3,MARGIN = 1,FUN = function(x){
  communities_temp <- communities_leaf3$membership[t(x)]
  modularity_temp <- modularity(igraph_leaf3,communities_temp,weights=E(igraph_leaf3)$weight)
  
  return(modularity_temp)
}
)

null_effects_leaf3 <- as.data.frame(null_effects_leaf3)
colMeans(null_effects_leaf3)
perm_pval_leaf3 = (sum(null_effects_leaf3 > modularity_leaf3)*1)/permutations

# 7. Make additional figures for publication  --------------------------------------------

# 7a. Three subnetworks in one figure ----
tiff(file = paste(output,"/network_leaves.tiff",sep = ""),units="in",width=5,height=5,res=300)
par(mfrow=c(2,2), tcl=-0.5, mai=c(0.3,0.3,0.3,0.3))
qgraph(fit_leaf1$pairwise$wadj,
       layout = "spring",
       edge.color = fit_leaf1$pairwise$edgecolor,
       groups = groups_leaf1,
       title = "A")
qgraph(fit_leaf2$pairwise$wadj,
       layout = "spring",
       edge.color = fit_leaf2$pairwise$edgecolor,
       groups = groups_leaf2,
       title = "B")
qgraph(fit_leaf3$pairwise$wadj,
       layout = "spring",
       edge.color = fit_leaf3$pairwise$edgecolor,
       groups = groups_leaf3,
       title = "C")
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend = mgm_data_long$descriptions[index_split],cex=0.5, bty='n',inset = c(0.05,0))
legend("topleft", legend = c(1:14),cex=0.5, bty='n')
mtext("Nodes", at=0.10, cex=0.5)
dev.off()
par(mfrow=c(1,1))

# 7b. All centrality plots in one figure ----
tiff(file = paste(output,"/centralities.tiff",sep = ""),units="in",width=9,height=12,res=300)
ggarrange(central_plot_c,central_plot_a,central_plot_leaf1,central_plot_leaf2,central_plot_leaf3,
          ncol=2,nrow = 3)
dev.off()

# 7c. All stability plots in one figure ----
tiff(file = paste(output,"/stabilities.tiff",sep = ""),units="in",width=9,height=12,res=300)
ggarrange(boots_c_plot,boots_a_plot,boots_leaf1_plot,boots_leaf2_plot,boots_leaf3_plot,
          labels = c("Overall Network","Adversities Network","First Subnetwork",
                     "Second Subnetwork","Third Subnetwork"),
          vjust = 5.3, hjust = -0.5, font.label = list(size = 10),
          ncol=2,nrow = 3)
dev.off()

