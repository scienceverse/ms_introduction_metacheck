# Reanalysis of McCarley & Yamani (2021) ####
## Load libraries ####
library(pacman)
p_load(tidyverse,magrittr,jagsUI,MCMCvis,tidybayes,cowplot,readxl,logspline,patchwork,lme4,lmerTest)

## Load & prepare data set ####
files <- list.files(path="~/",all.files = TRUE,full.names=TRUE) # Add the directory where the data files are
files<-files[-c(1,2)]
d <- files %>% map_df(read_tsv,skip = 2)
d[,10:11] <- NULL
colnames(d) <- c('ID', 'block', 'trial', 'onset_time', 'signal', 'intensity', 'response', 'ACC',  'RT')
d <- d %>% mutate(response_code = as.numeric(response == 'go'))

# task ran for 5 blocks of 4 minutes each,
# bu we occasionally have a stray trial labeled block 6
# since those are always last trial of session, 
# just lump with block 5
nblocks = 5 # for twenty minute run  
d %<>% mutate(ID = as_factor(ID), block = if_else(block == 6, nblocks, block))

# prepare data for modeling below
## get responses and counts of trial types
rn <- d %>% 
  filter(block==1|block==nblocks) %>%
  mutate(block=ifelse(block==1,"First block","Last block")) %>% 
  group_by (ID, block, intensity) %>% 
  summarise(r = sum(response_code), n = n()) %>% 
  ungroup()
rn$stim_code <- match(rn$intensity, sort(unique(rn$intensity)))   ## get unique stimulus levels, sort them
int_levels <- d %>% pull(intensity) %>% unique() %>% sort() ## number the stimulus levels 
nLevels <- length(int_levels) ## count the number of unique stimulus levels
int_mean <- int_levels %>% mean()
nsubj <- length(unique(d$ID))

# MODELS
# Bayesian estimation of psychometric curve
# Lee & Wagenmaker, ch. 12

## set model parameters
warmup <-10000
n_chains <- 4
n_samples_chain <- 50000
n_samples <- n_samples_chain * n_chains
thin <- 4

## Three-parameter model ####
parameters = c("mu_A", "mu_B", "mu_probitD",
               "delta_A", "delta_B", "delta_probitD", 
               "mu_alpha_A", "mu_alpha_B", "mu_alpha_probitD", 
               "sigma_A", "sigma_B",  "sigma_probitD", 
               "sigma_alpha_A", "sigma_alpha_B", "sigma_alpha_probitD",
               "A", "B", "probitD",
               "alpha_A", "alpha_B", "alpha_probitD")

dataList = list (totalN = dim(rn)[1],
                 ID = match(rn$ID, unique(rn$ID)),
                 block = rn %>% mutate(block = recode(block, 'First block' = -1/2, 'Last block' = 1/2)) %>% pull(block),
                 x = rn$intensity - int_mean,
                 r = rn$r, n = rn$n,
                 nSubjs = nsubj)

setwd("~/") # Add the directory where the model is

jags_out <- autojags(dataList, inits = NULL, parameters.to.save = parameters,
                     model.file ="model_ABD.txt", 
                     iter.increment = n_samples, save.all.iter = TRUE,
                     n.adapt = NULL,
                     n.chains=n_chains, n.burnin=warmup, n.thin=thin, max.iter = 210000)

draws <- jags_out$sims.list %>% as_tibble()

jags_out$summary %>% as_tibble(rownames = "rows") %>% 
  filter(!str_detect(rows, "[:digit:]"))

## Bayes Factors ####
# Calculate Savage-Dickey ratios
## B01 is likelihood of posterior at M = 0 over likelihood of prior at M = 0
## This is a Bayes factor measuring of how much the data support M = 0
## B0 > 1 is support for value of 0
## B0 < 1 is evidence against value of 0
dens <- logspline(draws$delta_A)
B01_delta_A <- dlogspline(0, dens) / dnorm(0, 0, 1)
B10_delta_A <- 1 / B01_delta_A

dens <- logspline(draws$delta_B)
B01_delta_B <- dlogspline(0, dens) / dnorm(0, 0, 1)
B10_delta_B <- 1 / B01_delta_B

dens <- logspline(draws$delta_probitD)
B01_delta_probitD <- dlogspline(0, dens) / dnorm(0, 0, 1)
B10_delta_probitD <- 1 / B01_delta_probitD

B01_delta_A; B01_delta_B; B01_delta_probitD
B10_delta_A; B10_delta_B; B10_delta_probitD


## Plot ####
logistic <- function(x, a, b, c = 0, d = 1, mean_intensity = 0){
  # include the mean_intensity argument to shift curves for fits based on zero-centered data
  c + (d-c)*1/(1+exp(-(x - mean_intensity -a )/b))
}


M2 <- draws %>% summarise(A = mean(mu_A),
                          B = mean(mu_B),
                          probitD = mean(mu_probitD),  
                          
                          block1_M_A = A - 1/2 * mean(mu_alpha_A),
                          block1_M_B = B - 1/2 * mean(mu_alpha_B),
                          block1_M_D = 1 - pnorm(probitD - 1/2 * mean(mu_alpha_probitD)),
                          
                          block5_M_A = A + 1/2 * mean(mu_alpha_A),
                          block5_M_B = B + 1/2 * mean(mu_alpha_B),
                          block5_M_D = 1 - pnorm(probitD + 1/2 * mean(mu_alpha_probitD)))

### first block, get mean rate of positive responses by stimulus intensity
### create a point plot of those values
### then superimpose a line plot of the model fit

block1 <- tibble(intensity = seq(0.75, 3.75, .01),
                 block = "First block",
                 M = map_dbl(intensity,
                             logistic,
                             a = M2$block1_M_A,
                             b = M2$block1_M_B,
                             d = M2$block1_M_D,
                             mean_intensity = int_mean))

block5 <- tibble(intensity = seq(0.75, 3.75, .01),
                 block = "Last block",
                 M = map_dbl(intensity,
                             logistic,
                             a = M2$block5_M_A,
                             b = M2$block5_M_B,
                             d = M2$block5_M_D,
                             mean_intensity = int_mean))

plot_dat2 <- rbind(block1,block5)

## Psychometric curves ####
mccarley_plot <- ggplot() +
  geom_line(data = plot_dat2, aes(x = intensity, y = M, group = block,color=block)) +
  scale_color_manual(values=c('darkgoldenrod3','gold'),
                     name="Task section",
                     breaks=c("First block","Last block"),
                     labels=c("First","Last"))+
  theme_classic() +
  theme(plot.margin=margin(t = 20, r = 0, b = 16, l = 16, "pt")) +
  theme(axis.line = element_line(colour = "#404040")) +
  theme(legend.position = c(0.175, 0.8),
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        legend.spacing.x = unit(.05, 'cm'),
        legend.spacing.y = unit(.15, 'cm'),
        legend.key.size = unit(.45, "cm"),
        legend.background = element_rect(fill=NA,color="#BABABA")) +
  labs(title = "McCarley & Yamani (2021)") +
  theme(plot.title = element_text(hjust = 0.5,vjust=3.5)) +
  theme(strip.background=element_rect(color=NA),
        strip.text = element_text(size = 10,color="#404040")) +
  labs(x="Distance (cm)", y="Proportion of signal responses") +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(axis.title.y = element_text(margin = margin(r=8)),
        axis.title.x = element_text(margin = margin(t=8))) + 
  theme(axis.ticks.length=unit(.15, "cm")) + 
  expand_limits(x=c(0.5,4),y=c(0,1)) +
  scale_x_continuous(expand = c(0,0),breaks = seq(0.75, 3.75, by = .5)) +
  scale_y_continuous(expand = c(0,0),breaks = seq(.25, 1, by = .25)) +
  theme(panel.spacing = unit(2, "lines"))

## Posterior plots ####
shift_plot <- ggplot() + 
  geom_density(data = draws,aes(x = delta_A, y = ..scaled..), color="#404040",fill = NA) + 
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-1, 2, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0.5, 1, 0.5), expand = c(0, 0)) + 
  panel_border(colour = 'black', size = .5) +
  theme_classic() +
  theme(axis.line = element_line(colour = "#404040")) +
  theme(aspect.ratio = .25, axis.text.x = element_blank()) +
  geom_vline(xintercept = 0, size = .5, linetype = 3) +
  annotate("text",
           label = expression("Shift"),
           x = -1.85, y = 0.85, size = 3, hjust = 0) +
  labs(x = element_blank(), y = element_blank())  +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(panel.spacing = unit(2, "lines"))

scale_plot <- ggplot() + 
  geom_density(data = draws,aes(x = delta_B, y = ..scaled..), color="#404040",fill = NA) + 
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-1, 2, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0.5, 1, 0.5), expand = c(0, 0)) + 
  panel_border(colour = 'black', size = .5) +
  theme_classic() +
  theme(axis.line = element_line(colour = "#404040")) +
  theme(aspect.ratio = .25, axis.text.x = element_blank()) +
  geom_vline(xintercept = 0, size = .5, linetype = 3) +
  annotate("text",
           label = expression("Scale"),
           x = -1.85, y = 0.85, size = 3, hjust = 0) +
  labs(x = element_blank(), y = element_blank())  +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(panel.spacing = unit(2, "lines"))

lapse_plot <- ggplot() + 
  geom_density(data = draws,aes(x = delta_probitD, y = ..scaled..), color="#404040",fill = NA) + 
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-1, 2, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0.5, 1, 0.5), expand = c(0, 0)) + 
  panel_border(colour = 'black', size = .5) +
  theme_classic() +
  theme(axis.line = element_line(colour = "#404040")) +
  theme(aspect.ratio = .25, axis.text.x = element_blank()) +
  geom_vline(xintercept = 0, size = .5, linetype = 3) +
  annotate("text",
           label = expression("Lapse rate"),
           x = -1.85, y = 0.85, size = 3, hjust = 0) +
  labs(x = element_blank(), y = element_blank())  +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(panel.spacing = unit(2, "lines"))

panel_plot <- shift_plot/scale_plot/lapse_plot


left_panel<-ggdraw(mccarley_plot) +
                  draw_plot(panel_plot,.45, .15, .6, .6)

## SDT plots: prepare noise/signal ratio ####
data_space<-d %>%
  mutate(Signal=ifelse(signal=="noGo","Noise","Signal"))

noise<-data_space %>%
  filter(Signal=="Noise")
target<-data_space %>%
  filter(Signal=="Signal")
Sratio <- sd(noise$intensity)/sd(target$intensity)

## SDT plots: d[a] plot ####
N_hits<-d %>%
  filter(signal=="go") %>%
  group_by(ID,block) %>%
  summarise(N_hits=sum(response=="go",na.rm=TRUE),N_EV=n())

N_fa<-d %>%
  filter(signal=="noGo") %>%
  group_by(ID,block) %>%
  summarise(N_fa=sum(response=="go",na.rm=TRUE),N_Anti=n())

data_da<-cbind(N_hits,N_fa$N_fa,N_fa$N_Anti)
data_da<-data_da %>%
  rename(N_fa=...5,
         N_Anti=...6) %>%
  mutate(zHits=qnorm((N_hits+0.5)/(N_EV+1))) %>%
  mutate(zFA=qnorm((N_fa+0.5)/(N_Anti+1))) %>%
  mutate(da=sqrt(2/(1+Sratio^2))*(zHits-Sratio*zFA))

data_da_averages<-data_da %>%
  group_by(block) %>%
  summarise(da=mean(da))

da_plot<-ggplot(data_da, aes(x=block,y=da)) +
  geom_smooth(method="lm",size=0.5,se=FALSE,aes(color=..x..)) +
  geom_point(data=data_da_averages,aes(color=block)) +
  theme_classic() +
  theme(plot.margin = unit(c(.8,.8,.2,0), "cm")) +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(axis.line = element_line(colour = "#404040")) +
  labs(x="Block", y=expression(italic("d")["a"])) +
  theme(axis.title.y = element_text(margin = margin(r=8, l=5)),
        axis.title.x = element_text(margin = margin(t=8, b=5))) +
  theme(axis.ticks.length=unit(.15, "cm")) + 
  theme(legend.position = "none") +
  expand_limits(x=c(0.8,5.2),y=c(2.4,3)) +
  scale_color_gradient(low = "darkgoldenrod3", high = "gold") +
  scale_x_continuous(expand = c(0,0),breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(expand = c(0,0),breaks = seq(2.6, 3, by = .2)) +
  theme(aspect.ratio = .5)

## SDT plots: Beta plot ####
data_beta<-cbind(N_hits,N_fa$N_fa,N_fa$N_Anti)
data_beta<-data_beta %>%
  rename(N_fa=...5,
         N_Anti=...6) %>%
  mutate(zHits=qnorm(((N_hits+0.5)/(N_EV+1)))) %>%
  mutate(zFA=qnorm(((N_fa+0.5)/(N_Anti+1)))) %>%
  mutate(yHits=dnorm(zHits)) %>%
  mutate(yFA=dnorm(zFA)) %>%
  mutate(beta=yHits/yFA)

data_beta_averages<-data_beta %>%
  group_by(block) %>%
  summarise(beta=mean(beta))

beta_plot<-ggplot(data_beta, aes(x=block,y=beta)) +
  geom_smooth(method="lm",size=0.5,se=FALSE,aes(color=..x..)) +
  geom_point(data=data_beta_averages,aes(color=block)) +
  theme_classic() +
  theme(plot.margin = unit(c(.8,.8,.2,0), "cm")) +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(axis.line = element_line(colour = "#404040")) +
  labs(x="Block", y=expression(beta)) +
  theme(axis.title.y = element_text(margin = margin(r=8, l=5)),
        axis.title.x = element_text(margin = margin(t=8, b=5))) +
  theme(axis.ticks.length=unit(.15, "cm")) + 
  theme(legend.position = "none") +
  expand_limits(x=c(0.8,5.2),y=c(0,6)) +
  scale_color_gradient(low = "darkgoldenrod3", high = "gold") +
  scale_x_continuous(expand = c(0,0),breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(expand = c(0,0),breaks = seq(2, 6, by = 2)) +
  theme(aspect.ratio = .5)

right_panel<-da_plot/beta_plot

plot_grid(left_panel, right_panel,rel_widths = c(1, 0.5))

# ANTI-Vea ####
## Load & prepare data set: Online ####
setwd("~/") # Add the directory where the data files are
data_original1<-read_excel("ANTIVea_Online.xlsx")

data1<-data_original1 %>%
  filter(Trial_Type!="AV") %>%
  mutate(Y_T=as.numeric(Y_T),   
         Y_R1=as.numeric(Y_R1),   
         Y_L1=as.numeric(Y_L1)) %>%   
  mutate(distance_max_t_l1_r1=0)

## Compute maximum target-distractor distance
for(i in 1:nrow(data1)){
  data1$distance_max_t_l1_r1[i]<-max(abs(data1$Y_T[i]-data1$Y_R1[i]),abs(data1$Y_T[i]-data1$Y_L1[i]))
}

## Delete participants with low accuracy in ANTI and EV trials (47 out of 303 participants)
data_Negl<-data1 %>% 
  distinct(Participant,Y_T) %>% 
  group_by(Participant) %>% 
  count()
delete1<-which(data_Negl$n==14)

data1<-data1 %>%
  filter(Participant %in% data_Negl$Participant[delete1])

data_accANTI<-data1 %>%
  filter(Trial_Type=="ANTI") %>%
  group_by(Participant) %>%
  summarise(Acc=mean(Accuracy))

data_accEV<-data1 %>%
  filter(Trial_Type=="EV") %>%
  group_by(Participant) %>%
  summarise(Acc=mean(Accuracy))

delete2<-which(data_accANTI$Acc>=(mean(data_accANTI$Acc)-3*sd(data_accANTI$Acc)))
delete3<-which(data_accEV$Acc>=(mean(data_accEV$Acc)-3*sd(data_accEV$Acc)))

data1<-data1 %>%
  filter(Participant %in% data_accANTI$Participant[delete2]) %>%
  filter(Participant %in% data_accANTI$Participant[delete3])

data_none<-data1 %>%
  group_by(Participant) %>%
  summarise(N_None=sum(Answer=="None",na.rm=TRUE),N=n())

delete4<-which(data_none$N_None<=(mean(data_none$N_None)+3*sd(data_none$N_None)))

data1<-data1 %>%
  filter(Participant %in% data_none$Participant[delete4])

data1<-data1 %>%
  mutate(distance_max_t_l1_r1=round(distance_max_t_l1_r1,3)) %>%
  filter(distance_max_t_l1_r1==0|
           distance_max_t_l1_r1==0.003|
           distance_max_t_l1_r1==0.005|
           distance_max_t_l1_r1==0.008|
           distance_max_t_l1_r1==0.01|
           distance_max_t_l1_r1==0.016|
           distance_max_t_l1_r1==0.018|
           distance_max_t_l1_r1==0.021|
           distance_max_t_l1_r1==0.023|
           distance_max_t_l1_r1==0.026) %>%
  mutate(distance_max_t_l1_r1=ifelse(distance_max_t_l1_r1==0,0,
                                     ifelse(distance_max_t_l1_r1==0.003,1,
                                            ifelse(distance_max_t_l1_r1==0.005,2,
                                                   ifelse(distance_max_t_l1_r1==0.008,3,
                                                          ifelse(distance_max_t_l1_r1==0.01,4,
                                                                 ifelse(distance_max_t_l1_r1==0.016,6,
                                                                        ifelse(distance_max_t_l1_r1==0.018,7,
                                                                               ifelse(distance_max_t_l1_r1==0.021,8,
                                                                                      ifelse(distance_max_t_l1_r1==0.023,9,10))))))))))


## Load & prepare data set: Offline ####
data_original2<-read_excel("ANTIVea_Offline.xlsx")

data2<-data_original2 %>%
  filter(Trial_Type!="AV") %>%
  mutate(Y_T=as.numeric(Y_T),   
         Y_R1=as.numeric(Y_R1),   
         Y_L1=as.numeric(Y_L1)) %>%   
  mutate(distance_max_t_l1_r1=0)

# Compute maximum target-distractor distance
for(i in 1:nrow(data2)){
  data2$distance_max_t_l1_r1[i]<-max(abs(data2$Y_T[i]-data2$Y_R1[i]),abs(data2$Y_T[i]-data2$Y_L1[i]))
}

# Delete participants with low accuracy in ANTI and EV trials
data_accANTI<-data2 %>%
  filter(Trial_Type=="ANTI") %>%
  group_by(Participant) %>%
  summarise(Acc=mean(Accuracy))

data_accEV<-data2 %>%
  filter(Trial_Type=="EV") %>%
  group_by(Participant) %>%
  summarise(Acc=mean(Accuracy))

delete1<-which(data_accANTI$Acc>=(mean(data_accANTI$Acc)-3*sd(data_accANTI$Acc)))
delete2<-which(data_accEV$Acc>=(mean(data_accEV$Acc)-3*sd(data_accEV$Acc)))

data2<-data2 %>%
  filter(Participant %in% data_accANTI$Participant[delete1]) %>%
  filter(Participant %in% data_accANTI$Participant[delete2])

data_none<-data2 %>%
  group_by(Participant) %>%
  summarise(N_None=sum(Answer=="None", na.rm=TRUE),N=n())

delete3<-which(data_none$N_None<=(mean(data_none$N_None)+3*sd(data_none$N_None)))

data2<-data2 %>%
  filter(Participant %in% data_none$Participant[delete3])

## Merge both data sets & create the analysis data frame ####
data1<-data1 %>%
  select(-Experiment,-ExperimentGroup,-Distancia)
data2<-data2 %>%
  select(-Procedence,-ProcedenceName)

data<-rbind(data1,data2)

rn<-data %>%
  mutate(intensity=distance_max_t_l1_r1,
         ID=Participant) %>%
  filter(Block!=3,Block!=4) %>%
  mutate(block=ifelse(Block==1|Block==2,"First block","Last block")) %>% 
  group_by(ID,block,intensity) %>%
  summarise(r=(sum(Answer=="Space", na.rm=TRUE)),n=n()) 

# prepare data for modeling below
## get responses and counts of trial types
rn$stim_code <- match(rn$intensity, sort(unique(rn$intensity)))   ## get unique stimulus levels, sort them
int_levels <- data %>% pull(distance_max_t_l1_r1) %>% unique() %>% sort() ## number the stimulus levels 
nLevels <- length(int_levels) ## count the number of unique stimulus levels
int_mean <- int_levels %>% mean()
nsubj <- length(unique(data$Participant))

# MODELS
# Bayesian estimation of psychometric curve
# Lee & Wagenmaker, ch. 12

## set model parameters
warmup <-10000
n_chains <- 4
n_samples_chain <- 50000
n_samples <- n_samples_chain * n_chains
thin <- 4

## Three-parameter model ####
parameters = c("mu_A", "mu_B", "mu_probitD",
               "delta_A", "delta_B", "delta_probitD", 
               "mu_alpha_A", "mu_alpha_B", "mu_alpha_probitD", 
               "sigma_A", "sigma_B",  "sigma_probitD", 
               "sigma_alpha_A", "sigma_alpha_B", "sigma_alpha_probitD",
               "A", "B", "probitD",
               "alpha_A", "alpha_B", "alpha_probitD")

dataList = list (totalN = dim(rn)[1],
                 ID = match(rn$ID, unique(rn$ID)),
                 block = rn %>% mutate(block = recode(block, 'First block' = -1/2, 'Last block' = 1/2)) %>% pull(block),
                 x = rn$intensity - int_mean,
                 r = rn$r, n = rn$n,
                 nSubjs = nsubj)

setwd("~/") # Add the directory where the model is

jags_out <- autojags(dataList, inits = NULL, parameters.to.save = parameters,
                     model.file ="model_ABD.txt", 
                     iter.increment = n_samples, save.all.iter = TRUE,
                     n.adapt = NULL,
                     n.chains=n_chains, n.burnin=warmup, n.thin=thin, max.iter = 210000)

draws <- jags_out$sims.list %>% as_tibble()

jags_out$summary %>% as_tibble(rownames = "rows") %>% 
  filter(!str_detect(rows, "[:digit:]"))

## Bayes Factors ####
# Calculate Savage-Dickey ratios
## B01 is likelihood of posterior at M = 0 over likelihood of prior at M = 0
## This is a Bayes factor measuring of how much the data support M = 0
## B0 > 1 is support for value of 0
## B0 < 1 is evidence against value of 0
dens <- logspline(drawsE$delta_A)
B01_delta_A <- dlogspline(0, dens) / dnorm(0, 0, 1)
B10_delta_A <- 1 / B01_delta_A

dens <- logspline(drawsE$delta_B)
B01_delta_B <- dlogspline(0, dens) / dnorm(0, 0, 1)
B10_delta_B <- 1 / B01_delta_B

dens <- logspline(drawsE$delta_probitD)
B01_delta_probitD <- dlogspline(0, dens) / dnorm(0, 0, 1)
B10_delta_probitD <- 1 / B01_delta_probitD

B01_delta_A; B01_delta_B; B01_delta_probitD
B10_delta_A; B10_delta_B; B10_delta_probitD

## Plot ####
logistic <- function(x, a, b, c = 0, d = 1, mean_intensity = 0){
  # include the mean_intensity argument to shift curves for fits based on zero-centered data
  c + (d-c)*1/(1+exp(-(x - mean_intensity -a )/b))
}


M2 <- draws %>% summarise(A = mean(mu_A),
                          B = mean(mu_B),
                          probitD = mean(mu_probitD),  
                          
                          block1_M_A = A - 1/2 * mean(mu_alpha_A),
                          block1_M_B = B - 1/2 * mean(mu_alpha_B),
                          block1_M_D = 1 - pnorm(probitD - 1/2 * mean(mu_alpha_probitD)),
                          
                          block5_M_A = A + 1/2 * mean(mu_alpha_A),
                          block5_M_B = B + 1/2 * mean(mu_alpha_B),
                          block5_M_D = 1 - pnorm(probitD + 1/2 * mean(mu_alpha_probitD)))

### first block, get mean rate of positive responses by stimulus intensity
### create a point plot of those values
### then superimpose a line plot of the model fit

block1 <- tibble(intensity = seq(0.75, 3.75, .01),
                 block = "First block",
                 M = map_dbl(intensity,
                             logistic,
                             a = M2$block1_M_A,
                             b = M2$block1_M_B,
                             d = M2$block1_M_D,
                             mean_intensity = int_mean))

block5 <- tibble(intensity = seq(0.75, 3.75, .01),
                 block = "Last block",
                 M = map_dbl(intensity,
                             logistic,
                             a = M2$block5_M_A,
                             b = M2$block5_M_B,
                             d = M2$block5_M_D,
                             mean_intensity = int_mean))

plot_dat2 <- rbind(block1,block5)

## Psychometric curves ####
antivea_plot <- ggplot() +
  geom_line(data = plot_dat2, aes(x = intensity, y = M, group = block, color=block)) +
  scale_color_manual(values=c('#31639B','steelblue1'),
                     name="Task section",
                     breaks=c("First block","Last block"),
                     labels=c("First","Last"))+
  theme_classic() +
  theme(plot.margin=margin(t = 20, r = 0, b = 16, l = 16, "pt")) +
  theme(axis.line = element_line(colour = "#404040")) +
  theme(legend.position = c(0.175, 0.8),
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        legend.spacing.x = unit(.05, 'cm'),
        legend.spacing.y = unit(.15, 'cm'),
        legend.key.size = unit(.45, "cm"),
        legend.background = element_rect(fill=NA,color="#BABABA")) +
  labs(title = "ANTI-Vea") +
  theme(plot.title = element_text(hjust = 0.5,vjust=3.5)) +
  theme(strip.background=element_rect(color=NA),
        strip.text = element_text(size = 10,color="#404040")) +
  labs(x="Distance (pixels)", y="Proportion of signal responses") +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(axis.title.y = element_text(margin = margin(r=8)),
        axis.title.x = element_text(margin = margin(t=8))) + 
  theme(axis.ticks.length=unit(.15, "cm")) + 
  expand_limits(x=c(-0.5,10.5),y=c(0,1)) +
  scale_x_continuous(expand = c(0,0),breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(expand = c(0,0),breaks = seq(.25, 1, by = .25)) +
  theme(panel.spacing = unit(2, "lines"))

## Posterior plots #### 
shift_plot <- ggplot() + 
  geom_density(data = draws,aes(x = delta_A, y = ..scaled..), color="#404040",fill = NA) + 
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-1, 2, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0.5, 1, 0.5), expand = c(0, 0)) + 
  panel_border(colour = 'black', size = .5) +
  theme_classic() +
  theme(axis.line = element_line(colour = "#404040")) +
  theme(aspect.ratio = .25, axis.text.x = element_blank()) +
  geom_vline(xintercept = 0, size = .5, linetype = 3) +
  annotate("text",
           label = expression("Shift"),
           x = -1.85, y = 0.85, size = 3, hjust = 0) +
  labs(x = element_blank(), y = element_blank())  +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(panel.spacing = unit(2, "lines"))

scale_plot <- ggplot() + 
  geom_density(data = draws,aes(x = delta_B, y = ..scaled..), color="#404040",fill = NA) + 
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-1, 2, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0.5, 1, 0.5), expand = c(0, 0)) + 
  panel_border(colour = 'black', size = .5) +
  theme_classic() +
  theme(axis.line = element_line(colour = "#404040")) +
  theme(aspect.ratio = .25, axis.text.x = element_blank()) +
  geom_vline(xintercept = 0, size = .5, linetype = 3) +
  annotate("text",
           label = expression("Scale"),
           x = -1.85, y = 0.85, size = 3, hjust = 0) +
  labs(x = element_blank(), y = element_blank())  +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(panel.spacing = unit(2, "lines"))

lapse_plot <- ggplot() + 
  geom_density(data = draws,aes(x = delta_probitD, y = ..scaled..), color="#404040",fill = NA) + 
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-1, 2, 1), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0.5, 1, 0.5), expand = c(0, 0)) + 
  panel_border(colour = 'black', size = .5) +
  theme_classic() +
  theme(axis.line = element_line(colour = "#404040")) +
  theme(aspect.ratio = .25, axis.text.x = element_blank()) +
  geom_vline(xintercept = 0, size = .5, linetype = 3) +
  annotate("text",
           label = expression("Lapse rate"),
           x = -1.85, y = 0.85, size = 3, hjust = 0) +
  labs(x = element_blank(), y = element_blank())  +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(panel.spacing = unit(2, "lines"))

panel_plot <- shift_plot/scale_plot/lapse_plot

left_panel<-ggdraw(antivea_plot) +
  draw_plot(panel_plot,.45, .15, .6, .6)

## SDT plots: prepare noise/signal ratio ####
data_space<-data %>%
  filter(Trial_Type!="AV",
         Reaction_Time!=0) %>%
  mutate(Trial_Type=ifelse(Trial_Type=="ANTI","Noise","Signal"))

noise<-data_space %>%
  filter(Trial_Type=="Noise")
target<-data_space %>%
  filter(Trial_Type=="Signal")
Sratio <- sd(noise$distance_max_t_l1_r1)/sd(target$distance_max_t_l1_r1)

## SDT plots: d[a] ####
data_original1<-data_original1 %>%
  select(-Experiment,-ExperimentGroup,-Distancia)
data_original2<-data_original2 %>%
  select(-Procedence,-ProcedenceName)

data_original <- rbind(data_original1, data_original2)

N_hits<-data_original %>%
  filter(Reaction_Time!=0,
         Trial_Type=="EV") %>%
  group_by(Participant,Block) %>%
  summarise(N_hits=sum(Answer=="Space",na.rm=TRUE),N_EV=n())

N_fa<-data_original %>%
  filter(Reaction_Time!=0,
         Trial_Type=="ANTI") %>%
  group_by(Participant,Block) %>%
  summarise(N_fa=sum(Answer=="Space",na.rm=TRUE),N_Anti=n())

data_da<-cbind(N_hits,N_fa$N_fa,N_fa$N_Anti)
data_da<-data_da %>%
  rename(N_fa=...5,
         N_Anti=...6) %>%
  mutate(zHits=qnorm((N_hits+0.5)/(N_EV+1))) %>%
  mutate(zFA=qnorm((N_fa+0.5)/(N_Anti+1))) %>%
  mutate(da=sqrt(2/(1+Sratio^2))*(zHits-Sratio*zFA))

summary(lmer(da~Block+(1|Participant),
          data=data_da))

data_da_averages<-data_da %>%
  group_by(Block) %>%
  summarise(da=mean(da))

da_plot<-ggplot(data_da, aes(x=Block,y=da)) +
  geom_smooth(method="lm",size=0.5,se=FALSE,aes(color=..x..)) +
  geom_point(data=data_da_averages,aes(color=Block)) +
  theme_classic() +
  theme(plot.margin = unit(c(.8,.4,.2,.4), "cm")) +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(axis.line = element_line(colour = "#404040")) +
  labs(x="Block", y=expression(italic("d")["a"])) +
  theme(axis.title.y = element_text(margin = margin(r=8, l=5)),
        axis.title.x = element_text(margin = margin(t=8, b=5))) +
  theme(axis.ticks.length=unit(.15, "cm")) + 
  theme(legend.position = "none") +
  expand_limits(x=c(0.8,6.2),y=c(2.2,3)) +
  scale_color_gradient(low = "#31639B", high = "steelblue1") +
  scale_x_continuous(expand = c(0,0),breaks = seq(1, 6, by = 1)) +
  scale_y_continuous(expand = c(0,0),breaks = seq(2.4, 3, by = .2))

## SDT plots: Beta ####
data_beta<-cbind(N_hits,N_fa$N_fa,N_fa$N_Anti)
data_beta<-data_beta %>%
  rename(N_fa=...5,
         N_Anti=...6) %>%
  mutate(zHits=qnorm(((N_hits+0.5)/(N_EV+1)))) %>%
  mutate(zFA=qnorm(((N_fa+0.5)/(N_Anti+1)))) %>%
  mutate(yHits=dnorm(zHits)) %>%
  mutate(yFA=dnorm(zFA)) %>%
  mutate(beta=yHits/yFA)

summary(lmer(beta~Block+(1|Participant),
         data=data_beta))

data_beta_averages<-data_beta %>%
  group_by(Block) %>%
  summarise(beta=mean(beta))

beta_plot<-ggplot(data_beta, aes(x=Block,y=beta)) +
  geom_smooth(method="lm",size=0.5,se=FALSE,aes(color=..x..)) +
  geom_point(data=data_beta_averages,aes(color=Block)) +
  theme_classic() +
  theme(plot.margin = unit(c(.8,.4,.2,.4), "cm")) +
  theme(text=element_text(family="Helvetica",size=10,color="#404040"))+
  theme(axis.line = element_line(colour = "#404040")) +
  labs(x="Block", y=expression(beta)) +
  theme(axis.title.y = element_text(margin = margin(r=8, l=5)),
        axis.title.x = element_text(margin = margin(t=8, b=5))) +
  theme(axis.ticks.length=unit(.15, "cm")) + 
  theme(legend.position = "none") +
  expand_limits(x=c(0.8,6.2),y=c(2,7)) +
  scale_color_gradient(low = "#31639B", high = "steelblue1") +
  scale_x_continuous(expand = c(0,0),breaks = seq(1, 6, by = 1)) +
  scale_y_continuous(expand = c(0,0),breaks = seq(3, 7, by = 1))

right_panel<-da_plot/beta_plot

plot_grid(left_panel, right_panel,rel_widths = c(1, 0.5))