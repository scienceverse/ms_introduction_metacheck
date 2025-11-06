#Simulations
library(data.table)
library(ggplot2)

#if you want to grid search for UPCO; uncomment below, and on the end of 
#loop at line 132-134
#r2 = c()
#for(upco_param in seq(.1,.9,.01)){ 
#.52 is best r2

####MODEL FIRST, DATA SECOND
####SET PARAMETER SPACE
theta = seq(0,1,.01)
n = 29
a = seq(1,n,1)
b = seq(1,n,1)
m = expand.grid(a, b)

####COMPETENCE MODEL PREDICTIONS
library(gmp)
competence = function(a,b){
  n = a + b
  cons = factorialZ(n+1) / (factorialZ(a)*factorialZ(b))
  integrand = function(x){(x^b)*((1-x)^a)}
  integ = integrate(integrand, lower = 0, upper = .5)
  res = as.numeric(cons)*integ$value
  res
}

####GENERATING CONTOURS FOR E2

####PARTICPANT DATA

#data wrangling
library(qualtRics)
library(data.table)
library(lme4)

#plotting libraries
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(psych)
library(cowplot)
library(ggpubr)
#Set your directory here
setwd(r"(YOUR PATH HERE)")

#Import data here
Raw <- read_survey("pilot3_data.csv")
dt <- data.table(copy(Raw))

#remove unrealistic completion times
dt = dt[which(dt$`Duration (in seconds)`>(60*7)),]

dt = dt[C1 == 1, ] #comprehension check

#Assign Subject #s
dt[,Subject := seq(1:nrow(dt))]
dt[,Subject := as.factor(Subject)]

dt = dt[,-(StartDate:Q11)]

library(dplyr)
library(tidyr)

# Function to extract numbers from a string
extract_numbers <- function(str) {
  as.numeric(unlist(strsplit(gsub("\\[|\\]", "", str), ",")))
}
extract_strings <- function(str) {
  jsonlite::fromJSON(str)
}

key_cols = c("t", "f", 
             "post_j", "prior_j", "prior_c")

dvs = c("post_j", "prior_j", "prior_c")

lab_cols = c("prior_lab","post_lab")

dt = dt[post_j != '-1', ]

library(purrr)
# Apply the function and unnest while keeping the source column
ldf <- data.frame(dt) %>%
  mutate(
    across(all_of(key_cols), ~ lapply(., extract_numbers))) %>%
  mutate(
    across(all_of(lab_cols), ~ lapply(., extract_strings))) %>%
  pivot_longer(
    cols = all_of(c(dvs)),
    names_to = "column_name",
    values_to = "numbers_list"
  ) %>%
  rowwise() %>%
  mutate(lab = list(case_when(
    column_name %in% dvs[1] ~ post_lab,
    column_name %in% dvs[2:3] ~ prior_lab,
  ))) %>%
  mutate(panel = list(map2(t, f, ~ paste(.x, .y, sep = "_"))))  

columns_to_melt <- c("numbers_list", "lab", "panel","t","f")
columns_to_keep <- setdiff(names(ldf), columns_to_melt)

njudg = 18
ldf = ldf %>%
  unnest_longer(columns_to_melt) %>% 
  mutate(order = rep(seq(1, njudg),nrow(dt)*length(dvs))) %>% 
  select(c(columns_to_melt,'Subject','column_name', 'order')) %>% 
  data.table()

ldf1 = ldf[column_name %in% dvs[1], ]

ldf1 = ldf1 %>% 
  pivot_wider(names_from = column_name,
              values_from = numbers_list,
              id_cols = c(lab,Subject,panel,t,f,order))  %>% data.table()

ldf2 = ldf[column_name %in% dvs[2:3], ]
ldf2[,  `:=`(t = NULL , f = NULL, panel = NULL)] 

ldf2 = ldf2 %>% 
  pivot_wider(names_from = column_name,
              values_from = numbers_list,
              id_cols = c(lab,Subject))  %>%
  #mutate(order_pr = order) %>%
  data.table() 

d = ldf1[ldf2, on = .(Subject = Subject, lab = lab)]

d_grid = copy(d)

issues = unique(d$lab)
d[,prior_j := prior_j / 100]
d[,prior_c := prior_c / 100]
d[,post_j := post_j / 100]

d[,ext := ifelse(prior_j == 1 | prior_j == 0, 1, 0)]
d[,mean(ext)]
d[,mean(ext), lab][order(V1)]
d_up = d[ext != 1, ]

#One parameter:
c_multiplier = seq(0,1000,1)
ervec = c()
for(i in 1:length(c_multiplier)){
  d_up[, prior_alpha := prior_j*prior_c*c_multiplier[i] + 1]
  d_up[, prior_beta := (1-prior_j)*prior_c*c_multiplier[i] + 1]
  d_up[, Bayes := (prior_alpha + t) / (prior_alpha + prior_beta + t + f)]
  
  error = d_up[, abs(Bayes - post_j)]
  ervec = c(ervec, mean(error))
}
d[ext != 1, b_c_overall := c_multiplier[which.min(ervec)]]
d[, prior_alpha := prior_j*prior_c*b_c_overall + 1]
d[, prior_beta := (1-prior_j)*prior_c*b_c_overall + 1]
d[ext != 1, Bayes := (prior_alpha + t) / (prior_alpha + prior_beta + t + f)]
d[ext == 1, Bayes := prior_j]
d[,b_err := abs(Bayes - post_j)]
plot(ervec)

#Fitting at the Issue level:
d[,update := (post_j - prior_j)]

d[ext != 1,mean(abs(Bayes-post_j))]

####UPCO MODEL PREDICTIONS
upco = function(p,a,b,prior){
  (p^(a-b))*prior / ((p^(a-b))*prior + (1-p)^(a-b)*(1-prior))
}

u_conversion = seq(.1,.9,.01)
ervec = c()
for(i in 1:length(u_conversion)){
  d[ , UPCO := upco(u_conversion[i],t,f,prior_j)]
  error = d[ , abs(UPCO - post_j)]
  ervec = c(ervec, mean(error))
}
opt_upco = u_conversion[which.min(ervec)]

d[, UPCO := upco(opt_upco,t,f,prior_j)]
d[, u_err := abs(UPCO - post_j)]
#implement Competence


#One parameter:
op_multiplier = seq(1,50,1)
ervec = c()
for(i in 1:length(op_multiplier)){
   d$Competence = apply(d[,.(t + round(prior_j*op_multiplier[i],0),
                             f +  round((1-prior_j)*op_multiplier[i],0))],1,function(X) competence(X[1],X[2]))
  
  error = d[, abs(Competence - post_j)]
  ervec = c(ervec, mean(error))
}
plot(ervec)
opt_comp = op_multiplier[which.min(ervec)]
d$Competence = apply(d[,.(t + round(prior_j*opt_comp),
                          f + round((1-prior_j)*opt_comp))],1,function(X) competence(X[1],X[2]))
d[, c_err := abs(Competence - post_j)]

d$lab = factor(d$lab,levels = d[,mean(prior_c),lab][order(V1)]$lab)

##Power Analysis
cor.test(d$post_j,d$Bayes)
cor.test(d$post_j,d$UPCO)


d_plots = melt.data.table(d, measure.vars = c('Bayes','UPCO','Competence'))

ggplot(data = d, aes(x = b_err, y = u_err, color = update)) + #facet_wrap(~lab) + 
  geom_jitter(alpha = .3, size = 3) + 
  theme_classic(base_size  = 14) + 
  geom_abline(slope = 1) +
  labs(x = 'Bayes |Error|', y = 'UPCO |Error|') + 
  scale_colour_viridis_c(option = 'C')

ggplot(data = d, aes(x = b_err, y = c_err, color = update)) + #facet_wrap(~lab) + 
  geom_jitter(alpha = .3, size = 3) + 
  theme_classic(base_size  = 14) + 
  geom_abline(slope = 1) +
  labs(x = 'Bayes |Error|', y = 'Competence |Error|') + 
  scale_colour_viridis_c(option = 'C')

ggplot(d, aes(x = Competence, y = Bayes)) + facet_wrap(~lab) + 
  geom_point(alpha = .2)

d[,Proportion := t / (t + f)]
d[,Prior := prior_j]

library(lme4)
library(MuMIn)
#d_no_ext = d[ext != 1,]

d$Subject = as.factor(d$Subject)

mall = lmer(formula = post_j ~ Bayes  + Competence + UPCO, data = d, na.action = "na.fail") 
all_mods = dredge(mall, rank = "AIC") #,  extra = alist(AIC, BIC, ICOMP, Cp, "R^2)"

#85 participants, 18 points = 1530

set.seed(47)
samps = seq(25,250,25)
powers = c()
for(s in samps){
  deltas = c()
  for(n in 1:2000){
    d_samp = d[sample(nrow(d), size = 18*s,replace = T), ]
    AIC_b = AIC(lm(formula = post_j ~ Bayes , data = d_samp))
    AIC_u = AIC(lm(formula = post_j ~ UPCO, data = d_samp))
    deltas = c(deltas, AIC_b - AIC_u) 
  }
  powers = c(powers, mean(deltas < 0))
}
#lower is better, so if bayes is better, should be negative
plot(samps, powers, xlab = 'Sample Size', ylab = 'Power to Detect Delta AIC')


modnames = c("Bayes", "Competence","UPCO")

library(data.table)
all_mods = data.table(all_mods)
binarized = all_mods[,(modnames) := lapply(.SD,function(x){ifelse(is.na(x), 0, 1)}), .SDcols = modnames][,..modnames]

namesvec <- character(0)
for (i in 1:nrow(binarized)) {
  model_name <- ""
  
  if (binarized$Bayes[i] == 1) {
    model_name <- paste0(model_name, "B")
  }
  if (binarized$UPCO[i] == 1) {
    model_name <- paste0(model_name, " + U")
  }
  if (binarized$Competence[i] == 1) {
    model_name <- paste0(model_name, " + C")
  }
  model_name <- gsub("^\\s*\\+\\s*", "", model_name)
  
  # Add the constructed model name to namesvec
  namesvec <- c(namesvec, model_name)
}
namesvec[length(namesvec)] = 'Intercept'
# Print the resulting model names
print(namesvec)

modelcomp = data.frame(preds = AICs, 
                       mods = namesvec)
modelcomp$mods <- factor(modelcomp$mods, levels = modelcomp$mods)
modelcomp = modelcomp[1:nrow(modelcomp)-1,]

modelcomp = data.table(modelcomp)
modelcomp[mods == 'B', mods := 'Bayes (B)']
modelcomp[mods == 'C', mods := 'Competence (C)']
modelcomp[mods == 'U', mods := 'UPCO (U)']

aicplot = ggplot(data = modelcomp,
                 aes(x = reorder(mods, -preds),y = preds, fill = preds)) +
  geom_col() + 
  coord_flip() + scale_y_reverse(expand = c(0,0)) + 
  labs(y = 'Akaike Information Criterion', x = '') + 
  theme_classic(base_size = 13) + 
  theme(axis.text = element_text(color = 'black')) + 
  theme(legend.position = 'none') + 
  scale_fill_gradient(high = 'lightblue', low = '#08306B')
aicplot


cors = d[,lapply(.SD, function(X) mean(cor(X, post_j))), by = Subject, .SDcols = modnames]
devs = d[,lapply(.SD, function(X) mean(abs(X - post_j))), by = Subject, .SDcols = modnames]
mdevs = melt(devs,id.vars = 'Subject')
mcors = melt(cors,id.vars = 'Subject')

###which models best fit at Subject level?
devps = c(table((mdevs[,which.min(value),Subject]$V1))/nrow(cors))
corps = table((mcors[,which.max(value),Subject]$V1))/nrow(cors)

percplotd = data.frame(
  value = c(devps,corps),
  name = c(rep('Deviation',length(corps)),rep('Correlation',length(corps))),
  model = rep(modnames,2)
)

percplotd$model = factor(percplotd$model, levels = modnames,ordered = T)

percplot = 
  ggplot(data = percplotd, aes(x = value*100, y = name, fill = model)) +
  geom_bar(position="stack", stat="identity") + 
  labs(y = '', x = '% of Participants Best Captured by Model',) + 
  theme_classic(base_size = 13) + 
  theme(axis.text = element_text(color = 'black')) + 
  scale_fill_brewer(name = 'Model',direction = -1,palette = 1) + 
  theme(legend.position = 'top', 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) 
percplot

modelcomps = cowplot::plot_grid(
  aicplot, percplot,
  labels= "AUTO",nrow = 2,rel_heights = c(1.25,1)
)
modelcomps

