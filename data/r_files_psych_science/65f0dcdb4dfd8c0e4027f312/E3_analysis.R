library(data.table)
library(ggplot2)
library(gmp)
library(qualtRics)
library(lme4)
library(gridExtra)
library(RColorBrewer)
library(psych)
library(cowplot)
library(ggpubr)
library(dplyr)
library(tidyr)
library(purrr)
library(MuMIn)
#Set your directory here
setwd(r"(YOUR DIRECTORY HERE)")

#Import data here
Raw <- read_survey("e3_data.csv")
dt <- data.table(copy(Raw))

table(dt$Gender)
mean(dt$Age,na.rm = T)

#remove unrealistic completion times
dt = dt[which(dt$`Duration (in seconds)`>(60*7)),]
295-243

dt = dt[C1 == 1, ]
243 - 235

#Assign Subject #s
dt[,Subject := seq(1:nrow(dt))]
dt[,Subject := as.factor(Subject)]

dt = dt[,-(StartDate:Q11)]

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

#Note that this very messy subsetting happens because we are using a 
#jspysch qualtrics hybrid, and have to extract the data from the long 
#js arrays that jspsych outputs. From here until line 118, we are merely 
#cleaning up data.
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

####Bayes MODEL PREDICTIONS
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

d[,update := (post_j - prior_j)]

#just some useful stats
d[ext != 1,mean(abs(Bayes-post_j))]
d[,mean(prior_c), lab][order(V1)]

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
plot(ervec)

d[, UPCO := upco(opt_upco,t,f,prior_j)]
d[, u_err := abs(UPCO - post_j)]

####Competence MODEL PREDICTIONS
competence = function(a,b){
  n = a + b
  cons = factorialZ(n+1) / (factorialZ(a)*factorialZ(b))
  integrand = function(x){(x^b)*((1-x)^a)}
  integ = integrate(integrand, lower = 0, upper = .5)
  res = as.numeric(cons)*integ$value
  res
}

op_multiplier = seq(1,100,1)
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

#Computing some additional columns for later analyses
d[, c_err := abs(Competence - post_j)]
d[lab == 'GUCKLEY', lab := 'HUXLEY']
d$lab = factor(d$lab,levels = d[,mean(prior_j),lab][order(V1)]$lab)
d$Subject = as.factor(d$Subject)
d[,Proportion := t / (t + f)]
d[,Prior := prior_j]


##KEY ANALYSES
#if you add + (1|lab) + (1|Subject) to 'mall' and convert it to an lmer model instead of lm, you can see the analyses
#for the mixed linear models rather than just the linear model.
mall = lm(formula = post_j ~ Bayes  + Competence + UPCO , data = d, na.action = "na.fail") 
all_mods = dredge(mall, rank = "AIC",  extra = alist(AICc, BIC)) #dredge computes every model combination
all_mods
AICs = all_mods$AIC

modnames = c("Bayes", "Competence","UPCO")
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

#we can compare Bayes AIC to prior and likelihood alone
AIC(lmer(data = d, post_j  ~ prior_j + (1|Subject) + (1|lab)))
AIC(lmer(data = d, post_j ~ (t / (t + f)) + (1|Subject) + (1|lab)))

aicplot = ggplot(data = modelcomp,
                 aes(x = reorder(mods, -preds),
                     y = (900-preds), #(3500 - preds)
                     fill = preds)) +
  geom_col() + 
  coord_flip(expand = F) + 
  scale_y_continuous(breaks = seq(0,600,100), 
                     labels = seq(900,300, -100),
                     limits = c(0,625)) + 
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

percplotd$model = factor(percplotd$model, levels = rev(modnames),ordered = T)

percplot = 
  ggplot(data = percplotd, aes(x = value*100, y = name, fill = model)) +
  geom_bar(position="stack", stat="identity") + 
  labs(y = '', x = '% of Participants Best Captured by Model',) + 
  theme_classic(base_size = 13) + 
  theme(axis.text = element_text(color = 'black')) + 
  scale_fill_brewer(name = 'Model',direction = 1,palette = 1,
                    breaks = c('Bayes', 'Competence','UPCO')) + 
  theme(legend.position = 'top', 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_x_continuous(expand = c(0.01,0.01))
percplot

modelcomps = cowplot::plot_grid(
  aicplot, percplot,
  labels= "AUTO",nrow = 2,rel_heights = c(1.25,1), align = 'v'
)
modelcomps
#ggsave('e3_aicplot.png',dpi = 600, scale = 1)


##The following code simply generates the plots from the manuscript and supplement.
truth_plotd = melt.data.table(d, measure.vars = c('prior_j'),id.vars = 'lab')
conf_plotd = melt.data.table(d, measure.vars = c('prior_c'),id.vars = 'lab')

ggplot(data = d, aes(x = lab, y = prior_j, color = prior_c)) + #facet_wrap(~lab) + 
  geom_jitter(alpha = .2,width = 0.2, height = 0.02, size = 3) + 
  stat_summary(fun.data = "mean_cl_boot", size = .75,stroke = 2, linewidth = 2, color = 'black', shape = 21, fill = 'white') + 
  labs(x = '', y = 'Truth Estimate') + 
  #geom_vline(xintercept=seq(1.5, length(unique(d$lab))-0.5, 1), 
  #           lwd=2, colour="white") + 
  scale_color_gradient('Confidence', breaks = seq(0,1,length.out = 5),
                       labels = c('0','.25','.5','.75','1')) +
  #scale_colour_viridis_c(option = 'B') +
  coord_flip() + 
  scale_y_continuous(breaks = seq(0,1,length.out = 5),
                     labels = c('0','.25','.5','.75','1')) +
  theme_minimal_grid() + 
  theme(legend.key.height= unit(1, 'cm'))

confplot = ggplot(data = conf_plotd, aes(x = lab, y = value, color = value)) + #facet_wrap(~lab) + 
  geom_jitter(alpha = .1,width = 0.2, height = 0.02, size = 3) + 
  stat_summary(fun.data = "mean_cl_boot", size = .75) + 
  theme_minimal_grid() + 
  labs(x = '', y = 'Confidence') + 
  scale_colour_viridis_c(option = 'C') +
  coord_flip() + 
  theme(legend.position = 'none') + 
  #theme(axis.text.y = element_blank()) +
  scale_y_continuous(breaks = seq(0,1,length.out = 5),
                     labels = c('0','.25','.5','.75','1')) 
confplot
#ggsave(filename = 'e3_confidence.png', dpi = 600, scale = 1.5)

d$lab = factor(d$lab,levels = d[,mean(prior_j),lab][order(V1)]$lab)

truthplot = ggplot(data = truth_plotd, aes(x = lab, y = value, color = value)) + #facet_wrap(~lab) + 
  #geom_violin(scale = 'width') +
  geom_jitter(alpha = .1,width = 0.2, height = 0.02, size = 3) + 
  #geom_point(alpha = .1, size = 1) + 
  stat_summary(fun.data = "mean_cl_boot", size = .75) + 
  theme_minimal_grid() + 
  labs(x = '', y = 'Truth Estimate') + 
  scale_colour_gradient(low = 'red', high = 'blue') +
  coord_flip() + 
  theme(legend.position = 'none') + 
  scale_y_continuous(breaks = seq(0,1,length.out = 5),
                     labels = c('0','.25','.5','.75','1')) 
truthplot

#plot_grid(truthplot,confplot,rel_widths = c(1,.6))
#ggsave('judgplot.svg',scale= 1.75)
#ggsave(filename = 'e3_truth.png', dpi = 600, scale = 1.5)

##Plot priors
#To plot subset
#chosen_items =  c('Photosynthesis','Franklin','Huxley')
#d_s$short = factor(d_s$short, levels = chosen_items,
#                   ordered = T)

d_s = d[,.(a = median(prior_alpha,na.rm = T), b = median(prior_beta,na.rm = T)), lab]
labord =  d_s[,a/(a+b),lab][order(V1)]$lab
d_s$lab = factor(d_s$lab,levels =labord)
d_s[,a := a + rnorm(18)]
d_s[,b := b + rnorm(18)]

plot <- ggplot() + xlim(0, 1)

#// function to plot layers of dashed line for each study
geom_layer <- function (a, b, lab) { 
  geom_function(aes(color=lab), 
                fun = dbeta, 
                args = c(a,b), 
                size = 1)}

#// set up geom_lines for all other studies and store in layers
layers <- mapply(geom_layer, d_s$a, d_s$b, d_s$lab, SIMPLIFY = F )

cc <- scales::seq_gradient_pal("red", "blue", "Lab")(seq(0,1,length.out=18))

#// combine plot and layers
plot + layers + 
  scale_colour_manual(values=cc,
                      labels = labord,
                      breaks = labord) +
  theme_classic(base_size = 13) + 
  labs(x = 'Probability', y = 'Density') + 
  guides(color=guide_legend(ncol=2)) + 
  coord_cartesian(expand = F) +
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'right')
#ggsave(filename = 'e3_priorfits.png', dpi = 600, scale = 1.5)

##Prior plot

d_plots = melt.data.table(d, measure.vars = c('Bayes','UPCO','Competence'))

ggplot(data = d_plots, aes(x = value, y = post_j)) + facet_grid(~variable) + 
  #geom_abline(slope = 1, color = 'red', size = 2) +
  geom_jitter(alpha = .05, size = 3, color = 'navy',
              width = .00, height = .00) + 
  theme_classic(base_size  = 14) + 
  labs(x = 'Model Prediction', y = 'Participant Judgment') + 
  scale_colour_viridis_c(option = 'C') +
  scale_y_continuous(breaks = seq(0,1,length.out = 5),
                     labels = c('0','.25','.5','.75','1')) +
  scale_x_continuous(breaks = seq(0,1,length.out = 5),
                     labels = c('0','.25','.5','.75','1')) +  
  coord_fixed()
#ggsave(filename = 'e3_modpoints.png', dpi = 600, scale = 1.5)

labs = c('CANBERRA','BEAGLE', 'FRANKLIN')
d_plots = melt.data.table(d[Subject == 1 & lab %in% labs, ], 
                          measure.vars = c('Bayes','UPCO','Competence','post_j'))
d_plots[,var2 := toupper(substr(variable, 1, 1))]

ggplot(d_plots, aes(x = value, y = rep(1,nrow(d_plots)), fill = variable)) + facet_wrap(~lab, strip.position = "bottom", nrow = 3, scales='free') +
  geom_bar(stat="identity", position = "dodge", width = .02) + 
  theme_minimal(base_size = 12) + 
  theme(legend.position = 'none') + 
  labs(y = '', x = 'Probability of Truth') + 
  coord_cartesian(xlim = c(-.1,1.1)) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 5), labels = c('0','.25','.5','.75','1'), expand = c(0,0)) + 
  geom_text(aes(y = 1.1, label = var2), fontface = 'bold', 
            size = 4, position = position_dodge(width=.7)) + 
  theme(panel.grid.major.y = element_blank()) + 
  theme(panel.grid.minor.y = element_blank()) + 
  theme(axis.text.y = element_blank()) + 
  theme(  strip.text.x = element_blank()) + 
  theme(axis.title.x = element_text())
#ggsave('e3_posts.svg')


bu_er = ggplot(data = d, aes(x = b_err, y = u_err, color = update)) + #facet_wrap(~lab) + 
  geom_jitter(alpha = .3, size = 3) + 
  theme_classic(base_size  = 14) + 
  geom_abline(slope = 1) +
  labs(x = 'Bayes |Error|', y = 'UPCO |Error|', color = 'Update') + 
  scale_colour_viridis_c(option = 'C') + 
  theme(legend.position = 'none') + 
  coord_fixed(expand = F)

bc_er = ggplot(data = d, aes(x = b_err, y = c_err, color = update)) + #facet_wrap(~lab) + 
  geom_jitter(alpha = .3, size = 3) + 
  theme_classic(base_size  = 14) + 
  geom_abline(slope = 1) +
  labs(x = 'Bayes |Error|', y = 'Competence |Error|', color = 'Update') + 
  scale_colour_viridis_c(option = 'C')+ 
  coord_fixed(expand = F)

plot_grid(bu_er,bc_er,labels = c('A', 'B'), 
          rel_widths = c(1, 1.33))
#ggsave(filename = 'e3_moderrs.png', dpi = 600, scale = 1.5)

