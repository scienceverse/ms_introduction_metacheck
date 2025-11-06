library(data.table)
library(ggplot2)
library(stringr)
library(qualtRics)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(psych)
library(cowplot)
library(ggpubr)
library(gmp)
library(sjPlot)
library(metR)

#if you want to grid search for UPCO; uncomment below, and on the end of 
#loop at line 132-134
#r2 = c()
#for(upco_param in seq(.1,.9,.01)){ 
#.53 is best r2
  
####MODEL FIRST, DATA SECOND
####SET PARAMETER SPACE
theta = seq(0,1,.01)
n = 29
a = seq(1,n,1)
b = seq(1,n,1)
m = expand.grid(a, b)

####UPCO MODEL PREDICTIONS
upco = function(p,a,b){
  p^(a-b) / ( p^(a-b) + (1-p)^(a-b))
}

####COMPETENCE MODEL PREDICTIONS
competence = function(a,b){
  n = a + b
  cons = factorialZ(n+1) / (factorialZ(a)*factorialZ(b))
  integrand = function(x){(x^b)*((1-x)^a)}
  integ = integrate(integrand, lower = 0, upper = .5)
  res = as.numeric(cons)*integ$value
  res
}

m$dif = apply(m,1,function(X) competence(X[1],X[2]))
m$UPCO = apply(m,1,function(X) upco(.53,X[1],X[2]))
m$b_pm = (m$Var1 + 1) / (m$Var1 + m$Var2 + 2) #this is using the conjugate prior
#for an exact computation (i.e., the beta-binomial model). if you would rather
#do grid approximation, uncomment the following chunk:

'
####BINOMIAL MODEL PREDICTIONS
binomial_l = function(data,p){
  s = data[1]
  n = data[1] + data[2]
  l = log(choose(n,s)) + log(p^s) + log((1-p)^(n-s))
  l = exp(l)
}

b_pm = c()
for(r in 1:nrow(m)){
  data = c(m[r,1],m[r,2])
  prior = dbeta(theta,1,1)
  ls_vec = sapply(theta, function(X)binomial_l(data,X)) 
  post = exp(log(prior) + log(ls_vec))
  const = sum(post)
  posts = post/const
  weighted = theta*posts
  meanpost = sum(weighted)
  b_pm = c(b_pm, meanpost)
}
'

####PARTICPANT DATA
#Set your directory here
setwd(r"(G:\My Drive\Research\Princeton2019-2024\Disagreement\Computational\OSF)")

#Import data here
Raw <- read_survey("e2_data.csv")
dt <- data.table(copy(Raw))

#remove unrealistic completion times
dt = dt[which(dt$`Duration (in seconds)`>180),]
dt = dt[,-(StartDate:Q11)]
#Assign Subject #s
dt[,Subject := seq(1:nrow(dt))]
dt[,Subject := as.factor(Subject)]
#nrow(dt[example_1 > 50,])
#those who gave good explanations after failing comprehension check:
good_exp  = dt[example_1 > 50,][c(1,2,4,5,7,14,16,21,25,38,44,58,60,72),Subject]

dt = dt[example_1 < 50 | Subject %in% good_exp,]

####Demographics
dt$Age <- as.numeric(dt$Age)
dt[,mean(Age,na.rm = T)]
table(dt$Gender)
mdt = melt(dt, id.vars = 'Subject',measure.vars = paste0(seq(1,225),"_t_1"))

#m is the data table that holds the model predictions

#description of sampling:there are 225 total cells to sample; each of them 
#from a square skip-1 from 0-30; but we need to skip a bunch of cells
sk_code = rep(seq(1,30,2),length.out = 225) + 
  rep(seq(0,15), each = 15, #this is the max Var1 that you go to /2 but round down
      length.out = 225)*58 #need to skip 29; but already went 29; so need to skip 29*2
m_sk = m[sk_code,]

dt$points = 0
for(sub in dt$Subject){
  ids = mdt[Subject == sub & !is.na(value), str_match(variable, pattern = '^[0-9]+')]
  mdt[Subject == sub & !is.na(value), Var1 := m_sk[as.numeric(ids), 'Var1']]
  mdt[Subject == sub & !is.na(value), Var2 := m_sk[as.numeric(ids), 'Var2']]
}

mdt = mdt[!is.na(mdt$value),]
means = mdt[,mean(value),.(Var1,Var2)]
means$V1 =means$V1/100 
m_sk$Participant = means$V1
mm = melt.data.table(as.data.table(m_sk), id.vars = c('Var1','Var2'))

mycolors <- colorRampPalette(brewer.pal(9, "Blues"))(10)

attributes(m_sk$Participant) <- NULL
names(m_sk) <- c('True','False',"Competence", "UPCO", "Bayes",'Participant')

mm[variable == 'dif', variable := 'Competence']
mm[variable == 'b_pm', variable := 'Bayes']

mm$variable = factor(mm$variable, levels = c('Bayes','UPCO', 'Competence', 'Participant'))
contourplot = ggplot(mm, aes(Var2,Var1,  z = value)) + facet_wrap(~variable) + 
  metR::geom_contour_fill() +
  scale_fill_distiller('Probability\nEstimate',direction = 1,
                       breaks = seq(0,1,.2), 
                       labels = c('0','.2','.4','.6','.8','1'),
                       limits = c(0,1)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_colourbar(barheight = 8)) + 
  scale_y_continuous(breaks = c(1,seq(10,50,10))) + 
  scale_x_continuous(breaks = c(1,seq(10,50,10))) + 
  labs(y = '# Panelists Voting True', 
       x = '# Panelists Voting False') 
contourplot 
#ggsave(plot = contourplot, 'E2contours.png',dpi = 600,width = 1.2, height = 1, scale = 5)

m_sk = data.table(m_sk)
m_sk[, `:=`('C - U' = Competence - UPCO, 'C - B' =  Competence - Bayes, 'B - U' = Bayes - UPCO)]
mmdif = melt.data.table(as.data.table(m_sk[,.(True,False,`C - U`,`C - B`,`B - U`)]), id.vars = c('True','False'))

e1points = data.frame( x = c(12, 20, 28, 33, 36, 41, 1, 3, 5, 7, 9, 11, 1, 11, 16, 22, 29, 33),
                       y = c(17, 28, 39, 46, 50, 57, 11, 13, 15, 17, 19, 21, 7, 23, 30, 38, 47, 52))

contourplotdif = ggplot(mmdif, aes(True,False,  z = value)) + facet_wrap(~variable) + 
  metR::geom_contour_fill() +
  scale_fill_gradient2('x',low = 'blue',high = 'red') +
  theme_classic(base_size = 13) + 
  guides(fill = guide_colourbar(barheight = 10)) + 
  scale_y_continuous(breaks = c(1,seq(10,50,10))) + 
  scale_x_continuous(breaks = c(1,seq(10,50,10))) + 
  labs(y = '# Panelists Voting True', 
       x = '# Panelists Voting False') 
contourplotdif

mb = lm(formula = Participant ~ Bayes, data = m_sk)
mu = lm(formula = Participant ~ Competence, data = m_sk)
mc = lm(formula = Participant ~ UPCO, data = m_sk)

muc = lm(formula = Participant ~ Competence + UPCO, data = m_sk)
mub = lm(formula = Participant ~ Bayes + UPCO, data = m_sk)
mcb = lm(formula = Participant ~ Bayes + Competence, data = m_sk)
mucb = lm(formula = Participant ~ Bayes + Competence  + UPCO, data = m_sk)

models = list(mb,mu, mc, muc, mub, mcb, mucb)
modnames = c('Bayes (B)','Competence (C)','UPCO (U)','C + U', 'B + U','B + C','B + C + U')
tablegen = tab_model(models,
          collapse.ci = T,show.aic = T,show.loglik = T,p.style = 'stars')#,
          #file = 'mod_unformatted.doc')
tablegen

#for plot
AICs = sapply(models, function(X) AIC(X))
modelcomp = data.frame(preds = AICs, 
           mods = modnames)
modelcomp$mods <- factor(modelcomp$mods, levels = modelcomp$mods)

mycolors <- colorRampPalette(brewer.pal(9, "Blues"))(10)

aicplot = ggplot(data = modelcomp,
       aes(x = reorder(modnames, -preds),y = preds, fill = preds)) +
  geom_col() + 
  coord_flip() + scale_y_reverse(expand = c(0,0)) + 
  labs(y = 'Akaike Information Criterion', x = '') + 
  theme_classic(base_size = 13) + 
  theme(axis.text = element_text(color = 'black')) + 
  theme(legend.position = 'none') + 
  scale_fill_gradient(high = 'lightblue', low = '#08306B')
#ggsave(plot = aicplot,'e2_aicplot.png', dpi = 600,width = 1.75, height = 1, scale = 2.5)

#Participant level analyses
n = 29
mods =c('Competence', 'UPCO', 'Bayes')
colnames(m) = c('Var1','Var2',mods)

m = data.table(m)
for(a in 1:n){
  for(b in 1:n){
    mod_preds = m[Var1 == a & Var2 == b, Competence:Bayes ]
    mdt[Var1 == a & Var2 == b, 'Competence' := mod_preds[[1]]]
    mdt[Var1 == a & Var2 == b, 'UPCO' := mod_preds[[2]]]
    mdt[Var1 == a & Var2 == b, 'Bayes' := mod_preds[[3]]]
  }
}
devs = mdt[,lapply(.SD, function(X) mean(abs(X - (value/100)))), by = Subject, .SDcols = mods]
mdevs = melt(devs,id.vars = 'Subject')
devp = ggplot(data = mdevs, aes(x = variable, y = value)) + 
  #geom_violin(size = .7,width = 1,trim = T , draw_quantiles = c(.25,.75)) +
  geom_jitter(alpha = .2, width = 0.1,height = 0) + 
  stat_summary(fun.data = "mean_cl_boot", color = 'red') + 
  labs(x = '', y = 'Mean Abs. Deviation Per Participant') + 
  coord_cartesian(expand = T) + 
  #scale_y_continuous(breaks = seq(-1,1,.2))  + 
  theme_classic(base_size = 15) + 
  coord_flip()

cors = mdt[,lapply(.SD, function(X) mean(cor(X, value))), by = Subject, .SDcols = mods]
mcors = melt(cors,id.vars = 'Subject')
corp = ggplot(data = mcors, aes(x = variable, y = value)) + 
  #geom_violin(size = .7,width = 1,trim = T , draw_quantiles = c(.25,.75)) +
  geom_jitter(alpha = .2, width = 0.1,height = 0) + 
  stat_summary(fun.data = "mean_cl_boot", color = 'red') + 
  labs(x = '', y = 'Mean Correlation Per Participant') + 
  coord_cartesian(expand = T) + 
  #scale_y_continuous(breaks = seq(-1,1,.2))  + 
  theme_classic(base_size = 15) + 
  coord_flip()

perparticipant = cowplot::plot_grid(
  devp, corp,
  labels= "AUTO",nrow = 2
  )
perparticipant 

#ggsave(plot = perparticipant,'perparticipant.png', dpi = 600,)

devs[,lapply(.SD, function(X) t.test(Bayes,X, paired = T)),.SDcols = mods[1:2]][
  ,ttname:=names(t.test(1:5))][]

cors[,lapply(.SD, function(X) t.test(Bayes,X, paired = T)),.SDcols = mods[1:2]][
  ,ttname:=names(t.test(1:5))][]


###which models best fit at participant level?
devps = table((mdevs[,which.min(value),Subject]$V1))/nrow(cors)
corps = table((mcors[,which.max(value),Subject]$V1))/nrow(cors)

percplotd = data.frame(
  value = c(devps,corps),
  name = c(rep('Deviation',3),rep('        Correlation',3)),
  model = rep(c('UPCO','Competence','Bayes'),2))

percplotd$model = factor(percplotd$model, levels = rev(c('Bayes','UPCO','Competence')),ordered = T)

percplot = 
  ggplot(data = percplotd, aes(x = value*100, y = name, fill = reorder(model,value))) +
  geom_bar(position="stack", stat="identity") + 
  labs(y = '', x = '% of Participants Best Captured by Model',) + 
  theme_classic(base_size = 13) + 
  theme(axis.text = element_text(color = 'black')) + 
  scale_fill_brewer(name = 'Model',direction = 1,palette = 1,  guide = guide_legend(reverse = TRUE)) + 
  theme(legend.position = 'top') + 
  scale_x_continuous(expand = c(0,1))
percplot

modelcomps = cowplot::plot_grid(
  aicplot, percplot,
  labels= c("A","B"),ncol = 1, rel_heights = c(1.4,1)
)
modelcomps
#ggsave(plot = modelcomps,'e2_modelcomps.png',dpi = 600, scale = 4, height = 1, width = 1.3)

#this is just a plot of the points sampled in this study for the supplement
ggplot(m_sk, aes(x = True,y = False)) +
  theme_bw(base_size = 13) + 
  geom_bin2d(bins = 29) + coord_cartesian(expand = F,xlim = c(1,28),ylim = c(1,28)) +
  theme(legend.position = 'none') + 
  labs(y = '# of Panelists Voting True',
       x = '# of Panelists Voting False')
#ggsave('e2_sampledpoints.png',dpi = 600, scale = 3, height = 1 , width = 1)




