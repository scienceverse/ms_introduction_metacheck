###Necessary Libraries: [Import with 'install.packages("package_name")' if library missing]

#data wrangling
library(qualtRics)
library(data.table)

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
Raw <- read_survey("e1_data.csv")
DT <- data.table(copy(Raw))

#Demographics
DT$Age <- as.numeric(DT$Age)
DT[,mean(Age,na.rm = T)]
DT$Sex <- as.numeric(DT$Sex)
DT[,mean(Sex-1,na.rm = T)]

#Assign Subject #s
DT[,Subject := seq(1:nrow(DT))]
DT[,Subject := as.factor(Subject)]

#remove unrealistic completion times
DT = DT[which(DT$`Duration (in seconds)`>120),]

DT = DT[`19_trial_1` == 7,]
DT$`19_trial_1` = NULL

fails = which(!is.na(DT$Q15)) #failed comprehension checks
DT[fails, Q15]
#1, 3, 4, 6, 8, 10, 11, 12, 13 of fails do not indicate comprehension of slider;
comp_exclude = fails[c(1, 3, 4, 6, 8, 10, 11, 12, 13)]

DT = DT[-comp_exclude,] #wide data table for mean analyses

#long data table for individual level analyses
mdt = melt.data.table(DT,id.vars = 
                        c('Subject'),
                measure.vars = list(
                  belief = c(paste0(seq(1,18),'_trial_1'))
                ))
#reversed labels; for both individual level and mean judgments 
revs = c(paste0(seq(2,18,2),'_trial_1'))
mdt[variable %in% revs, value := 100 - value] #mean
DT[, (revs) := lapply(.SD, function(x) 100 - as.numeric(x)), .SDcols = revs] #individual level (not reversing this was a previous error)

#These are different labeling schemes for Figure 1. 
#labs = c("12\n17", "20\n28", "28\n39", "33\n46", 
#        "36\n50", "41\n57", "1\n11", "3\n13", "5\n15", "7\n17", "9\n19", 
#       "11\n21", "1\n7", "11\n23", "16\n30", "22\n38", "29\n47", "33\n52")

#labs = c("42%\n17\U0394", "42%\n28\U0394", "42%\n39\U0394", "42%\n46\U0394", 
#         "42%\n50\U0394", "42%\n57\U0394", "8%\n10\U0394", "19%\n10\U0394", "25%\n10\U0394", "29%\n10\U0394", "32%\n11\U0394", 
#         "34%\n11\U0394", "13%\n6\U0394", "32%\n12\U0394", "35%\n14\U0394", "37%\n16\U0394", "38%\n18\U0394", "39%\n19\U0394")

#labs = c("42\n17", "42\n28", "42\n39", "42\n46", 
#         "42\n50", "42\n57", "8\n10", "19\n10", "25\n10", "29\n10", "32\n11", 
#         "34\n11", "13\n6", "32\n12", "35\n14", "37\n16", "38\n18", "39\n19")


labs = c("42\n12\n17\n29", "42\n20\n28\n48", "42\n28\n39\n67", "42\n33\n46\n79", 
         "42\n36\n50\n86", "42\n41\n57\n98", "8\n1\n11\n12", "19\n3\n13\n14", "25\n5\n15\n20", "29\n7\n17\n24", "32\n9\n19\n28", 
         "34\n11\n21\n32", "13\n1\n7\n8", "32\n11\n23\n34", "35\n16\n30\n46", "37\n22\n38\n60", "38\n29\n47\n76", "39\n33\n52\n85")

#apply labels to data table
lapply(seq(1,18), function(X)
  mdt[variable==paste0(X,'_trial_1'),'belief' := labs[X]])

#####Generate Model Predictions

#Bayesian model (computing the entire posterior; rather than just the MAP)
theta = seq(0,1,.001)
n = 100
a = seq(1,n,1)
b = seq(1,n,1)
m = expand.grid(a, b)

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
m$b_pm = b_pm

#UPCO
upco = function(p,a,b){
  p^(a-b) / ( p^(a-b) + (1-p)^(a-b))
}

#.51 is the optimized UPCO opinion mapping parameter
m$UPCO = apply(m,1,function(X) upco(.51,X[1],X[2]))

#Implementing competence
library(gmp)
competence = function(a,b){
  n = a + b
  cons = factorialZ(n+1) / (factorialZ(a)*factorialZ(b))
  integrand = function(x){(x^b)*((1-x)^a)}
  integ = integrate(integrand, lower = 0, upper = .5)
  res = as.numeric(cons)*integ$value
  res
}

m$dif = apply(m,1,function(X) competence(X[1],X[2]))

m = data.table(m)
m$d_ub = abs(m$UPCO - m$b_pm)
m$d_ud = abs(m$UPCO - m$dif)
m$d_bd = abs(m$b_pm - m$dif)

##sampling points --- code below was used to generate the maximally discriminating points
#sample distance
sampd = #seq(1,6) 
  seq(1,12,2)
#points that differentiate binomial from dif model
bds = m[order(m$d_bd,decreasing = T),][Var2 > Var1,]

dp1 = bds$b_pm[1]
s = .001
bdpoints = m[dp1 - s < b_pm & b_pm < dp1 + s, ][sampd,]

#points that differentiate binomial from upco
ubs = m[order(m$d_ub,decreasing = T),][Var2 > Var1,]

dp1 =ubs$UPCO[1]
ubpoints = m[dp1 - s < UPCO & UPCO < dp1 + s, ][sampd,]

#points that differentiate dif from upco
uds = m[order(m$d_ud,decreasing = T),][Var2 > Var1,]
dp1 =uds$dif[1]
udpoints = m[dp1 - s < dif & dif < dp1 + s, ][sampd,]

points = rbind(bdpoints,ubpoints,udpoints)

mpoints = melt(points,measure.vars = c('b_pm','dif','UPCO'), id.vars = c('Var1','Var2'))
mpoints$labs = paste0(mpoints$Var1,'\n',mpoints$Var2)  
mpoints$labs = factor( mpoints$labs, levels = mpoints$labs[1:(length(sampd)*3)])

mpoints[variable == 'b_pm', variable := 'Bayes']
mpoints[variable == 'dif', variable := 'Competence']
mpoints$variable = factor(mpoints$variable, 
                          levels = c('Bayes','UPCO','Competence'))


###This makes a one col plot of means; below generates a square
mpoints$totn = mpoints$Var1 + mpoints$Var2
nexs = (length(sampd)*3)

modgs = ggplot(data = mpoints, aes(x = labs, y = value)) + facet_wrap(~variable,nrow =4) + 
  geom_bar(stat = 'identity', fill = 'deepskyblue4') +    
  labs(x = NULL,
       y = "Probability Estimates") + 
  #title= 'Model Predictions &  Participant Judgments') + 
  theme_classic(base_size = 17) + 
  theme(panel.grid.major.y = element_line(color = 'gray', linetype = 'dashed')) + 
  #theme(panel.grid.minor.y = element_line(color = 'gray', linetype = 'dashed')) + 
  #theme(axis.text.x = element_text(size = 11)) + 
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.y = element_text(size = 12, color = 'black')) + 
  theme(axis.ticks.x = element_blank()) + 
  geom_vline(xintercept = nexs/3 + .5) + 
  geom_vline(xintercept = nexs*2/3 + .5) + 
  coord_cartesian(ylim = c(0,.5),expand = F) + 
  scale_y_continuous(breaks = seq(0,.4,.2),labels = c('0','20','40'))
modgs
###Now generate Participant plot

nexs = length(labs)
mdt$belief = factor(mdt$belief, levels = labs)
mdt$model = 'Participants'
parg = ggplot(mdt,aes(x = belief, y = value/100)) + facet_wrap(~model) +
  stat_summary(fun="mean",geom="bar", fill = 'deepskyblue4') + 
  stat_summary(fun.data=mean_cl_boot,position=position_dodge(0.95),geom="errorbar") + 
  labs(x = "Panel Opinion Distribution", y = " ") + 
  theme_classic(base_size = 17) + 
  theme(panel.grid.major.y = element_line(color = 'gray', linetype = 'dashed')) + 
  #theme(panel.grid.minor.y = element_line(color = 'gray', linetype = 'dashed')) + 
  theme(axis.text.y = element_text(size = 12, color = 'black')) + 
  theme(axis.text.x = element_text(size = 12, color = 'black')) + 
  geom_vline(xintercept = nexs/3 + .5) + 
  geom_vline(xintercept = nexs*2/3 + .5) + 
  coord_cartesian(ylim = c(0,.5),expand = F) + 
  scale_y_continuous(breaks = seq(0,.4,.2), labels = c('0','20','40'))

plot_grid(
  modgs, parg,
  #labels = "AUTO", 
  ncol = 1,
  rel_heights = c(1.7, 1)
)

sc1 = 4.5
##ggsave('E1_mean_comp_2.png',dpi = 600,scale = .8)


##Need to integrate mpoints and mdt

g1d1 = mdt[,mean(value)/100,belief]
g1d2 = mpoints[variable == 'UPCO',]
g1d2$value = g1d1$V1
g1d2$variable = 'Participant'
mpoints2 = rbind(mpoints,g1d2)

modgs = ggplot(data = mpoints2, aes(x = labs, y = value)) + facet_wrap(~variable,nrow =2) + 
  geom_bar(stat = 'identity', fill = 'deepskyblue4') +    
  labs(x = NULL,
       y = "Probability Estimates") + 
  #title= 'Model Predictions &  Participant Judgments') + 
  theme_classic(base_size = 17) + 
  theme(panel.grid.major.y = element_line(color = 'gray', linetype = 'dashed')) + 
  #theme(panel.grid.minor.y = element_line(color = 'gray', linetype = 'dashed')) + 
  theme(axis.text.x = element_text(size = 9, color = 'black')) + 
  #theme(axis.text.x = element_blank()) + 
  #theme(axis.ticks.x = element_blank()) + 
  geom_vline(xintercept = nexs/3 + .5) + 
  geom_vline(xintercept = nexs*2/3 + .5) + 
  coord_cartesian(ylim = c(0,.5),expand = F) + 
  scale_y_continuous(breaks = seq(0,.4,.2))
modgs

##ggsave('modgs.png',dpi = 600,width = 3, height = 1.5, scale = 2)


###Mean Correlations
ofq = mdt[,mean(value,na.rm = T)/100,belief]$V1

se <- function(x) sd(x)/sqrt(length(x))

odevs = mdt[,se(value)/100,belief]$V1

bfq = mpoints[variable == 'Bayes',value]
ufq = mpoints[variable == 'UPCO',value]
cfq = mpoints[variable == 'Competence',value]

c1 = cor.test(ofq,bfq)
c2 = cor.test(ofq,ufq)
c3 = cor.test(ofq,cfq)

#
c4 = cor.test(bfq,ufq)
c5 = cor.test(bfq,cfq)

paired.r(c1$estimate,c2$estimate,n = 18)
paired.r(c1$estimate,c3$estimate,n = 18)


mcors = melt(data.table(ufq, bfq, cfq))
mcors[variable == 'bfq', variable := 'Bayes']
mcors[variable == 'ufq', variable := 'UPCO']
mcors[variable == 'cfq', variable := 'Competence']

mcorp = 
  ggplot(mcors, aes(x= value, y = rep(ofq,3))) + facet_wrap(~variable,scales = 'free_x') + 
  geom_point() + 
  geom_linerange(aes(
    ymin = rep(ofq - (odevs),3),
    ymax = rep(ofq + (odevs),3))) +
  geom_smooth(method = 'lm', color = 'red') + 
  stat_cor(cor.coef.name = 'r',r.accuracy = 0.01, p.accuracy = 0.001,label.y = .55, size =3) +
  labs(x = "Model Prediction", y = "Mean Judgment") + 
  theme_classic(base_size = 13) + 
  scale_x_continuous(labels=function(x) ifelse(x == 0, '0', substr(as.character(round(x,2)),2,4)))

sc2 = 3.5
  #ggsave('meancors.png',dpi = 600,width = sc2*1.5, height = sc2)

bset = labs[1:6]
uset = labs[7:12]
cset = labs[13:18]

install.packages("PairedData")
library(PairedData)
Var.test(mdt[belief %in% bset, value], 
         mdt[belief %in% uset, value],paired = T)

Var.test(mdt[belief %in% bset, value], 
         mdt[belief %in% cset, value],paired = T)

##Participant Correlations
trials =  c(paste0(seq(1,18),'_trial_1'))
bcors = c()
ucors = c()
ccors = c()

for(n in 1:nrow(DT)){
  bcors = c(bcors, cor.test(
    as.numeric(DT[n,..trials])/100, bfq)$estimate)
  
  ucors = c(ucors, cor.test(
    as.numeric(DT[n,..trials])/100, ufq)$estimate)
  
  ccors = c(ccors, cor.test(
    as.numeric(DT[n,..trials])/100, cfq)$estimate)
}

cordat = melt(data.frame(bcors,ucors,ccors))


psubcors = ggplot(data = cordat, aes(x = reorder(variable,value), y = value)) + 
         #geom_violin(size = .7,width = 1,trim = T , draw_quantiles = c(.25,.75)) +
  geom_jitter(alpha = .2, width = 0.1,height = 0) + 
  stat_summary(fun.data = "mean_cl_boot", color = 'red') + 
  labs(x = '', y = 'Correlation Per Participant') + 
  coord_cartesian(expand = T) + #scale_y_continuous(breaks = seq(-1,1,.2))  + 
  scale_x_discrete(labels= c("UPCO","Competence","Bayes")) + 
  theme_classic(base_size = 13) + 
  geom_hline(yintercept = 0, linetype = 4, color = 'darkgray') + 
  coord_flip() +
  theme(axis.text.y = element_text(color = 'black')) +
  scale_y_continuous(breaks = seq(-.6,1,.4))
  
psubcors

sc3 = 3.5
#ggsave('psubcors.png',dpi = 600,width = sc3, height = sc3*1.2)


#joint plot
cowplot::plot_grid(
  mcorp, psubcors,nrow = 2,rel_heights = c(1.5,1),labels = 'AUTO', align = 'v' ,axis = "l"
)

#ggsave('e1_corrsplot.png', dpi = 600,height = 1, width = 1.15, scale = 4)

t.test(bcors,ucors, paired = T)
t.test(bcors,ccors, paired = T)

#but the correlations miss out on the differences in absolute magnitude
#so let's look at differences across the board
dpb = abs(ofq - bfq)
dpu = abs(ofq - ufq)
dpc = abs(ofq - cfq)

distdat = melt(data.frame(dpb,dpu,dpc))

meandist = ggplot(data = distdat, aes(x = variable, y = value)) + 
  #geom_violin(size = .7,width = 1,trim = T , draw_quantiles = c(.25,.75)) +
  geom_jitter(alpha = .2, width = 0.1,height = 0, size = 2) + 
  stat_summary(fun.data = "mean_cl_boot", color = 'red') + 
  labs(x = '', y = 'Mean Absolute Deviations') + 
  coord_cartesian(expand = T) + #scale_y_continuous(breaks = seq(-1,1,.2))  + 
  scale_x_discrete(labels= c("Bayes","UPCO","Competence")) + 
  theme_classic(base_size = 15) + coord_flip() + 
  theme(axis.text.y = element_text(color = 'black'))
meandist

sc3 = 3.5
#ggsave('meandist.png',dpi = 600,width = sc3, height = sc3*1.2)

sapply(list(dpb,dpu,dpc),mean)
t.test(dpb,dpu,paired = T)
t.test(dpb,dpc,paired = T)

bdifs = c()
udifs = c()
cdifs = c()

for(n in 1:nrow(DT)){
  bdifs = c(bdifs, 
    mean(abs(as.numeric(DT[n,..trials])/100 - bfq)))
  
  udifs = c(udifs, 
            mean(abs(as.numeric(DT[n,..trials])/100 - ufq)))
  
  cdifs = c(cdifs, 
            mean(abs(as.numeric(DT[n,..trials])/100 - cfq)))
}

difdat = melt(data.frame(bdifs,udifs,cdifs))


psubdist = ggplot(data = difdat, aes(x = variable, y = value)) + 
  #geom_violin(size = .7,width = 1,trim = T , draw_quantiles = c(.25,.75)) +
  geom_jitter(alpha = .2, width = 0.1,height = 0, size = 2) + 
  stat_summary(fun.data = "mean_cl_boot", color = 'red') + 
  labs(x = '', y = 'Absolute Deviation Per Participant') + 
  coord_cartesian(expand = T) + scale_y_continuous(breaks = seq(-1,1,.2))  + 
  scale_x_discrete(labels= c("Bayes","UPCO","Competence")) + 
  theme_classic(base_size = 15) + coord_flip() + 
  theme(axis.text.y = element_text(color = 'black'))
psubdist

sc4 = 3.5
#ggsave('persubdist.png',dpi = 600,width = sc4, height = sc3*1.2)

#joint plot
plot_grid(
  meandist, psubdist,nrow = 2,labels = 'AUTO'
)

#ggsave('e1_devsplot.png', dpi = 600,height = 1, width = 1.5, scale = 4)


t.test(bdifs,udifs, paired = T)
t.test(bdifs,cdifs, paired = T)

plot(points$Var1,points$Var2)

ggplot(points, aes(x = Var1,y = Var2)) +
  theme_bw(base_size = 13) + 
  geom_bin2d() + coord_cartesian(expand = F) + 
  theme(legend.position = 'none') + 
  labs(y = '# of Panelists Voting True',
       x = '# of Panelists Voting False')
##ggsave('e1_sampledpoints.png',dpi = 600, scale = 3, height = 1 , width = 1)