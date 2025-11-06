
# 23.09.2019 Quickpys thresholds 

# LMM of SoundLoc data for PSE values
# Below can be seen the results of LMMs with nlme R packages. 
# We reported the reuslt from nlme package, which did not provide different reuslts from lmerTest package
# The only difference is nlme package can take into account of difference variances

#import threhsold data
setwd("")
plane.thre.out <- read.table("All_PSE_threoutliersmarked-500B.txt", sep=",", header=T)


names(plane.thre.out)[4]<-c('head')
plane.thre.out$arm <- factor(plane.thre.out$arm)
plane.thre.out$subj <- factor(plane.thre.out$subj)
plane.thre.out$head <- factor(plane.thre.out$head)
plane.thre.out$planes <- factor(plane.thre.out$planes)
plane.thre.out$group <- ifelse(plane.thre.out$group=='EB', c('CB'), c('SC'))
plane.thre.out$group <-factor(plane.thre.out$group)

#omit all +-3SD outliers 
plane.thre.out<- subset(plane.thre.out, outlier==0)

plane.thre.out[which(abs(plane.thre.out$thre) >12),]
# 1 point. One is 4291 ! With mean+-2.5SD it will not change

#divide 
horizontal.thre.out <- subset(plane.thre.out,planes=='hor')
vertical.thre.out <- subset(plane.thre.out,planes=='ver')

## investigate possible outliers
#now PSE and threshold outliers are same ! 
rownames(horizontal.thre.out) <-c(1:nrow(horizontal.thre.out)) #correct the row numbers
horizontal.thre.out[which(abs(horizontal.thre.out$thre) >10),] # we know from glmm there's one outlier
horizontal.thre.out <-horizontal.thre.out[-c(188),] 

rownames(vertical.thre.out) <-c(1:nrow(vertical.thre.out))
vertical.thre.out[which(abs(vertical.thre.out$thre) >12),] # we know from glmm there's one outlier
vertical.thre.out <-vertical.thre.out[-c(144),] 


######################################## 
#prepare in back horizontal correctly
######################################## 
# in the back condition, lepoulpe left becomes participants' right
# and lepoulpe right becomes participants' left
hor.pse.front <- subset(horizontal.thre.out, head==1)
hor.pse.back <-subset(horizontal.thre.out, head ==2)

# in the horizontal back condition left arm is right area // right arm is left area of the subject
hor.pse.back$arm <-as.character(hor.pse.back$arm)
hor.pse.back <- within(hor.pse.back, arm[arm=="left"] <-("Alien"))#just to assign into a temp
hor.pse.back <- within(hor.pse.back, arm[arm=='right'] <- 'left')
hor.pse.back <- within(hor.pse.back, arm[arm=='Alien'] <- 'right')
#from now on it would make more sense to call instead of "arm" column, "Area" column. 

#put back together
horizontal.thre.out<-rbind(hor.pse.front,hor.pse.back)


# #let's try to visualise it
# horizontal.thre.out$armhead <-paste(horizontal.thre.out$arm, sep = "",horizontal.thre.out$head)
# # #plot it
# p <- ggplot(data = horizontal.thre.out, aes(x=armhead, y=pse)) +
#   geom_boxplot(aes(fill=group))
# p
#################### 

rownames(vertical.thre.out) <-c(1:nrow(vertical.thre.out))
vertical.thre.out[which(abs(vertical.thre.out$pse) >12),] 



##### GLMMs ###########################
# on PSE value
######################################################

# package for GLMM
library(lmerTest)
library(nlme)

#####
# GLMM on the horizontal thresholds
#clean data with 3SD outliers
horizontal.out.nlme <-lme(pse ~ 1 + arm * group * head,random = ~1|subj, data = horizontal.thre.out)


# As suggested by Alain, before looking at the results we look at the whether the assumptions are met
# i.e. we check the residuals
plot(horizontal.out.nlme) 

#
qqnorm(residuals(horizontal.out.nlme)) #same
qqline(residuals(horizontal.out.nlme))



# Results
# to see the p-values you can look at the summary of the model
# or at the "anova" to see the global effect of a variable

summary(horizontal.out.nlme)
my.hor.anova2<- anova(horizontal.out.nlme)
#output
my.hor.anova2


library(emmeans)
# 1. ARM
emmip(horizontal.out.nlme,  ~ arm)
#get DoF with satterthwaite approximation 
emm.hor0<- emmeans(horizontal.out.nlme, specs = pairwise ~ arm, mode = "satterthwaite",adjust = "none")
pair.hor.table0 <-as.data.frame(emm.hor0$contrasts)
DoF_Satt <-pair.hor.table0$df

emm.hor.t1 <- emmeans(horizontal.out.nlme, specs = pairwise ~ arm,  adjust = "none")

pair.hor.table <-as.data.frame(emm.hor.t1$contrasts)
pair.hor.table

#extract p values to adjust for the contrasts of interests
pair.hor.table$p.value <-round(pair.hor.table$p.value,digits = 4)
pvalues <-pair.hor.table$p.value

p.bonfer<- p.adjust(pvalues,method="bonferroni")
p.FDR<- p.adjust(pvalues,method="fdr")
p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')

pair.hor.table <- cbind(pair.hor.table,p.bonfer,p.FDR,DoF_Satt) 

#save
#write.table(pair.hor.table, "PSE_Hor_interaction_ARM.txt", sep=",", row.names=F, col.names=T)

# 2. ARM x Group
#compute Satterwaite - consider omitting the same outliers & re-run
emm.hor0<- emmeans(horizontal.out.nlme, specs = pairwise ~ arm * group, mode = "satterthwaite",adjust = "none")
pair.hor.table0 <-as.data.frame(emm.hor0$contrasts)
DoF_Satt <-pair.hor.table0$df

emm.hor <- emmeans(horizontal.out.nlme, specs = pairwise ~  arm * group,  adjust = "none")
emmip(horizontal.out.nlme, group ~ arm)
emmip(horizontal.out.nlme, group ~ arm | head)


pair.hor.table <-as.data.frame(emm.hor$contrasts)
pair.hor.table$p.value <- round(pair.hor.table$p.value,digits = 4)
pair.hor.table

#choose contrasts of interest
COI <- c(3, 8, 12) #compare groups

miss_val<-length(1:nrow(pair.hor.table)) - length(COI)
pair.hor.table$con_of_Int <- ifelse(seq_len(nrow(pair.hor.table)) %in% COI, 1, 0)

#extract p values to adjust for the contrasts of interests
pvalues_interest <- pair.hor.table$p.value[COI]
p.bonfer<-c(p.adjust(pvalues_interest,method="bonferroni"),rep.int(99,miss_val))
p.FDR<- c(p.adjust(pvalues_interest,method="fdr"),rep.int(99,miss_val))

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')
temp <-cbind(p.bonfer,p.FDR)


#order back to 
arr <-1:nrow(temp)
arr2<- arr[(!(arr %in% COI))]
temp$idx <- c(COI,arr2)

temp<-temp[order(temp$idx),]
temp$idx <-NULL
temp

pair.hor.table <- cbind(pair.hor.table,temp,DoF_Satt)

#save
#write.table(pair.hor.table, "PSE_Hor_interaction_ARMxGroup.txt", sep=",", row.names=F, col.names=T)


# quick visualisations
# arm x head interaction
emmip(horizontal.out.nlme, arm ~ head)
emmip(horizontal.out.nlme, head ~ arm)


#############################################
# GLMM on the vertical PSE
#############################################
vertical.out.nlme <- lme(pse ~ 1 + arm * group * head,random = ~1|subj, data = vertical.thre.out)

plot(vertical.out.nlme) # same


qqnorm(residuals(vertical.out.nlme))
qqline(residuals(vertical.out.nlme))


# Results
# to see the p-values you can look at the summary of the model
# or at the "anova" to see the global effect of a variable

summary(vertical.out.nlme)

#anova results of nlme package
my.ver.anova<- anova(vertical.out.nlme)
my.ver.anova


#####
# post-hoc comparisons
#1. arm
emmip(vertical.out.nlme,~ arm)
emmip(vertical.out.nlme, group ~ arm)
emmip(vertical.out.nlme, group ~ arm | head)

emm.ver0<- emmeans(vertical.out.nlme, specs = pairwise ~ arm, mode = "satterthwaite",adjust = "none")
pair.ver.table0 <-as.data.frame(emm.ver0$contrasts)
DoF_Satt <-pair.ver.table0$df

emm.ver.t1 <- emmeans(vertical.out.nlme, specs = pairwise ~ arm,  adjust = "none")
pair.ver.table <-as.data.frame(emm.ver.t1$contrasts)
pair.ver.table

#extract p values to adjust for the contrasts of interests
pair.ver.table$p.value <- round(pair.ver.table$p.value,digits = 4)

pvalues <- round(pair.ver.table$p.value,digits = 4)
p.bonfer<- p.adjust(pvalues,method="bonferroni")
p.FDR<- p.adjust(pvalues,method="fdr")

p.bonfer<-as.data.frame(p.bonfer)
p.FDR<-as.data.frame(p.FDR)
names(p.bonfer) <-c('p.bonferroni')

pair.ver.table <- cbind(pair.ver.table,p.bonfer,p.FDR,DoF_Satt)

#save
#write.table(pair.ver.table, "PSE_Ver_interaction_ARM.txt", sep=",", row.names=F, col.names=T)



#############################################
#inferences 
#############################################
library("piecewiseSEM")
#g.o.f.hor <- rsquared(my.horizontal.nlme, method = NULL)
g.o.f.hor <- rsquared(horizontal.out.nlme, method = NULL)
g.o.f.hor


g.o.f.ver <- rsquared(vertical.out.nlme, method = NULL)
g.o.f.ver

#####

