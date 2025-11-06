# Jonathan MIRAULT - 2018

# Clean the environment and the console
rm(list = ls())
cat("\014")

# Librairies
library('lme4')
library('readxl')

#Importer les fichiers
total_suj <- 58
for (i in 1:total_suj) {
  text_command <- paste("filepath<-'~/Documents/.../subject-",i,".csv'") #define the pathway
  text_command <- gsub(" ", "", text_command, fixed = TRUE)
  eval(parse(text=text_command))
  fichier <- file(filepath, "r")
  Tab_Line <- read.csv(fichier, header=TRUE)
  Tab_Line <- as.data.frame(Tab_Line)

  text1<-paste("Tab",i,"=NULL")
  text2<-paste("Tab",i,"$Sujet <- Tab_Line$subject_nr")
  text3<-paste("Tab",i,"$Item <- Tab_Line$Item")
  text4<-paste("Tab",i,"$Cond <- Tab_Line$Type")
  text5<-paste("Tab",i,"$RT <- Tab_Line$response_time_Joystick")
  text6<-paste("Tab",i,"$Rep <- Tab_Line$Response")
  text7<-paste("Tab",i,"<- as.data.frame(Tab",i,")")
  text8<-paste("Tab",i,"$Cond[Tab",i,"$Item==160]='NORM'")
  
  text1 <- gsub(" ", "", text1, fixed = TRUE)
  text2 <- gsub(" ", "", text2, fixed = TRUE)
  text3 <- gsub(" ", "", text3, fixed = TRUE)
  text4 <- gsub(" ", "", text4, fixed = TRUE)
  text5 <- gsub(" ", "", text5, fixed = TRUE)
  text6 <- gsub(" ", "", text6, fixed = TRUE)
  text7 <- gsub(" ", "", text7, fixed = TRUE)
  text8 <- gsub(" ", "", text8, fixed = TRUE)
  
  eval(parse(text=text1))
  eval(parse(text=text2))
  eval(parse(text=text3))
  eval(parse(text=text4))
  eval(parse(text=text5))
  eval(parse(text=text6))
  eval(parse(text=text7))
  eval(parse(text=text8))
  
  close(fichier)
}

# Concatenate all the tables
Tab=NULL
for (i in 1:total_suj){
  t1 <- paste("Tab=rbind(Tab, Tab",toString(i),")")
  t2 <- gsub(" ", "", t1)
  eval(parse(text=t2))
}

# Correction of item number for Group 2
for (j in 1:length(Tab$Sujet)){
  if ((Tab$Sujet[j]%%2==0) & (Tab$Item[j]>40) & (Tab$Item[j]<81)){
    Tab$Item[j] <- Tab$Item[j]-40
    Tab$Cond[j] <- ('CTRL')
  }
  
  else if ((Tab$Sujet[j]%%2==0) & (Tab$Item[j]<41)){
    Tab$Item[j] <- Tab$Item[j]+40
    Tab$Cond[j] <- ('TW')
  }
}

# Check the % for all subject
MCorr <- NULL
for (t in 1:total_suj){
  MCorr[t] <- mean(Tab$Rep[Tab$Sujet==t & Tab$Cond!='NORM'])
}

# Ajouter les 0 et les 1 pour les items avec et sans verbes
Tab_Item_Class <- read_excel("Documents/.../.xlsx") #excel file with frequency anf length values

Median_Frq <- median(as.numeric(Tab_Item_Class$Frq))
Median_Length <- median(as.numeric(Tab_Item_Class$Length))

Tab$Class <- 0
Tab$Frq <- 0
Tab$Length <- 0
Tab$Frq2 <- 0
Tab$Length2 <- 0

for (i in 1:length(Tab$Sujet)){
  val_item <- Tab[i,2]
  if (val_item<81){
    Tab[i,6] <- Tab_Item_Class$Class[Tab_Item_Class$Item == val_item]
    Tab[i,7] <- Tab_Item_Class$Frq[Tab_Item_Class$Item == val_item]
    Tab[i,8] <- Tab_Item_Class$Length[Tab_Item_Class$Item == val_item]
    
    if (as.numeric(Tab[i,7]) < Median_Frq){
      Tab[i,9]<-0
    } else {
      Tab[i,9]<-1
    }
    
    if (as.numeric(Tab[i,8]) < Median_Length){
      Tab[i,10]<-0
    } else {
      Tab[i,10]<-1
    }
    
  }
}

### % Correct
#Means and SDs
CORR_mean_TW <- mean(Tab$Rep[Tab$Cond=='TW'])
CORR_mean_CTRL <- mean(Tab$Rep[Tab$Cond=='CTRL'])
CORR_mean_NORM <- mean(Tab$Rep[Tab$Cond=='NORM'])
CORR_sd_TW <- sd(Tab$Rep[Tab$Cond=='TW'])
CORR_sd_CTRL <- sd(Tab$Rep[Tab$Cond=='CTRL'])
CORR_sd_NORM <- sd(Tab$Rep[Tab$Cond=='NORM'])

#GLME
Tab_GLME <- Tab[Tab$Cond!='NORM',]
for (k in 1:length(Tab_GLME$Sujet)){
  if (Tab_GLME$Cond[k]=='TW'){
    Tab_GLME$Cond2[k] <- '0' 
  }
  
  if (Tab_GLME$Cond[k]=='CTRL') {
    Tab_GLME$Cond2[k] <- '1'
  }
}

# Facteur + references
Tab_GLME$Cond2 <- factor(Tab_GLME$Cond2,labels=c("0", "1"))
Tab_GLME$Cond2 <- relevel(Tab_GLME$Cond2, ref="1") #1:CTRL

Tab_GLME$Frq2 <- factor(Tab_GLME$Frq2,labels=c("0", "1"))
Tab_GLME$Frq2 <- relevel(Tab_GLME$Frq2, ref="0")

Tab_GLME$Length2 <- factor(Tab_GLME$Length2,labels=c("0", "1"))
Tab_GLME$Length2 <- relevel(Tab_GLME$Length2, ref="0")

Tab_GLME$Class <- factor(Tab_GLME$Class,labels=c("0", "1"))
Tab_GLME$Class <- relevel(Tab_GLME$Class, ref="0")

Tab_GLME$Frq <- log10(as.numeric(Tab_GLME$Frq))+3

model1 <- glmer((1-Tab_GLME$Rep) ~ Cond2 + (1|Item) + (1|Sujet),
                data=Tab_GLME, family='binomial')

print(summary(model1))
                
### RT
# Wrong
Tab_Right <- Tab[Tab$Rep==1,]
Prct_wrong <- 100 - (length(Tab_Right$Sujet)*100/length(Tab$Sujet))

# Outliers
mean_RT <- mean(Tab_Right$RT)
sdt_RT <- sd(Tab_Right$RT)
Tab_new <- Tab_Right[Tab_Right$RT < mean_RT + 2.5*sdt_RT & Tab_Right$RT > mean_RT - 2.5*sdt_RT ,]
Prct_out <- 100 - (length(Tab_new$Sujet)*100/length(Tab_Right$Sujet))

# Mean and SD
RT_mean_TW <- mean(Tab_new$RT[Tab_new$Cond=='TW'])
RT_mean_CTRL <- mean(Tab_new$RT[Tab_new$Cond=='CTRL'])
RT_mean_NORM <- mean(Tab_new$RT[Tab_new$Cond=='NORM'])
RT_sd_TW <- sd(Tab_new$RT[Tab_new$Cond=='TW'])
RT_sd_CTRL <- sd(Tab_new$RT[Tab_new$Cond=='CTRL'])
RT_sd_NORM <- sd(Tab_new$RT[Tab_new$Cond=='NORM'])

#LME
Tab_LME <- Tab_new[Tab_new$Cond!='NORM',]
for (k in 1:length(Tab_LME$Sujet)){
  if (Tab_LME$Cond[k]=='TW'){
    Tab_LME$Cond2[k] <- '0' 
  }
  
  if (Tab_LME$Cond[k]=='CTRL') {
    Tab_LME$Cond2[k] <- '1'
  }
}

# Facteur + references
Tab_LME$Cond2 <- factor(Tab_LME$Cond2,labels=c("0", "1"))
Tab_LME$Cond2 <- relevel(Tab_LME$Cond2, ref="1") #1:CTRL

Tab_LME$Class <- factor(Tab_LME$Class,labels=c("0", "1"))
Tab_LME$Class <- relevel(Tab_LME$Class, ref="0") #1:different

Tab_LME$Length2 <- factor(Tab_LME$Length2,labels=c("0", "1"))
Tab_LME$Length2 <- relevel(Tab_LME$Length2, ref="0") #1:different

Tab_LME$Frq2 <- factor(Tab_LME$Frq2,labels=c("0", "1"))
Tab_LME$Frq2 <- relevel(Tab_LME$Frq2, ref="0") #1:different

# Freqeuncy in Zipf
Tab_LME$Frq <- log10(as.numeric(Tab_LME$Frq))+3


model2 <- lmer((-1000/Tab_LME$RT) ~ Cond2*as.numeric(Class) + (1|Sujet) + (1|Item),
              data=Tab_LME)
print(summary(model2))



### RESULTS
cat('>> RESUME <<\n')
cat('\n')
cat('CORRECT:\n')
cat('TW:',100-(CORR_mean_TW*100),'% ; (',CORR_sd_TW/sqrt(length(Tab_GLME$Sujet))*1.96,')\n') #Mean + IC 95%
cat('CTRL:',100-(CORR_mean_CTRL*100),'% ; (',CORR_sd_CTRL/sqrt(length(Tab_GLME$Sujet))*1.96,')\n')
cat('NORM:',100-(CORR_mean_NORM*100),'% ; (',CORR_sd_NORM/sqrt(length(Tab_GLME$Sujet))*1.96,')\n\n') 
print(summary(model1))

cat('\n\n')
cat('RT:\n')
cat('TW:',RT_mean_TW,'ms ; (',RT_sd_TW/sqrt(length(Tab_LME$Sujet))*1.96,')\n')
cat('CTRL:',RT_mean_CTRL,'ms ; (',RT_sd_CTRL/sqrt(length(Tab_LME$Sujet))*1.96,')\n')
cat('NORM:',RT_mean_NORM,'ms ; (',RT_sd_NORM/sqrt(length(Tab_LME$Sujet))*1.96,')\n\n')
print(summary(model2))