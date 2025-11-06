
#Analyses including Gender*Time interaction

Scientist1 <- Scientist1%>%
  mutate(block1 = ifelse(TaskPersistence > 2, 1,0))

Scientist1 <- Scientist1%>%
  mutate(block2 = ifelse(TaskPersistence > 4, 1,0))

Scientist1 <- Scientist1%>%
  mutate(block3 = ifelse(TaskPersistence > 6, 1,0))

Scientist1 <- Scientist1%>%
  mutate(block4 = ifelse(TaskPersistence > 8, 1,0))

Scientist1 <- Scientist1%>%
  mutate(block5 = ifelse(TaskPersistence > 9, 1,0))

Scientist1long<-Scientist1 %>% 
  gather(blocknum, stillgoing, block1:block5)

surv_object1<-Surv(time = Scientist1long$TaskPersistence, event = Scientist1long$censor)
surv_object1

modelblock<-coxph(surv_object1 ~ Condition*Gender + Gender:blocknum, data = Scientist1long)
summary(modelblock)
Anova(modelblock)


