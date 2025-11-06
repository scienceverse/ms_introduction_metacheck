
CRT_Data <- read.csv("Path/CRT_Data.csv") #Read data

#Linear regression models for testing T's influence on the CRT. 

CRT_A <- lm(CorrectAnswers ~ Treatment, data=CRT_Data) # model A1
CRT_B <- lm(CorrectAnswers ~ Treatment +  Math, data=CRT_Data) # model A2: control for math
CRT_C <- lm(CorrectAnswers ~ Treatment +  Math + Age + Negative.affect2 + Positive.affect2 + Treatment_Expectancy + DR_right, data=CRT_Data) #model A3: control for math, affect, belief about treatment and right hand 2d:4d

#Logistic regression models for testing T's influence on the CRT at the question level 
CRT_Q1_Correct <- glm(CRT1_5 ~ Treatment,  family = binomial(link = "logit"), data=CRT_Data)  
CRT_Q2_Correct <- glm(CRT2_5 ~ Treatment,  family = binomial(link = "logit"), data=CRT_Data)
CRT_Q3_Correct <- glm(CRT3_47 ~ Treatment,  family = binomial(link = "logit"), data=CRT_Data)
CRT_Q1_Intuitive <- glm(CRT1_10 ~ Treatment,  family = binomial(link = "logit"), data=CRT_Data)  
CRT_Q2_Intuitive <- glm(CRT2_100 ~ Treatment,  family = binomial(link = "logit"), data=CRT_Data)
CRT_Q3_Intuitive <- glm(CRT3_24 ~ Treatment,  family = binomial(link = "logit"), data=CRT_Data)


library(lme4) #mixed models package

CRT_Data_RT <- read.csv("Path/CRT_By_Question.csv") #Read data
CRT_RT_reg <- lmer(log(Time) ~ Treatment*Incorrect + (1|Subject) + (1|Question), data=CRT_Data_RT) #mixed model with subject and question random effects

# extract coefficients using normal approximation
coefs <- data.frame(coef(summary(CRT_RT_reg)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs