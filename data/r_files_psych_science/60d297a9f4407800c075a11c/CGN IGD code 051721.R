library(plyr); library(lmerTest); library(psych); library(lm.beta); library(ggplot2); library(dplyr); library(car); library (readxl); library(MatchIt)

#### READING IN; RECODING ####

# read in data
df <- read.csv("C:/Users/talia/Box Sync/CHH/Sexual orientation gender identity/cgn so data 042320.csv")

df <- df[which(df$group !="REL" | is.na(df$group)),]

# change variable types

headArray <- function(first,last, df = df) match(first, names(df)):match(last, names(df))

df[headArray("ID", "ihh_race", df)] <- data.frame(lapply(df[headArray("ID", "ihh_race", df)], function(x) as.factor(x)))
df[headArray("age", "cgnq24b_4_score", df)] <- data.frame(lapply(df[headArray("age", "cgnq24b_4_score", df)], function(x) as.numeric(as.character(x))))

# scale scores for each question (separately by sex)

df[(ncol(df) + 1):(ncol(df) + 31) ] <- df[headArray("cgnq1_score", "cgnq24b_4_score", df)]
df <- df %>%
  group_by(sex) %>%
  mutate_at(names(df[(ncol(df) - 30):ncol(df)]), scale)

names(df) <- c(names(df[1:(ncol(df) - 31)]), paste("cgnq", 1:23, "_scaled", sep = ""),
               paste("cgnq24a", 1:4, "_scaled", sep = ""), paste("cgnq24b", 1:4, "_scaled", sep = ""))

describe(df[df$sex == "m", headArray("cgnq1_scaled", "cgnq3_scaled", df)])
describe(df[df$sex == "f", headArray("cgnq1_scaled", "cgnq3_scaled", df)])

# Calculate  composites

df <- df[-c(match("cgnq24a2_scaled", names(df)), match("cgnq24b2_scaled", names(df)))]
df$cgn_comp <- rowMeans(df[headArray("cgnq1_scaled", "cgnq24b4_scaled", df)], na.rm = TRUE)
df$so <- rowMeans(df[c("Kinsey_Attr", "Kinsey_Fantasy", "ksoq5")], na.rm =TRUE)

#turns NaNs in NAs

is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
df[is.nan(df)] <- NA

# Race/ethnicity for both
df$race_combined<- 5 #other
df$race_combined[df$ihh_race == 5 & df$ihh_ethnicity == "n"] <- 1 #white non hispanic
df$race_combined[df$ihh_race == 2 & df$ihh_ethnicity == "n"] <- 2 #asian NH
df$race_combined[df$ihh_race == 3 & df$ihh_ethnicity == "n"] <- 3 #black NH
df$race_combined[df$ihh_ethnicity == "h"] <- 4

df$race_combined[df$msu_ethnicity == 6] <- 1 #white non hispanic
df$race_combined[df$msu_ethnicity == 2] <- 2
df$race_combined[df$msu_ethnicity == 3] <- 3
df$race_combined[df$msu_ethnicity == 4] <- 4

#Add in microphal and cryptorchidism data
crypt.microphal <- read.csv("C:/Users/talia/Box Sync/CHH/Analyses w patients/data_March_2018/crypt.microphal.csv")
df <- merge(df, crypt.microphal, by="subID", all = TRUE)

#Recode microphal & cryptorchidism data
df$microphallusbirth<-car::recode(df$microphallusbirth,'"No"=0;"Yes"=1; "" =NA')
df$cryptorchidism<-car::recode(df$cryptorchidism,'"No"=0;"Yes"=1; "" =NA')

df <- df[which(df$group != is.na(df$group) & df$sex != is.na(df$sex)),]

#Variable for patient groups
patient_group <- read_excel("C:/Users/talia/Box Sync/CHH/Sexual orientation gender identity/patient user groups.xlsx")
df <- merge(df, patient_group, by="subID", all = TRUE)
df$groupID2 <- df$groupID
df$groupID2[which(df$group == "CON")] <- 0
df$groupID2 <- as.factor(df$groupID2)
df$groupID <- as.factor(df$groupID)

#### CREATION OF CONTRAST TERMS FOR REGRESSIONS ####

df$contrast1 <- car:::recode(df$group, "'PAT' = 1; 'CON' = -2")
df$contrast2 <- car:::recode(df$groupID, "2 = -1; NA = 0")

#### RELIABILITY OF MEASURES; SAMPLE SIZE ####

# Sex-specific datasets
df.m <- df[which(df$sex=="m"),]
df.f <- df[which(df$sex=="f"),]

#Cronbach's alpha for CGN
cgn <- data.frame(df.m[,c(55:83)])
psych:::alpha(cgn)

cgn <- data.frame(df.f[,c(55:83)])
psych:::alpha(cgn)

#### ANALYSES: PRIMARY WAY ####

# Datasets with just controls and clinical patients
df.m2 <- df.m[which(df.m$groupID2!=2),]
df.f2 <- df.f[which(df.f$groupID2!=2),]

# Function to calculate standardized regression coefficients
# From https://www.researchgate.net/post/Multilevel_Modeling_standardized_beta-coefficients_Cohens_r_effect_size
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  lower <- sc - 2*se
  upper <- sc + 2*se
  return(data.frame(stdcoef=sc, stdse=se, lower = lower, upper = upper))
}

#### ANALYSES: MEN ####
summary(m1 <- lmer(cgn_comp ~ contrast1 + contrast2 + age + so + (1|sibID), data = df.m))
stdCoef.merMod(m1)
summary(m2 <- lmer(cgn_comp ~ groupID2 + age + so + (1|sibID), data = df.m2))
stdCoef.merMod(m2)
summary(lmer(so ~ groupID2 + age + (1|sibID), data = df.m2))

#### ANALYSES: MEN: PRENATAL HORMONE EXPOSURE MARKERS ####

levels(df.m$microphallusbirth) <- c(levels(df.m$microphallusbirth), "3")
levels(df.m$cryptorchidism) <- c(levels(df.m$cryptorchidism), "3")
df.m$microphallusbirth[which(df.m$group=="CON")] <- 3
df.m$cryptorchidism[which(df.m$group=="CON")] <- 3
df.m$microphallusbirth <- as.factor(df.m$microphallusbirth)
df.m$cryptorchidism <- as.factor(df.m$cryptorchidism)

table(df.m$cryptorchidism)
table(df.m$microphallusbirth)

#Comparing to controls
df.m$cryptorchidism <- relevel(df.m$cryptorchidism, ref = "3")
df.m$microphallusbirth <- relevel(df.m$microphallusbirth, ref = "3")

summary(m3<-lmer(cgn_comp~ cryptorchidism + age + so + (1|sibID), data=df.m))
stdCoef.merMod(m3)

summary(m3<-lmer(cgn_comp~ microphallusbirth + age + so + (1|sibID), data=df.m))
stdCoef.merMod(m3)

df.m$cryptorchidism <- relevel(df.m$cryptorchidism, ref = "0")
summary(m3<-lmer(cgn_comp~cryptorchidism + age + so + (1|sibID), data=df.m))
stdCoef.merMod(m3)

# Linear and quadratic terms for cryptorchidism at birth

df.m2 <- df.m %>%
  mutate(linear = case_when(cryptorchidism == 3 ~ -1, cryptorchidism == 0 ~ 0, cryptorchidism == 1 ~ 1),
         quadratic = case_when(cryptorchidism == 1 ~ -1, cryptorchidism == 0 ~ 2, cryptorchidism == 3 ~ -1))

summary(m3<-lmer(cgn_comp ~ linear+ quadratic + age + so + (1|sibID), data=df.m2))
stdCoef.merMod(m3)

# remove SO

summary(m3<-lmer(cgn_comp ~ linear+ quadratic + age + (1|sibID), data=df.m2))

# Linear and quadratic terms for microphallus at birth

df.m2 <- df.m %>%
  mutate(linear = case_when(microphallusbirth == 3 ~ -1, microphallusbirth == 0 ~ 0, microphallusbirth == 1 ~ 1),
         quadratic = case_when(microphallusbirth == 1 ~ -1, microphallusbirth == 0 ~ 2, microphallusbirth == 3 ~ -1))

summary(m4<-lmer(cgn_comp ~ linear + quadratic + age + so + (1|sibID), data=df.m2))
stdCoef.merMod(m4)

# remove SO

summary(m4<-lmer(cgn_comp ~ linear + quadratic + age + (1|sibID), data=df.m2))
stdCoef.merMod(m4)

#### ANALYSES: WOMEN ####
summary(m5 <- lmer(cgn_comp ~ contrast1 + contrast2 + age + so + (1|sibID), data = df.f))
stdCoef.merMod(m5)
summary(m6 <- lmer(cgn_comp ~ groupID2 + age + so + (1|sibID), data = df.f2))
stdCoef.merMod(m6)
summary(lmer(so ~ groupID2 + age + (1|sibID), data = df.f2))

df.f3 <- df.f %>%
  dplyr::filter(groupID2 == 0 | groupID2 == 2)

summary(m5 <- lmer(cgn_comp ~ group + age + so + (1|sibID), data = df.f))
stdCoef.merMod(m5)

#### FIGURES ####

group.colors1 <-   c("navyblue","grey45", "grey60")
group.colors2 <-   c("darkred","grey45", "grey60")

cgn.men <- ggplot(df.m,aes(x=groupID2,y=cgn_comp,color=groupID2,fill=groupID2))+
  geom_violin(alpha=0.4)+
  geom_jitter(aes(shape = group), size = 1.1, width = 0.11, color = "navyblue", stroke = 1.4) +
  scale_shape_manual(values = c(1, 2)) + 
  theme_classic()+
  xlab("")+
  ylab("CGN")+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18)) +
  scale_x_discrete(labels=c("Control", "IGD-clinical", "IGD-web")) +
  scale_fill_manual(values = group.colors1) + 
  scale_color_manual(values = group.colors1) +
  stat_summary(fun=mean, color = "black", geom = "point", size = 1.2, aes(shape = group), stroke = 2) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), color = "black", width = 0.2, size = 1.2) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5))
ggsave(filename = "cgn.men.png", plot = cgn.men, device = "png")

cgn.women <- ggplot(df.f,aes(x=groupID2,y=cgn_comp,color=groupID2,fill=groupID2))+
  geom_violin(alpha=0.4)+
  geom_jitter(aes(shape = group), size = 1.1, width = 0.11, color = "darkred", stroke = 1.4) +
  scale_shape_manual(values = c(1, 2)) + 
  theme_classic()+
  xlab("")+
  ylab("CGN")+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18)) +
  scale_x_discrete(labels=c("Control", "IGD-clinical", "IGD-web")) +
  scale_fill_manual(values = group.colors2) + 
  scale_color_manual(values = group.colors2) +
  stat_summary(fun=mean, color = "black", geom = "point", size = 1.2, aes(shape = group), stroke = 2) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), color = "black", width = 0.2, size = 1.2) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5))
ggsave(filename = "cgn.women.png", plot = cgn.women, device = "png")

df.mc <- df.m %>% filter(!is.na(cryptorchidism))

crypto <- ggplot(df.mc,aes(x=cryptorchidism,y=cgn_comp,color=cryptorchidism,fill=cryptorchidism))+
  geom_violin(alpha=0.4)+
  geom_jitter(aes(shape = group), size = 1.1, width = 0.11, color = "navyblue", stroke = 1.4) +
  scale_shape_manual(values = c(1, 2)) + 
  theme_classic()+
  xlab("")+
  ylab("CGN")+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18)) +
  scale_x_discrete(labels=c("Control", "CRYP -", "CRYP +")) +
  scale_fill_manual(values = group.colors1) + 
  scale_color_manual(values = group.colors1) +
  stat_summary(fun=mean, color = "black", geom = "point", size = 1.2, aes(shape = group), stroke = 2) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), color = "black", width = 0.2, size = 1.2) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5))
ggsave(filename = "crypto.png", plot = crypto, device = "png")

df.mm <- df.m %>% filter(!is.na(microphallusbirth))

micro <- ggplot(df.mm,aes(x=microphallusbirth,y=cgn_comp,color=microphallusbirth,fill=microphallusbirth))+
  geom_violin(alpha=0.4)+
  geom_jitter(aes(shape = group), size = 1.1, width = 0.11, color = "navyblue", stroke = 1.4) +
  scale_shape_manual(values = c(1, 2)) + 
  theme_classic()+
  xlab("")+
  ylab("CGN")+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18)) +
  scale_x_discrete(labels=c("Control", "MICRO -", "MICRO +")) +
  scale_fill_manual(values = group.colors1) + 
  scale_color_manual(values = group.colors1) +
  stat_summary(fun=mean, color = "black", geom = "point", size = 1.2, aes(shape = group), stroke = 2) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), color = "black", width = 0.2, size = 1.2) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5))
ggsave(filename = "micro.png", plot = micro, device = "png")

micro.cgn <- ggplot(df.mm,aes(x=microphallusbirth,y=cgn_comp,color=microphallusbirth,fill=microphallusbirth))+
  geom_violin(alpha=0.2)+
  geom_jitter(size = 1, width = 0.15)+
  theme_classic()+
  xlab("")+
  ylab("CGN")+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18)) +
  scale_x_discrete(labels=c("Control", "MICRO -", "MICRO +")) +
  scale_fill_manual(values = group.colors1) + 
  scale_color_manual(values = group.colors1) +
  stat_summary(fun=mean, color = "black", geom = "point", size = 3) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), color = "black", width = 0.2, size = 1.2) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5))

#### ANALYSES: ROBUSTNESS CHECK 1 ####

# MEN

# Do patient groups differ?
summary(lm(cgn_comp ~ groupID + age + so, data = df.m))

# They don't. Keep combined for analysis.
summary(m <-lmer(cgn_comp ~ group + age + so + (1|sibID), data = df.m))
stdCoef.merMod(m)

# WOMEN

# Do patient groups differ?
summary(lm(cgn_comp ~ groupID + age + so, data=df.f))

# They don't. Keep combined for analysis.
summary(m<-lmer(cgn_comp ~ group + age + (1|sibID), data = df.f))
stdCoef.merMod(m)

#### ANALYSES: ROBUSTNESS CHECK 2 (age restriction) ####

# Sample restricted to age 22+
df.m3 <- df.m[which(df.m$age>22),]
df.m4 <- df.m2[which(df.m2$age>22),]

df.m3 %>% group_by(group) %>% summarise(meanage = mean(age))

# MEN

summary(m <- lmer(cgn_comp ~ contrast1 + contrast2 + age + so + (1|sibID), data = df.m3))
stdCoef.merMod(m)
summary(lmer(cgn_comp ~ groupID2 + age + so + (1|sibID), data = df.m4))

df.m3 %>% group_by(group) %>% summarise(meanage = mean(age))

# WOMEN

df.f3 <- df.f[which(df.f$age>22),]
df.f4 <- df.f2[which(df.f2$age>22),]

df.f3 %>% group_by(group) %>% summarise(meanage = mean(age))

summary(m <- lmer(cgn_comp ~ contrast1 + contrast2 + age + so + (1|sibID), data = df.f3))
stdCoef.merMod(m)
summary(lmer(cgn_comp ~ groupID2 + age + so + (1|sibID), data = df.f4))

# Chi square analyses for individual questions ----------------------------

df.chi2 <- df %>%
  filter(group == "CON") %>%
  dplyr::select(sex, cgnq1_raw:cgnq24b_4_binary)

options(scipen=999)

var_names <- c()
chi2s <- c()
p_vals <- c()
Columns <- names(df.chi2)
for (i in 2:ncol(df.chi2)){
  chi2 <- chisq.test(table(df.chi2[, eval(quote(Columns[i]))], df.chi2$sex))
  var_names <- c(var_names,(eval(quote(Columns[i]))))
  chi2s <- c(chi2s, unname(chi2$statistic))
  p_vals <- c(p_vals, unname(chi2[3]))
}

chi2.results <- data.frame("variable" = var_names, "chi2_stat" = chi2s, "p-value" = unlist(p_vals))

# Composite and comparisons for qs identical for men and women
df2 <- df %>%
  filter(!is.na(cgn_comp)) %>%
  dplyr::select(sex, age, groupID, groupID2, cgnq1_score, cgnq3_score, cgnq5_score, cgnq6_score, cgnq20_score, cgnq24a_1_score:cgnq24b_4_score, sibID, so, contrast1, contrast2) %>%
  mutate(cgnmean2 = rowMeans(.[,5:17], na.rm = T)) %>%
  mutate(cgnmean2 = case_when(sex == "m" ~cgnmean2 * -1, sex == "f" ~ cgnmean2))

ggplot(df2,aes(x = groupID2,y=cgnmean2,color=sex,fill=sex))+
  geom_violin(alpha=0.2)+
  geom_jitter(size = 1, position = position_jitterdodge(dodge.width = 0.9, jitter.height = 0.2))+
  theme_classic()+
  xlab("")+
  ylab("CGNQ masculinity-femininity")+
  #theme(legend.position="none")+
  theme(axis.text=element_text(size=18),axis.title = element_text(size=18)) +
  scale_x_discrete(labels=c("Control", "IGD-clinical", "IGD-web")) +
  stat_summary(fun=mean, color = "black", geom = "point", size = 2, position = position_dodge(0.9)) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), color = "black", width = 0.3, size = 0.8, position = position_dodge(0.9)) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5, 2, 2.5))

# Same-sex comparisons of this composite
summary(m <- lmer(cgnmean2 ~ contrast1 + contrast2 + age + so + (1|sibID), data = df2[df2$sex == "m",]))
stdCoef.merMod(m)
summary(m <- lmer(cgnmean2 ~ contrast1 + contrast2 + age + so + (1|sibID), data = df2[df2$sex == "f",]))
stdCoef.merMod(m)
cohen.d(cgnmean2 ~ sex, data = df2[df2$groupID2==0,])

# Comparing control women to IGD men
df3 <- df2 %>%
  filter((sex == "f" & groupID2 == 0) | (sex == "m" & groupID2 != 0))
summary(m <- lmer(cgnmean2 ~ contrast1 + contrast2 + age + so + (1|sibID), data = df3))
stdCoef.merMod(m)

# Comparing control men to IGD women
df4 <- df2 %>%
  filter((sex == "m" & groupID2 == 0) | (sex == "f" & groupID2 != 0))
summary(m <- lmer(cgnmean2 ~ contrast1 + contrast2 + age + so + (1|sibID), data = df4))
stdCoef.merMod(m)

#### ANALYSES: Remove SO variable ####

summary(m1 <- lmer(cgn_comp ~ contrast1 + contrast2 + age + (1|sibID), data = df.m))
summary(m1 <- lmer(cgn_comp ~ contrast1 + contrast2 + age + (1|sibID), data = df.f))

summary(m3<-lmer(cgn_comp~cryptorchidism + age + (1|sibID), data=df.m))
summary(m3<-lmer(cgn_comp~microphallusbirth + age + so + (1|sibID), data=df.m))

#### PCAS for CGNQ ####

# Complete cases for men and women for CGN questions

df.m <- df.m %>%
  tidyr::drop_na(cgnq1_scaled:cgnq24b4_scaled)

df.f <- df.f %>%
  tidyr::drop_na(cgnq1_scaled:cgnq24b4_scaled)

#PCA for men

pca1 <- psych::principal(df.m[,c(116:144)], rotate="varimax", nfactors = 1, scores = TRUE)
scores <- pca1$scores
pca1$loadings
summary(lmer(scores ~ contrast1 + contrast2 + age + (1|sibID), data = df.m))

#PCA for women
pca1 <- psych::principal(df.f[,c(116:144)], rotate="varimax", nfactors = 1, scores = TRUE)
scores <- pca1$scores
pca1$loadings
summary(lmer(scores ~ contrast1 + contrast2 + age + (1|sibID), data = df.f))