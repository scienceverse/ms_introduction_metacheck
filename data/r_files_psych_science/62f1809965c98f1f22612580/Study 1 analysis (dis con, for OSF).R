# ####################################################################
# Disease Concealment relevance (study 1) cleaning and analysis
# December 30, 2021
# ####################################################################

# --------------------------------------------------------------------
# Set Environment and load packages
# --------------------------------------------------------------------

rm(list=ls())
options(stringsAsFactors = FALSE)

if("pacman" %in% installed.packages() == FALSE) {install.packages("pacman")}
pacman::p_load(psych,
               ggplot2,
               tidyverse,
               magrittr,
               ggcorrplot,
               Hmisc,
               DT,
               foreign,
               plyr,
               cocor,
               viridis)

# Set your own working directory here
setwd("")

load("./Study 1 cleaned and recoded data (dis con).rda")

# Filter out participants such that the final dataset includes only people who:
# 1. Were sick between  March 2020 - December 2021
# 2. Had social, in-person interaction at work or school during this time
# 3. Completed the study in under 60 seconds or more than 45 minutes
test <- filter(cleaned, problem == 0)

# --------------------------------------------------------------------
# Main Manuscript Analyses
# --------------------------------------------------------------------

### 1. Overall concealment
# Add column for if they concealed at all, across the three concealment types
test %<>% mutate(commission_yn = ifelse(commission >= 2, 1, 0),
                 omission_yn = ifelse(omission >= 2, 1, 0),
                 responsiblue_yn = ifelse(responsiblue >= 2, 1, 0),
                 conceal_any = ifelse(commission_yn == 1 | omission_yn == 1 | responsiblue_yn == 1, 1, 0))

# Percentage who concealed at least once across groups. NA responses represent groups other than students and medical employees (e.g., professors)
test %>%
  group_by(role_clean) %>%
  dplyr::summarize(commission = sum(commission_yn, na.rm = TRUE),
                   omission = sum(omission_yn, na.rm = TRUE),
                   responsiblue = sum(responsiblue_yn, na.rm = TRUE),
                   conceal = sum(conceal_any, na.rm = TRUE),
                   total = n(),
                   com_per = round(commission / total * 100, 2),
                   om_per = round(omission / total * 100, 2),
                   responsiblue_per = round(responsiblue / total * 100, 2),
                   conceal_any_per = round(conceal / total * 100, 2))

# --------------------------------------------------------------------
# Supplemental Materials Analyses
# --------------------------------------------------------------------

### 1. Concealment frequency

# Add column for average concealment
test$conceal_avg <- rowMeans(select(test, commission, omission, responsiblue))

# Concealment frequency among students
avgconcealfreq_student <- test %>% 
  filter(role_clean == "student") %>% 
  summarize(stud_freq = mean(conceal_avg, na.rm = TRUE)) %>% 
  pull(stud_freq)

# Concealment frequency among medical employees
avgconcealfreq_michmed <- test %>% 
  filter(role_clean == "michmed") %>% 
  summarize(med_freq = mean(conceal_avg, na.rm = TRUE)) %>% 
  pull(med_freq)

# Plot frequencies
test %>%
  filter(role_clean == "student" | role_clean == "michmed") %>%
  ggplot(aes(x = conceal_avg, color = role_clean, fill = role_clean)) +
  geom_density(alpha = .3, size = .75, position = "identity") +
  geom_vline(xintercept = avgconcealfreq_michmed,
             color = "#88CCEE",
             linetype = "dashed") +
  geom_vline(xintercept = avgconcealfreq_student,
             color = "#332288",
             linetype = "dashed") +
  scale_color_manual(values=c("#88CCEE", "#332288"), labels = c("Healthcare Employees", "Students")) +
  scale_fill_manual(values = c("#88CCEE", "#332288"), labels = c("Healthcare Employees", "Students")) +
  labs(title = "Average across all concealment types",
       x = "Concealment Frequency") +
  theme_bw() +
  theme(legend.position = "top",
        axis.title.x = element_text(family = "serif", color = "Black", size = 12),
        axis.title.y = element_text(family = "serif", color = "Black", size = 12),
        axis.text.x = element_text(family = "serif", color = "Black", size = 12),
        axis.text.y = element_text(family = "serif", color = "Black", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "serif", color = "Black", size = 12),
        plot.title = element_text(family = "serif", color = "Black" , size = 12, hjust = .5))

### 2. Number of concealment strategies

# Add column for total concealment strategies
test$conceal_diftypes <- rowSums(select(test, commission_yn, omission_yn, responsiblue_yn))

# How many types of concealment?

# Overall
test %>% summarize(conceal_type_mean = mean(conceal_diftypes, na.rm = TRUE),
                   conceal_type_sd = sd(conceal_diftypes, na.rm = TRUE))

# Students
test %>% filter(role_clean == "student") %>%
         summarize(conceal_type_mean = mean(conceal_diftypes, na.rm = TRUE),
                   conceal_type_sd = sd(conceal_diftypes, na.rm = TRUE))

# Medical employees
test %>% filter(role == "michmed") %>%
  summarize(conceal_type_mean = mean(conceal_diftypes, na.rm = TRUE),
            conceal_type_sd = sd(conceal_diftypes, na.rm = TRUE))

### 3. Demographics
table(test$gender, exclude = NULL)
summary(test$Age)
table(test$Ethnicity, exclude = NULL)
