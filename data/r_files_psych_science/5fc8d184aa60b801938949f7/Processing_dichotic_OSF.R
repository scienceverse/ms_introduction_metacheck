library(tidyverse)

# Load data from each group #
data_1_1 <- read.csv("./data_exp_21082-v28_task-94q6.csv")

data_1_2 <- read.csv("./data_exp_21082-v30_task-94q6.csv")

data_1 <- rbind(data_1_1, data_1_2)

data_1$Group <- as.numeric("1")

data_2 <- read.csv("./data_exp_24339-v3_task-94q6.csv")

data_2$Group <- as.numeric("2")

data_3 <- read.csv("./data_exp_24582-v2_task-94q6.csv")

data_3$Group <- as.numeric("3")

data_4 <- read.csv("./data_exp_24653-v2_task-94q6.csv")

data_4$Group <- as.numeric("4")

data_5_1 <- read.csv("./data_exp_24832-v2_task-94q6.csv")

data_5_2 <- read.csv("./data_exp_24326-v5_task-94q6.csv")

data_5 <- rbind(data_5_1, data_5_2)

data_5$Group <- as.numeric("5")

# Getting rid of the data from OA participants that are 70+ (included in next group instead)

data_5 <- data_5[data_5$Participant.Private.ID!=1921811 & data_5$Participant.Private.ID!=1893168 & data_5$Participant.Private.ID!=1893170 & data_5$Participant.Private.ID!=1893173, ]

data_6_1 <- read.csv("./data_exp_24861-v2_task-94q6.csv")

data_6_2 <- read.csv("./data_exp_24326-v5_task-94q6.csv")

# keep 70+ subjects only from OA participants. IDs: 1921811, 1893168, 1893170, 1893173
data_6_2 <- filter(data_6_2, Participant.Private.ID=="1921811"| Participant.Private.ID=="1893168"| Participant.Private.ID=='1893170'| Participant.Private.ID== '1893173')

data_6 <- rbind(data_6_1, data_6_2)

data_6$Group <- as.numeric("6")



# remove participant that failed questionnaire screen
data_2 <- data_2[data_2$Participant.Private.ID!=1894932, ]

# Combine all groups
all_data <- rbind(data_1, data_2, data_3, data_4, data_5, data_6)


raw_data <- all_data %>% 
  filter(Zone.Type == "response_keyboard")

# remove practice trials
raw_data <- raw_data %>% 
  filter(Spreadsheet.Row > 6)

raw_data %>%                             
  group_by(Participant.Private.ID) %>%    
  count() 

trial_count <- raw_data %>% 
  group_by(Participant.Private.ID) %>% 
  count()

trial_data <- raw_data %>% 
  select(Participant.Private.ID, Correct, Reaction.Time, Group)

trial_data <- trial_data %>% 
  set_names(c("ID", "acc", "RT", "Group"))

trial_data$RT <- as.numeric(trial_data$RT)


# Calculate proportion correct 
trial_data <- trial_data %>% 
  group_by(ID) %>%
  mutate(pc = mean(acc))

# Include correct RTs only
Correct_RT_trials <- trial_data %>%
  filter(acc==1)

# remove trials where RT < or > 3 SDs from the individual subject mean
Calculating_RT_outliers <- Correct_RT_trials
RT_trial_outliers <- Calculating_RT_outliers %>%
  group_by(ID) %>%
  mutate(n = length(ID)) %>%
  mutate(srt = sd(RT)) %>%
  mutate(mrt = mean(RT)) %>%
  mutate(zrt = (RT - mrt)/srt)

RT_outliers <- RT_trial_outliers %>%
  filter(zrt < -3| zrt > 3)

RT_outliers_removed <- RT_trial_outliers %>%
  filter(zrt > -3 & zrt < 3)

# Calculate mean RTs for correct responses only 
Subject_correct_RT <- RT_outliers_removed %>% 
  group_by(ID) %>%
  mutate(mean_rt_c = mean(RT))

# Merge correct response RTs and pc
Subject_data <- Subject_correct_RT %>%
  select(ID, mean_rt_c, pc)

Subject_data$group <- as.numeric("1")

Subject_data <- unique(Subject_data)

# Calculate BIS

#data = BIS(data)
#This function combines RTs and PC into a combined measure termed 'Balanced
#Integration Score' (Liesefeld, Fu, & Zimmer, 2015, JEP:LMC, 41, 1140-1151;
#                    Liesefeld & Janczyk, in press)
#the input dataframe must contain variables 'mean_rt_c' and 'pc'
#the function calculates BIS with values standardized across all fields of the 
#input and adds the field 'bis', which has the same format as 'rt' and 'pc'

BIS <- function(data) {
  n <- length(data$group)    # sample size to correct var()-function result (which uses n-1)
  srt <- sqrt( ((n-1)/n) * var(data$mean_rt_c) )     # sample standard deviation across all rts
  spc <- sqrt( ((n-1)/n) * var(data$pc) )            # sample standard deviation across all rts
  mrt <- mean(data$mean_rt_c)                        # mean across all rts
  mpc <- mean(data$pc)                               # mean across all pcs
  zrt <- (data$mean_rt_c-mrt)/srt                    # standardized rts
  zpc <- (data$pc-mpc)/spc                           # z-standardized pcs
  data$bis <- zpc - zrt                              # Balanced Integration Score
  
  return(data)                                       # return data.frame with added variable 'bis'
}



BIS_data <- BIS(Subject_data)

BIS_data <- unique(BIS_data)