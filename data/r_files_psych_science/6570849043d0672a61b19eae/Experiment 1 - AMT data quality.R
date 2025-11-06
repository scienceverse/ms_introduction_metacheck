# The purpose of this file is to show the inclusion rate for Exp. 1 using AMT data.

# load library
library(tidyverse)
library(readr)
library(dplyr)
# read data
## AMT (n=60)
data <- read_csv("./data/exp-1-amt-data/00.exp1-raw-amt.csv")
attn_checks <- read_csv("./data/exp-1-amt-data/01.exp1-attnchecks-amt.csv")

# apply inclusion criteria
L_filtered_id <- attn_checks %>%
  distinct(unique_id)

L_data <- data%>%
  filter(unique_id %in% L_filtered_id$unique_id)%>%
  mutate(data_type="within subjects",
         manipulation_type = case_when(str_detect(trial_condition, "1 agent,")==TRUE ~ "baseline",
                                       str_detect(trial_condition, "attention_checks")==TRUE ~ "attention_checks",
                                       TRUE ~ "main"),
         A_intentionality=case_when(str_detect(trial_condition, "intentional")==TRUE ~ "intentional drop",
                                    str_detect(trial_condition, "no drop")==TRUE ~ "no drop-off",
                                    TRUE ~ "accidental drop"),
         B_intentionality=case_when(str_detect(trial_condition, "pickup")==TRUE ~ "intend to own",
                                    str_detect(trial_condition, "walkaway")==TRUE ~ "no intent to own",
                                    TRUE ~ "Not Applicable"),
         B_knowledge=case_when(str_detect(trial_condition, "aware")==TRUE ~ "knowledgeable",
                               str_detect(trial_condition, "ignorant")==TRUE ~ "ignorant",
                               TRUE ~ "Not Applicable"),
         blue_agent_first=case_when(str_detect(agent_order, "A_2,A_1")==TRUE ~ FALSE,
                                    TRUE ~ TRUE))%>%
  dplyr::select(unique_id,data_type,trial_condition,manipulation_type,A_intentionality,B_intentionality,B_knowledge,
                blue_agent_first, A1_value, A2_value, object_cost_response,id)

L_data_long <- L_data%>%
  filter(manipulation_type!="attention_checks")%>%
  rename(A=A1_value, B = A2_value)%>%
  gather(agent, value, A,B)%>%
  select(-object_cost_response)

L_data_long$value <- as.numeric(L_data_long$value)
L_data_long$agent <- factor(L_data_long$agent, levels=c("A", "B"))

## get baseline trial data
baseline_data<- L_data_long%>%
  filter(manipulation_type=="baseline" & agent=="B")%>% 
  spread(agent,value)%>%
  spread(trial_condition,B)

baseline_wide <- baseline_data%>%
  group_by(unique_id)%>%
  summarise_all(function(x){x[!is.na(x)]})%>%
  filter(A_intentionality=="accidental drop")

## check exclusion
strict_baseline_filter_id <- baseline_wide%>%
filter(`1 agent,accidental`==0 & `1 agent,intentional`==0 & `1 agent,no drop`==0)

strict_baseline_filtered_data_long <- L_data_long%>%
  filter(unique_id %in% strict_baseline_filter_id$unique_id)

strict_baseline_filter_id%>%
  mutate(data_source="amt")%>%
  group_by(data_source)%>%
  summarise(passed_inclusion = n(),
            percentage_passed = passed_inclusion/60)
