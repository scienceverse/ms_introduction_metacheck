library(mgcv)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)

############################################################################
############################################################################
###                                                                      ###
###                                  NZ                                  ###
###                                                                      ###
############################################################################
############################################################################
#Load data

  mutate(depression = ifelse(depression == "Yes", 1, 0) %>% as.numeric(),
         anxiety = ifelse(anxiety == "Yes", 1, 0) %>% as.numeric(),
         Gender = ifelse(Gender == "Woman", 1, 0) |> as.numeric()) |> 
  drop_na(anxiety, dis50)

hist(df_NZ$dis50)
hist(df_NZ$depression)
hist(df_NZ$anxiety)
hist(df_NZ$dis50)
hist(df_NZ$depression)
hist(df_NZ$anxiety)
table(df_NZ$Gender)
hist(df_NZ$age)
mean(df_NZ$age, na.rm = TRUE)
min(df_NZ$age, na.rm = TRUE)
max(df_NZ$age, na.rm = TRUE)

plot_gam_predictions_nz <- function(df, output_col, predictor_col, distance_cut, ylabel, with_controls = FALSE,
                                    ymin = 0, ymax = 0.15,
                                    predict_cut = 0.05) {
  # Prepare data
  df_clean <- df %>%
    dplyr::select(!!sym(predictor_col), !!sym(output_col), age, Gender) %>%
    drop_na(!!sym(output_col), !!sym(predictor_col)) %>%
    filter(!!sym(predictor_col) < 0.15)
  print(nrow(df_clean))
  
  print(paste("Number of rows in the data frame:", nrow(df_clean)))
  
  # Fit the GAM model
  gam_formula <- as.formula(paste(output_col, "~ s(", predictor_col, ", bs = \"cr\", k = 4)", 
                                  if (with_controls) "+ age + Gender" else ""))
  gam_model <- gam(gam_formula, family = "binomial", data = df_clean)
  
  # Create prediction data
  pred_data <- data.frame(predictor = seq(0, predict_cut, length.out = 1000),
                          age = mean(df_clean$age, na.rm = TRUE),
                          Gender = mean(df_clean$Gender, na.rm = TRUE))
  
  # Rename the predictor column to match the original column name
  colnames(pred_data)[1] <- predictor_col
  
  # Generate predictions
  predictions <- predict(gam_model, newdata = pred_data, se.fit = TRUE, type = "response")
  pred_data <- pred_data %>%
    mutate(pred = predictions$fit,
           lower = predictions$fit - 1.96 * predictions$se.fit,
           upper = predictions$fit + 1.96 * predictions$se.fit)
  
  # Plotting
  plot <- ggplot(pred_data, aes(x = !!sym(predictor_col))) +
    geom_line(aes(y = pred, color = "Prediction"), size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Prediction"), alpha = 0.2) +
    scale_color_manual(name = "Prediction", values = c("Prediction" = "#9e2a2b")) +
    scale_fill_manual(name = "Prediction", values = c("Prediction" = "#9e2a2b")) +
    xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    ylab(ylabel) +
    theme_classic() +
    theme(text = element_text(size = 8)) +
    #remove legend
    theme(legend.position = "none")
  
  res_df <- data.frame(pred_min = min(pred_data$pred), 
                       dis_min = pred_data[which.min(pred_data$pred), predictor_col],
                       pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "pred"],
                       pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "pred"],
                       lower_min = pred_data[which.min(pred_data$pred), "lower"],
                       upper_min = pred_data[which.min(pred_data$pred), "upper"],
                       lower_pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "lower"],
                       upper_pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "upper"],
                       lower_pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "lower"],
                       upper_pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "upper"])
  
  
  return(list(plot = plot, res_df = res_df))
}

###
### Depression (New Zealand)
###

NZ_d_50_c_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "depression", predictor_col = "dis50", 
                                       #distance_cut = 0.1, 
                                       ylabel = "N.Z. depression",  with_controls = TRUE,
                                       ymin = 0.11, 
                                       ymax = 0.16)
NZ_d_50_c_1[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("NZ_depression.pdf", NZ_d_50_c_1[[1]], width = 6, height = 4, units = "cm")

NZ_d_50_c_1[[2]]

# Distance robustness
NZ_d_20_c_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "depression", predictor_col = "dis20", 
                                       #distance_cut = 0.1, 
                                       ylabel = "N.Z. dep. dis 20",  with_controls = TRUE,
                                       ymin = 0.11, 
                                       ymax = 0.16)
NZ_d_20_c_1[[1]]
ggsave("NZ_depression_20.pdf",NZ_d_20_c_1[[1]], width = 6, height = 4, units = "cm")

# In NZ data, res100 is named dis_pop_adj
NZ_d_100_c_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "depression", predictor_col = "dis100", 
                                        #distance_cut = 0.1,
                                        ylabel = "N.Z. dep. dis. 100",  with_controls = TRUE,
                                        ymin = 0.11, 
                                        ymax = 0.16)
NZ_d_100_c_1[[1]]
ggsave("NZ_depression_100.pdf",NZ_d_100_c_1[[1]], width = 6, height = 4, units = "cm")

NZ_d_200_c_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "depression", predictor_col = "dis200", 
                                        distance_cut = 0.1, ylabel = "N.Z. dep. dis. 200",  with_controls = TRUE,
                                        ymin = 0.11, 
                                        ymax = 0.16)
NZ_d_200_c_1[[1]]
ggsave("NZ_depression_200.pdf",NZ_d_200_c_1[[1]], width = 6, height = 4, units = "cm")

# Without controls
NZ_d_50_no_control <- plot_gam_predictions_nz(df = df_NZ, output_col = "depression", predictor_col = "dis50", 
                                     distance_cut = 0.1, ylabel = "NZ dep. no control",
                                     ymin = 0.12, 
                                     ymax = 0.17)
NZ_d_50_no_control[[1]]
ggsave("NZ_depression_no_control.pdf",NZ_d_50_no_control[[1]], width = 6, height = 4, units = "cm") 

###
### Anxiety (New Zealand)
###

###
### Depression (New Zealand)
###


NZ_a_50_c_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "anxiety", predictor_col = "dis50", 
                                       distance_cut = 0.1, ylabel = "N.Z. anxiety",  with_controls = TRUE,
                                       ymin = 0.09, 
                                       ymax = 0.14)
NZ_a_50_c_1[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("NZ_anxiety.pdf", NZ_a_50_c_1[[1]], width = 6, height = 4, units = "cm")
NZ_a_50_c_1[[2]]


# Distance robustness
NZ_a_20_c_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "anxiety", predictor_col = "dis20", 
                                       distance_cut = 0.1, ylabel = "N.Z. anxiety dis. 20",  with_controls = TRUE,
                                       ymin = 0.09, 
                                       ymax = 0.14)
NZ_a_20_c_1[[1]]
ggsave("NZ_anxiety_20.pdf",NZ_a_20_c_1[[1]], width = 6, height = 4, units = "cm")

# In NZ data, res100 is named dis_pop_adj
NZ_a_100_c_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "anxiety", predictor_col = "dis100", 
                                        distance_cut = 0.1, ylabel = "N.Z. anxiety dis. 100",  with_controls = TRUE,
                                        ymin = 0.09, 
                                        ymax = 0.14)
NZ_a_100_c_1[[1]]
ggsave("NZ_anxiety_100.pdf",NZ_a_100_c_1[[1]], width = 6, height = 4, units = "cm")

NZ_a_200_c_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "anxiety", predictor_col = "dis200", 
                                        distance_cut = 0.1, ylabel = "N.Z. anxiety dis. 200",  with_controls = TRUE,
                                        ymin = 0.09, 
                                        ymax = 0.14)
NZ_a_200_c_1[[1]]
ggsave("NZ_anxiety_200.pdf",NZ_a_200_c_1[[1]], width = 6, height = 4, units = "cm")

# Without controls
NZ_a_50_1 <- plot_gam_predictions_nz(df = df_NZ, output_col = "anxiety", predictor_col = "dis50", 
                                     distance_cut = 0.1, ylabel = "N.Z. anxiety no control",
                                     ymin = 0.11, 
                                     ymax = 0.16)
NZ_a_50_1[[1]]
ggplot2::ggsave("NZ_anxiety_no_control.pdf",NZ_a_50_1[[1]], width = 6, height = 4, units = "cm")



############################################################################
############################################################################
###                                                                      ###
###                                  UK                                  ###
###                                                                      ###
############################################################################
############################################################################

#results without london


hist(full_uk$dis50)
hist(full_uk$recorded_depression)
hist(full_uk$recorded_phobic_anxiety)
table(full_uk$sex)
hist(full_uk$age)
mean(full_uk$age, na.rm = TRUE)
min(full_uk$age, na.rm = TRUE)
max(full_uk$age, na.rm = TRUE)

df_uk <- full_uk |> 
  filter(dis_adj_city50 != "London")

# Recode negative values as NA for specified columns
df_uk <- df_uk %>%
  mutate(depression_one_week = ifelse(depression_one_week < 0, NA, depression_one_week),
         recorded_depression = ifelse(recorded_depression < 0, NA, recorded_depression),
         recorded_anxiety_disorders = ifelse(recorded_anxiety_disorders < 0, NA, recorded_anxiety_disorders))

# Define the function to plot GAM predictions
plot_gam_predictions <- function(df, output_col, predictor_col, distance_cut, ylabel, with_controls = FALSE, 
                                 predict_cut = 0.05,
                                 ymin = 0,
                                 ymax = 0.15) {
  # Prepare data
  df_clean <- df %>%
    dplyr::select(!!sym(predictor_col), !!sym(output_col), age, sex) %>%
    drop_na(!!sym(output_col), !!sym(predictor_col)) %>%
    filter(!!sym(predictor_col) < distance_cut)
  
  print(paste("Number of rows in the data frame:", nrow(df_clean)))
  
  # Fit the GAM model
  gam_formula <- as.formula(paste(output_col, "~ s(", predictor_col, ", bs = 'cr', k = 4)", 
                                  if (with_controls) "+ age + sex" else ""))
  gam_model <- gam(gam_formula, family = "binomial", data = df_clean)
  
  # Create prediction data
  pred_data <- data.frame(predictor = seq(0, predict_cut, length.out = 1000),
                          age = mean(df_clean$age, na.rm = TRUE),
                          sex = mean(df_clean$sex, na.rm = TRUE))
  
  # Rename the predictor column to match the original column name
  colnames(pred_data)[1] <- predictor_col
  
  # Generate predictions
  predictions <- predict(gam_model, newdata = pred_data, se.fit = TRUE, type = "response")
  pred_data <- pred_data %>%
    mutate(pred = predictions$fit,
           lower = predictions$fit - 1.96 * predictions$se.fit,
           upper = predictions$fit + 1.96 * predictions$se.fit)
  
  # Plotting
  plot <- ggplot(pred_data, aes(x = !!sym(predictor_col))) +
    geom_line(aes(y = pred, color = "Prediction"), size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Prediction"), alpha = 0.2) +
    scale_color_manual(name = "Prediction", values = c("Prediction" = "#9e2a2b")) +
    scale_fill_manual(name = "Prediction", values = c("Prediction" = "#9e2a2b")) +
    xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    ylab(ylabel) +
    theme_classic() +
    theme(text = element_text(size = 8)) +
    #remove legend
    theme(legend.position = "none")
  
  res_df <- data.frame(pred_min = min(pred_data$pred), 
                       dis_min = pred_data[which.min(pred_data$pred), predictor_col],
                       pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "pred"],
                       pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "pred"],
                       lower_min = pred_data[which.min(pred_data$pred), "lower"],
                       upper_min = pred_data[which.min(pred_data$pred), "upper"],
                       lower_pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "lower"],
                       upper_pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "upper"],
                       lower_pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "lower"],
                       upper_pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "upper"])
  
  
  return(list(plot = plot, res_df = res_df))
}


###
### Depression
###

uk_d_50_c_1 <- plot_gam_predictions(df = df_uk, output_col = "recorded_depression", predictor_col = "dis50", 
                                  distance_cut = 0.1, ylabel = "U.K. depression",  with_controls = TRUE,
                                  ymin = 0.05, 
                                  ymax = 0.1)
uk_d_50_c_1[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("UK_depression.pdf", uk_d_50_c_1[[1]], width = 6, height = 4, units = "cm")

uk_d_50_c_1[[2]]

#Distance robustness
uk_d_20_c_1 <- plot_gam_predictions(df = df_uk, output_col = "recorded_depression", predictor_col = "dis20", 
                                    distance_cut = 0.1, ylabel = "U.K. depression dis. 20",  with_controls = TRUE,
                                    ymin = 0.05, 
                                    ymax = 0.1)
uk_d_20_c_1[[1]]
ggsave("UK_depression_20.pdf",uk_d_20_c_1[[1]], width = 6, height = 4, units = "cm")

uk_d_100_c_1 <- plot_gam_predictions(df = df_uk, output_col = "recorded_depression", predictor_col = "dis100", 
                                    distance_cut = 0.1, ylabel = "U.K. depression dis. 100",  with_controls = TRUE,
                                    ymin = 0.05, 
                                    ymax = 0.1)
uk_d_100_c_1[[1]]
ggplot2::ggsave("UK_depression_100.pdf",uk_d_100_c_1[[1]], width = 6, height = 4, units = "cm")

uk_d_200_c_1 <- plot_gam_predictions(df = df_uk, output_col = "recorded_depression", predictor_col = "dis200", 
                                     distance_cut = 0.1, ylabel = "U.K. depression dis. 200",  with_controls = TRUE,
                                     ymin = 0.05, 
                                     ymax = 0.1)
uk_d_200_c_1[[1]]
ggsave("UK_depression_200.pdf",uk_d_200_c_1[[1]], width = 6, height = 4, units = "cm")

#without controls
uk_d_50_no_control <- plot_gam_predictions(df = df_uk, output_col = "recorded_depression", predictor_col = "dis50", 
                                  distance_cut = 0.1, ylabel = "UK dep. no control",
                                  ymin = 0.05, 
                                  ymax = 0.1)
uk_d_50_no_control[[1]]
ggsave("UK_depression_no_control.pdf",uk_d_50_no_control[[1]], width = 6, height = 4, units = "cm")

###
### Anxiety
###

uk_a_50_c_01 <- plot_gam_predictions(df = df_uk, output_col = "recorded_anxiety_disorders", predictor_col = "dis50", 
                              distance_cut = 0.1, ylabel = "U.K. anxiety", with_controls = TRUE,
                              ymin = 0.04, 
                              ymax = 0.09)
uk_a_50_c_01[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("UK_anxiety.pdf", uk_a_50_c_01[[1]], width = 6, height = 4, units = "cm")
uk_a_50_c_01[[2]]

#Distance robustness
uk_a_20_c_01 <- plot_gam_predictions(df = df_uk, output_col = "recorded_anxiety_disorders", predictor_col = "dis20", 
                                     distance_cut = 0.1, ylabel = "U.K. anxiety dis. 20", with_controls = TRUE,
                                     ymin = 0.04, 
                                     ymax = 0.09)
uk_a_20_c_01[[1]]
ggsave("UK_anxiety_20.pdf",uk_a_20_c_01[[1]], width = 6, height = 4, units = "cm")
uk_a_100_c_01 <- plot_gam_predictions(df = df_uk, output_col = "recorded_anxiety_disorders", predictor_col = "dis100", 
                                     distance_cut = 0.1, ylabel = "U.K. anxiety dis. 100", with_controls = TRUE,
                                     ymin = 0.04, 
                                     ymax = 0.09)
uk_a_100_c_01[[1]]
ggsave("UK_anxiety_100.pdf",uk_a_100_c_01[[1]], width = 6, height = 4, units = "cm")

uk_a_200_c_01 <- plot_gam_predictions(df = df_uk, output_col = "recorded_anxiety_disorders", predictor_col = "dis200", 
                                     distance_cut = 0.1, ylabel = "U.K. anxiety dis. 200", with_controls = TRUE,
                                     ymin = 0.04, 
                                     ymax = 0.09)
uk_a_200_c_01[[1]]
ggsave("UK_anxiety_200.pdf",uk_a_200_c_01[[1]], width = 6, height = 4, units = "cm")

uk_a_50_01_no_control <- plot_gam_predictions(df = df_uk, output_col = "recorded_anxiety_disorders", predictor_col = "dis50", 
                                     distance_cut = 0.1, ylabel = "U.K. anxiety no control", 
                                     ymin = 0.04, 
                                     ymax = 0.09)
uk_a_50_01_no_control[[1]]
ggsave("UK_anxiety_no_control.pdf",uk_a_50_01_no_control[[1]], width = 6, height = 4, units = "cm")



setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/data")
df_london <- full_uk |> 
  filter(dis_adj_city200 == "London")

hist(df_london$dis50)

london_d_con <- plot_gam_predictions(df = df_london, output_col = "recorded_depression", predictor_col = "dis50", 
                              distance_cut = 0.1, ylabel = "London Depression", with_controls = TRUE)
london_d_con[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("London_depression.pdf", london_d_con[[1]], width = 6, height = 4, units = "cm")
london_d_con[[2]]

london_a_con <- plot_gam_predictions(df = df_london, output_col = "recorded_anxiety_disorders", predictor_col = "dis50", 
                              distance_cut = 0.1, ylabel = "London anxiety", with_controls = TRUE)
london_a_con[[1]]
ggsave("London_anxiety.pdf", london_a_con[[1]], width = 6, height = 4, units = "cm")
london_a_con[[2]]

############################################################################
############################################################################
###                                                                      ###
###                            NO continuous                             ###
###                                                                      ###
############################################################################
############################################################################
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/data")
df_NO <- read.csv("processed_NO.csv")

hist(df_NO$dis50)
hist(df_NO$depression)
hist(df_NO$anxiety)
table(df_NO$sex)
hist(df_NO$age)
mean(df_NO$age)
min(df_NO$age)
max(df_NO$age)
# Define the function to plot GAM predictions
plot_gam_predictions_no <- function(df, output_col, predictor_col, distance_cut, ylabel, with_controls = FALSE,
                                    ymin = 0, ymax = 0.15) {
  # Prepare data
  df_clean <- df %>%
    dplyr::select(!!sym(predictor_col), !!sym(output_col), age, sex) %>%
    drop_na(!!sym(output_col), !!sym(predictor_col)) %>%
    filter(!!sym(predictor_col) < 2)
  
  print(paste("Number of rows in the data frame:", nrow(df_clean)))
  
  # Fit the GAM model
  gam_formula <- as.formula(paste(output_col, "~ s(", predictor_col, ", bs = 'cr', k = 4)", 
                                  if (with_controls) "+ age +  sex" else ""))
  gam_model <- gam(gam_formula, family = "nb", data = df_clean)
  
  # Create prediction data
  pred_data <- data.frame(predictor = seq(0, 0.05, length.out = 1000),
                          age = mean(df_clean$age, na.rm = TRUE),
                          sex = mean(df_clean$sex, na.rm = TRUE))
  
  # Rename the predictor column to match the original column name
  colnames(pred_data)[1] <- predictor_col
  
  # Generate predictions
  predictions <- predict(gam_model, newdata = pred_data, se.fit = TRUE, type = "response")
  pred_data <- pred_data %>%
    mutate(pred = predictions$fit,
           lower = predictions$fit - 1.96 * predictions$se.fit,
           upper = predictions$fit + 1.96 * predictions$se.fit)
  
  # Plotting
  plot <- ggplot(pred_data, aes(x = !!sym(predictor_col))) +
    geom_line(aes(y = pred, color = "Prediction"), size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Prediction"), alpha = 0.2) +
    scale_color_manual(name = "Prediction", values = c("Prediction" = "#9e2a2b")) +
    scale_fill_manual(name = "Prediction", values = c("Prediction" = "#9e2a2b")) +
    xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
    ylab(ylabel) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    theme_classic() +
    theme(text = element_text(size = 8)) +
    #remove legend
    theme(legend.position = "none")
  
  
  res_df <- data.frame(pred_min = min(pred_data$pred), 
                       dis_min = pred_data[which.min(pred_data$pred), predictor_col],
                       pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "pred"],
                       pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "pred"],
                       lower_min = pred_data[which.min(pred_data$pred), "lower"],
                       upper_min = pred_data[which.min(pred_data$pred), "upper"],
                       lower_pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "lower"],
                       upper_pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "upper"],
                       lower_pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "lower"],
                       upper_pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "upper"])
  
  
  return(list(plot = plot, res_df = res_df))
}

###
### Depression
###

NO_d_50_c_1 <- plot_gam_predictions_no(df = df_NO, output_col = "depression", predictor_col = "dis50", 
                                    distance_cut = 0.1, ylabel = "Norway depression",  with_controls = TRUE,
                                    ymin = 4, 
                                    ymax = 7)
NO_d_50_c_1[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("NO_depression.pdf", NO_d_50_c_1[[1]], width = 6, height = 4, units = "cm")
NO_d_50_c_1[[2]]



###
### Anxiety
###

NO_a_50_c_1 <- plot_gam_predictions_no(df = df_NO, output_col = "anxiety", predictor_col = "dis50", 
                                       distance_cut = 0.1, ylabel = "Norway anxiety",  with_controls = TRUE,
                                       ymin = 2, 
                                       ymax = 7)
NO_a_50_c_1[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("NO_anxiety.pdf", NO_a_50_c_1[[1]], width = 6, height = 4, units = "cm")
NO_a_50_c_1[[2]]



############################################################################
############################################################################
###                                                                      ###
###                     NO self reported diagnose                        ###
###                                                                      ###
############################################################################
############################################################################

# Define the function to plot GAM predictions
plot_gam_predictions_no_prob <- function(df, output_col, predictor_col, distance_cut, ylabel, with_controls = FALSE,
                                    ymin = 0, ymax = 0.15,
                                    predict_cut = 0.05) {
  # Prepare data
  df_clean <- df %>%
    dplyr::select(!!sym(predictor_col), !!sym(output_col), age, sex) %>%
    drop_na(!!sym(output_col), !!sym(predictor_col)) %>%
    filter(!!sym(predictor_col) < 2)
  
  print(paste("Number of rows in the data frame:", nrow(df_clean)))
  
  # Fit the GAM model
  gam_formula <- as.formula(paste(output_col, "~ s(", predictor_col, ", bs = 'cr', k = 4)", 
                                  if (with_controls) "+ age +  sex" else ""))
  gam_model <- gam(gam_formula, family = "binomial", data = df_clean)
  
  # Create prediction data
  pred_data <- data.frame(predictor = seq(0, predict_cut, length.out = 1000),
                          age = mean(df_clean$age, na.rm = TRUE),
                          sex = mean(df_clean$sex, na.rm = TRUE))
  
  # Rename the predictor column to match the original column name
  colnames(pred_data)[1] <- predictor_col
  
  # Generate predictions
  predictions <- predict(gam_model, newdata = pred_data, se.fit = TRUE, type = "response")
  pred_data <- pred_data %>%
    mutate(pred = predictions$fit,
           lower = predictions$fit - 1.96 * predictions$se.fit,
           upper = predictions$fit + 1.96 * predictions$se.fit)
  
  # Plotting
  plot <- ggplot(pred_data, aes(x = !!sym(predictor_col))) +
    geom_line(aes(y = pred, color = "Prediction"), size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Prediction"), alpha = 0.2) +
    scale_color_manual(name = "Prediction", values = c("Prediction" = "#9e2a2b")) +
    scale_fill_manual(name = "Prediction", values = c("Prediction" = "#9e2a2b")) +
    xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
    ylab(ylabel) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    theme_classic() +
    theme(text = element_text(size = 8)) +
    #remove legend
    theme(legend.position = "none")
  
  
  res_df <- data.frame(pred_min = min(pred_data$pred), 
                       dis_min = pred_data[which.min(pred_data$pred), predictor_col],
                       pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "pred"],
                       pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "pred"],
                       lower_min = pred_data[which.min(pred_data$pred), "lower"],
                       upper_min = pred_data[which.min(pred_data$pred), "upper"],
                       lower_pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "lower"],
                       upper_pred_min_dis = pred_data[which.min(pred_data[[predictor_col]]), "upper"],
                       lower_pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "lower"],
                       upper_pred_max_dis = pred_data[which.max(pred_data[[predictor_col]]), "upper"])
  
  
  return(list(plot = plot, res_df = res_df, gam_model = gam_model))
}



setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/data")

df_NO <- read.csv("processed_NO.csv") |> 
  filter(!is.na(self_reported_anxiety)) 
  #filter(study == "CIPA")

#hist(df_NO$self_reported_anxiety)

###
### Depression
###

NO_d_50_c_1 <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_depression", predictor_col = "dis50", 
                                            distance_cut = 0.1, ylabel = "Norway depression",  with_controls = TRUE,
                                            ymin = 0.05, 
                                            ymax = 0.10)
NO_d_50_c_1[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("NO_self_report_depression.pdf", NO_d_50_c_1[[1]], width = 6, height = 4, units = "cm")
NO_d_50_c_1[[2]]
summary(NO_d_50_c_1[[3]])

#Distance robustness
NO_d_20_c_1 <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_depression", predictor_col = "dis20", 
                                       distance_cut = 0.1, ylabel = "Norway dep. dis. 20",  with_controls = TRUE,
                                       ymin = 0.05, 
                                       ymax = 0.11)
#in NO data, res100 is named dis_pop_adj
NO_d_20_c_1[[1]]
ggsave("NO_SR_depression_20.pdf",NO_d_20_c_1[[1]], width = 6, height = 4, units = "cm")

NO_d_100_c_1 <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_depression", predictor_col = "dis_pop_adj", 
                                        distance_cut = 0.1, ylabel = "Norway dep. dis. 100",  with_controls = TRUE,
                                        ymin = 0.05, 
                                        ymax = 0.11)

NO_d_100_c_1[[1]]
ggsave("NO_SR_depression_100.pdf",NO_d_100_c_1[[1]], width = 6, height = 4, units = "cm")

NO_d_200_c_1 <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_depression", predictor_col = "dis200", 
                                        distance_cut = 0.1, ylabel = "Norway dep. dis. 200",  with_controls = TRUE,
                                        ymin = 0.05, 
                                        ymax = 0.11)
NO_d_200_c_1[[1]]
ggsave("NO_SR_depression_200.pdf",NO_d_200_c_1[[1]], width = 6, height = 4, units = "cm")

#without controls
NO_d_50_no_control <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_depression", predictor_col = "dis50", 
                                              distance_cut = 0.1, ylabel = "Norway dep. no control",
                                              ymin = 0.05, 
                                              ymax = 0.11)
NO_d_50_no_control[[1]]
ggsave("NO_SR_depression_no_control.pdf",NO_d_50_no_control[[1]], width = 6, height = 4, units = "cm")
###
### Anxiety
###

###
### Anxiety
###

NO_a_50_c_1 <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_anxiety", predictor_col = "dis50", 
                                       distance_cut = 0.1, ylabel = "Norway anxiety",  with_controls = TRUE,
                                       ymin = 0.04, 
                                       ymax = 0.09)
NO_a_50_c_1[[1]]
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("NO_SR_anxiety.pdf", NO_a_50_c_1[[1]], width = 6, height = 4, units = "cm")
NO_a_50_c_1[[2]]

#Distance robustness
NO_a_20_c_1 <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_anxiety", predictor_col = "dis20", 
                                       distance_cut = 0.1, ylabel = "Norway anxiety dis. 20",  with_controls = TRUE,
                                       ymin = 0.04, 
                                       ymax = 0.09)
NO_a_20_c_1[[1]]
ggsave("NO_SR_anxiety_20.pdf",NO_a_20_c_1[[1]], width = 6, height = 4, units = "cm")

# In NO data, res100 is named dis_pop_adj
NO_a_100_c_1 <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_anxiety", predictor_col = "dis_pop_adj", 
                                        distance_cut = 0.1, ylabel = "Norway anxiety dis. 100",  with_controls = TRUE,
                                        ymin = 0.04, 
                                        ymax = 0.09)
NO_a_100_c_1[[1]]
ggsave("NO_SR_anxiety_100.pdf",NO_a_100_c_1[[1]], width = 6, height = 4, units = "cm")

NO_a_200_c_1 <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_anxiety", predictor_col = "dis200", 
                                        distance_cut = 0.1, ylabel = "Norway anxiety dis. 200",  with_controls = TRUE,
                                        ymin = 0.04, 
                                        ymax = 0.09)
NO_a_200_c_1[[1]]
ggsave("NO_SR_anxiety_200.pdf",NO_a_200_c_1[[1]], width = 6, height = 4, units = "cm")

# Without controls
NO_a_50_1_no_control <- plot_gam_predictions_no_prob(df = df_NO, output_col = "self_reported_anxiety", predictor_col = "dis50", 
                                                distance_cut = 0.1, ylabel = "Norway anx. no control",
                                                ymin = 0.04, 
                                                ymax = 0.09)
NO_a_50_1_no_control[[1]]
ggsave("NO_SR_anxiety_no_control.pdf",NO_a_50_1_no_control[[1]], width = 6, height = 4, units = "cm")



#####
##### Histograms
#####

df_NO_hist <- df_NO |> filter(dis50 < 0.3)

(nrow(df_NO) - nrow(df_NO_hist)) / nrow(df_NO) 

his_NO <- df_NO_hist |> 
  ggplot(aes(x = dis20)) +
  geom_histogram(bins = 50, fill = "#e09f3e") +
  xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
  ylab("Density Norway") +#ylab(ifelse(type == "mu", "Z-transformed mean","Z-transformed SD" ))  +
  theme_classic() +
  theme(text = element_text(size=9))

setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("NO_dis20.pdf", his_NO, width = 10, height = 6, units = "cm")

df_nz_hist <- df_NZ |> filter(dis50 < 0.3)
(nrow(df_NZ) - nrow(df_nz_hist)) / nrow(df_NZ) #1504

his_NZ <- df_nz_hist |> 
  ggplot(aes(x = dis50)) +
  geom_histogram(bins = 50, fill = "#e09f3e") +
  xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
  ylab("Density N.Z.") +#ylab(ifelse(type == "mu", "Z-transformed mean","Z-transformed SD" ))  +
  theme_classic() +
  theme(text = element_text(size=9))
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("NZ_dis50.pdf", his_NZ, width = 10, height = 6, units = "cm")

df_uk_hist <- full_uk |> filter(dis50 < 0.3)
nrow(full_uk) - nrow(df_uk_hist) # 0
his_UK <- df_uk_hist |>
  ggplot(aes(x = dis50)) +
  geom_histogram(bins = 50, fill = "#e09f3e") +
  xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
  ylab("Density U.K.") +#ylab(ifelse(type == "mu", "Z-transformed mean","Z-transformed SD" ))  +
  theme_classic() +
  coord_cartesian(xlim = c(0, 0.3)) +
  theme(text = element_text(size=9))
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("UK_dis50.pdf", his_UK, width = 10, height = 6, units = "cm")

library(patchwork)

combined_hist <- his_UK + his_NZ + his_NO
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("combined_hist.pdf", combined_hist, width = 20, height = 6, units = "cm")



####
#### Combined plots with extended rurality
####


# Define the combined function to fit 3 GAMs and plot their predictions
combined_gam_prediction_plot <- function(df_NO, df_uk, df_NZ, output_col, predictor_col, ylabel, with_controls = FALSE) {
  
  # Helper function to fit GAM and extract predictions
  fit_gam_and_predict <- function(df, predictor_col, output_col, distance_cut, predict_cut, with_controls) {
    
    # Clean data
    df_clean <- df %>%
      dplyr::select(!!sym(predictor_col), !!sym(output_col), age, sex) %>%
      drop_na(!!sym(output_col), !!sym(predictor_col)) %>%
      filter(!!sym(predictor_col) < distance_cut)
    
    # Fit the GAM model
    gam_formula <- as.formula(paste(output_col, "~ s(", predictor_col, ", bs = 'cr', k = 4)", 
                                    if (with_controls) "+ age + sex" else ""))
    gam_model <- gam(gam_formula, family = "binomial", data = df_clean)
    
    # Create prediction data
    pred_data <- data.frame(predictor = seq(0, predict_cut, length.out = 1000),
                            age = mean(df_clean$age, na.rm = TRUE),
                            sex = mean(df_clean$sex, na.rm = TRUE))
    
    # Rename the predictor column to match the original column name
    colnames(pred_data)[1] <- predictor_col
    
    # Generate predictions
    predictions <- predict(gam_model, newdata = pred_data, se.fit = TRUE, type = "response")
    pred_data <- pred_data %>%
      mutate(pred = predictions$fit,
             lower = predictions$fit - 1.96 * predictions$se.fit,
             upper = predictions$fit + 1.96 * predictions$se.fit)
    
    return(pred_data)
  }
  
  # Get predictions for Norway (df_NO) with distance cutoff 0.2
  predictions_NO <- fit_gam_and_predict(df_NO, predictor_col, output_col, distance_cut = 0.22, predict_cut = 0.22, with_controls = with_controls)
  predictions_NO$country <- "Norway"
  
  # Get predictions for UK (df_uk) with distance cutoff 0.05
  predictions_uk <- fit_gam_and_predict(df_uk, predictor_col, output_col, distance_cut = 0.05, predict_cut = 0.05, with_controls = with_controls)
  predictions_uk$country <- "UK"
  
  # Get predictions for New Zealand (df_NZ) with distance cutoff 0.2
  predictions_NZ <- fit_gam_and_predict(df_NZ, predictor_col, output_col, distance_cut =  0.218, predict_cut =  0.218, with_controls = with_controls)
  predictions_NZ$country <- "New Zealand"
  
  # Combine all predictions into one data frame
  combined_predictions <- bind_rows(predictions_NO, predictions_uk, predictions_NZ)
  
  # Plotting
  plot <- ggplot(combined_predictions, aes(x = !!sym(predictor_col), y = pred, color = country)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = country), alpha = 0.2) +
    scale_color_manual(values = c("Norway" = "#1b9e77", "UK" = "#d95f02", "New Zealand" = "#7570b3")) +
    scale_fill_manual(values = c("Norway" = "#1b9e77", "UK" = "#d95f02", "New Zealand" = "#7570b3")) +
    xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
    ylab(ylabel) +
    coord_cartesian(ylim = c(0, 0.15)) +
    theme_classic() +
    theme(text = element_text(size = 8)) +
    theme(legend.position = "right")
  
  return(plot)
}


quantile(df_NO$dis50, probs = c(0.25, 0.5, 0.9)) # 0.22
quantile(df_uk$dis50, probs = c(0.25, 0.5, 0.9)) #0.024
quantile(df_NZ$dis50, probs = c(0.25, 0.5, 0.9),na.rm = TRUE) #90% quantile is 0.218

df_NO <- df_NO |> 
  mutate(anxiety = self_reported_anxiety,
         depression = self_reported_depression)
df_uk <- df_uk |> 
  mutate(anxiety = recorded_anxiety_disorders,
         depression = recorded_depression)
df_NZ <- df_NZ |> 
  mutate(sex = Gender)

predictor_col <- "dis50"  # Replace with the actual distance variable

# Define the label for the y-axis
ylabel <- "Predicted Outcome"





output_col <- "depression"  # Replace with the actual output variable
# Call the function with the actual data and arguments
plot_dep <- combined_gam_prediction_plot(
  df_NO = df_NO,          # Replace with your actual Norway data frame
  df_uk = df_uk,          # Replace with your actual UK data frame
  df_NZ = df_NZ,          # Replace with your actual New Zealand data frame
  output_col = output_col, 
  predictor_col = predictor_col, 
  ylabel = "Depression", 
  with_controls = TRUE    # Set to TRUE if you want to include age and sex as controls
)
plot_dep
setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("combined_depression.pdf", plot_dep, width = 10, height = 6, units = "cm")



output_col <- "anxiety"  # Replace with the actual output variable
# Call the function with the actual data and arguments
plot_anx <- combined_gam_prediction_plot(
  df_NO = df_NO,          # Replace with your actual Norway data frame
  df_uk = df_uk,          # Replace with your actual UK data frame
  df_NZ = df_NZ,          # Replace with your actual New Zealand data frame
  output_col = output_col, 
  predictor_col = predictor_col, 
  ylabel = "Anxiety", 
  with_controls = TRUE    # Set to TRUE if you want to include age and sex as controls
)
plot_anx

setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("combined_anxiety.pdf", plot_anx, width = 10, height = 6, units = "cm")


###
### below 0.05?
###


df_NO |>  filter(dis50 < 0.05) |> nrow()
df_NZ |>  filter(dis50 < 0.05) |> nrow()
df_UK |> filter(dis50 < 0.05) |> nrow()

7961 /  nrow(df_NO)
22120 / nrow(df_NZ)
448358 / nrow(df_UK)



###
### n cities
###

no_cities <- read.csv("no_cities.csv")
no_cities_50 <- no_cities %>% filter(population_proper > 50000) #14 cities


nz_cities_50 <- nz_cities %>% filter(population_proper > 50000)


setwd("/Users/adam/Documents/PhD/UMH/distance fu
cities_50_uk <- uk_cities |> filter(population_proper > 50000)
