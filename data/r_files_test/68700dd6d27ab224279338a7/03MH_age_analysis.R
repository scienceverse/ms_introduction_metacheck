library(mgcv)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)


############################################################################
############################################################################
###                                                                      ###
###                          Age and urbanicity                          ###
###                                                                      ###
############################################################################
############################################################################
  mutate(depression = ifelse(depression == "Yes", 1, 0) %>% as.numeric(),
         anxiety = ifelse(anxiety == "Yes", 1, 0) %>% as.numeric(),
         Gender = ifelse(Gender == "Woman", 1, 0) |> as.numeric())


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

plot_gam_interaction <- function(df, output_col, predictor_col, distance_cut, ylabel, with_controls = FALSE,
                                    ymin = 0, ymax = 0.15) {
  # Prepare data
  df_clean <- df %>%
    dplyr::select(!!sym(predictor_col), !!sym(output_col), age, Gender) %>%
    drop_na(!!sym(output_col), !!sym(predictor_col)) %>%
    filter(!!sym(predictor_col) < distance_cut)
  print(nrow(df_clean))
  
  print(paste("Number of rows in the data frame:", nrow(df_clean)))
  
  # Fit the GAM model
  gam_formula <- as.formula(paste(
    output_col,
    "~ s(", predictor_col, ", bs = 'cr', k = 4) +",
    "s(age, bs = 'cr', k = 4) +",
    "ti(", predictor_col, ", age, bs = 'cr', k = 4)",
    if (with_controls) "+ Gender" else ""
  ))
  
  gam_model <- gam(gam_formula, family = "binomial", data = df_clean)
  
  ### 1. Urbanicity Main Effect Plot ###
  # Generate predictions over the range of predictor_col, holding age at mean value
  pred_data_urbanicity <- data.frame(
    predictor = seq(min(df_clean[[predictor_col]], na.rm = TRUE), max(df_clean[[predictor_col]], na.rm = TRUE), length.out = 1000),
    age = mean(df_clean$age, na.rm = TRUE),
    Gender = mean(df_clean$Gender, na.rm = TRUE)
  )
  colnames(pred_data_urbanicity)[1] <- predictor_col
  
  predictions_urbanicity <- predict(gam_model, newdata = pred_data_urbanicity, se.fit = TRUE, type = "response")
  pred_data_urbanicity$pred <- predictions_urbanicity$fit
  pred_data_urbanicity$lower <- predictions_urbanicity$fit - 1.96 * predictions_urbanicity$se.fit
  pred_data_urbanicity$upper <- predictions_urbanicity$fit + 1.96 * predictions_urbanicity$se.fit
  
  # Plot urbanicity main effect
  plot_urbanicity <- ggplot(pred_data_urbanicity, aes(x = !!sym(predictor_col), y = pred)) +
    geom_line(color = "#9e2a2b", size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9e2a2b", alpha = 0.2) +
    xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
    ylab(ylabel) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    theme_classic() +
    theme(text = element_text(size = 8))
  
  ### 2. Age Main Effect Plot ###
  # Generate predictions over the range of age, holding predictor_col at mean value
  pred_data_age <- data.frame(
    predictor = mean(df_clean[[predictor_col]], na.rm = TRUE),
    age = seq(min(df_clean$age, na.rm = TRUE), max(df_clean$age, na.rm = TRUE), length.out = 1000),
    Gender = mean(df_clean$Gender, na.rm = TRUE)
  )
  colnames(pred_data_age)[1] <- predictor_col
  
  predictions_age <- predict(gam_model, newdata = pred_data_age, se.fit = TRUE, type = "response")
  pred_data_age$pred <- predictions_age$fit
  pred_data_age$lower <- predictions_age$fit - 1.96 * predictions_age$se.fit
  pred_data_age$upper <- predictions_age$fit + 1.96 * predictions_age$se.fit
  
  # Plot age main effect
  plot_age <- ggplot(pred_data_age, aes(x = age, y = pred)) +
    geom_line(color = "#2a9e2b", size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#2a9e2b", alpha = 0.2) +
    xlab("Age") +
    ylab(ylabel) +
    coord_cartesian(ylim = c(0, 0.5)) +
    theme_classic() +
    theme(text = element_text(size = 8))
  
  ### 3. Interaction Plot ###
  # Generate predictions over a grid of predictor_col and age values
  grid_data <- expand.grid(
    predictor_col = seq(min(df_clean[[predictor_col]], na.rm = TRUE), max(df_clean[[predictor_col]], na.rm = TRUE), length = 100),
    age = seq(min(df_clean$age, na.rm = TRUE), max(df_clean$age, na.rm = TRUE), length = 100),
    Gender = mean(df_clean$Gender, na.rm = TRUE)
  )
  colnames(grid_data)[1] <- predictor_col
  
  grid_data$pred <- predict(gam_model, newdata = grid_data, type = "response")
  
  # Plot interaction as a heatmap
  plot_interaction <- ggplot(grid_data, aes(x = !!sym(predictor_col), y = age, fill = pred)) +
    geom_tile() +
    scale_fill_gradient(name = "Predicted Probability", low = "white", high = "red") +
    xlab(expression(paste("Adjusted distance  (", italic(km)/sqrt(italic(N)),")"))) +
    ylab("Age") +
    theme_classic() +
    theme(text = element_text(size = 8))
  
  # Return the plots
  return(list(
    plot_urbanicity = plot_urbanicity,
    plot_age = plot_age,
    plot_interaction = plot_interaction,
    gam_model = gam_model
  ))
}

###
### Depression (New Zealand)
###

NZ_d_50_c_1 <- plot_gam_interaction(df = df_NZ, output_col = "depression", predictor_col = "dis50", 
                                       distance_cut = 0.1, ylabel = "N.Z. depression",  with_controls = TRUE,
                                       ymin = 0.11, 
                                       ymax = 0.16)
#Inspect model
summary(NZ_d_50_c_1[[4]])


#results without london
  drop_na(recorded_depression, dis50)

hist(full_uk$dis50)
hist(full_uk$recorded_depression)
hist(full_uk$recorded_phobic_anxiety)
table(full_uk$sex)
hist(full_uk$age)
mean(full_uk$age, na.rm = TRUE)
min(full_uk$age, na.rm = TRUE)
max(full_uk$age, na.rm = TRUE)

df_uk <- fread("processed_UK.csv") |> 
  filter(dis_adj_city50 != "London") |> 
  rename(Gender = sex)



uk_d_50_c_1 <- plot_gam_interaction(df = df_uk, output_col = "recorded_depression", predictor_col = "dis50", 
                                    distance_cut = 0.1, ylabel = "U.K. depression",  with_controls = TRUE,
                                    ymin = 0, 
                                    ymax = 0.3)

summary(uk_d_50_c_1[[4]])
uk_d_50_c_1[[2]]


###
### Norway
###


  mutate(probable_depression = ifelse(depression > 9, 1,0)) |> 
  rename(Gender = sex)


NO_d_50_c_1 <- plot_gam_interaction(df = df_NO, output_col = "probable_depression", predictor_col = "dis50", 
                                            distance_cut = 0.1, ylabel = "Norway probable depression",  with_controls = TRUE,
                                            ymin = 0.12, 
                                            ymax = 0.25)
summary(NO_d_50_c_1[[4]])

NO_d_50_c_1[[2]]
