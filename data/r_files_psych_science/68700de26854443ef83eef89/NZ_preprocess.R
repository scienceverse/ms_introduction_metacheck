library(foreign)
library(tidyverse)

# Function to process the data
process_data <- function(main_df, distance_df, mb_col_main) {
  # Remove the TXX suffix from the column names
  colnames(main_df) <- gsub("\\.T\\d{2}$", "", colnames(main_df))
  
  # Discard missing geographical information
  main_df <- main_df %>% drop_na(all_of(mb_col_main))
  
  # Select relevant variables from main data frame
  analysis_df <- main_df %>%
    left_join(distance_df, by = setNames("mb_col", mb_col_main)) %>%
    dplyr::select(
      HLTH.Diagnose.Depression.L2,
      HLTH.Diagnose.Anxiety.L2,
      Alcohol.Intensity,
      Gender,
      Age,
      all_of(mb_col_main),
      Household.INC,
      LifeMeaning01, # My life has a clear sense of purpose.
      SWB.LifeSat01, # I am satisfied with my life.
      Emp.JobSat, # How satisfied are you with your current job?
      Perfectionism03, # I am hardly ever satisfied with my performance.
      dis200,
      dis100,
      dis50,
      dis20,
      LONGITUDE,
      LATITUDE
    ) %>%
    rename(
      "depression" = "HLTH.Diagnose.Depression.L2",
      "anxiety" = "HLTH.Diagnose.Anxiety.L2",
      "alcohol" = "Alcohol.Intensity",
      "meaning" = "LifeMeaning01",
      "income" = "Household.INC",
      "age" = "Age",
      "happiness" = "SWB.LifeSat01",
      "job_sat" = "Emp.JobSat",
      "perf" = "Perfectionism03",
      "longitude" = "LONGITUDE",
      "latitude" = "LATITUDE"
    ) %>%
    mutate(
      alcohol = alcohol %>% as.numeric() %>% scale(),
      meaning = meaning %>% as.numeric() %>% scale(),
      income = income %>% as.numeric() %>% scale(),
      happiness = happiness %>% as.numeric() %>% scale(),
      job_sat = job_sat %>% as.numeric() %>% scale(),
      perf = perf %>% as.numeric() %>% scale(),
      dis100 = dis100 / 1000, # Converting to kilometers
      dis50 = dis50 / 1000,
      dis20 = dis20 / 1000,
      dis200 = dis200 / 1000
    )
  
  return(analysis_df)
}


# Apply the function to the 2023 data
# Giving MBU name without the ending TXX where XX is a number. This part is stripped in the function so it works across different years
processed_data_2023 <- process_data(main_df = df23,distance_df =  dis_df23, mb_col_main = "MBU_2023")

hist(processed_data_2023$dis200)
hist(processed_data_2023$dis100)
hist(processed_data_2023$dis50)
hist(processed_data_2023$dis20)

sum(is.na(processed_data_2023$dis100))

sum(processed_data_2023$dis100 < processed_data_2023$dis50, na.rm = TRUE)
sum(processed_data_2023$dis50 < processed_data_2023$dis20, na.rm = TRUE)



