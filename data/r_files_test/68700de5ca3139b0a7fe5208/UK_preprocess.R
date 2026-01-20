#################################################################
##                        Preprocess NO                        ##
#################################################################
library(tidyverse)
library(data.table)


diagnose <- full_df[, grep("f41270", colnames(full_df)), with = FALSE]

# Initialize vectors for recorded conditions
recorded_depression <- rep(0, nrow(full_df))
recorded_anxiety_disorders <- rep(0, nrow(full_df))

# Check for "F32" in primary diagnoses and create the new column
for (i in 1:nrow(full_df)) {
  primary_row <- diagnose[i,]
  
  # Check if any diagnosis code contains "F32"
  if (any(grepl("F32", primary_row))) {
    recorded_depression[i] <- 1
  }
  
  
  # Check if any diagnosis code contains "F41" for other anxiety disorders
  if (any(grepl("F41", primary_row))) {
    recorded_anxiety_disorders[i] <- 1
  }
  
  # Check if all empty, then put NA
  if (all(primary_row == "")) {
    recorded_depression[i] <- NA
    recorded_phobic_anxiety[i] <- NA
    recorded_anxiety_disorders[i] <- NA
  }
}

sum(is.na(recorded_depression))

#extracting the columns that we need
df_coord <- full_df %>% 
  rename("Eastings" =home_location_at_assessment_east_co_ordinate_1km_resolution_f22688_0_0,
         "Northings" = home_location_at_assessment_north_co_ordinate_1km_resolution_f22689_0_0) %>% 
  dplyr::select(Eastings,Northings, average_total_household_income_before_tax_f738_0_0,
                eid, sex_f31_0_0, age_at_recruitment_f21022_0_0, ever_depressed_for_a_whole_week_f4598_0_0,
                ) %>% 
  mutate(recorded_depression = recorded_depression,
         recorded_phobic_anxiety = recorded_phobic_anxiety,
         recorded_anxiety_disorders = recorded_anxiety_disorders) 
  

setwd("/Users/adam/Documents/PhD/UMH/distance functions")
source("distance_functions.R")

df_coord <- transform_coords(df_coord,
                                   "Eastings", 
                             northing_col = "Northings")


cities_50_uk <- uk_cities |> filter(population_proper > 50000)

df_coord <- calculate_distances_adj(df_coord, uk_cities, c(20000, 50000, 100000, 200000))


#dangerous move! Check it
colnames(df_coord) <- c("Eastings", "Northings", "income", "eid",
                        "sex", "age", "depression_one_week","recorded_depression",
                        "recorded_phobic_anxiety", "recorded_anxiety_disorders",
                        "longitude", "latitude", "dis20", "dis_adj_city20", 
                        "dis50", "dis_adj_city50",
                        "dis100", "dis_adj_city100",
                        "dis200", "dis_adj_city200",
                        "population20", "population50", "population100","population200")
                        
#sanity check
sum(df_coord$dis50 > df_coord$dis100)
sum(df_coord$dis100 > df_coord$dis150)
sum(df_coord$dis150 > df_coord$dis200)



