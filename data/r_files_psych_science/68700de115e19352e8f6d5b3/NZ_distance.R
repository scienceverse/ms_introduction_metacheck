###########################################################################
###########################################################################
###                                                                     ###
###                      NZ CENTROID TO DISTANCE                        ###
###                                                                     ###
###########################################################################
###########################################################################
## Adam Finnemann

library(sf)
library(dplyr)
library(geosphere)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Function definition
process_meshblock <- function(shapefile_path, shapefile_name, city_data_path, mb_col = "MB2023_V1_") {
  # Set working directory and read the shapefile
  setwd(shapefile_path)
  df <- st_read(shapefile_name) %>% st_drop_geometry()
  
  # Convert and filter the data
  df <- df %>%
    mutate(LONGITUDE = LONGITUDE %>% as.character() %>% as.numeric(),
           LATITUDE = LATITUDE %>% as.character() %>% as.numeric()) %>% 
    filter(LONGITUDE >= 166, LONGITUDE <= 179, LATITUDE <= -34, LATITUDE >= -47)
  
  # Read city data
  setwd(city_data_path)
  nz_cities <- read.csv("nz_cities.csv")
  
  # Create subsets based on population sizes
  nz_cities_200 <- nz_cities %>% filter(population_proper > 200000)
  nz_cities_100 <- nz_cities %>% filter(population_proper > 100000)
  nz_cities_50 <- nz_cities %>% filter(population_proper > 50000)
  nz_cities_20 <- nz_cities %>% filter(population_proper > 20000)
  
  # Function to calculate distance from meshblock to city centers, adjusted by population
  min_size_fun_adj <- function(df, city_df) {
    df$dis_pop_adj <- rep(NA, nrow(df))
    df$city_pop_adj <- rep(NA, nrow(df))
    
    for (sub in 1:nrow(df)) {
      distances <- pointDistance(c(df$LONGITUDE[sub], df$LATITUDE[sub]),
                                 city_df[,c("lng","lat")], lonlat = TRUE)
      
      distances_pop_adj <- distances / sqrt(city_df$population_proper)
      df$dis_pop_adj[sub] <- min(distances_pop_adj)
      df$city_pop_adj[sub] <- city_df$city[which.min(distances_pop_adj)]
    }
    
    return(df)
  }
  
  # Run analyses for the three city size conditions
  res200 <- min_size_fun_adj(df, nz_cities_200)
  res100 <- min_size_fun_adj(df, nz_cities_100)
  res50 <- min_size_fun_adj(df, nz_cities_50)
  res20 <- min_size_fun_adj(df, nz_cities_20)
  
  # Create the final dataframe
  final <- res100 %>%
    mutate(dis100 = dis_pop_adj,
           dis50 = res50$dis_pop_adj,
           dis20 = res20$dis_pop_adj,
           dis200 = res200$dis_pop_adj,
           mb_col = df[,mb_col]) %>%
    dplyr::select(LONGITUDE, LATITUDE,dis200, dis100, dis50, dis20, mb_col)
  
  return(final)
}

# Apply the function to the 2023 dataset
shapefile_path_2023 <- "/Users/adam/Documents/PhD/UMH/mental_health_proj/data/NZ/statsnz-meshblock-2023-centroid-true-SHP"
shapefile_name_2023 <- "meshblock-2023-centroid-true.shp"
city_data_path <- "/Users/adam/Documents/PhD/UMH/mental_health_proj/data/NZ"
result_2023 <- process_meshblock(shapefile_path = shapefile_path_2023, 
                                shapefile_name = shapefile_name_2023, 
                                city_data_path = city_data_path)

# Save the results
setwd(city_data_path)
write.csv(result_2023, "NZ_MB1_distance2023.csv")


