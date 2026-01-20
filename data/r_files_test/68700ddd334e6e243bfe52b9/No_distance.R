###########################################################################
###########################################################################
###                                                                     ###
###                    NORWAY CENTROID TO DISTANCE                      ###
###                                                                     ###
###########################################################################
###########################################################################
## Adam Finnemann


#Load packages and data
library(tidyverse)
library(raster) #pointdistance



colnames(df) <- c("country code","postal code","place name","admin name1" ,"admin code1" ,"admin name2" ,"admin code2","admin name3" ,"admin code3" ,"latitude","longitude","accuracy") 
#country code      : iso country code, 2 characters
#postal code       : varchar(20)
#place name        : varchar(180)
#admin name1       : 1. order subdivision (state) varchar(100)
#admin code1       : 1. order subdivision (state) varchar(20)
#admin name2       : 2. order subdivision (county/province) varchar(100)
#admin code2       : 2. order subdivision (county/province) varchar(20)
#admin name3       : 3. order subdivision (community) varchar(100)
#admin code3       : 3. order subdivision (community) varchar(20)
#latitude          : estimated latitude (wgs84)
#longitude         : estimated longitude (wgs84)
#accuracy          : accuracy of lat/lng from 1=estimated, 4=geonameid, 6=centroid of addresses or shape


#Longitudes should be 7.5 to 30
#latitude should be 57 to 71
hist(df$latitude)
hist(df$longitude)


#no cities and city centers:https://simplemaps.com/data/no-cities

#Creating a data frame of all cities that +100 and 50k
no_cities_200 <- no_cities %>% filter(population_proper > 200000) #2 cities
no_cities_100 <- no_cities %>% filter(population_proper > 100000) #7 cities
no_cities_50 <- no_cities %>% filter(population_proper > 50000) #14 cities
no_cities_20 <- no_cities %>% filter(population_proper > 20000) #14 cities




###Function to calculate distance from no post code 6 to no city centers.
###Distances are divided by the size of city
###Shortest distance is retained
min_size_fun_adj = function(df, city_df){
  
  df$short_dis = rep(NA, nrow(df))
  df$short_city = rep(NA, nrow(df))
  
  Sys.time()
  
  
  for (sub in 1:nrow(df)) {#loop over subjects
    
    distances = pointDistance(c(df$longitude[sub],df$latitude[sub]),
                              city_df[,c("lng","lat")],lonlat =T) 
    
    
    distances_pop_adj = distances / sqrt(city_df$population_proper) #distance adjusted by sqrt(N) in meters
    df$dis_pop_adj[sub] = min(distances_pop_adj) / 1000 #extracting min adjusted distance
    df$city_pop_adj[sub] = city_df$city[which.min(distances_pop_adj)]
  }
  return(df)
}

#### Analysis:


#final two columns shows adjusted distance and cloests city. Unit: m / sqrt(N)
res200 = min_size_fun_adj(df, city_df = no_cities_200)
res100 = min_size_fun_adj(df, city_df = no_cities_100)
res50 = min_size_fun_adj(df, city_df = no_cities_50)
res20 = min_size_fun_adj(df, city_df = no_cities_20)

cor(res200$dis_pop_adj, res100$dis_pop_adj)
cor(res100$dis_pop_adj, res50$dis_pop_adj)
cor(res50$dis_pop_adj, res20$dis_pop_adj)
cor(res100$dis_pop_adj, res20$dis_pop_adj)

#### Sanity checks
hist(res100$dis_pop_adj)
hist(res50$dis_pop_adj)
#Should be monotonic decreasing. At 200 there is bump -> missed city?
hist(res20$dis_pop_adj)
hist(res200$dis_pop_adj)
#when we include more cities all distances should get smaller or stay the same. We check if this is case:
sum(res100$dis_pop_adj < res50$dis_pop_adj)  
#None are larger
sum(res50$dis_pop_adj < res20$dis_pop_adj)  
#None are larger


######## Lastly, we visualise the results:
# Install and load necessary packages
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# no MAP
no_map <- ne_countries(scale = "medium", 
                       returnclass = "sf") %>% 
  subset( admin == "Norway")

# Create a base map including all cities > 100k
ggplot(data = no_map) +
  geom_sf() +
  coord_sf(datum = NA) +
  geom_point(data = res100, aes(x = longitude, y = latitude, color = dis_pop_adj), size = 0.1) +
  geom_point(data = no_cities_100, aes(x = lng, y = lat), color = "darkred", size = 1) +
  coord_sf(xlim = c(1, 35), ylim = c(55, 75), expand = FALSE) +
  geom_text(data = no_cities_100, aes(x = lng, y = lat, label = city), color = "darkred", size = 2, vjust = -1) +#adding label
  theme_classic() +
  ggtitle("Map of Norway")


#20K
ggplot(data = no_map) +
  geom_sf() +
  coord_sf(datum = NA) +
  geom_point(data = res20, aes(x = longitude, y = latitude, color = dis_pop_adj), size = 0.1) +
  geom_point(data = no_cities_20, aes(x = lng, y = lat), color = "darkred", size = 1) +
  coord_sf(xlim = c(1, 35), ylim = c(55, 75), expand = FALSE) +
  geom_text(data = no_cities_20, aes(x = lng, y = lat, label = city), color = "darkred", size = 2, vjust = -1) +#adding label
  theme_classic() +
  ggtitle("Map of the Norway")



##Final df

final <- res100 %>% 
  mutate(postcode = `postal code`,
         dis50 = res50$dis_pop_adj,
         dis20 = res20$dis_pop_adj,
         dis200 = res200$dis_pop_adj) 


