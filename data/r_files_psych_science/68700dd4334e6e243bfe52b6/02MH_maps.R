# Load required libraries
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)  # For combining plots



# Calculate the overall range of dis50 across all datasets
min_dis50 <- min(df$dis50, df_nz$dis50, df_UK$dis50, na.rm = TRUE)
max_dis50 <- max(df$dis50, df_nz$dis50, df_UK$dis50, na.rm = TRUE)

# Create shared color gradient for all plots
shared_color_scale <- scale_color_gradient(low = "#264653", high = "#f4a261", limits = c(min_dis50, max_dis50))

############################################################################################################
####  Norway  ####
############################################################################################################

no_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  subset(admin == "Norway") 


no_cities_50 <- no_cities %>% filter(population_proper > 50000)

no_plot <- ggplot(data = no_map) +
  geom_sf() +
  coord_sf(datum = NA) +
  coord_fixed(ratio = 1.3) +  # Adjust the ratio to make it visually consistent
  geom_point(data = df, aes(x = longitude, y = latitude, color = dis50), size = 0.01) +
  shared_color_scale +
  geom_point(data = no_cities_50, aes(x = lng, y = lat), color = "#9e2a2b", size = 0.5) +
  coord_sf(xlim = c(3, 33), ylim = c(57.5, 72), expand = FALSE) +
  theme_classic() +
  labs(color = NULL) +  # Remove individual color label
  theme(text = element_text(size = 7)) +
  theme(legend.position = "none") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Norway with cities > 50,000 population")

setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave(plot = no_plot, "NO_map_50k.pdf", width = 7, height = 9, units = "cm")

############################################################################################################
####  New Zealand  ####
############################################################################################################

nz_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  subset(admin == "New Zealand")


nz_cities_50 <- nz_cities %>% filter(population_proper > 50000)

NZ_plot <- ggplot(data = nz_map) +
  geom_sf() +
  coord_sf(datum = NA) +
  coord_fixed(ratio = 1.3) +  # Adjust the ratio to make it visually consistent
  geom_point(data = df_nz, aes(x = longitude, y = latitude, color = dis50), size = 0.01) +
  shared_color_scale +
  geom_point(data = nz_cities_50, aes(x = lng, y = lat), color = "#9e2a2b", size = 0.5) +
  coord_sf(xlim = c(166, 179), ylim = c(-48, -34), expand = FALSE) +
  theme_classic() +
  labs(color = NULL) +  # Remove individual color label
  theme(text = element_text(size = 7)) +
  theme(legend.position = "none") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("New Zealand with cities > 50,000 population")

setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave(plot = NZ_plot, "NZ_map_50k.pdf", width = 7, height = 9, units = "cm")


library(patchwork)

# Assuming no_plot and NZ_plot are your two plots


############################################################################################################
####  United Kingdom  ####
############################################################################################################

UK_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  subset(admin == "United Kingdom")

uk_cities_50 <- uk_cities %>% filter(population_proper > 50000)

UK_plot <- ggplot(data = UK_map) +
  geom_sf() +
  coord_sf(datum = NA) +
  coord_fixed(ratio = 1.3) +  # Adjust the ratio to make it visually consistent
  geom_point(data = df_UK, aes(x = longitude, y = latitude, color = dis50), size = 0.01) +
  shared_color_scale +
  geom_point(data = uk_cities_50, aes(x = lng, y = lat), color = "#9e2a2b", size = 0.5) +
  coord_sf(xlim = c(-9, 5), ylim = c(49.5, 59.2), expand = FALSE) +
  theme_classic() +
  theme(text = element_text(size = 7)) +
  theme(legend.position = "none") +
  labs(color = NULL) +  # Remove individual color label
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("U.K with cities > 50,000 population")

setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave(plot = UK_plot, "UK_map_50k.pdf", width = 7, height = 9, units = "cm")

############################################################################################################
####  Shared color scale plot  ####
############################################################################################################

# Create a blank plot to hold the shared color scale
color_scale_plot <- ggplot(data = df, aes(x = longitude, y = latitude, color = dis50)) +
  geom_point() +
  shared_color_scale +
  theme_void() +  # Remove all elements except the color scale
  theme(legend.position = "bottom") +  # Place legend at bottom
  labs(color = "Distance")  # Add label for color scale

setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave(plot = color_scale_plot, "color_scale.pdf", width = 5, height = 5, units = "cm")

############################################################################################################
####  Combine plots using patchwork  ####
############################################################################################################

combined_plot <- no_plot + NZ_plot + UK_plot + plot_layout(ncol = 3)

setwd("/Users/adam/Documents/PhD/UMH/mental_health_proj/Figures")
ggsave("combined_map.pdf", plot = combined_plot, width = 21, height = 9, units = "cm")
# Save the combined plot
