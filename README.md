
# Load necessary packages
library(dplyr)
library(ggplot2)
library(sf)

# Load your CSV data
setwd("D:\\NDVI_Afar")  # Set working directory
data <- read.csv("NDVI_DISINT.csv", header = TRUE)

# Load Ethiopian boundary shapefile
ethiopia_shapefile <- st_read("E:\\ARC_documents\\Great_Rift\\Shape_file\\Eth\\ETH_adm0.shp")

# Filter your data based on bounds
data_filtered <- data %>%
  filter(Lat >= 3.3 & Lat <= 15.0,
         Long >= 33.0 & Long <= 48.0)

# Function to get p-values for each row (using all rows for model)
get_p_value <- function(data, independent_var) {
  model_data <- data %>% filter(!is.na(NDVI) & !is.na(.data[[independent_var]]))
  if (nrow(model_data) < 2) {
    return(NA)
  }
  model <- lm(as.formula(paste("NDVI ~", independent_var)), data = model_data)
  summary_model <- summary(model)
  p_value <- summary_model$coefficients[2, 4]
  return(p_value)
}

# Sample the filtered data for plotting
set.seed(42)  # Setting a seed for reproducibility
data_sampled <- data_filtered %>% sample_n(5000)

# Calculate p-values for each variable using the entire dataset for fitting
p_temp <- get_p_value(data_filtered, "Temp")
p_precipitation <- get_p_value(data_filtered, "PREC")
p_radiation <- get_p_value(data_filtered, "RAD")

# Create p-value categories for the sample
p_values <- data_sampled %>%
  mutate(p_temp = p_temp,
         p_precipitation = p_precipitation,
         p_radiation = p_radiation)

# Create a new column for p-value categories for each variable
p_values$category_temp <- cut(p_values$p_temp, 
                              breaks = c(-Inf, 0.05, 0.1, Inf), 
                              labels = c("p < 0.05", "0.05 <= p < 0.1", "p > 0.1"),
                              right = FALSE)

p_values$category_precipitation <- cut(p_values$p_precipitation, 
                                       breaks = c(-Inf, 0.05, 0.1, Inf), 
                                       labels = c("p < 0.05", "0.05 <= p < 0.1", "p > 0.1"),
                                       right = FALSE)

p_values$category_radiation <- cut(p_values$p_radiation, 
                                   breaks = c(-Inf, 0.05, 0.1, Inf), 
                                   labels = c("p < 0.05", "0.05 <= p < 0.1", "p > 0.1"),
                                   right = FALSE)

# Convert to spatial data frames
data_sf_temp <- st_as_sf(p_values, coords = c("Long", "Lat"), crs = 4326)
data_sf_precipitation <- st_as_sf(p_values, coords = c("Long", "Lat"), crs = 4326)
data_sf_radiation <- st_as_sf(p_values, coords = c("Long", "Lat"), crs = 4326)

# Create and export the map for Temperature Significant Areas
temp_map <- ggplot() +
  geom_sf(data = ethiopia_shapefile, fill = NA, color = "black") +
  geom_sf(data = data_sf_temp, aes(color = category_temp), alpha = 0.7) +
  scale_color_manual(values = c("p < 0.05" = "red", 
                                "0.05 <= p < 0.1" = "orange", 
                                "p > 0.1" = "grey")) +
  theme_minimal() +
  labs(title = "NDVI vs Temperature p-values in Ethiopia",
       color = "P-value Category") +
  theme(legend.position = "bottom")
temp_map
ggsave("p_values_NDVI_vs_Temperature.png", plot = temp_map, width = 10, height = 8, dpi = 300)
![image](https://github.com/user-attachments/assets/d3919db9-1dd9-4df7-9ddd-81a1f7f5a60b)
# Create and export the map for Precipitation Significant Areas
precip_map <- ggplot() +
  geom_sf(data = ethiopia_shapefile, fill = NA, color = "black") +
  geom_sf(data = data_sf_precipitation, aes(color = category_precipitation), alpha = 0.7) +
  scale_color_manual(values = c("p < 0.05" = "blue", 
                                "0.05 <= p < 0.1" = "lightblue", 
                                "p > 0.1" = "grey")) +
  theme_minimal() +
  labs(title = "NDVI vs Precipitation p-values in Ethiopia",
       color = "P-value Category") +
  theme(legend.position = "bottom")

ggsave("p_values_NDVI_vs_Precipitation.png", plot = precip_map, width = 10, height = 8, dpi = 300)


# Create and export the map for Radiation Significant Areas
radiation_map <- ggplot() +
  geom_sf(data = ethiopia_shapefile, fill = NA, color = "black") +
  geom_sf(data = data_sf_radiation, aes(color = category_radiation), alpha = 0.7) +
  scale_color_manual(values = c("p < 0.05" = "green", 
                                "0.05 <= p < 0.1" = "yellow", 
                                "p > 0.1" = "grey")) +
  theme_minimal() +
  labs(title = "NDVI vs Radiation p-values in Ethiopia",
       color = "P-value Category") +
  theme(legend.position = "bottom")

ggsave("p_values_NDVI_vs_Radiation.png", plot = radiation_map, width = 10, height = 8, dpi = 300)
