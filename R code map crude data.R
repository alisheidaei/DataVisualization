rm(list = ls())
setwd("Z:\\Kimiya\\Multiple outcome exposure modeling\\Prepare result paper multivariate")
library(tidyverse)
library(ggplot2)
library(maps)
library(ggpubr)
library(data.table)


rm(list = ls())
setwd("Z:\\Kimiya\\Multiple outcome exposure modeling\\Prepare result paper multivariate")


##### Read Data #####
# LocationData <- fread('Data for Modeling all components wide version 5.csv', select = c(3,4,5))
# LocationData <- LocationData %>% distinct()
# write.csv(LocationData, "LocationData.csv")
LocationData <- read.csv("LocationData.csv")

# Multivariate datasets
MXGBoostDataTest <- read.csv("Prediction all year mahalanobis - MXGBoost.csv")
MRFDataTest <- read.csv("Prediction all year mahalanobis - MRF.csv")

MXGBoostDataTrain <- read.csv("Prediction all year mahalanobis train - MXGBoost.csv")
MRFDataTrain <- read.csv("Prediction all year mahalanobis train - MRF.csv")

MXGBoostData <- rbind(MXGBoostDataTest, MXGBoostDataTrain)
MRFData <- rbind(MRFDataTest, MRFDataTrain)

##### End #####

##### Merge datasets #####
TotalData <- MXGBoostData %>% # Multivariate XGBoost model
  rename(ca_pred_MXGBoost = ca_pred, ec_pred_MXGBoost = ec_pred,
         si_pred_MXGBoost = si_pred, so4_pred_MXGBoost = so4_pred) %>%
  select(-c(ca_obs, ec_obs, si_obs, so4_obs)) %>%
  left_join(MRFData, by = c("date", "site_id")) %>% # Multivariate Random Forest
  rename(ca_pred_MRF = ca_pred, ec_pred_MRF = ec_pred,
         si_pred_MRF = si_pred, so4_pred_MRF = so4_pred) %>%
  left_join(LocationData, by = "site_id")



MapData <- TotalData %>%
  select(site_id, lat, lon, date, so4_obs, ca_obs, si_obs, ec_obs) %>%
  filter(complete.cases(.)) %>%
  group_by(site_id, lat, lon) %>%
  summarise(ca = mean(ca_obs), ec = mean(ec_obs), 
            si = mean(si_obs), so4 = mean(so4_obs)) %>%
  ungroup() 



# Get U.S. map data
us_map <- map_data("state")


##### CA
MapData <- MapData %>%
  mutate(Value = cut(ca, breaks = 5))
# Plot the map with locations
P1 <- ggplot() +
  # Add the U.S. map as the base layer
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  # Overlay the locations from your dataset
  geom_point(data = MapData, aes(x = lon, y = lat, color = Value), size = 4) +
  # Add labels and theme adjustments
  labs(title = "", color = "Mean of CA \n") +
  coord_fixed(1.3) +  # Ensure map proportions
  theme_minimal() +
  theme(
    axis.line = element_blank(),    # Remove axis lines
    axis.text = element_blank(),    # Remove axis text
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),    # Remove axis titles
    panel.grid = element_blank(),
    plot.margin = margin(0,0,0,0)
  ) +
  scale_color_brewer(palette = "YlOrRd")

P1


##### EC
MapData <- MapData %>%
  mutate(Value = cut(ec, breaks = 5))
# Plot the map with locations
P2 <- ggplot() +
  # Add the U.S. map as the base layer
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  # Overlay the locations from your dataset
  geom_point(data = MapData, aes(x = lon, y = lat, color = Value), size = 4) +
  # Add labels and theme adjustments
  labs(title = "", color = "Mean of EC \n") +
  coord_fixed(1.3) +  # Ensure map proportions
  theme_minimal() +
  theme(
    axis.line = element_blank(),    # Remove axis lines
    axis.text = element_blank(),    # Remove axis text
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),    # Remove axis titles
    panel.grid = element_blank(),
    plot.margin = margin(0,0,0,0)
  ) +
  scale_color_brewer(palette = "YlOrRd")

P2


##### SI
MapData <- MapData %>%
  mutate(Value = cut(si, breaks = 5))
# Plot the map with locations
P3 <- ggplot() +
  # Add the U.S. map as the base layer
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  # Overlay the locations from your dataset
  geom_point(data = MapData, aes(x = lon, y = lat, color = Value), size = 4) +
  # Add labels and theme adjustments
  labs(title = "", color = "Mean of SI \n") +
  coord_fixed(1.3) +  # Ensure map proportions
  theme_minimal() +
  theme(
    axis.line = element_blank(),    # Remove axis lines
    axis.text = element_blank(),    # Remove axis text
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),    # Remove axis titles
    panel.grid = element_blank(),
    plot.margin = margin(0,0,0,0)
  ) +
  scale_color_brewer(palette = "YlOrRd")

P3

##### SO4
MapData <- MapData %>%
  mutate(Value = cut(so4, breaks = 5))
# Plot the map with locations
P4 <- ggplot() +
  # Add the U.S. map as the base layer
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  # Overlay the locations from your dataset
  geom_point(data = MapData, aes(x = lon, y = lat, color = Value), size = 4) +
  # Add labels and theme adjustments
  labs(title = "", color = "Mean of SO4 \n") +
  coord_fixed(1.3) +  # Ensure map proportions
  theme_minimal() +
  theme(
    axis.line = element_blank(),    # Remove axis lines
    axis.text = element_blank(),    # Remove axis text
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),    # Remove axis titles
    panel.grid = element_blank(),
    plot.margin = margin(-10,-10,-10,-10)
  ) +
  scale_color_brewer(palette = "YlOrRd")

P4



ggarrange(P1, P2, P3, P4, ncol = 2, nrow = 2,
          align = "hv", 
          widths = c(.5, .5), 
          heights = c(.5, .5))

ggsave("Geographical distribution of components crude.pdf", width = 12, height = 7, units = "in", scale = 1.8)

