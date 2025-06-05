# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(broom.mixed)
library(readr)
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(units)
library(spdep)
library(spatialreg)
#library(nlme)

setwd("CC_Effects_on_Infrastructure")
#We discussed assessing the geographic variation per state

#-----------------------------------------------------------------Filtered Data-set--------------------------------------------------------------------------------------------------------------------------------------
Filtered_Dataset_den <- readRDS("Filtered_Dataset_den.rds")#Per capita, tracts from 25 cities, (1000 people)
Filtered_Dataset_aden <- readRDS("Filtered_Dataset_aden.rds")#Per area, tracts from 25 cities, (Sq. Miles)
Filtered_Dataset_den2 <- readRDS("Filtered_Dataset_den2.rds")#Per capita, all census tracts, (1000 people)
Filtered_Dataset_aden2 <- readRDS("Filtered_Dataset_aden2.rds")#Per area, all census tracts, (Sq. Miles)
all_tracts <- readRDS("all_tracts.rds")#'ALAND' from 'all_tracts' reflects the land-area in square meters
#--------------------------------------------------------------WIP---------------------------------------------------------------------------------------------------------------------------------------------



#Spatial data
tracts_sf <- all_tracts
#Creating neighbors list using queen contiguity
tracts_nb <- poly2nb(tracts_sf, queen = TRUE)
#Converting to spatial weights list
tracts_weights <- nb2listw(tracts_nb, style = "W", zero.policy = TRUE)
#Fit Standard OLS Model

ols_model <- lm(nri_risk ~ infra_density, data = tracts_sf)
summary(ols_model)
# Moran’s I on residuals
moran_test <- moran.test(residuals(ols_model), listw = tracts_weights, zero.policy = TRUE)
print(moran_test)
#p-value < 0.05 == significant spatial autocorrelation
#Lag Model
lag_model <- lagsarlm(nri_risk ~ infra_density + median_income + poverty_rate,
                      data = tracts_sf, listw = tracts_weights, zero.policy = TRUE)
summary(lag_model)
#Spatial Error Model Fitting

error_model <- errorsarlm(nri_risk ~ infra_density + median_income + poverty_rate,
                          data = tracts_sf, listw = tracts_weights, zero.policy = TRUE)
summary(error_model)
#Model Comparison

AIC(ols_model, lag_model, error_model)

#Decaying Spatial Weights Matrix:

#Use Centroids for Distance Calculations
tracts_sf_centroids <- st_centroid(tracts_sf)
#Extract coordinates for distance calculations
coords <- st_coordinates(tracts_sf_centroids)
# Build a Distance-Based Neighbors List
# Set distance threshold in map units (meters if using EPSG: 5070 or 4326 degrees if lat/long)
# 50000 meters = 50 km
tracts_nb_dist <- dnearneigh(coords, d1 = 0, d2 = 50000)
# Check for isolated tracts
table(card(tracts_nb_dist) == 0)

#Apply a Decaying Function
#(Compute distances between neighbors)
distances <- nbdists(tracts_nb_dist, coords)
inv_dist_weights <- lapply(distances, function(x) 1 / (x + 1e-6))  # or try 1 / (x^2 + 1e-6)

# Convert to listw object
tracts_weights_decay <- nb2listw(tracts_nb_dist, glist = inv_dist_weights, style = "W", zero.policy = TRUE)


# Spatial Lag Model with decaying weights
lag_model_decay <- lagsarlm(nri_risk ~ infra_density + median_income + poverty_rate,
                            data = tracts_sf, listw = tracts_weights_decay, zero.policy = TRUE)
summary(lag_model_decay)

# Spatial Error Model
error_model_decay <- errorsarlm(nri_risk ~ infra_density + median_income + poverty_rate,
                                data = tracts_sf, listw = tracts_weights_decay, zero.policy = TRUE)
summary(error_model_decay)

#------------------------------------------------Notes & Best Practices
#Ensure data for CRS in meters (e.g., EPSG:5070 or 3857). In lat/lon (EPSG:4326), convert it first:
# tracts_sf <- st_transform(tracts_sf, crs = 5070)
#Experiment with different distance thresholds (d2)/ decay types (1/x vs 1/x^2) 
