# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(readr)
library(dplyr)
library(lme4)
library(MuMIn)
#library(nlme)

load("LoadedData.RData")

#Joining by the X-Y Coords-----------------------------------------------------

#Using "data" as a placeholder for county level disaster damage data.
#data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
#tracts_transformed <- st_transform(all_tracts, st_crs(data_sf))

# Creating a 'geoid' column to identify the tract each point belongs to
#data_sf <- data_sf %>%
#  mutate(
#    tract_fips10 = apply(st_intersects(data_sf, tracts_transformed, sparse = FALSE), 1, function(x) {
#      geoids <- tracts_transformed$GEOID[x]  # Get GEOIDs
#      paste(geoids, collapse = ", ")  # If multiple intersections, combine GEOIDs
#    })
#  )

colnames(NANDA_Data)[1]<-"TRACTFIPS"

#Joining the two datasets based on the GEOID value
Combined_Dataset <- left_join(NRI_Table_CensusTracts, NANDA_Data, by = "TRACTFIPS")

#Analyzing Different Simulations!----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creating a linear model to determine the correlation between Museum Count (as an Example) against Risk Index
Linear_Model <- lm(count_museums ~ RISK_SCORE + RISK_VALUE + RISK_SCORE * RISK_VALUE, data = Combined_Dataset)

#Linear Model Assessing All 'Count' Outputs at once
Linear_Model <- lm(cbind(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels) ~ RISK_SCORE + RISK_VALUE + RISK_SCORE * RISK_VALUE, data = Combined_Dataset)

# Summary of the model
summary(Linear_Model)

#Creating a Multilevel Model Grouping by State
MLM <- lmer(count_museums ~ RISK_SCORE + RISK_VALUE + RISK_SCORE * RISK_VALUE + (1 | STATE), data = Combined_Dataset)
summary(MLM)

#Creating a Multilevel Model Grouping by State
MLM <- lmer(count_museums ~ RISK_SCORE + RISK_VALUE + RISK_SCORE * RISK_VALUE + (1 | COUNTY), data = Combined_Dataset)
summary(MLM)

#Creating a Multilevel Model Grouping by Tract (Tried adding another layer but it added a ton to the processing time)
MLM <- lmer(count_museums ~ RISK_SCORE + RISK_VALUE + RISK_SCORE * RISK_VALUE + (1 | TRACTFIPS), data = Combined_Dataset)
summary(MLM)

r.squaredGLMM(MLM)
#Note Going from State->County->Tract, appears to add R-squared in explanatory power per level of additional granularity, though not fully
#It appears the NRI itself explains very little (<1%) but the TRACTFIPS explains nearly 75%
#MLM <- lme(fixed= cbind(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels)  ~ RISK_SCORE + RISK_VALUE + RISK_SCORE * RISK_VALUE, random= ~ 1 | TRACTFIPS, data = Combined_Dataset)
#summary(MLM)
#r.squaredGLMM(MLM)
