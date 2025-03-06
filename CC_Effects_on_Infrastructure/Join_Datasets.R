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
#library(nlme)

setwd("CC_Effects_on_Infrastructure")
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

#First checking for any discrepancies between datasets--------------------------
Missing=anti_join(NANDA_Data, NRI_Table_CensusTracts, by = "TRACTFIPS")
max(Missing$totpop,na.rm=TRUE)
Missing= Missing %>% pivot_longer(cols=c(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels),names_to="type",values_to="Infrastructure_Count")
max(Missing$Infrastructure_Count,na.rm=TRUE)
print("It appears that the only discrepancies are for uninhabited census tracts which hold no data")

#Filtering out uninhabited census tracts----------------------------------------
Combined_Dataset <- semi_join(NANDA_Data, NRI_Table_CensusTracts, by = "TRACTFIPS")
Combined_Dataset <- full_join(Combined_Dataset, NRI_Table_CensusTracts, by = "TRACTFIPS")

#Filtering out Irrelevant Cases-------------------------------------------------
Filtered_Dataset = Combined_Dataset %>% 
  select(year,RISK_SCORE, STATE, COUNTY, STCOFIPS, TRACTFIPS, count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels) %>% 
  pivot_longer(cols=c(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels),names_to="type",values_to="Infrastructure_Count")


#Filtering down to Cities of Interest-------------------------------------------------------------
colnames(all_tracts)[4]<-"TRACTFIPS"
all_tracts_temp= all_tracts %>% select(city,TRACTFIPS)

all_tracts_temp=st_drop_geometry(all_tracts_temp)

Filtered_Dataset <- inner_join(Filtered_Dataset,all_tracts_temp, by = "TRACTFIPS")

#Evaluating the Variability per Type of Infrastructure--------------------------
stats= Filtered_Dataset %>%
  group_by(type,year,STATE, COUNTY,city) %>% 
  #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
  #recent risk scores from Mar-2023
  summarise(
    mean_Infra_Count = mean(Infrastructure_Count, na.rm = TRUE),
    sd_Infra_Count = sd(Infrastructure_Count, na.rm = TRUE),
    mean_RISK_SCORE = mean(RISK_SCORE, na.rm = TRUE),
    sd_RISK_SCORE = sd(RISK_SCORE, na.rm = TRUE)
  )
#Analyzing Different Simulations!----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Seeing if the count of a specific kind of Infrastructure influences the RISK_SCORE and by how much?
Linear_Model = Filtered_Dataset %>% 
  group_by(year, type) %>% 
  reframe(lm(formula= RISK_SCORE ~ Infrastructure_Count, data=. ) %>% broom::tidy())
print("Notably results do not appear to vary much by year as the risk score is only from 2023")
# Summary of the model
summary(Linear_Model)


#Creating a Multilevel Model, per Type of Infrastructure, Grouping by State, Country and Tract
Filtered_Dataset$Risk_Score_Scaled = scale(Filtered_Dataset$RISK_SCORE)
MLM= Filtered_Dataset %>%
  group_by(type) %>%
  group_modify(~ {
    # Fit the mixed-effects model for each group
    model <- lmer(Risk_Score_Scaled ~ Infrastructure_Count_Scaled + (1 |TRACTFIPS), data = .x)
    
    # Extract the tidy results for fixed effects
    broom.mixed::tidy(model)})
#Notably this analysis produces a lot of warnings. This may be due to the fact the Infrastructure count has a very small effect on the Risk_score whilst the TRACTFIPS has a much larger one
summary(MLM)

#Examining the Correlation between Variables as a check:
summary(lm(formula=Filtered_Dataset$TRACTFIPS ~ Filtered_Dataset$RISK_SCORE, data=Filtered_Dataset))
summary(lm(formula=Filtered_Dataset$TRACTFIPS ~ Filtered_Dataset$Infrastructure_Count, data=Filtered_Dataset))
summary(lm(formula=Filtered_Dataset$Infrastructure_Count ~ Filtered_Dataset$RISK_SCORE, data=Filtered_Dataset))
print("TRACTFIPS Appears to have the biggest effect on the Infrastructure Count and Risk Score.")

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
