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
Combined_Dataset <- Combined_Dataset %>% filter(totpop != 0)

#Filtering out Irrelevant Cases-------------------------------------------------
#Note the RISK_VALUE column captures risk in $dollars while, the risk score is a relative percentile.
Filtered_Dataset = Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels) %>% 
  pivot_longer(cols=c(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels),names_to="type",values_to="Infrastructure_Count") 

Filtered_Dataset_den=Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         ,den_museums,den_theatricalproductions, den_amusementparks,den_movietheaters,den_zoosaquariumsgardens,den_bingocardsgambling,den_poolhallsbowlingalleys,den_totartsentertainment,den_hotels,den_casinohotels) %>% 
  pivot_longer(cols=c(den_museums,den_theatricalproductions, den_amusementparks,den_movietheaters,den_zoosaquariumsgardens,den_bingocardsgambling,den_poolhallsbowlingalleys,den_totartsentertainment,den_hotels,den_casinohotels),names_to="den_type",values_to="Infrastructure_Density")


#Filtering down to Cities of Interest-------------------------------------------------------------
colnames(all_tracts)[4]<-"TRACTFIPS"
all_tracts_temp= all_tracts %>% select(city,TRACTFIPS)

all_tracts_temp=st_drop_geometry(all_tracts_temp)

Filtered_Dataset <- inner_join(Filtered_Dataset,all_tracts_temp, by = "TRACTFIPS")
Filtered_Dataset_den <- inner_join(Filtered_Dataset_den,all_tracts_temp, by = "TRACTFIPS")

#Filtering out to just the latest census, since NRI does not have year-over-year datapoints
Filtered_Dataset = Filtered_Dataset %>% filter(year==2020)
Filtered_Dataset_den = Filtered_Dataset_den %>% filter(year==2020)

#Modifying the RISK_VALUE to control for Building_Value and Socioeconomic Factors:
Filtered_Dataset$RISK_VALUE = (Filtered_Dataset$RISK_VALUE/Filtered_Dataset$CRF_VALUE)-Filtered_Dataset$EAL_VALB
Filtered_Dataset_den$RISK_VALUE = (Filtered_Dataset_den$RISK_VALUE/Filtered_Dataset_den$CRF_VALUE)-Filtered_Dataset_den$EAL_VALB

#Save Final Result
saveRDS(Filtered_Dataset, "Filtered_Dataset.rds")
saveRDS(Filtered_Dataset_den, "Filtered_Dataset_den.rds")

#Skip to hear to just work off of the Filtered Dataset------------------------------------------------------------------------------------------------------
Filtered_Dataset <- readRDS("Filtered_Dataset.rds")
Filtered_Dataset_den <- readRDS("Filtered_Dataset_den.rds")

#Evaluating the Variability per Type of Infrastructure--------------------------
stats= Filtered_Dataset %>%
  group_by(type,year,STATE, COUNTY,city) %>% 
  #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
  #recent risk scores from Mar-2023
  summarise(
    mean_Infra_Count = mean(Infrastructure_Count, na.rm = TRUE),
    sd_Infra_Count = sd(Infrastructure_Count, na.rm = TRUE),
    mean_RISK_VALUE = mean(RISK_VALUE, na.rm = TRUE),
    sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE)
  )
stats_den= Filtered_Dataset_den %>%
  group_by(den_type,year,STATE, COUNTY,city) %>% 
  #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
  #recent risk scores from Mar-2023
  summarise(
    mean_Infra_Count = mean(Infrastructure_Density, na.rm = TRUE),
    sd_Infra_Count = sd(Infrastructure_Density, na.rm = TRUE),
    mean_RISK_VALUE = mean(RISK_VALUE, na.rm = TRUE),
    sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE)
  )

#Looking at sites with at least 1 x-variable, and population, social vulnerability index, building value, county year. 1 result by city and overall,
#Analyzing Different Simulations!----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Linear Model does not work since the RISK_VALUE is based upon the census tract (i.e., the result will appear )
#Linear_Model = Filtered_Dataset %>% 
#  group_by(type) %>% 
#  reframe(lm(formula= RISK_VALUE ~ Infrastructure_Count, data=. ) %>% broom::tidy())
#Since the risk-score is only from 2023 I can't model over time
# Summary of the model
#summary(Linear_Model)

#Note to self: examine if density of totarts entertainment, or emps (Employee count) or admission count has a different estimate. 
#(Density could be better if its a large census tract for example)
#NRI calculates Risk in dollars based upon the Expected Annual Loss to Buildings, People, and Agriculture; for each census tract.
#As such it makes sense there is quite a bit of residuals in the model. The NRI dataset, if we control for building value, the system should primarily
#be just examining what kinds of infrastructure effect people and agriculture the most in terms of economic value. 
#If anything we're describing a shortcoming in how FEMA prioritizes hazards (i.e., in primarily economic terms rather than human needs).


#Creating a Multilevel Model, per Type of Infrastructure, Grouping by State, Country and Tract
MLM= Filtered_Dataset %>%
  group_by(type) %>%
  group_modify(~ {
    # Fit the mixed-effects model for each group
    model <- lmer(RISK_VALUE ~ Infrastructure_Count +POPULATION+(1 |STATE)+ (1 |STATE:STCOFIPS), data = .x)
    # Extract the tidy results for fixed effects
    broom.mixed::tidy(model)})
summary(MLM)

MLM_den= Filtered_Dataset_den %>%
  group_by(den_type) %>%
  group_modify(~ {
    # Fit the mixed-effects model for each group
    model <- lmer(RISK_VALUE ~ Infrastructure_Density +POPULATION+(1 |STATE)+ (1 |STATE:STCOFIPS), data = .x)
    # Extract the tidy results for fixed effects
    broom.mixed::tidy(model)})
summary(MLM_den)

print("Interestingly the raw count per county appears to have a larger effect than the density per county per type of social infrastructure.")


#Examining the Correlation between Variables as a check:
summary(lm(formula=Filtered_Dataset$TRACTFIPS ~ Filtered_Dataset$RISK_VALUE, data=Filtered_Dataset))
summary(lm(formula=Filtered_Dataset$TRACTFIPS ~ Filtered_Dataset$Infrastructure_Count, data=Filtered_Dataset))
summary(lm(formula=Filtered_Dataset$Infrastructure_Count ~ Filtered_Dataset$RISK_VALUE, data=Filtered_Dataset))
print("TRACTFIPS Appears to have the biggest effect on the Infrastructure Count and Risk Score.")

#Note Going from State->County->Tract, appears to add R-squared in explanatory power per level of additional granularity, though not fully
#It appears the NRI itself explains very little (<1%) but the TRACTFIPS explains nearly 75%
#MLM <- lme(fixed= cbind(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels)  ~ RISK_SCORE + RISK_VALUE + RISK_SCORE * RISK_VALUE, random= ~ 1 | TRACTFIPS, data = Combined_Dataset)
#summary(MLM)
#r.squaredGLMM(MLM)
