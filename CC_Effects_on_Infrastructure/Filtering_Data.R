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
#-------------------------------------------------------------------------Full Data-set Loaded here------
load("LoadedData.RData")

colnames(NANDA_Data)[1]<-"TRACTFIPS"

#First checking for any discrepancies between datasets
Missing=anti_join(NANDA_Data, NRI_Table_CensusTracts, by = "TRACTFIPS")
max(Missing$totpop,na.rm=TRUE)
Missing= Missing %>% pivot_longer(cols=c(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels),names_to="type",values_to="Infrastructure_Count")
max(Missing$Infrastructure_Count,na.rm=TRUE)
print("It appears that the only discrepancies are for uninhabited census tracts which hold no data")

#Filtering out uninhabited census tracts
Combined_Dataset <- semi_join(NANDA_Data, NRI_Table_CensusTracts, by = "TRACTFIPS")
Combined_Dataset <- full_join(Combined_Dataset, NRI_Table_CensusTracts, by = "TRACTFIPS")
Combined_Dataset <- Combined_Dataset %>% filter(totpop != 0)

#Filtering out Irrelevant Cases
#Note the RISK_VALUE column captures risk in $dollars while, the risk score is a relative percentile.
Filtered_Dataset = Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels) %>% 
  pivot_longer(cols=c(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels),names_to="type",values_to="Infrastructure_Count") 

Filtered_Dataset_den=Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         ,den_museums,den_theatricalproductions, den_amusementparks,den_movietheaters,den_zoosaquariumsgardens,den_bingocardsgambling,den_poolhallsbowlingalleys,den_totartsentertainment,den_hotels,den_casinohotels) %>% 
  pivot_longer(cols=c(den_museums,den_theatricalproductions, den_amusementparks,den_movietheaters,den_zoosaquariumsgardens,den_bingocardsgambling,den_poolhallsbowlingalleys,den_totartsentertainment,den_hotels,den_casinohotels),names_to="den_type",values_to="Infrastructure_Density")

Filtered_Dataset_aden=Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         ,aden_museums,aden_theatricalproductions, aden_amusementparks,aden_movietheaters,aden_zoosaquariumsgardens,aden_bingocardsgambling,aden_poolhallsbowlingalleys,aden_totartsentertainment,aden_hotels,aden_casinohotels) %>% 
  pivot_longer(cols=c(aden_museums,aden_theatricalproductions, aden_amusementparks,aden_movietheaters,aden_zoosaquariumsgardens,aden_bingocardsgambling,aden_poolhallsbowlingalleys,aden_totartsentertainment,aden_hotels,aden_casinohotels),names_to="den_type",values_to="Infrastructure_Density")


#---------------------------------------------------------Filtering down to Cities of Interest-------------------------------------------------------------
colnames(all_tracts)[4]<-"TRACTFIPS"
all_tracts_temp= all_tracts %>% select(city,TRACTFIPS)

all_tracts_temp=st_drop_geometry(all_tracts_temp)

Filtered_Dataset <- inner_join(Filtered_Dataset,all_tracts_temp, by = "TRACTFIPS")
Filtered_Dataset_den <- inner_join(Filtered_Dataset_den,all_tracts_temp, by = "TRACTFIPS")
Filtered_Dataset_aden <- inner_join(Filtered_Dataset_aden,all_tracts_temp, by = "TRACTFIPS")

#Filtering out to just the latest census, since NRI does not have year-over-year datapoints
Filtered_Dataset = Filtered_Dataset %>% filter(year==2020)
Filtered_Dataset_den = Filtered_Dataset_den %>% filter(year==2020)
Filtered_Dataset_aden <- inner_join(Filtered_Dataset_aden,all_tracts_temp, by = "TRACTFIPS")


#--------------------------------------------------------------Saving/Reading Results------------------------------------------------------------------------------------------------------------------------------------

#Save Final Result
saveRDS(Filtered_Dataset, "Filtered_Dataset.rds")
saveRDS(Filtered_Dataset_den, "Filtered_Dataset_den.rds")
saveRDS(Filtered_Dataset_aden, "Filtered_Dataset_aden.rds") 