# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(broom.mixed)
library(readr)
library(dplyr)
library(data.table)

setwd("CC_Effects_on_Infrastructure")

# This script cleans in the two full-datasets once downloaded from their respective source pages online -----------------------------------------------------
# Note: The Full-Datasets are quite large (~GB scale!)


# Reference Code: I used this to parse in the .csv after downloading it from:
#   NANDA
#     https://archive.icpsr.umich.edu/nanda/view/studies/209163 -----------------------------------------------------------------------
#   NRI
#     https://hazards.fema.gov/nri/data-resources#csvDownload


# Loading in the NANDA Data -----------------------------------------------------
# NOTE: When NANDA is downloaded the data is broken up into separate folders for different versions
#       These different versions reflect how census tracts have had their bounds changed over the years
#       This is to read from the version using the latest census tract data
#       It is based off the tract bounds from 2020.


#The csv's are named similarly this one is obtained from going through the following sub-directories: ICPSR_209163-V2/ICPSR_209163-V2/nanda_artsentleisure_1990-2021_CSVs/nanda_artsentleisure_1990-2021_CSVs/
NANDA_Data <- read_csv("nanda_artsentleisure_Tract20_1990-2021_01P.csv")

View(NANDA_Data)

# Loading in the FEMA Risk Index Data -----------------------------------------------------
# NOTE: Risk Value reflects the calculated risk to natural hazards in dollars.

NRI_Table_CensusTracts <- read_csv("NRI_Table_CensusTracts.csv")

View(NRI_Table_CensusTracts)


# Loading in all the census block data for each city we are interested in-----------------------------------------------------
city_info <- data.frame(
  city = c("NYC", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia", "San Antonio", 
           "San Diego", "Dallas", "San Jose", "Austin", "Jacksonville", "Fort Worth", "Columbus", 
           "Indianapolis", "Charlotte", "San Francisco", "Seattle", "Denver", "Washington DC", 
           "Nashville", "Oklahoma City", "El Paso", "Boston", "Portland"),
  state_fips = c("36", "06", "17", "48", "04", "42", "48", "06", "48", "06", "48", "12", "48", 
                 "39", "18", "37", "06", "53", "08", "11", "47", "40", "48", "25", "41"),
  county_fips = c("061", "037", "031", "201", "013", "029", "029", "073", "113", "085", "453", "031", 
                  "439", "049", "097", "119", "075", "033", "031", "001", "037", "109", "141", "025", "051")
)

tracts_list <- list()

# Loop through each city and its corresponding state and county FIPS
for (i in 1:nrow(city_info)) {
  state_fips <- city_info$state_fips[i]
  county_fips <- city_info$county_fips[i]
  # Try to get census tracts for the state and county
  try({
    city_tracts <- tracts(state = state_fips, county = county_fips, year = 2020)
    # Add city name, state FIPS, and county FIPS as columns
    city_tracts$city <- city_info$city[i]
    city_tracts$state_fips <- state_fips
    city_tracts$county_fips <- county_fips
    # Append the data to the list
    tracts_list[[i]] <- city_tracts
  }, silent = TRUE)
}
#Combine it all--------------------------------------------------------------------------------------------------------------------------------------------------------
all_tracts <- bind_rows(tracts_list)
setnames(all_tracts, "GEOID", "TRACTFIPS")


#-------------------------------------------------------------------------Full Data-set Filtering----------------------------------------------------------------------
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

#---------------------------------------------------------Filtering To Key Variables of Interest-----------------------------------------------------------------------
#Note the RISK_VALUE column captures risk in USD2020 while, the risk score is a relative percentile.

#(ALL CENSUS TRACTS, INFRASTRUCTURE COUNT)
Filtered_Dataset = Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels) %>% 
  pivot_longer(cols=c(count_museums, count_theatricalproductions, count_amusementparks,count_movietheaters,count_zoosaquariumsgardens,count_bingocardsgambling,count_poolhallsbowlingalleys,count_totartsentertainment,count_hotels,count_casinohotels),names_to="type",values_to="Infrastructure_Count") 

#(ALL CENSUS TRACTS, INFRASTRUCTURE DENSITY PER CAPITA)
Filtered_Dataset_den2=Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         ,den_museums,den_theatricalproductions, den_amusementparks,den_movietheaters,den_zoosaquariumsgardens,den_bingocardsgambling,den_poolhallsbowlingalleys,den_totartsentertainment,den_hotels,den_casinohotels) %>% 
  pivot_longer(cols=c(den_museums,den_theatricalproductions, den_amusementparks,den_movietheaters,den_zoosaquariumsgardens,den_bingocardsgambling,den_poolhallsbowlingalleys,den_totartsentertainment,den_hotels,den_casinohotels),names_to="den_type",values_to="Infrastructure_Density")

#(ALL CENSUS TRACTS, INFRASTRUCTURE DENSITY PER SQ.MILE)
Filtered_Dataset_aden2=Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         ,aden_museums,aden_theatricalproductions, aden_amusementparks,aden_movietheaters,aden_zoosaquariumsgardens,aden_bingocardsgambling,aden_poolhallsbowlingalleys,aden_totartsentertainment,aden_hotels,aden_casinohotels) %>% 
  pivot_longer(cols=c(aden_museums,aden_theatricalproductions, aden_amusementparks,aden_movietheaters,aden_zoosaquariumsgardens,aden_bingocardsgambling,aden_poolhallsbowlingalleys,aden_totartsentertainment,aden_hotels,aden_casinohotels),names_to="den_type",values_to="Infrastructure_Density")

#---------------------------------------------------------Filtering To Applicable Year-----------------------------------------------------------------------
#Filtering out to just the latest census, since NRI does not have year-over-year datapoints
Filtered_Dataset = Filtered_Dataset %>% filter(year==2020)
Filtered_Dataset_den2 = Filtered_Dataset_den2 %>% filter(year==2020)
Filtered_Dataset_aden2 = Filtered_Dataset_aden2 %>% filter(year==2020)

#---------------------------------------------------------Filtering down to Cities of Interest-------------------------------------------------------------
colnames(all_tracts)[4]<-"TRACTFIPS"
all_tracts_temp= all_tracts %>% select(city,TRACTFIPS)

all_tracts_temp=st_drop_geometry(all_tracts_temp)

Filtered_Dataset <- inner_join(Filtered_Dataset,all_tracts_temp, by = "TRACTFIPS")
Filtered_Dataset_den <- inner_join(Filtered_Dataset_den2,all_tracts_temp, by = "TRACTFIPS")
Filtered_Dataset_aden <- inner_join(Filtered_Dataset_aden2,all_tracts_temp, by = "TRACTFIPS")
#--------------------------------------------------------------Saving/Reading Results------------------------------------------------------------------------------------------------------------------------------------
#Save Final Result
saveRDS(Filtered_Dataset, "Filtered_Dataset.rds")
saveRDS(Filtered_Dataset_den, "Filtered_Dataset_den.rds")
saveRDS(Filtered_Dataset_aden, "Filtered_Dataset_aden.rds")
#------------------------------------------------------------------------------------------------------------------------------------------------------------
# LEGEND for FILTER SUFFIX:
# _den=INFRASTRUCTURE DENSITY PER CAPITA (Only Census Tracts from Top 25 Cities)
# _den2=INFRASTRUCTURE DENSITY PER CAPITA (All Census Tracts)
# _aden=INFRASTRUCTURE DENSITY PER SQ. MILE (Only Census Tracts from Top 25 Cities)
# _aden2=INFRASTRUCTURE DENSITY PER Sq. MILE (All Census Tracts)
# no suffix = INFRASTRUCTURE COUNT.

#Save Image for use in Subsequent Scripts------------------------------------
save.image("LoadedData.RData")
