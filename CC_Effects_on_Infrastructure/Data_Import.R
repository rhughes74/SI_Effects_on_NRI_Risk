# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(readr)
library(dplyr)

# This script cleans in the two full-datasets once downloaded from their respective source pages online -----------------------------------------------------
# Note: The Full-Datasets are quite large (~GB scale!)


# Reference Code: I used this to parse in the .csv after downloading it from https://archive.icpsr.umich.edu/nanda/view/studies/209163 -----------------------------------------------------------------------

# Loading in the NANDA Data -----------------------------------------------------
#When NANDA is downloaded the data is broken up into separate folders for different versions---------
#These different versions reflect how census tracts have had their bounds changed over the years
#This is to read from the version using the latest census tract data
# It is based off the tract bounds from 2020.

#NANDA_Data <- read_csv("DATASETS/ICPSR_209163-V2/ICPSR_209163-V2/nanda_artsentleisure_1990-2021_CSVs/nanda_artsentleisure_1990-2021_CSVs/nanda_artsentleisure_Tract20_1990-2021_01P.csv")

#View(nanda_artsentleisure_Tract20)

# Loading in the FEMA Risk Index Data -----------------------------------------------------
#Risk Value reflects the calculated risk to natural hazards in dollars.
#NRI_Table_CensusTracts <- read_csv("NRI_Table_CensusTracts.csv")

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
#Combine it all-------------------------------------------------------------
all_tracts <- bind_rows(tracts_list)
setnames(all_tracts, "GEOID", "TRACTFIPS")

#Save for Reference for use in Subsequent Scripts------------------------------------
save.image("LoadedData.RData")
