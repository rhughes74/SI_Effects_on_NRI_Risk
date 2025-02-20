# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(readr)
library(dplyr)

# Loading in the NANDA Data -----------------------------------------------------

#nanda_artsentleisure_Tract20 <- read_csv("DATASETS/ICPSR_209163-V2/ICPSR_209163-V2/nanda_artsentleisure_1990-2021_CSVs/nanda_artsentleisure_1990-2021_CSVs/nanda_artsentleisure_Tract20_1990-2021_01P.csv")
#View(nanda_artsentleisure_Tract20)

#Splitting it in half to reduce the filesize for upload purposes
#n <- 1368448
#split into two data frames
#NANDA_Data_Pt1 <- nanda_artsentleisure_Tract20[row.names(nanda_artsentleisure_Tract20) %in% 1:n, ]
#NANDA_Data_Pt2 <- nanda_artsentleisure_Tract20[row.names(nanda_artsentleisure_Tract20) %in% (n+1):nrow(nanda_artsentleisure_Tract20), ]

NANDA_Data_Pt1 <-readRDS("NANDA_Data_Pt1.rds")
NANDA_Data_Pt2 <-readRDS("NANDA_Data_Pt2.rds")
NANDA_Data <- rbind(NANDA_Data_Pt1, NANDA_Data_Pt2)

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
#Combine it all
all_tracts <- bind_rows(tracts_list)

#Removing the duplicate data now that we have everything downloaded from Github
remove(NANDA_Data_Pt1)
remove(NANDA_Data_Pt2)
#Save for Reference for use in Subsequent Scripts
save.image("LoadedData.RData")
