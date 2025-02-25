# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(readr)
library(dplyr)


# Reference Code: I used this to parse in the .csv after downloading it from https://archive.icpsr.umich.edu/nanda/view/studies/209163 -----------------------------------------------------------------------

#nanda_artsentleisure_Tract20 <- read_csv("DATASETS/ICPSR_209163-V2/ICPSR_209163-V2/nanda_artsentleisure_1990-2021_CSVs/nanda_artsentleisure_1990-2021_CSVs/nanda_artsentleisure_Tract20_1990-2021_01P.csv")
#View(nanda_artsentleisure_Tract20)

# Number of parts to split into
#num_parts <- 5

#n <- nrow(NRI_Table_CensusTracts)  # total number of rows

# Calculate the size of each part
#part_size <- floor(n / num_parts)

# Create the 5 data frames
#NRI_Table_CensusTracts_Pt1 <- NRI_Table_CensusTracts[1:part_size, ]
#NRI_Table_CensusTracts_Pt2 <- NRI_Table_CensusTracts[(part_size+1):(2*part_size), ]
#NRI_Table_CensusTracts_Pt3 <- NRI_Table_CensusTracts[(2*part_size+1):(3*part_size), ]
#NRI_Table_CensusTracts_Pt4 <- NRI_Table_CensusTracts[(3*part_size+1):(4*part_size), ]
#NRI_Table_CensusTracts_Pt5 <- NRI_Table_CensusTracts[(4*part_size+1):n, ]

# Loading in the NANDA and FEMA Risk Index Data -----------------------------------------------------
NANDA_Data_Pt1 <-readRDS("NANDA_Data_Pt1.rds")
NANDA_Data_Pt2 <-readRDS("NANDA_Data_Pt2.rds")
NANDA_Data <- rbind(NANDA_Data_Pt1, NANDA_Data_Pt2)

NRI_Table_CensusTracts_Pt1 <- read_csv("NRI_Table_CensusTracts_Pt1.rds")
NRI_Table_CensusTracts_Pt2 <- read_csv("NRI_Table_CensusTracts_Pt2.rds")
NRI_Table_CensusTracts_Pt3 <- read_csv("NRI_Table_CensusTracts_Pt3.rds")
NRI_Table_CensusTracts_Pt4 <- read_csv("NRI_Table_CensusTracts_Pt4.rds")
NRI_Table_CensusTracts_Pt5 <- read_csv("NRI_Table_CensusTracts_Pt5.rds")
NRI_Table_CensusTracts <- rbind(NRI_Table_CensusTracts_Pt1, NRI_Table_CensusTracts_Pt2,NRI_Table_CensusTracts_Pt3,NRI_Table_CensusTracts_Pt4,NRI_Table_CensusTracts_Pt5)

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

#Removing the duplicate data now that we have everything downloaded from Github---------------------------------
remove(NANDA_Data_Pt1)
remove(NANDA_Data_Pt2)
remove(NRI_Table_CensusTracts_Pt1)
remove(NRI_Table_CensusTracts_Pt2)
remove(NRI_Table_CensusTracts_Pt3)
remove(NRI_Table_CensusTracts_Pt4)
remove(NRI_Table_CensusTracts_Pt5)
#Save for Reference for use in Subsequent Scripts------------------------------------
save.image("LoadedData.RData")
