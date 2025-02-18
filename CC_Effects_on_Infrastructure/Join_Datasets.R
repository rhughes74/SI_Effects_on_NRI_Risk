# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(readr)
library(dplyr)


load("CC_Effects_on_Infrastructure/LoadedData.RData")

#Joining by the X-Y Coords-----------------------------------------------------

#Using "data" as a placeholder for county level disaster damage data.
data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
tracts_transformed <- st_transform(all_tracts, st_crs(data_sf))

# Creating a 'geoid' column to identify the tract each point belongs to
data_sf <- data_sf %>%
  mutate(
    tract_fips10 = apply(st_intersects(data_sf, tracts_transformed, sparse = FALSE), 1, function(x) {
      geoids <- tracts_transformed$GEOID[x]  # Get GEOIDs
      paste(geoids, collapse = ", ")  # If multiple intersections, combine GEOIDs
    })
  )

#Joining the two datasets based on the GEOID value
result <- left_join(data_sf, nanda_artsentleisure_Tract10_1990_2021_01P, by = "tract_fips10")

# View the resulting data
print(result)