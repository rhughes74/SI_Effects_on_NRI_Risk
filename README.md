# Exploring the Relationship between Natural Hazards and Social Infrastructure
Hello! This repo contains the code used to assess the relationship between Natural Hazards and Social Infrastructure.

## Datasets Used
There are 2 datasets that are utilized:

- [ ] University of Michigan’s National Neighborhood Data Archive (NANDA) on Arts, Entertainment and Leisure, to capture social infrastructure density
and
- [ ] FEMA's National Risk Index (NRI) for Estimated Annual Losses attributable to Natural Hazards  (USD2020 Per Capita)


NANDA Data is the nanda_artsentleisure_Tract20_1990-2021_01P.csv obtained via: 
https://search.icpsr.umich.edu/search/search/nanda/studies 
"Parks by Census Tract, Arts Entertainment, and Leisure Establishments by Census Tract"

NRI Data is obtained from: https://hazards.fema.gov/nri/

Website links provided as the full datasets are quite large

## Datasets Included
To avoid file-size constraints, filtered-down versions of the data leveraged from NANDA and the NRI, are included as apart of the GitHub repo.
There are several Filtered-Down datasets included as a .rds file.
These datasets are saved as follows:

 - Filtered_Dataset_den.rds   =INFRASTRUCTURE DENSITY PER CAPITA (Only Census Tracts from Top 25 Cities)
 - Filtered_Dataset_den2.rds  =INFRASTRUCTURE DENSITY PER CAPITA (All Census Tracts)
 - Filtered_Dataset_aden.rds  =INFRASTRUCTURE DENSITY PER SQ. MILE (Only Census Tracts from Top 25 Cities)
 - Filtered_Dataset_aden2.rds =INFRASTRUCTURE DENSITY PER Sq. MILE (All Census Tracts)
 - Filtered_Dataset.rds (i.e., no suffix) = INFRASTRUCTURE COUNT.


## R Libraries Used:

```bash
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(broom.mixed)
library(readr)
library(dplyr)
library(lme4)
library(MuMIn)
library(data.table)
library(rsm) # for RSM
library(metR) # for contour plot labels in ggplot
library(patchwork) #To create combined plots
library(ggallin)
library(ggplot2)
library(units)
```

## Scripts Included
The GitHub Repo consists of 3 scripts.

- Data_Import_and_Filtering.R ~ To demonstrate how the Filtered_Datasets were made, and to facilitate replication.
- Linear_Model_Anaysis.R ~ A supplemental analysis creating a direct linear model of the relationship between NANDA and NRI
- Simulation_Anaysis.R ~ A simulation linear model of the relationship between NANDA and NRI. 

The Simulation_Analysis is used to produce the graphics and confidence interval results contained in the paper, whilst the other 2 scripts are supplemental to facilitate data transparency and replication.

## CSV's Included
Lastly, there are several CSVs included within the GitHub repo to demonstrate the exact results produced from the simulator that are illustrated with the graphics in the paper:

 - Adding_Additional_CommunityRiskFactor.csv
 - Adding_Additional_Density.csv
 - Adding_Additional_DensityPerSqMi.csv
 - Adding_Additional_Population.csv
 - Adding_Additional_Sites.csv
 - Adding_Additional_Sites_at_HighCRF.csv
 - Adding_Additional_Sites_at_LowCRF.csv
 - The_Effect_of_InfrastructureDensityPerSqMi_on_NRI_Annual_Loss_Per_Capita.csv

