library(readr)
library(dplyr)
data = read_rds("CC_Effects_on_Infrastructure/NRI_Table_CensusTracts_Pt5.rds")

data %>% head()

data = read_rds("CC_Effects_on_Infrastructure/NANDA_Data_Pt1.rds")
data %>% head()
