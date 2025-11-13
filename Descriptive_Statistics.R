# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(broom.mixed)
library(readr)
library(dplyr)
library(ggplot2)
library(units)


#-----------------------------------------------------------------Filtered Data-set--------------------------------------------------------------------------------------------------------------------------------------
Filtered_Dataset_den <- readRDS("Datasets/Filtered_Dataset_den.rds")#Per capita, tracts from 25 cities, (1000 people)
Filtered_Dataset_aden <- readRDS("Datasets/Filtered_Dataset_aden.rds")#Per area, tracts from 25 cities, (Sq. Miles)
Filtered_Dataset_den2 <- readRDS("Datasets/Filtered_Dataset_den2.rds")#Per capita, all census tracts, (1000 people)
Filtered_Dataset_aden2 <- readRDS("Datasets/Filtered_Dataset_aden2.rds")#Per area, all census tracts, (Sq. Miles)
#--------------------------------------------------------------Summary Stats---------------------------------------------------------------------------------------------------------------------------------------------

#Evaluating the Variability per Type of Infrastructure--------------------------

#Per Capita
stats_den= Filtered_Dataset_den %>%
  group_by(den_type,year,STATE, COUNTY,city) %>% 
  #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
  #recent risk scores from Mar-2023
  summarise(
    mean_Infra_Den = mean(Infrastructure_Density, na.rm = TRUE),
    sd_Infra_Den = sd(Infrastructure_Density, na.rm = TRUE),
    mean_RISK_VALUE = mean(RISK_VALUE, na.rm = TRUE),
    sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE),
    max_EAL = max(RISK_VALUE/CRF_VALUE, na.rm = TRUE),
    min_EAL = min(RISK_VALUE/CRF_VALUE, na.rm = TRUE),
    max_EAL_VALB = max(EAL_VALB, na.rm = TRUE),
    min_EAL_VALB = min(EAL_VALB, na.rm = TRUE),
  )
#Range of Estimated Annual Losses Range (Ignoring Tracts with 0 Losses)
max(stats_den$max_EAL)
min(stats_den$min_EAL[stats_den$min_EAL>0])

#Per Sq. Mile
stats_aden= Filtered_Dataset_aden %>%
  group_by(den_type,year,STATE, COUNTY,city) %>% 
  #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
  #recent risk scores from Mar-2023
  summarise(
    mean_Infra_Den = mean(Infrastructure_Density, na.rm = TRUE),
    sd_Infra_Den = sd(Infrastructure_Density, na.rm = TRUE),
    Var_Coef_Infra=sd(Infrastructure_Density, na.rm = TRUE)/mean(Infrastructure_Density, na.rm = TRUE),
    mean_RISK_VALUE = mean(RISK_VALUE, na.rm = TRUE),
    sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE),
    Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/mean(RISK_VALUE, na.rm = TRUE)
  )

#--------------------------------------------------------Examining the Effects of Geography---------------------------------------------------------------------------------------------------------------------------------------------
#Note: Ignoring Census Tracts with No Infrastructure Density
Temp=Filtered_Dataset_aden[Filtered_Dataset_aden$Infrastructure_Density !=0,]
Temp2=Filtered_Dataset_aden[Filtered_Dataset_aden$RISK_VALUE !=0,]

#Calculating Coefficient of Variation per Infrastructure Type
stats_coef_Infr <-
  Filtered_Dataset_aden %>%
  group_by(den_type) %>% 
  summarise(#Calculating the Coefficient of Variation
    min_Var_Coef_Infra=sd(Infrastructure_Density, na.rm = TRUE)/max(Infrastructure_Density, na.rm = TRUE),
    avg_Var_Coef_Infra=sd(Infrastructure_Density, na.rm = TRUE)/mean(Infrastructure_Density, na.rm = TRUE),
    max_Var_Coef_Infra=sd(Infrastructure_Density, na.rm = TRUE)/min(Temp$Infrastructure_Density, na.rm = TRUE),
    min_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/max(RISK_VALUE, na.rm = TRUE),
    avg_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/mean(RISK_VALUE, na.rm = TRUE),
    max_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/min(Temp2$RISK_VALUE, na.rm = TRUE)
  )
stats_coef_Infr <- stats_coef_Infr %>% mutate(avg_Var_Coef_Infra = ifelse(is.na(avg_Var_Coef_Infra), 0, avg_Var_Coef_Infra),
                                    avg_Var_Coef_RISK = ifelse(is.na(avg_Var_Coef_RISK), 0, avg_Var_Coef_RISK))

#Calculating Coefficient of Variation per City Examined
stats_coef_cities <-
  Filtered_Dataset_aden %>%
  group_by(city) %>% 
  summarise(#Calculating the Coefficient of Variation
    min_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/max(RISK_VALUE, na.rm = TRUE),
    avg_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/mean(RISK_VALUE, na.rm = TRUE),
    max_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/min(Temp2$RISK_VALUE, na.rm = TRUE)
  )
stats_coef_cities <- stats_coef_cities %>% mutate(avg_Var_Coef_RISK = ifelse(is.na(avg_Var_Coef_RISK), 0, avg_Var_Coef_RISK))


#Adjusting Coefficient of Variation's to Account for Differences in City Size/Sprawl--------------------------------------------------------------------------------------------------------------

#Determining the Area (in sq. mi) of each of the 25 cities
options(tigris_class = "sf")
places <- places(state = unique(Filtered_Dataset_aden$STATE))
cities <- places %>% filter(NAME %in% c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia", "San Antonio", 
                                        "San Diego", "Dallas", "San Jose", "Austin", "Jacksonville", "Fort Worth", "Columbus", 
                                        "Indianapolis city (balance)", "Charlotte", "San Francisco", "Seattle", "Denver", "Washington", 
                                        "Nashville", "Oklahoma City", "El Paso", "Boston", "Portland"))
cities <- cities %>%
  mutate(area_mi2 = set_units(st_area(.), mi^2) %>% as.numeric())
cities=cities %>% select(NAME,area_mi2)
cities <- cities %>%
  group_by(NAME) %>%
  summarise(total_area = sum(area_mi2, na.rm = TRUE))

stats_coef_cities=stats_coef_cities %>% mutate(min_Adjusted=min_Var_Coef_RISK/cities$total_area,Avg_Adjusted=avg_Var_Coef_RISK/cities$total_area,max_Adjusted=max_Var_Coef_RISK/cities$total_area)
