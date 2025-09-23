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
library(ggplot2)
library(units)
#library(nlme)

setwd("CC_Effects_on_Infrastructure")
#We discussed assessing the geographic variation per state

#-----------------------------------------------------------------Filtered Data-set--------------------------------------------------------------------------------------------------------------------------------------
Filtered_Dataset_den <- readRDS("Filtered_Dataset_den.rds")#Per capita, tracts from 25 cities, (1000 people)
Filtered_Dataset_aden <- readRDS("Filtered_Dataset_aden.rds")#Per area, tracts from 25 cities, (Sq. Miles)
Filtered_Dataset_den2 <- readRDS("Filtered_Dataset_den2.rds")#Per capita, all census tracts, (1000 people)
Filtered_Dataset_aden2 <- readRDS("Filtered_Dataset_aden2.rds")#Per area, all census tracts, (Sq. Miles)
all_tracts <- readRDS("all_tracts.rds")#'ALAND' from 'all_tracts' reflects the land-area in square meters
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
    sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE)
  )
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
#replace NA values with zero in columns col1 and col2
Temp=Filtered_Dataset_aden[Filtered_Dataset_aden$Infrastructure_Density !=0,]
Temp2=Filtered_Dataset_aden[Filtered_Dataset_aden$RISK_VALUE !=0,]

stats_coef <-
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
stats_coef <- stats_coef %>% mutate(avg_Var_Coef_Infra = ifelse(is.na(avg_Var_Coef_Infra), 0, avg_Var_Coef_Infra),
                                    avg_Var_Coef_RISK = ifelse(is.na(avg_Var_Coef_RISK), 0, avg_Var_Coef_RISK))
stats_coef <-
  Filtered_Dataset_aden %>%
  group_by(city) %>% 
  summarise(#Calculating the Coefficient of Variation
    min_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/max(RISK_VALUE, na.rm = TRUE),
    avg_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/mean(RISK_VALUE, na.rm = TRUE),
    max_Var_Coef_RISK=sd(RISK_VALUE, na.rm = TRUE)/min(Temp2$RISK_VALUE, na.rm = TRUE)
  )
stats_coef <- stats_coef %>% mutate(avg_Var_Coef_RISK = ifelse(is.na(avg_Var_Coef_RISK), 0, avg_Var_Coef_RISK))

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


stats_coef=stats_coef %>% mutate(min_Adjusted=min_Var_Coef_RISK/cities$total_area,Avg_Adjusted=avg_Var_Coef_RISK/cities$total_area,max_Adjusted=max_Var_Coef_RISK/cities$total_area)


#-----------Analyzing Density per Capita (25 cities)---------------------------------------------------------------------------------------------------------------
Linear_Model1 = Filtered_Dataset_den %>% filter(den_type=='den_museums') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model2 = Filtered_Dataset_den %>% filter(den_type=='den_theatricalproductions') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model3 = Filtered_Dataset_den %>% filter(den_type=='den_amusementparks') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model4 = Filtered_Dataset_den %>% filter(den_type=='den_movietheaters') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model5 = Filtered_Dataset_den %>% filter(den_type=='den_zoosaquariumsgardens') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model6 = Filtered_Dataset_den %>% filter(den_type=='den_bingocardsgambling') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model7 = Filtered_Dataset_den %>% filter(den_type=='den_poolhallsbowlingalleys') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model8 = Filtered_Dataset_den %>% filter(den_type=='den_hotels') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model9 = Filtered_Dataset_den %>% filter(den_type=='den_casinohotels') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model10 = Filtered_Dataset_den %>% filter(den_type=='den_totartsentertainment') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model_Capita=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)



#Since the risk-score is only from 2023 I can't model over time
# Summary of the model
Linear_Model_Capita= Linear_Model_Capita %>% filter(term != "(Intercept)")
Linear_Model_Capita= Linear_Model_Capita %>% filter(term != "POPULATION")
Linear_Model_Capita=Linear_Model_Capita %>% mutate(estimate=scale(estimate))
hist((Linear_Model_Capita$estimate))
Linear_Model_Capita=Linear_Model_Capita %>% mutate(den_type=c('den_museums','den_theatricalproductions','den_amusementparks','den_movietheaters','den_zoosaquariumsgardens','den_bingocardsgambling','den_poolhallsbowlingalleys','den_hotels','den_casinohotels','den_totartsentertainment'))
Full_Capita <- full_join(Filtered_Dataset_den,Linear_Model_Capita, by = "den_type")




r21=Filtered_Dataset_den %>% filter(den_type=='den_museums') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r21=summary(r21)[["r.squared"]]
r22=Filtered_Dataset_den %>% filter(den_type=='den_theatricalproductions') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r22=summary(r22)[["r.squared"]]
r23=Filtered_Dataset_den %>% filter(den_type=='den_amusementparks') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r23=summary(r23)[["r.squared"]]
r24=Filtered_Dataset_den %>% filter(den_type=='den_movietheaters') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r24=summary(r24)[["r.squared"]]
r25=Filtered_Dataset_den %>% filter(den_type=='den_zoosaquariumsgardens') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r25=summary(r25)[["r.squared"]]
r26=Filtered_Dataset_den %>% filter(den_type=='den_bingocardsgambling') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r26=summary(r26)[["r.squared"]]
r27=Filtered_Dataset_den %>% filter(den_type=='den_poolhallsbowlingalleys') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r27=summary(r27)[["r.squared"]]
r28=Filtered_Dataset_den %>% filter(den_type=='den_hotels') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r28=summary(r28)[["r.squared"]]
r29=Filtered_Dataset_den %>% filter(den_type=='den_casinohotels') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r29=summary(r29)[["r.squared"]]
r210=Filtered_Dataset_den %>% filter(den_type=='den_totartsentertainment') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r210=summary(r210)[["r.squared"]]
AllRSquared=c(r21,r22,r23,r24,r25,r26,r27,r28,r29,r210)
remove(r21,r22,r23,r24,r25,r26,r27,r28,r29,r210)
hist(AllRSquared)

#------Reference Code------
#Filtered_Dataset_den2=Combined_Dataset %>% 
#  select(year,RISK_VALUE,SOVI_SCORE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
#         ,den_museums,den_theatricalproductions, den_amusementparks,den_movietheaters,den_zoosaquariumsgardens,den_bingocardsgambling,den_poolhallsbowlingalleys,den_totartsentertainment,den_hotels,den_casinohotels) %>% 
#  pivot_longer(cols=c(den_museums,den_theatricalproductions, den_amusementparks,den_movietheaters,den_zoosaquariumsgardens,den_bingocardsgambling,den_poolhallsbowlingalleys,den_totartsentertainment,den_hotels,den_casinohotels),names_to="den_type",values_to="Infrastructure_Density")

#Filtering down to Cities of Interest-------------------------------------------------------------
#Filtered_Dataset_den <- inner_join(Filtered_Dataset_den2,all_tracts, by = "TRACTFIPS")
#Filtered_Dataset_den= Filtered_Dataset_den %>% select(den_type,RISK_VALUE, Infrastructure_Density,POPULATION)

#Filtering out to just the latest census, since NRI does not have year-over-year datapoints
#Filtered_Dataset_den2 = Filtered_Dataset_den2 %>% filter(year==2020)
#Filtered_Dataset_den2= Filtered_Dataset_den2 %>% select(den_type,RISK_VALUE, Infrastructure_Density,POPULATION)

#-----------Analyzing Density per Capita (all census tracts)---------------------------------------------------------------------------------------------------------------

Linear_Model1 = Filtered_Dataset_den2 %>% filter(den_type=='den_museums') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model2 = Filtered_Dataset_den2 %>% filter(den_type=='den_theatricalproductions') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model3 = Filtered_Dataset_den2 %>% filter(den_type=='den_amusementparks') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model4 = Filtered_Dataset_den2 %>% filter(den_type=='den_movietheaters') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model5 = Filtered_Dataset_den2 %>% filter(den_type=='den_zoosaquariumsgardens') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model6 = Filtered_Dataset_den2 %>% filter(den_type=='den_bingocardsgambling') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model7 = Filtered_Dataset_den2 %>% filter(den_type=='den_poolhallsbowlingalleys') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model8 = Filtered_Dataset_den2 %>% filter(den_type=='den_hotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model9 = Filtered_Dataset_den2 %>% filter(den_type=='den_casinohotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model10 = Filtered_Dataset_den2 %>% filter(den_type=='den_totartsentertainment') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model_Capita2=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
Linear_Model_Capita2= Linear_Model_Capita2 %>% filter(term != "(Intercept)")
Linear_Model_Capita2= Linear_Model_Capita2 %>% filter(term != "POPULATION")

Linear_Model_Capita2= Linear_Model_Capita2 %>% mutate(den_type=c('den_museums','den_theatricalproductions','den_amusementparks','den_movietheaters','den_zoosaquariumsgardens','den_bingocardsgambling','den_poolhallsbowlingalleys','den_hotels','den_casinohotels','den_totartsentertainment'))
Full_Capita2 <- full_join(Filtered_Dataset_den2,Linear_Model_Capita2, by = "den_type")



#--------------------------------------------------------------Section-END------------------------------------------------------------------------------------------------------------------------------------

#------Reference Code------
#Filtered_Dataset_aden2=Combined_Dataset %>% 
# select(year,RISK_VALUE,SOVI_SCORE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
#      ,aden_museums,aden_theatricalproductions, aden_amusementparks,aden_movietheaters,aden_zoosaquariumsgardens,aden_bingocardsgambling,aden_poolhallsbowlingalleys,aden_totartsentertainment,aden_hotels,aden_casinohotels) %>% 
#pivot_longer(cols=c(aden_museums,aden_theatricalproductions, aden_amusementparks,aden_movietheaters,aden_zoosaquariumsgardens,aden_bingocardsgambling,aden_poolhallsbowlingalleys,aden_totartsentertainment,aden_hotels,aden_casinohotels),names_to="den_type",values_to="Infrastructure_Density")

#Filtering down to Cities of Interest-------------------------------------------------------------
#Filtered_Dataset_aden <- inner_join(Filtered_Dataset_aden,all_tracts, by = "TRACTFIPS")
#Filtering out to just the latest census, since NRI does not have year-over-year datapoints
#Filtered_Dataset_aden2 = Filtered_Dataset_aden2 %>% filter(year==2020)

#Filtered_Dataset_aden2= Filtered_Dataset_aden2 %>% select(den_type,RISK_VALUE, Infrastructure_Density,POPULATION)

#--------------------------------------------------------------Analyzing via Density per Unit Area (all census tracts)------------------------------------------------------------------------------------------------------------

Linear_Model1 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_museums') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model2 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_theatricalproductions') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model3 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_amusementparks') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model4 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_movietheaters') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model5 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_zoosaquariumsgardens') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model6 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_bingocardsgambling') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model7 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_poolhallsbowlingalleys') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model8 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_hotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model9 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_casinohotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model10 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_totartsentertainment') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy(conf.int=TRUE))
Linear_Model_Area2=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
Linear_Model_Area2= Linear_Model_Area2 %>% filter(term != "(Intercept)")
Linear_Model_Area2= Linear_Model_Area2 %>% filter(term != "POPULATION")

Linear_Model_Area2= Linear_Model_Area2 %>% mutate(den_type=c('aden_museums','aden_theatricalproductions','aden_amusementparks','aden_movietheaters','aden_zoosaquariumsgardens','aden_bingocardsgambling','aden_poolhallsbowlingalleys','aden_hotels','aden_casinohotels','aden_totartsentertainment'))
Full_Area2 <- full_join(Filtered_Dataset_aden2,Linear_Model_Area2, by = "den_type")

#--------------------------------------------------------------Analyzing via Density per Unit Area (25 cities)------------------------------------------------------------------------------------------------------------
Linear_Model1 = Filtered_Dataset_aden %>% filter(den_type=='aden_museums') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model2 = Filtered_Dataset_aden %>% filter(den_type=='aden_theatricalproductions') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model3 = Filtered_Dataset_aden %>% filter(den_type=='aden_amusementparks') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model4 = Filtered_Dataset_aden %>% filter(den_type=='aden_movietheaters') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model5 = Filtered_Dataset_aden %>% filter(den_type=='aden_zoosaquariumsgardens') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model6 = Filtered_Dataset_aden %>% filter(den_type=='aden_bingocardsgambling') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model7 = Filtered_Dataset_aden %>% filter(den_type=='aden_poolhallsbowlingalleys') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model8 = Filtered_Dataset_aden %>% filter(den_type=='aden_hotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model9 = Filtered_Dataset_aden %>% filter(den_type=='aden_casinohotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model10 = Filtered_Dataset_aden %>% filter(den_type=='aden_totartsentertainment') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model_Area=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)

Linear_Model_Area= Linear_Model_Area %>% filter(term != "(Intercept)")
Linear_Model_Area= Linear_Model_Area %>% filter(term != "POPULATION")

Linear_Model_Area= Linear_Model_Area %>% mutate(den_type=c('aden_museums','aden_theatricalproductions','aden_amusementparks','aden_movietheaters','aden_zoosaquariumsgardens','aden_bingocardsgambling','aden_poolhallsbowlingalleys','aden_hotels','aden_casinohotels','aden_totartsentertainment'))
Full_Area <- full_join(Filtered_Dataset_aden,Linear_Model_Area, by = "den_type")

# Summary of the model

#Linear_Model_Area=Linear_Model_Area %>% mutate(estimate=scale(estimate))
hist((Linear_Model_Area$estimate))


r21=Filtered_Dataset_aden %>% filter(den_type=='aden_museums') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r21=summary(r21)[["r.squared"]]
r22=Filtered_Dataset_aden %>% filter(den_type=='aden_theatricalproductions') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r22=summary(r22)[["r.squared"]]
r23=Filtered_Dataset_aden %>% filter(den_type=='aden_amusementparks') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r23=summary(r23)[["r.squared"]]
r24=Filtered_Dataset_aden %>% filter(den_type=='aden_movietheaters') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r24=summary(r24)[["r.squared"]]
r25=Filtered_Dataset_aden %>% filter(den_type=='aden_zoosaquariumsgardens') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r25=summary(r25)[["r.squared"]]
r26=Filtered_Dataset_aden %>% filter(den_type=='aden_bingocardsgambling') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r26=summary(r26)[["r.squared"]]
r27=Filtered_Dataset_aden %>% filter(den_type=='aden_poolhallsbowlingalleys') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r27=summary(r27)[["r.squared"]]
r28=Filtered_Dataset_aden %>% filter(den_type=='aden_hotels') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r28=summary(r28)[["r.squared"]]
r29=Filtered_Dataset_aden %>% filter(den_type=='aden_casinohotels') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r29=summary(r29)[["r.squared"]]
r210=Filtered_Dataset_aden %>% filter(den_type=='aden_totartsentertainment') %>% lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
r210=summary(r210)[["r.squared"]]
AllRSquared_Area=c(r21,r22,r23,r24,r25,r26,r27,r28,r29,r210)
remove(r21,r22,r23,r24,r25,r26,r27,r28,r29,r210)
hist(AllRSquared_Area)


#--------------------------------------------------------------Plotting Results!------------------------------------------------------------------------------------------------------------

# Scatterplot of Betas vs. Infrastructure Density per Sq. Mile
ggplot(Full_Area, aes(x = estimate, y = Infrastructure_Density)) +
  geom_point(color = "blue", size = 3) +  # Custom point color and size
  labs(title = "Betas vs. Infrastructure Density per Sq. Mile", x = "Beta Coeff.", y = "Infrastructure Density per Sq. Mile")  # Titles and labels

# Scatterplot of Betas vs. Infrastructure Density per Sq. Mile
ggplot(Full_Area2, aes(x = estimate, y = Infrastructure_Density)) +
  geom_point(color = "blue", size = 3) +  # Custom point color and size
  labs(title = "Betas vs. Infrastructure Density per Sq. Mile", x = "Beta Coeff.", y = "Infrastructure Density per Sq. Mile")  # Titles and labels

# (As your level of social infrastructure increases, how much more risk are you experiencing?)

# Note neither NANDA nor NRI has median household income data saved unfortunately.

# Scatterplot of Betas vs. Infrastructure Density per 1000 People
ggplot(Full_Capita, aes(x = estimate, y = Infrastructure_Density)) +
  geom_point(color = "blue", size = 3) +  # Custom point color and size
  labs(title = "Betas vs. Infrastructure Density per 1000 People", x = "Beta Coeff.", y = "Infrastructure Density per 1000 People")  # Titles and labels

# Scatterplot of Betas vs. Infrastructure Density per 1000 People
ggplot(Full_Capita2, aes(x = estimate, y = Infrastructure_Density)) +
  geom_point(color = "blue", size = 3) +  # Custom point color and size
  labs(title = "Betas vs. Infrastructure Density per 1000 People", x = "Beta Coeff.", y = "Infrastructure Density per 1000 People")  # Titles and labels

# Note population is calculated per census tract so we can't just plot against one another

# TODO: Scatter plot of Betas vs. social vulnerability index

#ggplot(Full_Area, aes(x = estimate, y = SOVI_SCORE)) +
#  geom_point(color = "blue", size = 3) +  # Custom point color and size
#  labs(title = "Betas vs. Social Vulnerability Index", x = "Beta Coeff.", y = "Social Vulnerability Index")  # Titles and labels
# 


#--------------------------------------------------------------Section-END------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------TODO: Multi Level Model Section------------------------------------------------------------------------------------------------------------------------------------


#Note to self: examine if density of totarts entertainment, or emps (Employee count) or admission count has a different estimate. 
#(Density could be better if its a large census tract for example)
#NRI calculates Risk in dollars based upon the Expected Annual Loss to Buildings, People, and Agriculture; for each census tract.
#As such it makes sense there is quite a bit of residuals in the model. The NRI dataset, if we control for building value, the system should primarily
#be just examining what kinds of infrastructure effect people and agriculture the most in terms of economic value. 
#If anything we're describing a shortcoming in how FEMA prioritizes hazards (i.e., in primarily economic terms rather than human needs).

#Creating a Multilevel Model, per Type of Infrastructure, Grouping by State, Country and Tract
#MLM= Filtered_Dataset %>%
#  group_by(type) %>%
#  group_modify(~ {
    # Fit the mixed-effects model for each group
#    model <- lmer(RISK_VALUE ~ Infrastructure_Count +POPULATION+(1 |STATE)+ (1 |STATE:STCOFIPS), data = .x)
#    # Extract the tidy results for fixed effects
#    broom.mixed::tidy(model)})
#summary(MLM)

#MLM_den= Filtered_Dataset_den %>%
#  group_by(den_type) %>%
#  group_modify(~ {
    # Fit the mixed-effects model for each group
#    model <- lmer(RISK_VALUE ~ Infrastructure_Density +POPULATION+(1 |STATE)+ (1 |STATE:STCOFIPS), data = .x)
    # Extract the tidy results for fixed effects
#    broom.mixed::tidy(model)})
#summary(MLM_den)

#print("Interestingly the raw count per county appears to have a larger effect than the density per county per type of social infrastructure.")


#Examining the Correlation between Variables as a check:
#summary(lm(formula=Filtered_Dataset$TRACTFIPS ~ Filtered_Dataset$RISK_VALUE, data=Filtered_Dataset))
#summary(lm(formula=Filtered_Dataset$TRACTFIPS ~ Filtered_Dataset$Infrastructure_Count, data=Filtered_Dataset))
#summary(lm(formula=Filtered_Dataset$Infrastructure_Count ~ Filtered_Dataset$RISK_VALUE, data=Filtered_Dataset))
#print("TRACTFIPS Appears to have the biggest effect on the Infrastructure Count and Risk Score.")