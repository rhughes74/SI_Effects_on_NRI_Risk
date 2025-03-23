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

#-----------------------------------------------------------------Filtered Data-set--------------------------------------------------------------------------------------------------------------------------------------
Filtered_Dataset <- readRDS("Filtered_Dataset.rds")
Filtered_Dataset_den <- readRDS("Filtered_Dataset_den.rds")
Filtered_Dataset_aden <- readRDS("Filtered_Dataset_aden.rds")

#--------------------------------------------------------------Summary Stats---------------------------------------------------------------------------------------------------------------------------------------------

#Evaluating the Variability per Type of Infrastructure--------------------------
stats= Filtered_Dataset %>%
  group_by(type,year,STATE, COUNTY,city) %>% 
  #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
  #recent risk scores from Mar-2023
  summarise(
    mean_Infra_Count = mean(Infrastructure_Count, na.rm = TRUE),
    sd_Infra_Count = sd(Infrastructure_Count, na.rm = TRUE),
    mean_RISK_VALUE = mean(RISK_VALUE, na.rm = TRUE),
    sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE)
  )
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



#-----------Analyzing via Density per Capita---------------------------------------------------------------------------------------------------------------
Linear_Model1 = Filtered_Dataset_den %>% filter(den_type=='den_museums') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model2 = Filtered_Dataset_den %>% filter(den_type=='den_theatricalproductions') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model3 = Filtered_Dataset_den %>% filter(den_type=='den_amusementparks') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model4 = Filtered_Dataset_den %>% filter(den_type=='den_movietheaters') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model5 = Filtered_Dataset_den %>% filter(den_type=='den_zoosaquariumsgardens') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model6 = Filtered_Dataset_den %>% filter(den_type=='den_bingocardsgambling') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model7 = Filtered_Dataset_den %>% filter(den_type=='den_poolhallsbowlingalleys') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model8 = Filtered_Dataset_den %>% filter(den_type=='den_hotels') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model9 = Filtered_Dataset_den %>% filter(den_type=='den_casinohotels') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model10 = Filtered_Dataset_den %>% filter(den_type=='den_totartsentertainment') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model_Capita=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)



#Since the risk-score is only from 2023 I can't model over time
# Summary of the model
Linear_Model_Capita= Linear_Model_Capita %>% filter(term != "(Intercept)")
Linear_Model_Capita= Linear_Model_Capita %>% filter(term != "POPULATION")
Linear_Model_Capita=Linear_Model_Capita %>% mutate(estimate=scale(estimate))
hist((Linear_Model_Capita$estimate))




r2=Filtered_Dataset_den %>% filter(den_type=='den_museums') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_theatricalproductions') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_amusementparks') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_movietheaters') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_zoosaquariumsgardens') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_bingocardsgambling') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_poolhallsbowlingalleys') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_hotels') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_casinohotels') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_den %>% filter(den_type=='den_totartsentertainment') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)





#--------------------------------------------------------------Section-END------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------Analyzing via Density per Unit Area------------------------------------------------------------------------------------------------------------
Filtered_Dataset_aden=Combined_Dataset %>% 
  select(year,RISK_VALUE,CRF_VALUE,EAL_VALB,POPULATION, STATE, COUNTY, STCOFIPS, TRACTFIPS, 
         ,aden_museums,aden_theatricalproductions, aden_amusementparks,aden_movietheaters,aden_zoosaquariumsgardens,aden_bingocardsgambling,aden_poolhallsbowlingalleys,aden_totartsentertainment,aden_hotels,aden_casinohotels) %>% 
  pivot_longer(cols=c(aden_museums,aden_theatricalproductions, aden_amusementparks,aden_movietheaters,aden_zoosaquariumsgardens,aden_bingocardsgambling,aden_poolhallsbowlingalleys,aden_totartsentertainment,aden_hotels,aden_casinohotels),names_to="den_type",values_to="Infrastructure_Density")

#Filtering down to Cities of Interest-------------------------------------------------------------
Filtered_Dataset_aden <- inner_join(Filtered_Dataset_aden,all_tracts_temp, by = "TRACTFIPS")
#Filtering out to just the latest census, since NRI does not have year-over-year datapoints
Filtered_Dataset_aden = Filtered_Dataset_aden %>% filter(year==2020)

stats_aden= Filtered_Dataset_aden %>%
  group_by(den_type,year,STATE, COUNTY,city) %>% 
  #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
  #recent risk scores from Mar-2023
  summarise(
    mean_Infra_Den = mean(Infrastructure_Density, na.rm = TRUE),
    sd_Infra_Den = sd(Infrastructure_Density, na.rm = TRUE),
    mean_RISK_VALUE = mean(RISK_VALUE, na.rm = TRUE),
    sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE)
  )


Linear_Model1 = Filtered_Dataset_aden %>% filter(den_type=='aden_museums') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model2 = Filtered_Dataset_aden %>% filter(den_type=='aden_theatricalproductions') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model3 = Filtered_Dataset_aden %>% filter(den_type=='aden_amusementparks') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model4 = Filtered_Dataset_aden %>% filter(den_type=='aden_movietheaters') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model5 = Filtered_Dataset_aden %>% filter(den_type=='aden_zoosaquariumsgardens') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model6 = Filtered_Dataset_aden %>% filter(den_type=='aden_bingocardsgambling') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model7 = Filtered_Dataset_aden %>% filter(den_type=='aden_poolhallsbowlingalleys') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model8 = Filtered_Dataset_aden %>% filter(den_type=='aden_hotels') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model9 = Filtered_Dataset_aden %>% filter(den_type=='aden_casinohotels') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model10 = Filtered_Dataset_aden %>% filter(den_type=='aden_totartsentertainment') %>% summarize(lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model_Area=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)



#Since the risk-score is only from 2023 I can't model over time
# Summary of the model
Linear_Model_Area= Linear_Model_Area %>% filter(term != "(Intercept)")
Linear_Model_Area= Linear_Model_Area %>% filter(term != "POPULATION")
Linear_Model_Area=Linear_Model_Area %>% mutate(estimate=scale(estimate))
hist((Linear_Model_Area$estimate))



r2=Filtered_Dataset_aden %>% filter(den_type=='aden_museums') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_theatricalproductions') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_amusementparks') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_movietheaters') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_zoosaquariumsgardens') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_bingocardsgambling') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_poolhallsbowlingalleys') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_hotels') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_casinohotels') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)
r2=Filtered_Dataset_aden %>% filter(den_type=='aden_totartsentertainment') %>% lm(formula= log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. )
summary(r2)




#--------------------------------------------------------------Section-END------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------Multi Level Model Section------------------------------------------------------------------------------------------------------------------------------------


#Note to self: examine if density of totarts entertainment, or emps (Employee count) or admission count has a different estimate. 
#(Density could be better if its a large census tract for example)
#NRI calculates Risk in dollars based upon the Expected Annual Loss to Buildings, People, and Agriculture; for each census tract.
#As such it makes sense there is quite a bit of residuals in the model. The NRI dataset, if we control for building value, the system should primarily
#be just examining what kinds of infrastructure effect people and agriculture the most in terms of economic value. 
#If anything we're describing a shortcoming in how FEMA prioritizes hazards (i.e., in primarily economic terms rather than human needs).

#Creating a Multilevel Model, per Type of Infrastructure, Grouping by State, Country and Tract
MLM= Filtered_Dataset %>%
  group_by(type) %>%
  group_modify(~ {
    # Fit the mixed-effects model for each group
    model <- lmer(RISK_VALUE ~ Infrastructure_Count +POPULATION+(1 |STATE)+ (1 |STATE:STCOFIPS), data = .x)
    # Extract the tidy results for fixed effects
    broom.mixed::tidy(model)})
summary(MLM)

MLM_den= Filtered_Dataset_den %>%
  group_by(den_type) %>%
  group_modify(~ {
    # Fit the mixed-effects model for each group
    model <- lmer(RISK_VALUE ~ Infrastructure_Density +POPULATION+(1 |STATE)+ (1 |STATE:STCOFIPS), data = .x)
    # Extract the tidy results for fixed effects
    broom.mixed::tidy(model)})
summary(MLM_den)

print("Interestingly the raw count per county appears to have a larger effect than the density per county per type of social infrastructure.")


#Examining the Correlation between Variables as a check:
summary(lm(formula=Filtered_Dataset$TRACTFIPS ~ Filtered_Dataset$RISK_VALUE, data=Filtered_Dataset))
summary(lm(formula=Filtered_Dataset$TRACTFIPS ~ Filtered_Dataset$Infrastructure_Count, data=Filtered_Dataset))
summary(lm(formula=Filtered_Dataset$Infrastructure_Count ~ Filtered_Dataset$RISK_VALUE, data=Filtered_Dataset))
print("TRACTFIPS Appears to have the biggest effect on the Infrastructure Count and Risk Score.")