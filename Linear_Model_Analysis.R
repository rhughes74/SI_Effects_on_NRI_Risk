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

#-----------Analyzing Density per Capita (25 cities)---------------------------------------------------------------------------------------------------------------
#Creating Linear Model per Infrastructure Type
Linear_Model1 = Filtered_Dataset_den %>% filter(den_type=='den_museums') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model2 = Filtered_Dataset_den %>% filter(den_type=='den_theatricalproductions') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model3 = Filtered_Dataset_den %>% filter(den_type=='den_amusementparks') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model4 = Filtered_Dataset_den %>% filter(den_type=='den_movietheaters') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model5 = Filtered_Dataset_den %>% filter(den_type=='den_zoosaquariumsgardens') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model6 = Filtered_Dataset_den %>% filter(den_type=='den_bingocardsgambling') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model7 = Filtered_Dataset_den %>% filter(den_type=='den_poolhallsbowlingalleys') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model8 = Filtered_Dataset_den %>% filter(den_type=='den_hotels') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model9 = Filtered_Dataset_den %>% filter(den_type=='den_casinohotels') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model10 = Filtered_Dataset_den %>% filter(den_type=='den_totartsentertainment') %>% summarize(lm(formula= (RISK_VALUE) ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model_Capita=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)




# Summary of the model
Linear_Model_Capita= Linear_Model_Capita %>% filter(term != "(Intercept)")
Linear_Model_Capita= Linear_Model_Capita %>% filter(term != "POPULATION")
Linear_Model_Capita=Linear_Model_Capita %>% mutate(estimate=scale(estimate))
hist((Linear_Model_Capita$estimate))
Linear_Model_Capita=Linear_Model_Capita %>% mutate(den_type=c('den_museums','den_theatricalproductions','den_amusementparks','den_movietheaters','den_zoosaquariumsgardens','den_bingocardsgambling','den_poolhallsbowlingalleys','den_hotels','den_casinohotels','den_totartsentertainment'))
Full_Capita <- full_join(Filtered_Dataset_den,Linear_Model_Capita, by = "den_type")



#Evaluating R-Squared
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


#-----------Analyzing Density per Capita (all census tracts)---------------------------------------------------------------------------------------------------------------
#Creating Linear Model per Infrastructure Type
Linear_Model1 = Filtered_Dataset_den2 %>% filter(den_type=='den_museums') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model2 = Filtered_Dataset_den2 %>% filter(den_type=='den_theatricalproductions') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model3 = Filtered_Dataset_den2 %>% filter(den_type=='den_amusementparks') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model4 = Filtered_Dataset_den2 %>% filter(den_type=='den_movietheaters') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model5 = Filtered_Dataset_den2 %>% filter(den_type=='den_zoosaquariumsgardens') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model6 = Filtered_Dataset_den2 %>% filter(den_type=='den_bingocardsgambling') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model7 = Filtered_Dataset_den2 %>% filter(den_type=='den_poolhallsbowlingalleys') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model8 = Filtered_Dataset_den2 %>% filter(den_type=='den_hotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model9 = Filtered_Dataset_den2 %>% filter(den_type=='den_casinohotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model10 = Filtered_Dataset_den2 %>% filter(den_type=='den_totartsentertainment') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model_Capita2=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
Linear_Model_Capita2= Linear_Model_Capita2 %>% filter(term != "(Intercept)")
Linear_Model_Capita2= Linear_Model_Capita2 %>% filter(term != "POPULATION")

Linear_Model_Capita2= Linear_Model_Capita2 %>% mutate(den_type=c('den_museums','den_theatricalproductions','den_amusementparks','den_movietheaters','den_zoosaquariumsgardens','den_bingocardsgambling','den_poolhallsbowlingalleys','den_hotels','den_casinohotels','den_totartsentertainment'))
Full_Capita2 <- full_join(Filtered_Dataset_den2,Linear_Model_Capita2, by = "den_type")

#--------------------------------------------------------------Analyzing via Density per Unit Area (all census tracts)------------------------------------------------------------------------------------------------------------
#Creating Linear Model per Infrastructure Type
Linear_Model1 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_museums') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model2 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_theatricalproductions') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model3 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_amusementparks') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model4 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_movietheaters') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model5 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_zoosaquariumsgardens') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model6 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_bingocardsgambling') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model7 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_poolhallsbowlingalleys') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model8 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_hotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model9 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_casinohotels') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model10 = Filtered_Dataset_aden2 %>% filter(den_type=='aden_totartsentertainment') %>% summarize(lm(formula= RISK_VALUE ~ Infrastructure_Density + POPULATION, data=. ) %>% broom::tidy())
Linear_Model_Area2=bind_rows(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
remove(Linear_Model1,Linear_Model2,Linear_Model3,Linear_Model4,Linear_Model5,Linear_Model6,Linear_Model7,Linear_Model8,Linear_Model9,Linear_Model10)
Linear_Model_Area2= Linear_Model_Area2 %>% filter(term != "(Intercept)")
Linear_Model_Area2= Linear_Model_Area2 %>% filter(term != "POPULATION")

Linear_Model_Area2= Linear_Model_Area2 %>% mutate(den_type=c('aden_museums','aden_theatricalproductions','aden_amusementparks','aden_movietheaters','aden_zoosaquariumsgardens','aden_bingocardsgambling','aden_poolhallsbowlingalleys','aden_hotels','aden_casinohotels','aden_totartsentertainment'))
Full_Area2 <- full_join(Filtered_Dataset_aden2,Linear_Model_Area2, by = "den_type")

#--------------------------------------------------------------Analyzing via Density per Unit Area (25 cities)------------------------------------------------------------------------------------------------------------
#Creating Linear Model per Infrastructure Type
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

# Scatterplot of Betas vs. Infrastructure Density per 1000 People
ggplot(Full_Capita, aes(x = estimate, y = Infrastructure_Density)) +
  geom_point(color = "blue", size = 3) +  # Custom point color and size
  labs(title = "Betas vs. Infrastructure Density per 1000 People", x = "Beta Coeff.", y = "Infrastructure Density per 1000 People")  # Titles and labels

# Scatterplot of Betas vs. Infrastructure Density per 1000 People
ggplot(Full_Capita2, aes(x = estimate, y = Infrastructure_Density)) +
  geom_point(color = "blue", size = 3) +  # Custom point color and size
  labs(title = "Betas vs. Infrastructure Density per 1000 People", x = "Beta Coeff.", y = "Infrastructure Density per 1000 People")  # Titles and labels
