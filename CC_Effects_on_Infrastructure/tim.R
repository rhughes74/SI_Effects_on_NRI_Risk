library(dplyr)
library(readr)
library(broom)
library(lme4)
library(broom.mixed)
# install.packages(c("broom.mixed", "lmer"))
setwd("CC_Effects_on_Infrastructure")

Filtered_Dataset_den <- readRDS("Filtered_Dataset.rds")
# Filtered_Dataset_den <- readRDS("Filtered_Dataset_den.rds")
# Filtered_Dataset_den %>% glimpse()

# #Evaluating the Variability per Type of Infrastructure--------------------------
# stats= Filtered_Dataset %>%
#   group_by(type,year,STATE, COUNTY,city) %>% 
#   #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
#   #recent risk scores from Mar-2023
#   summarise(
#     mean_Infra_Count = mean(Infrastructure_Count, na.rm = TRUE),
#     sd_Infra_Count = sd(Infrastructure_Count, na.rm = TRUE),
#     mean_RISK_VALUE = mean(RISK_VALUE, na.rm = TRUE),
#     sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE)
#   )


stats


# Other Study: 
# -- dollar - infrastructure relationship
# --- general, at the county level, over time.



# This study:
# -- fancy models
# -- risk value metric
# -- a range of infrastructure counts
# -- tract level

# Across cities, how does the relationship between risk and social infrastructure vary?
# In New York it's good
# In San Diego it's bad
# How much

# Descriptive statistics study - but with big data
# value added would be stellar visuals + iterative models

# We're going to get a bunch of beta coefficients

# Can we explore how those beta coefficients vary across these cities?
# n = 25 - so not much
# ALTHOUGH - we could scale up to every county.

# Beta Coefficient - as Infrastructure increases by 1 site per sqkm, how much more risk do they encounter
# [This infrastructure type tends to see this much more dollars of risk]


# RQ: How does the risk felt by social infrastructure vary across cities/counties?


# Distribution of betas - what is the distribution OF risk felt by social infrastructure,
# where each value is a beta coefficient for one county

# Distribution of R2

# Scatterplot of Betas vs. Infrastructure Density
# (As your level of social infrastructure increases, how much more risk are you experiencing?)
# Scatterplot of Betas vs. median household income
# Scatterplot of Betas vs. population
# Scatterplot of Betas vs. social vulnerability index

# Analyze them! 
# Find some really interesting patterns.


# For each county, give me a beta
stats = Filtered_Dataset_den %>%
  group_by(STATE, STCOFIPS, type) %>%
  summarize(
    lm(formula = log(RISK_VALUE) ~ Infrastructure_Density + POPULATION, data = .) %>%
      broom::tidy()
  ) %>%
  filter(term != "(Intercept)")


stat$estimate %>% hist()  


  # group_modify(~ {
  #   # Fit the mixed-effects model for each group
  #   model <- lmer(log(RISK_VALUE) ~ Infrastructure_Count +POPULATION+(1 |STATE)+ (1 |STATE:STCOFIPS), data = .x)
  #   # Extract the tidy results for fixed effects
  #   broom.mixed::tidy(model)})


# 
# #Creating a Multilevel Model, per Type of Infrastructure, Grouping by State, Country and Tract
# MLM= Filtered_Dataset %>%
#   group_by(type) %>%
#   group_modify(~ {
#     # Fit the mixed-effects model for each group
#     model <- lmer(log(RISK_VALUE) ~ Infrastructure_Count +POPULATION+(1 |STATE)+ (1 |STATE:STCOFIPS), data = .x)
#     # Extract the tidy results for fixed effects
#     broom.mixed::tidy(model)})
# summary(MLM)
# 
# # Filtered_Dataset$RISK_VALUE %>% hist()
# # sum(Filtered_Dataset$RISK_VALUE == 0)
# # Risk value = in dollars - estimated annual loss, adjusted for social vulnerability
# 
# Filtered_Dataset %>% glimpse()
# stats_den= Filtered_Dataset_den %>%
#   group_by(den_type,year,STATE, COUNTY,city) %>% 
#   #Note the NANDA dataset holds information from 1990-2021 whilst the NRI scores only capture the most
#   #recent risk scores from Mar-2023
#   summarise(
#     mean_Infra_Count = mean(Infrastructure_Density, na.rm = TRUE),
#     sd_Infra_Count = sd(Infrastructure_Density, na.rm = TRUE),
#     mean_RISK_VALUE = mean(RISK_VALUE, na.rm = TRUE),
#     sd_RISK_VALUE = sd(RISK_VALUE, na.rm = TRUE)
#   )
