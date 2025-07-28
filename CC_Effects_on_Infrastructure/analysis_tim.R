# analysis_tim.R

# Load packages
library(sf)
library(tigris)
library(stringr)
library(tidyverse)
library(readr)
library(dplyr)
library(rsm) # for RSM
library(metR) # for contour plot labels in ggplot
library(patchwork) #To create combined plots

setwd(paste0(rstudioapi::getActiveProject(), "/CC_Effects_on_Infrastructure"))

# reformat dataset #######################################

data1 = read_rds("Filtered_Dataset.rds")  %>%
  select(year, city, county = COUNTY, geoid = TRACTFIPS, risk = RISK_VALUE, crf = CRF_VALUE, eal = EAL_VALB,  type = type, count = Infrastructure_Count, pop = POPULATION) %>%
  mutate(type = stringr::str_remove(type, "count_"))

data2 = read_rds("Filtered_Dataset_den.rds") %>% 
  select(year, city, county = COUNTY, geoid = TRACTFIPS, risk = RISK_VALUE, crf = CRF_VALUE, eal = EAL_VALB, type = den_type, den = Infrastructure_Density, pop = POPULATION) %>%
  mutate(type = stringr::str_remove(type, "den_"))

data3 = read_rds("Filtered_Dataset_aden.rds") %>%
  select(year, city, county = COUNTY, geoid = TRACTFIPS, risk = RISK_VALUE, crf = CRF_VALUE, eal = EAL_VALB,  type = den_type, aden = Infrastructure_Density, pop = POPULATION) %>%
  mutate(type = stringr::str_remove(type, "aden_"))

data1 %>%
  left_join(by = c("year", "city", "geoid", "type"),
            y = data2 %>% select(year, city, geoid, type, den)) %>%
  left_join(by = c("year", "city", "geoid", "type"),
            y = data3 %>% select(year, city, geoid, type, aden)) %>%
  mutate(category = type %>% dplyr::recode(
    "zoosaquariumsgardens" = "exhibits",
    "museums" = "exhibits",
    "theatricalproductions" = "entertainment",
    "movietheaters" = "entertainment",
    "amusementparks" = "entertainment",
    "totartsentertainment" = "entertainment",
    "poolhallsbowlingalleys" = "games",
    "bingocardsgambling" = "gambling",
    "casinohotels" = "gambling",
    "hotels" = "hotels", 
    .default = NA_character_)) %>%
  # narrow to include only populated tracts
  filter(pop > 0) %>%
  # We can retrieve the area for each tract by backtransforming
  # count / area = aden
  # count / aden = area
  # get area
  mutate(area = count / aden) %>%
  mutate(area = if_else(is.nan(area), NA_real_, area)) %>%
  
  # Join in 
  left_join(
    by = c("year", "city", "geoid"),
    y =   data %>%
      group_by(year, city, geoid) %>%
      summarize(any = sum(count, na.rm = TRUE) > 0)
  ) #%>%

 # saveRDS("data.rds")


  # aden and count
  
  # aden / count = 1 / area
  
rm(list = ls())


# evaluate ############################################
data = read_rds("data.rds")

data %>% head()


# How many have at least 1 type
data %>% summarize(any = sum(any, na.rm = TRUE) / n())

# Ideas to test out:


# 0. Social Businesses

# Social Infrastructure
# - community spaces
# - places of worship
# - parks
# - social businesses --> LARGE SCALE SOCIAL BUSINESSES

# --- do some social businesses HELP?
# --- do some social business HARM rather than HELP
# --- how much?

# -------------------------------------------------
# --- MVP: zoos, aquariums, museums, and gardens
# -------------------------------------------------


# 1. Zero-Inflation - are tracts with NO sites fundamentally different from tracts with at least 1 site?

# Almost all tracts have no social businesses.
# Strong zero-inflation
data %>% summarize(count = sum(aden == 0), percent = count / n())

# None in the outcome variable
data %>% summarize(count = sum(risk == 0), percent = count / n())



# --- use a complex model call a zero-inflation model
# --- does having ANY infra produced different outcomes? -- binary
# --- assuming you have at least 1 site, how does the site affect the outcome?




# 2. Transformations - are trends vanishing because of non-normal distributions?

# Strong right skew
data$risk %>% log() %>% hist() 
data %>% filter(aden > 0) %>% with(aden) %>% {log(.)} %>% hist()
data$pop %>% log() %>% hist()
data %>% filter(area > 0) %>% with(area) %>% log() %>% hist()
data$crf %>% hist()
data$eal %>% log() %>% hist()

# 2. Zoom into CLASSIC social businesses - total arts and entertainment 


# 3. LOSS PER CAPITA might be a more useful metric for us?
# RISK = units of dollars
#      = expected annual loss (EAL) x Community Risk Factor
#      = expected annual loss (EAL) x Social Vulnerability / Community Resilience

# This says, hey, your exposure to natural hazards suggests 
# that you'll lose this much money, BUT
# you're X amount socially vulnerable AND
# you're Y amount resilient, SO
# that will adjust your final outcome.

# SO - we need to turn RISK into a expected loss PER CAPITA measure,
# if we aim to compare 


# data %>% filter(type == "hotels") %>%
#   arrange(city, geoid)

# Do places with more social infrastructure see greater risk?

# Adjusting for area and population, how much does every additional site add?
# How much risk per social infrastructure site?


data %>%
  group_by(type) %>%
  reframe(
    # DOLLAR VALUE OF DAMAGE ~ COUNT + POPULATION + AREA
    lm(formula = log(eal / pop) ~ log(den + 0.01) + log(pop) + log(area) + I(risk / eal) ) %>%
      broom::glance()
  )




# risk = eal * community risk factor

data %>%
  filter(any > 0) %>%
  group_by(category, type) %>%
  reframe(
    lm(formula = log(eal / pop) ~ log(den) + log(pop) + log(area) + I(risk / eal) ) %>%
      broom::tidy() %>%
      filter(term == "log(den)")
  ) %>%
  mutate(stars = gtools::stars.pval(p.value))



data %>%
  select(year, city, geoid, pop, area) %>%
  summarize()


get_estimates = function(data, x = 1, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.50, ci = 0.95){
  
  alpha = 1 - ci
  # Let's see the impacts...
  stat = data %>%
    filter(any > 0) %>%
    group_by(category, type) %>%
    reframe(
      # RISK ~ COUNT + POPULATION + AREA
      {
        m = lm(formula = log(eal / pop) ~ log(den + 0.01) + log(pop) + log(area) + crf )
        newdata = tibble(
          den = quantile(den, na.rm = TRUE, prob = pden), 
          pop = quantile(pop, na.rm = TRUE, prob = ppop), 
          area = quantile(area, na.rm = TRUE, prob = parea), 
          crf = quantile(crf, na.rm = TRUE, prob = pcrf)
        )
        y0 = predict(m, newdata = newdata)
        y1 = predict(m, newdata = newdata %>% mutate(den = den + x) )
        se = broom::glance(m)$sigma
        tibble(y0 = y0, y1 = y1, se = se)
      }
    ) %>%
    group_by(category, type) %>%
    reframe(
      ysim0 = rnorm(n = 1000, mean = y0, sd = se) %>% exp(),
      ysim1 = rnorm(n = 1000, mean = y1, sd = se) %>% exp(),
      diff = ysim0 - ysim1
    ) %>%
    group_by(category,type) %>%
    summarize(
      estimate = median(diff, na.rm = TRUE),
      lower = quantile(diff, prob = alpha / 2),
      upper = quantile(diff, prob = ci + alpha/2), .groups = "drop"
    ) %>%
    mutate(x = x,pcrf=pcrf,pden=pden,ppop=ppop,parea=parea,bound_range=upper-lower)
  return(stat)
}


#get_estimates(data = data, x = 10, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.50, ci = 0.95)
#get_estimates(data = data, x = 10, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.25, ci = 0.95)
#get_estimates(data = data, x = 10, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.75, ci = 0.95)

#Adding additional sites
points_x = bind_rows(
  lapply(1:50, function(i) {
    get_estimates(data = data, x = i, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.50, ci = 0.95)
  })) %>%
  mutate(group = paste0(type, "-", x)) %>%
  arrange(desc(type), x)

#Adding additional sites (Low CRF Facet)
points_x_lowcrf = bind_rows(
  lapply(1:50, function(i) {
    get_estimates(data = data, x = i, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.25, ci = 0.95)
  })) %>%
  mutate(group = paste0(type, "-", x)) %>%
  arrange(desc(type), x)

#Adding additional sites (High CRF Facet)
points_x_hicrf = bind_rows(
  lapply(1:50, function(i) {
    get_estimates(data = data, x = i, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.75, ci = 0.95)
  })) %>%
  mutate(group = paste0(type, "-", x)) %>%
  arrange(desc(type), x)

#Increasing the count
points_den = bind_rows(
  lapply(seq(0.1, 0.95, by = 0.05), function(i) {
    get_estimates(data = data, x = 10, pden = i, ppop = 0.50, parea = 0.50, pcrf = 0.50, ci = 0.95)
  })) %>%
  mutate(group = paste0(type, "-", x)) %>%
  arrange(desc(type), x)

#Increasing the population
points_pop = bind_rows(
  lapply(seq(0.1, 0.95, by = 0.05), function(i) {
    get_estimates(data = data, x = 10, pden = 0.5, ppop = i, parea = 0.50, pcrf = 0.50, ci = 0.95)
  })) %>%
  mutate(group = paste0(type, "-", x)) %>%
  arrange(desc(type), x)

#Increasing the density per sq. mile
points_area = bind_rows(
  lapply(seq(0.1, 0.95, by = 0.05), function(i) {
    get_estimates(data = data, x = 10, pden = 0.50, ppop = 0.50, parea = i, pcrf = 0.50, ci = 0.95)
  })) %>%
  mutate(group = paste0(type, "-", x)) %>%
  arrange(desc(type), x)

#Increasing the CRF
points_crf = bind_rows(
  lapply(seq(0.1, 0.95, by = 0.05), function(i) {
    get_estimates(data = data, x = 10, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = i, ci = 0.95)
  })) %>%
  mutate(group = paste0(type, "-", x)) %>%
  arrange(desc(type), x)

# Plan:



# Contributions to the Literature:

# 1. Classification!
# Can we identify any common heroes?
# Can we identify any common villains?


# 2. Does the Community Risk Factor matter to the effect?
# Does varying the community risk factor change the effect?


# 3. EFFECT SIZE - how much social infra do you need to make a difference?
# Does increasing the rate of social infrastructure change the effect?


# Ribbon Plots (as you change 1 variable, what happens?)
#Note x=number of additional social infrastructure sites
#     pden corresponds to number of sites
#     ppop corresponds to per capita
#     parea corresponds to size of the region
#     pcrf corresponds to magnitude of the community risk factor

#Bidrectional scaling, or do multiple/separate plots
#Have no more than 7 different things on a plot (Rule of Thumb for DataViz)
  #Perhaps using the categories
#In the scales package, there's "scales::pseudo_log_trans"
# Ribbon Plots with multiple ribbons (as change 1 variable at level A, at level B, etc.) what happens?
ggplot() +
  geom_ribbon(data = points_x %>% filter(type=='zoosaquariumsgardens'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "zoosaquariumsgardens", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='theatricalproductions'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "theatricalproductions", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='poolhallsbowlingalleys'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "poolhallsbowlingalleys", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='museums'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "museums", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='movietheaters'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "movietheaters", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='hotels'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "hotels", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='casinohotels'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "casinohotels", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='bingocardsgambling'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "bingocardsgambling", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='amusementparks'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "amusementparks", alpha = 0.5))+
  geom_ribbon(data = points_x %>% filter(type=='totartsentertainment'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "totartsentertainment", alpha = 0.5))+
  labs(title="The Effect of Adding Additional Social Infrastructure Sites",x="Sites Added",y="Estimate (Log Estimated Annual Loss in USD, per Capita)")+
  theme_bw(base_size = 14)



Low=ggplot() +
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='zoosaquariumsgardens'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "zoosaquariumsgardens", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='theatricalproductions'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "theatricalproductions", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='poolhallsbowlingalleys'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "poolhallsbowlingalleys", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='museums'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "museums", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='movietheaters'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "movietheaters", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='hotels'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "hotels", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='casinohotels'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "casinohotels", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='bingocardsgambling'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "bingocardsgambling", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='amusementparks'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "amusementparks", alpha = 0.5))+
  geom_ribbon(data = points_x_lowcrf %>% filter(type=='totartsentertainment'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "totartsentertainment", alpha = 0.5))+
  labs(title="The Effect of Adding Additional Social Infrastructure Sites at Low CRF",x="Sites Added",y="Estimate (Log Estimated Annual Loss in USD, per Capita)")+
  theme_bw(base_size = 14)

Low

High= ggplot() +
  geom_ribbon(data = points_x_hicrf %>% filter(type=='zoosaquariumsgardens'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "zoosaquariumsgardens", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='theatricalproductions'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "theatricalproductions", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='poolhallsbowlingalleys'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "poolhallsbowlingalleys", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='museums'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "museums", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='movietheaters'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "movietheaters", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='hotels'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "hotels", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='casinohotels'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "casinohotels", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='bingocardsgambling'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "bingocardsgambling", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='amusementparks'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "amusementparks", alpha = 0.5))+
  geom_ribbon(data = points_x_hicrf %>% filter(type=='totartsentertainment'), mapping = aes(x = x, ymin =lower, ymax = upper, fill = "totartsentertainment", alpha = 0.5))+
  labs(title="The Effect of Adding Additional Social Infrastructure Sites at High CRF",x="Sites Added",y="Estimate (Log Estimated Annual Loss in USD, per Capita)")+
  theme_bw(base_size = 14)

High

ggplot() +
  geom_ribbon(data = points_den %>% filter(type=='zoosaquariumsgardens'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "zoosaquariumsgardens", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='theatricalproductions'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "theatricalproductions", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='poolhallsbowlingalleys'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "poolhallsbowlingalleys", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='museums'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "museums", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='movietheaters'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "movietheaters", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='hotels'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "hotels", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='casinohotels'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "casinohotels", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='bingocardsgambling'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "bingocardsgambling", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='amusementparks'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "amusementparks", alpha = 0.5))+
  geom_ribbon(data = points_den %>% filter(type=='totartsentertainment'), mapping = aes(x = pden, ymin =lower, ymax = upper, fill = "totartsentertainment", alpha = 0.5))+
  labs(title="The Effect of Increasing Social Infrastructure Density",x="Quantile of Social Infrastructure Density",y="Estimate (Log Estimated Annual Loss in USD, per Capita)")+
  theme_bw(base_size = 14)

ggplot() +
  geom_ribbon(data = points_pop %>% filter(type=='zoosaquariumsgardens'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "zoosaquariumsgardens", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='theatricalproductions'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "theatricalproductions", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='poolhallsbowlingalleys'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "poolhallsbowlingalleys", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='museums'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "museums", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='movietheaters'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "movietheaters", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='hotels'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "hotels", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='casinohotels'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "casinohotels", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='bingocardsgambling'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "bingocardsgambling", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='amusementparks'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "amusementparks", alpha = 0.5))+
  geom_ribbon(data = points_pop %>% filter(type=='totartsentertainment'), mapping = aes(x = ppop, ymin =lower, ymax = upper, fill = "totartsentertainment", alpha = 0.5))+
  labs(title="The Effect on Larger Population Census Tracts",x="Quantile of the Population",y="Estimate (Log Estimated Annual Loss in USD, per Capita)")+
  theme_bw(base_size = 14)

ggplot() +
  geom_ribbon(data = points_area %>% filter(type=='zoosaquariumsgardens'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "zoosaquariumsgardens", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='theatricalproductions'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "theatricalproductions", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='poolhallsbowlingalleys'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "poolhallsbowlingalleys", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='museums'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "museums", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='movietheaters'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "movietheaters", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='hotels'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "hotels", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='casinohotels'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "casinohotels", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='bingocardsgambling'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "bingocardsgambling", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='amusementparks'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "amusementparks", alpha = 0.5))+
  geom_ribbon(data = points_area %>% filter(type=='totartsentertainment'), mapping = aes(x = parea, ymin =lower, ymax = upper, fill = "totartsentertainment", alpha = 0.5))+
  labs(title="The Effect on Larger Area Census Tracts",x="Quantile of the Area (sq.mi)",y="Estimate (Log Estimated Annual Loss in USD, per Capita)")+
  theme_bw(base_size = 14)


ggplot() +
  geom_ribbon(data = points_crf %>% filter(type=='zoosaquariumsgardens'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "zoosaquariumsgardens", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='theatricalproductions'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "theatricalproductions", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='poolhallsbowlingalleys'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "poolhallsbowlingalleys", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='museums'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "museums", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='movietheaters'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "movietheaters", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='hotels'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "hotels", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='casinohotels'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "casinohotels", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='bingocardsgambling'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "bingocardsgambling", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='amusementparks'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "amusementparks", alpha = 0.5))+
  geom_ribbon(data = points_crf %>% filter(type=='totartsentertainment'), mapping = aes(x = pcrf, ymin =lower, ymax = upper, fill = "totartsentertainment", alpha = 0.5))+
  labs(title="The Effect of Increasing Community Risk Factor",x="Quantile of the Community Risk Factor",y="Estimate (Log Estimated Annual Loss in USD, per Capita)")+
  theme_bw(base_size = 14)



# RSM - tile plots

get_estimates_rsm = function(data, x = 1, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.50, ci = 0.95){
  
  alpha = 1 - ci
  # Let's see the impacts...
  stat = data %>%
    #First lets filter to census tracts that are not empty (i.e., they have at least 1 building of some kind of social infrastructure in them)
    filter(any > 0) %>% 
    group_by(category, type) %>%
    reframe(
      # RISK ~ COUNT + POPULATION + AREA
      {
        m = rsm(formula = log(eal / pop) ~ FO(log(den + 0.01), log(pop), log(area), crf) 
                + SO(log(den + 0.01), log(pop), log(area), crf), data=.)
        newdata = tibble(
          den = quantile(den, na.rm = TRUE, prob = pden), 
          pop = quantile(pop, na.rm = TRUE, prob = ppop), 
          area = quantile(area, na.rm = TRUE, prob = parea), 
          crf = quantile(crf, na.rm = TRUE, prob = pcrf)
        )
        y0 = predict(m, newdata = newdata)
        y1 = predict(m, newdata = newdata %>% mutate(den = den + x) )
        se = broom::glance(m)$sigma
        tibble(y0 = y0, y1 = y1, se = se)
      }
    ) %>%
    group_by(category, type) %>%
    reframe(
      ysim0 = rnorm(n = 1000, mean = y0, sd = se) %>% exp(),
      ysim1 = rnorm(n = 1000, mean = y1, sd = se) %>% exp(),
      diff = ysim0 - ysim1
    ) %>%
    group_by(category,type) %>%
    summarize(
      estimate = median(diff, na.rm = TRUE),
      lower = quantile(diff, prob = alpha / 2),
      upper = quantile(diff, prob = ci + alpha/2), .groups = "drop"
    ) %>%
    mutate(x = x,pcrf=pcrf,pden=pden,ppop=ppop,parea=parea,)
  return(stat)
}
# This freezes up: needs troubleshooting
#get_estimates_rsm(data = data, x = 10, pden = 0.50, ppop = 0.50, parea = 0.50, pcrf = 0.50, ci = 0.95)


rm(list = ls())

