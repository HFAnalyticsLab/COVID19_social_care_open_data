##Cleaning the Outbreaks and Incidents data


# Loading library ---------------------------------------------------------

library(curl)
library(ISOweek)
library(dplyr)
library(janitor)
library(readODS)
library(readxl)
library(tidyverse)

# Outbreaks ---------------------------------------------------------------

outbreaks <- read_ods(here::here('sprint_2', 'data', "Care_home_outbreaks.ods"), sheet='PHE_centres', skip=1)


outbreaks<- outbreaks %>% 
  clean_names()

outbreaks_long <- outbreaks %>%
  select(c(-all_outbreaks, -number_of_care_homes, -phec15cd, -percentage_of_care_homes_that_have_reported_an_outbreak)) %>% 
  pivot_longer(-phe_centre, names_to = "week", values_to = "CV_outbreaks") %>% 
  group_by(week) %>% 
  summarise(CV_outbreaks = sum(CV_outbreaks)) %>% 
  mutate(week_start = as.Date(gsub("x", "", week), format = "%Y_%m_%d"),
         week = date2ISOweek(week_start))

# Calculate cumulative % of homes that had at least one outbreak
outbreaks_long <- outbreaks_long %>% 
  mutate(num_ch = outbreaks %>% select(number_of_care_homes) %>% sum(),
         CV_outbreaks_cum_pct = cumsum(100*CV_outbreaks/num_ch))
  
saveRDS(tot_ch, here::here('sprint_2', 'data', 'clean', 'care_home_total.rds'))

# Incidents ---------------------------------------------------------------

##National flu and COVID-19 surveillance reports

i3 <- read_excel(here::here('sprint_2', 'data', "Care_home_incident2.xlsx"),
                 sheet = "Figure 18. ARI Care Home", skip = 7)

incidents <-i3 %>% 
  clean_names() %>% 
  rename("CV_incidents"="sars_cov_2") %>% 
  select("week_number", "CV_incidents") %>% 
  mutate(week_start = if_else(week_number >= 27,
                              ISOweek2date(str_c("2020-W", str_pad(week_number, 2, side = "left", pad = "0"), "-1")),
                              ISOweek2date(str_c("2021-W", str_pad(week_number, 2, side = "left", pad = "0"), "-1"))), 
         week = date2ISOweek(week_start))


##Combining the incidents and outbreaks data 
incidents_full<- outbreaks_long %>% 
  full_join(incidents) %>% 
  fill(num_ch, .direction = "down") %>% 
  mutate(CV_incidents_cum_pct = cumsum(100*coalesce(CV_incidents,0)/num_ch))

##Date here is week starting, and started in Monday and then the following Sunday

saveRDS(incidents_full, here::here('sprint_2', 'data','clean','care_home_outbreaks_incidents.rds'))






