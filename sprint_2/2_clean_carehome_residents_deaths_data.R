#Cleaning care home residents deaths data and COVID-19 deaths
#The ONS data set is for England and Wales, so need to clean Wales data
#to just get England


# Library -----------------------------------------------------------------

library(curl)
library(ISOweek)
library(janitor)
library(readxl)
library(tidyverse)
library(tidylog)
library(lubridate)


#### Care home residents deaths in England  ----

chdeaths <- read_excel(here::here('sprint_2','data', "referencetable10052021114704.xlsx"),
                           sheet = "Table 1 ", skip = 4, col_types = c("numeric", "date", rep("numeric", 17)), n_max = 55) %>% 
  clean_names() %>% 
  select(week_number, week_ending, COVID19_deaths_cum = deaths_involving_covid_19_7, all_deaths_cum = all_deaths_8, 
         ch_deaths_all_avg_2015_2019_england_cum = x5_year_average_comparison_9,
         EngWales_COVID19_deaths_cum = deaths_involving_covid_19_3) %>% 
  mutate(week_ending = as.Date(week_ending, "%Y-%m-%d"),
         nonCOVID19_deaths_cum = all_deaths_cum - COVID19_deaths_cum) %>% 
  arrange(week_ending) %>% 
  mutate(ch_COVID19_deaths = COVID19_deaths_cum - replace_na(lag(COVID19_deaths_cum), 0),
         ch_nonCOVID19_deaths = nonCOVID19_deaths_cum - replace_na(lag(nonCOVID19_deaths_cum), 0),
         ch_all_deaths = all_deaths_cum - replace_na(lag(all_deaths_cum), 0),
         ch_deaths_all_avg_2015_2019_england = ch_deaths_all_avg_2015_2019_england_cum - replace_na(lag(ch_deaths_all_avg_2015_2019_england_cum), 0),
         EngWales_COVID19_deaths = EngWales_COVID19_deaths_cum - replace_na(lag(EngWales_COVID19_deaths_cum), 0)) %>% 
  select(week_number, week_ending, ch_COVID19_deaths, ch_nonCOVID19_deaths, ch_all_deaths, ch_deaths_all_avg_2015_2019_england,EngWales_COVID19_deaths) %>% 
  mutate(week_start = week_ending - 6)
  
  
# we don't have the 5-year average for week 53, so we will re-use week 52
# (this is what the ONS did in the latest deahts in the care sector analysis, May 2021)

# Adding COVID-19 deaths for everyone in England -------------------------------------

CV_weekly <- readRDS(here::here('sprint_2','data', 'CV_ENG_ONS.rds'))

CV_weekly <- CV_weekly %>% 
  pivot_wider(names_from = "geography", values_from = "nr_deaths") %>% 
  clean_names() %>% 
    mutate(covid_deaths_england = england_and_wales - wales) %>% 
  filter(recorded_deaths=="deaths-involving-covid-19-registrations") %>% 
  mutate(week_start = ISOweek2date(str_c(calendar_years , "-W", 
                                         str_pad(gsub("week-", "", week_number), 2, side = "left", pad = "0"), "-1")),
         week_start = week_start - days(2)) %>% 
  arrange(week_start)

CV_weekly <- CV_weekly %>% 
  select(-recorded_deaths, - week_number, - wales, -england_and_wales, -calendar_years)
  
   
# Combine all data sets ---------------------------------------------------

combined_data <- chdeaths %>% 
  left_join(CV_weekly) %>% 
  select(week_number, week_start, everything()) %>% 
  arrange(week_start)

saveRDS(combined_data, here::here('sprint_2', 'data', 'clean', 'ONS_care_home_deaths_full_ENG.rds'))


