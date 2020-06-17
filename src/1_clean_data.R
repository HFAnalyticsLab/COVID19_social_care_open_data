library(readxl)
library(tidyverse)
library(janitor)
library(unpivotr)


## Load data table 8
home_care_table_8 <- read_excel(here::here('data', 'original data', "deathsinvolvingcovid19inthecaresectordataset.xlsx"),
                                sheet = "Table 8", skip = 2)
home_care_table_8 <- home_care_table_8 %>% 
  remove_empty( c("rows", "cols")) 
  


home_care_table_8 <- home_care_table_8 %>% 
  janitor::row_to_names(row_number = 1) %>% 
  clean_names()

## only keep covid deaths
home_care_table_8 <- home_care_table_8 %>% 
  select(date, ends_with('_2')) %>% 
  mutate(across(everything(), as.numeric))
  names(home_care_table_8) <- str_remove(names(home_care_table_8), '_2')
## Change names to snake case
home_care_table_8 <- janitor::clean_names(home_care_table_8) %>%
  mutate(date=as.Date(as.numeric(date), origin = "1899-12-30")) %>% 
  mutate(date=lubridate::date(date)) %>% 
  filter(!is.na(date))

## Save data

saveRDS(home_care_table_8, here::here('data', 'home_care_table_8.rds'))


## Load data table 9
home_care_table_9 <- read_excel(here::here('data', 'original data', "deathsinvolvingcovid19inthecaresectordataset.xlsx"),
           sheet = "Table 9", skip = 2)

## Change names to snake case
home_care_table_9 <- janitor::clean_names(home_care_table_9) %>% 
  mutate(date=lubridate::date(date)) %>% 
  remove_empty( c("rows", "cols")) 

## Save data

saveRDS(home_care_table_9, here::here('data', 'home_care_table_9.rds'))


## combine tables

home_care <- home_care_table_8 %>% 
  mutate(home_care_service_user_covid=hospital + home_care + elsewhere + not_stated, covid_19=1) %>% 
  select(date, home_care_service_user_covid) %>% 
  right_join( home_care_table_9, by='date') %>% 
  mutate(home_care_service_user_covid=replace_na(home_care_service_user_covid, 0)) %>% 
  mutate(home_care_service_user_non_covid=home_care_service_user-home_care_service_user_covid) %>% 
  select(-care_home_resident, -home_care_service_user) %>% 
  pivot_longer(-date, names_to = 'type') %>% 
  mutate(type=factor(type),
                type=fct_rev(type))
  
## Save data

saveRDS(home_care, here::here('data', 'home_care.rds'))


## Table 1 -----

home_care_table_1 <- read_excel(here::here('data', 'original data', "deathsinvolvingcovid19inthecaresectordataset.xlsx"),
                                sheet = "Table 1 ", skip = 2)
home_care_table_1 <- home_care_table_1 %>% 
  remove_empty( c("rows", "cols")) %>% 
  clean_names()

names(home_care_table_1)[1] <- 'date'

home_care_table_1_transposed <- home_care_table_1 %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = 'top_header') %>% 
  mutate(top_header=ifelse(str_detect(top_header,'x[0-9]'), NA, top_header)) %>% 
  fill(top_header) 
eng_ons <- home_care_table_1_transposed %>% 
  filter(top_header=='england_ons_data' | top_header=='date') %>% 
  select(-top_header) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(1) %>% 
  clean_names() %>% 
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>% 
  mutate(date=as.Date(date, origin = "1899-12-30")) %>% 
  filter(!is.na(all_deaths)) %>% 
  mutate(deaths_involving_covid_19=replace_na(deaths_involving_covid_19, 0),
           non_covid=all_deaths-deaths_involving_covid_19) %>% 
  rename(covid=deaths_involving_covid_19) %>% 
  mutate(covid_daily=ifelse(date=='2019-12-28',covid, covid-lag(covid,1)),
         non_covid_daily=ifelse(date=='2019-12-28',non_covid,non_covid-lag(non_covid,1)), 
         non_covid_daily_2019= ifelse(date=='2019-12-28',x2019_comparison,x2019_comparison-lag(x2019_comparison,1)) 
         )

## Save data

saveRDS(eng_ons, here::here('data', 'care_home_deaths_england.rds'))

