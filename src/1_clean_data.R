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



