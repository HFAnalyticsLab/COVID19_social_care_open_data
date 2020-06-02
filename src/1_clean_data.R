library(readxl)
library(tidyverse)

## Load data table 9
home_care_table_9 <- read_excel(here::here('data', 'original data', "deathsinvolvingcovid19inthecaresectordataset.xlsx"),
           sheet = "Table 9", skip = 2)

## Change names to snake case
home_care_table_9 <- janitor::clean_names(home_care_table_9) %>% 
  mutate(date=lubridate::date(date))

## Save data

saveRDS(home_care_table_9, here::here('data', 'home_care_table_9.rds'))
