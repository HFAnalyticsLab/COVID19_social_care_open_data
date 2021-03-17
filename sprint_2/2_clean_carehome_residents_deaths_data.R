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

#### Care home residents deaths - All cause England and Wales ----

chdeaths_all <- read_excel(here::here('sprint_2', 'data', "carehomeresidentsdeaths.xlsx"),
                           sheet = "Weekly registrations", skip = 3, n_max = 9)

chdeaths_all<- chdeaths_all %>% 
  remove_empty( c("rows", "cols")) 


chdeaths_all_t<-chdeaths_all %>%
  pivot_longer(-`Week number`, names_to = "week_number", values_to = "count") %>%
  rename("variable" = `Week number`) %>% 
  filter(variable != "Week ended") %>% 
  pivot_wider(names_from = "variable", values_from = "count") %>% 
  clean_names()
  
colnames(chdeaths_all_t) <- gsub("care_home_resident_", "ch_", colnames(chdeaths_all_t))
colnames(chdeaths_all_t) <- gsub("causes_", "", colnames(chdeaths_all_t))
colnames(chdeaths_all_t) <- gsub("_1", "", colnames(chdeaths_all_t))
colnames(chdeaths_all_t) <- gsub("_5", "", colnames(chdeaths_all_t))
colnames(chdeaths_all_t) <- gsub("average_of_corresponding_week_in_", "avg_", colnames(chdeaths_all_t))

# Rearrange to have 2020 and 2021 in one column
# week starts on Saturday, not Monday -> need to shift week starts
chdeaths_all_long <- chdeaths_all_t %>% 
  select(-ch_deaths_all_2021) %>% 
  rename("ch_deaths_all_england_and_wales" = "ch_deaths_all_2020") %>% 
  mutate(week_start = ISOweek2date(str_c("2020-W", 
                                         str_pad(week_number, 2, side = "left", pad = "0"), "-1")),
         week_start = week_start - days(2)) %>% 
  bind_rows(chdeaths_all_t %>% 
              select(-ch_deaths_all_2020) %>% 
              rename("ch_deaths_all_england_and_wales" = "ch_deaths_all_2021") %>% 
              mutate(week_start = ISOweek2date(str_c("2021-W", 
                                                     str_pad(week_number, 2, side = "left", pad = "0"),
                                                     "-1")),
                     week_start = week_start - days(2))) %>% 
  filter(!is.na(ch_deaths_all_england_and_wales))

# week 53 in 2020 was not included in the table, but as a footnote, need to add manually
chdeaths_all_long <- chdeaths_all_long %>% 
  add_row(week_number = "53",
          ch_deaths_all_england_and_wales = 2341,
          week_start = as.Date("2020-12-26"))

#### Care home residents deaths - CVOID England and Wales ----

chdeaths_CV<-read_excel(here::here('sprint_2', 'data', "carehomeresidentsdeaths.xlsx"),
                        sheet = "COVID-19 weekly registrations", skip = 3, n_max = 4, na = "-",
                        col_types = c("text", rep("numeric", 53)))

chdeaths_CV<- chdeaths_CV %>% 
  remove_empty( c("rows", "cols")) 

chdeaths_CV_t<-chdeaths_CV %>%
  pivot_longer(-`Week number`, names_to = "week_number", values_to = "count") %>%
  rename("variable" = `Week number`) %>% 
  filter(variable != "Week ended") %>% 
  pivot_wider(names_from = "variable", values_from = "count") %>% 
  clean_names()

colnames(chdeaths_CV_t) <- gsub("care_home_resident_deaths_involving_covid_19", "ch_covid_deaths", colnames(chdeaths_CV_t))
colnames(chdeaths_CV_t) <- gsub("_6", "", colnames(chdeaths_CV_t))
colnames(chdeaths_CV_t) <- gsub("20211", "2021", colnames(chdeaths_CV_t))

# Rearrange to have 2020 and 2021 in one column
chdeaths_CV_long <- chdeaths_CV_t %>% 
  select(-ch_covid_deaths_2021) %>% 
  rename("ch_covid_deaths_england_and_wales" = "ch_covid_deaths_2020") %>% 
  mutate(week_start = ISOweek2date(str_c("2020-W", 
                                         str_pad(week_number, 2, side = "left", pad = "0"), "-1")),
         week_start = week_start - days(2)) %>% 
  bind_rows(chdeaths_CV_t %>% 
              select(-ch_covid_deaths_2020) %>% 
              rename("ch_covid_deaths_england_and_wales" = "ch_covid_deaths_2021") %>% 
              mutate(week_start = ISOweek2date(str_c("2021-W", 
                                                     str_pad(week_number, 2, side = "left", pad = "0"),
                                                     "-1")),
                     week_start = week_start - days(2))) %>% 
  filter(!is.na(ch_covid_deaths_england_and_wales))


# week 53 in 2020 was not included in the table, but as a footnote, need to add manually
chdeaths_CV_long <- chdeaths_CV_long %>% 
  add_row(week_number = "53",
          ch_covid_deaths_england_and_wales = 741,
          week_start = as.Date("2020-12-26"))

#### Care home residents deaths - All cause and COVID in England 2020 ----
chdeaths_England_2020 <- read_excel(here::here('sprint_2', 'data', "carehomeresidentsdeaths_2020.xlsx"),
                           sheet = "Data", skip = 2, n_max = 6, na = "-",
                           col_types = c("text", rep("numeric", 54)))

chdeaths_England_2020<- chdeaths_England_2020 %>% 
  remove_empty( c("rows", "cols")) 


chdeaths_England_2020_t <- chdeaths_England_2020 %>%
  pivot_longer(-`Week number`, names_to = "week_number", values_to = "count") %>%
  rename("variable" = `Week number`) %>% 
  filter(variable != "Week ended (2020 only)" & variable != "England") %>% 
  pivot_wider(names_from = "variable", values_from = "count") %>% 
  clean_names()

colnames(chdeaths_England_2020_t) <- gsub("care_home_resident_", "ch_", colnames(chdeaths_England_2020_t))
colnames(chdeaths_England_2020_t) <- gsub("_causes", "", colnames(chdeaths_England_2020_t))
colnames(chdeaths_England_2020_t) <- gsub("_5", "", colnames(chdeaths_England_2020_t))

# Rearrange to have 2020 and 2021 in one column
# week starts on Saturday, not Monday -> need to shift week starts
chdeaths_England_2020_long <- chdeaths_England_2020_t %>% 
  rename("ch_deaths_all_england" = "ch_deaths_all",
         "ch_covid_deaths_england" = "ch_deaths_involving_covid_19") %>% 
  mutate(week_start = ISOweek2date(str_c("2020-W", 
                                         str_pad(week_number, 2, side = "left", pad = "0"), "-1")),
         week_start = week_start - days(2)) 

#### Care home residents deaths - All cause and COVID in Wales 2021 ----


chdeaths_all_wales <- read_excel(here::here('sprint_2', 'data', "carehomeresidentsdeaths.xlsx"),
                           sheet = "Weekly registrations", skip = 3, n_max = 20)

chdeaths_all_wales <- chdeaths_all_wales %>% 
  remove_empty( c("rows", "cols")) %>% 
  filter(`Week number` == "W92000004")  %>%
  select(-2)

chdeaths_all_wales <- chdeaths_all_wales %>% 
  pivot_longer(-`Week number`, names_to = "week_number", values_to = "ch_deaths_all_wales")


chdeaths_covid_wales <- read_excel(here::here('sprint_2', 'data', "carehomeresidentsdeaths.xlsx"),
                                 sheet = "COVID-19 weekly registrations", skip = 3, n_max = 16,
                                 col_types = c("text", "text", rep("numeric", 52)))

chdeaths_covid_wales <- chdeaths_covid_wales %>% 
  remove_empty( c("rows", "cols")) %>% 
  filter(`Week number` == "W92000004")  %>%
  select(-2)

chdeaths_covid_wales <- chdeaths_covid_wales %>% 
  pivot_longer(-`Week number`, names_to = "week_number", values_to = "ch_deaths_covid_wales")


chdeaths_wales <- chdeaths_all_wales %>% 
  left_join(chdeaths_covid_wales) %>% 
  select(-`Week number`) %>% 
  filter(!is.na(ch_deaths_all_wales)) %>% 
  mutate(week_start = ISOweek2date(str_c("2021-W", 
                                         str_pad(week_number, 2, side = "left", pad = "0"), "-1")),
         week_start = week_start - days(2)) 

  
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

combined_data <- chdeaths_all_long %>% 
  left_join(chdeaths_CV_long) %>% 
  left_join(chdeaths_England_2020_long) %>% 
  left_join(chdeaths_wales) %>% 
  left_join(CV_weekly) %>% 
  select(week_number, week_start, everything()) %>% 
  arrange(week_start)

combined_data <- combined_data %>% 
  mutate(ch_deaths_all_england = if_else(is.na(ch_deaths_all_england), ch_deaths_all_england_and_wales - ch_deaths_all_wales, ch_deaths_all_england),
         ch_covid_deaths_england = if_else(is.na(ch_covid_deaths_england), ch_covid_deaths_england_and_wales - ch_deaths_covid_wales, ch_covid_deaths_england))

saveRDS(combined_data, here::here('sprint_2', 'data', 'clean', 'ONS_care_home_deaths_full_ENG.rds'))


