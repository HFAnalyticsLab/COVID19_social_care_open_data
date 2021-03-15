#Cleaning care home residents deaths data and COVID-19 deaths
#The ONS data set is for England and Wales, so need to clean Wales data
#to just get England


# Library -----------------------------------------------------------------

library(curl)
library(ISOweek)
library(janitor)
library(readxl)
library(tidyverse)
library(plotly)
library(ISOweek)
library(readODS)
library(flipTime)


# Care home residents deaths ----------------------------------------------

chdeaths_all <- read_excel(here::here('sprint_2', 'data', "carehomeresidentsdeaths.xlsx"),
                           sheet = "Weekly registrations", skip = 1)

chdeaths_all<- chdeaths_all %>% 
  clean_names() %>% 
  remove_empty( c("rows", "cols")) 

chdeaths_all_t<-chdeaths_all %>% 
  t() %>% 
  as.data.frame() %>% 
  remove_rownames() %>% 
  select(V1:V6,V18) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  mutate(across(everything(), as.numeric)) %>% 
  rename("all_ch_2021"= names(.)[3], "all_ch_2020"= names(.)[4], "prev_ch_all_eng"= names(.)[6], "eng_wal"= names(.)[5], "wales_all_ch"= names(.)[7])  %>% 
  select(-eng_wal)
ch_all<-chdeaths_all_t %>% 
  select(week_number:week_ended, all_ch_2020:prev_ch_all_eng) %>% 
  rename(all_ch_deaths=all_ch_2020) %>% 
  mutate(week_ended=week_ended-371, wales_all_ch=0) %>% 
  bind_rows(chdeaths_all_t %>% 
              filter(!is.na(all_ch_2021)) %>% 
              select(week_number:week_ended, all_ch_2021, prev_ch_all_eng, wales_all_ch) %>% 
              rename(all_ch_deaths=all_ch_2021)) %>% 
  mutate(date=as.Date(week_ended, origin = "1899-12-30")) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  separate(., date2, sep="-", into=c("year", "week2", "month")) %>% 
  select(-c("year", "month")) %>% 
  drop_na()

##all_ch_deaths is for England and Wales, for this need to add Wales total to just get England
##prev_ch_all_eng is for England only


# Covid-19 care home deaths -----------------------------------------------

chdeaths_CV<-read_excel(here::here('sprint_2', 'data', "carehomeresidentsdeaths.xlsx"),
                        sheet = "COVID-19 weekly registrations", skip = 1)

chdeaths_CV<- chdeaths_CV %>% 
  clean_names() %>% 
  remove_empty( c("rows", "cols")) 

chdeaths_CV_t<-chdeaths_CV %>% 
  t() %>% 
  as.data.frame() %>% 
  remove_rownames() %>% 
  select(V1:V4,V15) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  mutate(across(everything(), as.numeric)) %>% 
  rename("CV_2021"= names(.)[3], "CV_2020"= names(.)[4], "wales_ch_cv"= names(.)[5])
ch_CV<-chdeaths_CV_t %>% 
  select(week_number:week_ended, CV_2020) %>% 
  rename(CV_ch_deaths=CV_2020) %>% 
  mutate(week_ended=week_ended-371,wales_ch_cv=0) %>% 
  bind_rows(chdeaths_CV_t %>% 
              filter(!is.na(CV_2021)) %>% 
              select(week_number:week_ended, CV_2021, wales_ch_cv) %>% 
              rename(CV_ch_deaths=CV_2021)) %>% 
  mutate(date=as.Date(week_ended, origin = "1899-12-30")) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  separate(., date2, sep="-", into=c("year", "week2", "month")) %>% 
  select(-c("year", "month")) %>% 
  drop_na()

W53_col<- c("week_ended","date", "week2","all_ch_deaths","CV_ch_deaths")
W53_val<-c("44197","2021-01-01","W53",2341,741)

W53<-as.data.frame(rbind(W53_col,W53_val)) %>% 
  row_to_names(1) %>% 
  mutate(week_ended=as.numeric(week_ended),
    date=as.Date(date), 
         all_ch_deaths=as.numeric(all_ch_deaths),
         CV_ch_deaths=as.numeric(CV_ch_deaths)) %>% 
  remove_rownames()

ch_deaths<-ch_all %>% 
  left_join (ch_CV) %>% 
  select(-week_number) %>% 
  bind_rows(W53) %>% 
  arrange(date) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


##CV_ch_deaths is for England and Wales and the Wales figure is missing for 2020
##so need to extract Wales data for 2020 to get the full picture

wales <- readRDS(here::here('sprint_2','data', 'care_home_residents_deaths_wales.rds'))

wales<- wales %>% 
  clean_names() %>% 
  remove_empty( c("rows", "cols")) 

wales_wide <-wales %>% 
  arrange(date_sort_order) %>% 
  filter(grepl(' [0-9]{4}', date_hierarchy)) %>% 
  select(data:date_code) %>% 
  mutate(data=as.numeric(data), date_code=flipTime::AsDate(date_code)) %>% 
  mutate(date2=(lubridate::ceiling_date(date_code, unit='week', week_start = 5))) %>% 
  filter(date_code >as.Date('2019-12-01'))
wales_all<-wales_wide %>% 
  filter(causeof_death_item_name_eng!="COVID19 Related") %>% 
  rename("other_ch_deaths"="data") %>% 
  select(other_ch_deaths, date_code, date2) %>% 
  left_join(wales_wide %>% 
              filter(causeof_death_item_name_eng=="COVID19 Related") %>% 
              rename("CV_ch_deaths"="data") %>% 
              select(CV_ch_deaths, date_code, date2)) %>% 
  mutate(CV_ch_deaths=replace_na(CV_ch_deaths, 0)) %>% 
  group_by(date2) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(all_ch_deaths=CV_ch_deaths+other_ch_deaths,date=date2ISOweek(date2)) %>% 
  separate(., date, sep="-", into=c("year", "week2", "month")) %>% 
  select(-c("year", "month")) %>% 
  drop_na()

ch_deaths_full<-ch_deaths %>%
  left_join(wales_all %>% 
              rename("wales_cv"= "CV_ch_deaths", "wales_all"= "all_ch_deaths",  "wales_other_ch_deaths"="other_ch_deaths", "date"="date2")) %>% 
  mutate(wales_all_ch=ifelse(wales_all_ch==0,wales_all,wales_all_ch),wales_ch_cv=ifelse(wales_ch_cv==0,wales_cv,wales_ch_cv)) %>% 
  select(-c(wales_all,wales_cv)) %>% 
  mutate(all_ch_deaths_eng=all_ch_deaths-wales_all_ch, CV_ch_deaths_eng=CV_ch_deaths-wales_ch_cv) %>% 
  select(-c(all_ch_deaths,CV_ch_deaths,wales_ch_cv,all_ch_deaths,wales_all_ch,wales_other_ch_deaths)) %>% 
  mutate(CV_ch_deaths_eng=replace_na(CV_ch_deaths_eng, 0)) %>% 
  mutate(other_ch_deaths_eng=all_ch_deaths_eng-CV_ch_deaths_eng, excess_deaths_eng=ifelse(week2=="W53", 0,all_ch_deaths_eng-prev_ch_all_eng))


# Adding COVID-19 deaths for everyone -------------------------------------

CV_weekly<-readRDS(here::here('sprint_2','data', 'CV_ENG_ONS.rds'))

CV_weekly_ENG<-CV_weekly %>% 
  clean_names() %>% 
  filter(geography=="England and Wales") %>% 
  rename("ENG_WAL"=nr_deaths) %>% 
  select(-geography) %>% 
  left_join(CV_weekly %>% 
              clean_names() %>% 
              filter(geography=="Wales") %>% 
              rename("WAL"=nr_deaths) %>% 
              select(-geography)) %>% 
  mutate(ENG=ENG_WAL-WAL) %>% 
  arrange(.,calendar_years, week_number, recorded_deaths) %>% 
  filter(recorded_deaths=="deaths-involving-covid-19-registrations") %>% 
  separate(., week_number, sep="-", into=c("X","week")) %>% 
  mutate(week=as.numeric(week)) %>% 
  select(-c(X, recorded_deaths, ENG_WAL, WAL)) %>% 
  arrange(., calendar_years, week) %>% 
  mutate(date=c(seq(as.Date("2020-01-03"),as.Date("2021-01-01"),by="week"), seq(as.Date("2021-01-08"),as.Date("2021-02-26"),by="week"))) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  separate(., date2, sep="-", into=c("year", "week2", "month")) %>% 
  select(-c("year", "month")) %>% 
  drop_na()

ch_deaths_full_ENG<- CV_weekly_ENG %>% 
  select(-c(week, calendar_years)) %>% 
  rename(CV_all=ENG) %>% 
  left_join(ch_deaths_full)



##saving data set
##date here is week ending and reporting starts from 1st January, so Friday-Thursday the following week

saveRDS(ch_deaths_full_ENG, here::here('sprint_2', 'data', 'clean', 'ONS_care_home_deaths_full_ENG.rds'))





