##Cleaning the Outbreaks and Incidents data


# Loading library ---------------------------------------------------------

library(curl)
library(ISOweek)
library(dplyr)
library(janitor)
library(readODS)
library(readxl)

# Outbreaks ---------------------------------------------------------------

outbreaks <- read_ods(here::here('sprint_2', 'data', "Care_home_outbreaks.ods"), sheet='PHE_centres', skip=1)

dat<-seq(as.Date("2020-03-09"),as.Date("2020-07-13"),by="week")

outbreaks<- outbreaks %>% 
  clean_names()

outbreaks_long<-outbreaks %>% 
  bind_rows( 
    outbreaks %>% 
      summarise_if(is.numeric, sum) %>% 
      mutate(phe_centre= "Total")
  ) %>% 
  filter(phe_centre=="Total") %>% 
  pivot_longer(contains('2020'), names_to='week', values_to='CV_new') %>% 
  mutate(date=dat, date2=date2ISOweek(dat)) %>% 
  select(week:date2)

#Total care homes

tot_ch<-outbreaks %>% 
  select(phe_centre,number_of_care_homes) %>% 
  summarise_if(is.numeric, sum)

saveRDS(tot_ch, here::here('sprint_2', 'data', 'clean', 'care_home_total.rds'))

# Incidents ---------------------------------------------------------------


##COVID-19 Surveillance data 

dat2<-seq(as.Date("2019-09-30"),as.Date("2020-09-21"),by="week")

cv1<-read_excel(here::here('sprint_2', 'data', "Care_home_incident1.xlsx"),
                sheet = "Figure 20. COVID-19 Incidents", skip = 8)

cv1<- cv1 %>% 
  clean_names() %>% 
  rename("week_number"= "x1", "CV"="care_home") %>% 
  select("week_number", "CV") 


i1<- read_excel(here::here('sprint_2', 'data', "Care_home_incident1.xlsx"),
                sheet = "Figure 19. ARI Incidents", skip = 8)

incidents1<- i1 %>% 
  clean_names() %>% 
  rename("week_number"= "x1", "ARI"="care_home") %>% 
  select("week_number", "ARI") %>% 
  left_join(cv1) %>% 
  mutate(date=dat2, date2=date2ISOweek(dat2))

##National flu and COVID-19 surveillance reports

dat3<-seq(as.Date("2020-06-29"),as.Date("2021-02-15"),by="week")

i2 <- read_excel(here::here('sprint_2', 'data', "Care_home_incident2.xlsx"),
                 sheet = "Figure 17. ARI IncidentsEngland", skip = 7)

i2<- i2 %>% 
  clean_names() %>% 
  rename("ARI"="care_home") %>% 
  select("week_number", "ARI")

i3 <- read_excel(here::here('sprint_2', 'data', "Care_home_incident2.xlsx"),
                 sheet = "Figure 18. ARI Care Home", skip = 7)

incidents2<-i3 %>% 
  clean_names() %>% 
  rename("CV"="sars_cov_2") %>% 
  select("week_number", "CV") %>% 
  left_join(i2) %>% 
  mutate(date=dat3, date2=date2ISOweek(dat3))

##the number are the same which means that we can just combine it as one long data set

incidents<-incidents1 %>% 
  full_join(incidents2) 


##combining the incidents and outbreaks data and adding the total number of care homes based on the last available data that we know which is the PHE report
incidents_full<-incidents %>% 
  full_join(outbreaks_long) %>% 
  select(-week) %>% 
  rename("CV_outbreaks"="CV_new", "CV_incidents"="CV")

##Date here is week starting, and started in Monday and then the following Sunday

saveRDS(incidents_full, here::here('sprint_2', 'data','clean','care_home_outbreaks_incidents.rds'))






