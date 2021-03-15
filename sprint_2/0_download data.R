#Downloading the Incident/Outbreaks data sets 
#and deaths of care home residents

library(statswalesr)
library(curl)
library(tidyverse)
library(monstR)
library(data.table)
library(here)

# COVID-19 Outbreaks and Incidents ----------------------------------------

##PHE outbreaks data 
##data the first data point is dated 9/3/2020 and covers the Monday to following Sunday so 9th-15th March

link <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/891406/Care_home_outbreaks_of_COVID-19_Management_Information.ods'

destfile <- here::here('sprint_2', 'data', "Care_home_outbreaks.ods")
curl_download(link, destfile = destfile)


## COVID-19 surveillance data started publishing on 23rd of April to 2nd October (for week 16 to week 39) 
##week starts on monday and ends on sunday and is reported on Thursday
##week 16= 13th April-19th April
##From week 27 so reporting on week 26 (22nd June-28th June they started reporting COVID-19 specific outbreaks)

link <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/923669/Weekly_COVID19_report_data_w40.xlsx'

destfile <- here::here('sprint_2', 'data', "Care_home_incident1.xlsx")
curl_download(link, destfile = destfile)



## National flu and COVID-19 surveillance data 
##This report started publishing from 8th October-25th Feb (for weeks 40-7). 
##week starts on monday and ends on sunday and is reported on Thursday
##week 40 is 28th Sept-4th Oct 

link <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/964734/Weekly_Influenza_and_COVID19_report_data_W8.xlsx'

destfile <- here::here('sprint_2', 'data', "Care_home_incident2.xlsx")
curl_download(link, destfile = destfile)


# Care home residents deaths and COVID-19 deaths --------------------------

##there are currently different sources for this and will edit depending on 
##which one we use on the final version 


##CQC Deaths of care home residents 2020

link<-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/numberofdeathsincarehomesnotifiedtothecarequalitycommissionengland/2020/20210211coviddeathnotifs2020only.xlsx'

destfile <- here::here('sprint_2', 'data', "Care_homedeaths2020.xlsx")
curl_download(link, destfile = destfile)

##CQC Deaths of care home residents 2021

link<-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/numberofdeathsincarehomesnotifiedtothecarequalitycommissionengland/2021/20210214cqccoviddeathnotifications.xlsx'

destfile <- here::here('sprint_2', 'data', "Care_homedeaths2021.xlsx")
curl_download(link, destfile = destfile)


##Deaths in care home residents from previous data set that is no longer being updated 

link <- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsinvolvingcovid19inthecaresectorenglandandwales/current/julydeathsinvolvingcovid19inthecaresectordataset02072020155122.xlsx'

destfile <- here::here('sprint_2', 'data', "deathsinvolvingcovid19inthecaresectordataset.xlsx")
curl_download(link, destfile = destfile)


##ONS Deaths in care home residents the new update

link<-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/carehomeresidentdeathsregisteredinenglandandwalesprovisional/2021/weeklydeathscarehomesweek72021.xlsx'

destfile <- here::here('sprint_2', 'data', "carehomeresidentsdeaths.xlsx")
curl_download(link, destfile = destfile)

##Covid-19 deaths in general from ONS


##getting the weekly mortality data set from ONS
datasets <- ons_available_datasets()

##Finding the identifiers
mortality_id <- datasets %>% 
  filter(str_detect(tolower(title),'deaths')) %>%
  filter(str_detect(tolower(title),'by region')) %>%
  pull(id)

mortality_id

datasets %>%
  filter(id %in% mortality_id) %>%
  select(title)

##finding the right editions and versions of the data set 
ids_and_editions_mort <- map(mortality_id, ons_available_editions) %>% 
  set_names(mortality_id) %>% 
  bind_rows(.id='id') %>%
  mutate(.,id_edition=paste(id,edition,sep="-"))

ids_and_editions_and_versions_mort <- mapply(id=ids_and_editions_mort$id,
                                             edition=ids_and_editions_mort$edition,
                                             ons_available_versions)
names(ids_and_editions_and_versions_mort) <- ids_and_editions_mort$id_edition

## so I want the `covid-19` edition and 21 edition for the newest file 

##Downloading the data sets

set_up_df <- monstr_pipeline_defaults(download_root = here('sprint_2')) %>% 
  ons_datasets_setup() 

set_up_df %>%
  ons_dataset_by_id(id=mortality_id,edition="covid-19",version=21)  %>%
  ons_download(format="csv") %>%
  monstr_read_file() %>%  
  monstr_clean() %>%
  monstr_write_clean(format="all")

CV_weekly<- fread(here("sprint_2","data","clean","ons","weekly-deaths-region","covid-19","weekly-deaths-region-v21.csv"), header=TRUE, sep=",", check.names=TRUE) %>%
  rename(.,nr_deaths=v4_1) %>%
  arrange(.,administrative_geography,desc(calendar_years)) %>% 
  filter(geography %in% c("England and Wales", "Wales"),!is.na(nr_deaths)) %>% 
  select(nr_deaths,calendar_years,geography, week_number,recorded_deaths)

saveRDS(CV_weekly, here::here('sprint_2', 'data', 'CV_ENG_ONS.rds'))


##Wales care home residents deaths data 


df<-statswalesr::statswales_get_dataset("hlth0098")

df2<-df %>% 
  filter(LocalAuthority_ItemName_ENG=="Wales" & LocationofDeath_ItemName_ENG=="All locations"& CauseofDeath_Hierarchy=="Total Deaths") %>% 
  select(Data,CauseofDeath_ItemName_ENG,Date_Code,Date_ItemName_ENG,Date_SortOrder,Date_Hierarchy)

saveRDS(df2, here::here('sprint_2', 'data', 'care_home_residents_deaths_wales.rds'))