#Downloading the Incident/Outbreaks data sets and deaths of care home residents

library(statswalesr)
library(curl)
library(tidyverse)

## Outbreaks in care home data from PHE


link <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/891406/Care_home_outbreaks_of_COVID-19_Management_Information.ods'

destfile <- here::here('sprint_2', 'data', "Care_home_outbreaks.ods")
curl_download(link, destfile = destfile)


## COVID-19 surveillance data 

link <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/897200/Weekly_COVID19_report_data_w27.xlsx'

destfile <- here::here('sprint_2', 'data', "Care_home_incident1.xlsx")
curl_download(link, destfile = destfile)

## National flu and COVID-19 surveillance data 

link <- 'https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/962517/Weekly_Influenza_and_COVID19_report_data_w7.xlsx'

destfile <- here::here('sprint_2', 'data', "Care_home_incident2.xlsx")
curl_download(link, destfile = destfile)


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

##2020

link<-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2020/publishedweek532020.xlsx'

destfile <- here::here('sprint_2', 'data', "2020CVdeaths.xlsx")
curl_download(link, destfile = destfile)


##2021 

link<-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2021/publishedweek07202101032021165557.xlsx'

destfile <- here::here('sprint_2', 'data', "2021CVdeaths.xlsx")
curl_download(link, destfile = destfile)


##Wales care home residents deaths data 


df<-statswalesr::statswales_get_dataset("hlth0098")

df2<-df %>% 
  filter(LocalAuthority_ItemName_ENG=="Wales" & LocationofDeath_ItemName_ENG=="All locations"& CauseofDeath_Hierarchy=="Total Deaths") %>% 
  select(Data,CauseofDeath_ItemName_ENG,Date_Code,Date_ItemName_ENG,Date_SortOrder,Date_Hierarchy)

saveRDS(df2, here::here('sprint_2', 'data', 'care_home_residents_deaths_wales.rds'))