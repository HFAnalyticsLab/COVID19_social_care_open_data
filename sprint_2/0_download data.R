#Downloading the Incident/Outbreaks data sets 
#and deaths of care home residents

library(statswalesr)
library(curl)
library(tidyverse)
library(monstR)
# monstR is available on GitHub: https://github.com/HFAnalyticsLab/monstR
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


## ONS Deaths in care home residents the new update

link<-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/carehomeresidentdeathsregisteredinenglandandwalesprovisional/2021/weeklydeathscarehomesweek72021.xlsx'

destfile <- here::here('sprint_2', 'data', "carehomeresidentsdeaths.xlsx")
curl_download(link, destfile = destfile)

# ONS COVID deaths for care home residents for England only, 2020


link<-'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/socialcare/adhocs/12812weeklyprovisionalfiguresofcarehomeresidentdeathsregisteredinenglandandwales2020/weeklydeathsincareresidents2020.xlsx'

destfile <- here::here('sprint_2', 'data', "carehomeresidentsdeaths_2020.xlsx")
curl_download(link, destfile = destfile)

