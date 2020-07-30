# COVID-19 social care open data


#### Project Status: [Completed]

## Project Description

This analysis describes the impact of COVID-19 on care home residents and home care users. The main focus is on all cause and COVID-19 deaths as well as COVID-19 outbreaks in care homes. 

## Outputs

The analysis is shown in the Health Foundation briefing [Adult social care and COVID-19: Assessing the impact on social care users and staff in England so far](https://www.health.org.uk/publications/report/adult-social-care-and-covid-19-assessing-the-impact-on-social-care-users-and-staff-in-england-so-far). 

## Data source

This project only uses public data. 
[Deaths involving COVID-19 in the care sector](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsinvolvingcovid19inthecaresectorenglandandwales) 
[Outbreaks in care homes](https://www.gov.uk/government/statistical-data-sets/covid-19-number-of-outbreaks-in-care-homes-management-information)

## How does it work?

The code can be used to download and analyse the data. 

### Requirements

These scripts were written in R version (3.6.3) and RStudio Version 1.3.959 
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/)(1.3.0)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html) (2.0.1)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html) (1.7.8)
* [**testthat**](https://cran.r-project.org/web/packages/testthat/index.html) (2.3.2)
* [**curl**](https://cran.r-project.org/web/packages/curl/index.html) (4.3)
* [**taskscheduleR**](https://cran.r-project.org/web/packages/taskscheduleR/) (1.4) (Windows only)
* **THFstyle** internal package

### Getting started



The 'src' folder contains

* [0_download_data.R](https://github.com/HFAnalyticsLab/COVID19_social_care_open_data/blob/master/src/0_download_data.R) - Download data - links need to be updated for latest data
* (https://github.com/HFAnalyticsLab/COVID19_social_care_open_data/blob/master/src/1_clean_data.R)[1_clean_data.R] - Clean and save data
* [1_b_clean_care_homes_data](https://github.com/HFAnalyticsLab/COVID19_social_care_open_data/blob/master/src/1_b_clean_care_homes_data.R) - Clean care homes data 
* [2_timeseries_plots.Rmd](https://github.com/HFAnalyticsLab/COVID19_social_care_open_data/blob/master/src/2_timeseries_plots.Rmd) - Plot long time series
* 3_regional_deaths_in_care_homes.R (https://github.com/HFAnalyticsLab/COVID19_social_care_open_data/blob/master/src/3_regional_deaths_in_care_homes.R) - plot deaths in care homes by region
* [4_national_deaths_care_home_residents.R](https://github.com/HFAnalyticsLab/COVID19_social_care_open_data/blob/master/src/4_national_deaths_care_home_residents.R) - Plot deaths in care homes during the pandemic
* [5_regional_care_home_outbreaks](https://github.com/HFAnalyticsLab/COVID19_social_care_open_data/blob/master/src/5_regional_care_home_outbreaks.R) - plot time line of outbreaks in care homes

## Authors

* **Emma Vestesson** - [Health Foundation profile](https://www.health.org.uk/about-the-health-foundation/our-people/improvement-analytics-unit-iau/emma-vestesson) [@gummifot](https://twitter.com/gummifot) - [emmavestesson](https://github.com/emmavestesson)
* **Fiona Grimm** - on [Twitter](https://twitter.com/fiona_grimm) or [GitHub](https://github.com/fiona-grimm)


## License

This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/COVID19_social_care_open_data/blob/master/LICENSE).

