
library(tidyverse) 
library(tidylog)
library(readxl)
library(janitor)
library(lubridate)
library(THFstyle) # required for graphs, can be downloaded from https://github.com/THF-evaluative-analytics/THFstyle
library(ISOweek)

# Import and clean historical mortality data ---------------------------------

dom_care_deaths_historical <- read_excel(here::here('sprint_2','data', "julydeathsinvolvingcovid19inthecaresectordataset02072020155122.xlsx"),
                                         sheet = "Table 10", skip = 3,col_types = c("date", "numeric", "numeric"), n_max = 1266) %>% 
  clean_names() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) 
  

dom_care_deaths_historical <- dom_care_deaths_historical %>% 
  select(deaths = home_care_service_user, date) %>% 
  mutate(week_start = if_else(weekdays(date) == "Saturday", date, NA_real_))  %>% 
  fill(week_start, .direction = "down") %>% 
  filter(!is.na(week_start) )
    
# check all groups have 7 days
dom_care_deaths_historical %>%  group_by(week_start) %>% count() %>%  arrange(desc(n))
    
dom_care_deaths_historical <- dom_care_deaths_historical %>% 
  group_by(week_start) %>% 
  summarise(deaths = sum(deaths)) %>% 
  mutate(week_number = date2ISOweek(week_start),
         week_number_dummy = gsub("^.{4}", "0004", week_number))

dom_care_deaths_historical_avg <- dom_care_deaths_historical %>% 
  filter(year(week_start) != "2020") %>% 
  group_by(week_number_dummy) %>% 
  summarise(mean_deaths_2017_to_2019 = round(mean(deaths, na.rm = TRUE),0))


dom_care_deaths_historical_avg <-  dom_care_deaths_historical_avg %>% 
  bind_rows(dom_care_deaths_historical_avg %>%  
              mutate(week_number_dummy = gsub("^.{4}", "0005", week_number_dummy)))

# we don't have the 5-year average for week 53, so we will re-use week 52
# (this is what the ONS did in the latest deaths in the care sector analysis, May 2021)
dom_care_deaths_historical_avg <- dom_care_deaths_historical_avg %>% 
  add_row(week_number_dummy = "0004-W53-6",
          mean_deaths_2017_to_2019 =  dom_care_deaths_historical_avg$mean_deaths_2017_to_2019[dom_care_deaths_historical_avg$week_number_dummy == "0004-W52-6"])



# Import and clean 2020/21 data -------------------------------------------

# week commencing on Saturdays

dom_care_deaths <- read_excel(here::here('sprint_2','data', "referencetable10052021114704.xlsx"),
                              sheet = "Table 6", skip = 4, col_types = c("numeric", "date", rep("numeric", 9)), n_max = 51) %>% 
  clean_names() %>% 
  mutate(week_ending = as.Date(week_ending, "%Y-%m-%d"),
         all_deaths = hospital_3 + home_care_4 + elsewhere_5 + not_stated_6,
         COVID19_deaths = hospital_8 + home_care_9 + elsewhere_10 + not_stated_11,
         nonCOVID19_deaths = all_deaths - COVID19_deaths) %>% 
  select(week_ending, COVID19_deaths, nonCOVID19_deaths)


dom_care_deaths <- dom_care_deaths %>% 
  mutate(week_starting = week_ending - 6,
         week_starting_dummy = if_else(year(week_starting) == "2020", `year<-`(week_starting, 0004), `year<-`(week_starting, 0005)),
         week_number = date2ISOweek(week_starting),
         week_number_dummy = if_else(substr(week_number, 1, 4) == "2020", gsub("^.{4}", "0004", week_number), gsub("^.{4}", "0005", week_number))) 


# combine and visualise -----------------------------------------------

dom_care_deaths_combined <- dom_care_deaths %>% 
  left_join(dom_care_deaths_historical_avg, by = "week_number_dummy") %>% 
  select(week_starting, COVID19_deaths, nonCOVID19_deaths, mean_deaths_2017_to_2019)


dom_care_deaths_combined <- dom_care_deaths_combined %>%
  mutate(wave = case_when(week_starting >= as.Date('2020-03-14')& week_starting<=as.Date('2020-06-13') ~ "first_wave",
                          week_starting>=as.Date('2020-09-05') ~ "second_wave",
                          TRUE ~ NA_character_))

calcs <-  dom_care_deaths_combined %>%
  filter(!is.na(wave)) %>% 
  group_by(wave) %>% 
  summarise(domcare_deaths_noncovid = sum(nonCOVID19_deaths, na.rm = TRUE),
            domcare_deaths_covid = sum(COVID19_deaths, na.rm = TRUE),
            domcare_deaths_all_avg_2017_2019 = sum(mean_deaths_2017_to_2019, na.rm = TRUE)) %>% 
  mutate(domcare_deaths_all = domcare_deaths_noncovid + domcare_deaths_covid,
        excess_domcare_deaths = domcare_deaths_all - domcare_deaths_all_avg_2017_2019,
        pct_excess_deaths = round(100*domcare_deaths_all/domcare_deaths_all_avg_2017_2019 - 100, 0),
        prop_domcare_covid_deaths = round(100*domcare_deaths_covid / domcare_deaths_all, 1))

lab_firstwave<-paste0("First wave (14 March - 19 June):\n",
                      format(calcs$domcare_deaths_covid[calcs$wave == "first_wave"],big.mark = ","),
                      " COVID-19 deaths,\n", 
                      format(calcs$excess_domcare_deaths[calcs$wave == "first_wave"],big.mark = ","),
                      " excess deaths, compared to 2017-2019 average")
lab_secondwave<-paste0("Second wave (5 September - 2 April):\n",
                       format(calcs$domcare_deaths_covid[calcs$wave == "second_wave"],big.mark = ","),
                       " COVID-19 deaths,\n", 
                       format(calcs$excess_domcare_deaths[calcs$wave == "second_wave"],big.mark = ","),
                       " excess deaths, compared to 2017-2019 average")

# Bar graph
## FIGURE ? ##

dom_care_deaths_combined %>% 
  select(week_starting, nonCOVID19_deaths, COVID19_deaths) %>% 
  pivot_longer(-week_starting, names_to="variable", values_to="count") %>% 
  mutate(variable = case_when(variable == "nonCOVID19_deaths" ~ "NON-COVID-19",
                              variable == "COVID19_deaths" ~  "COVID-19")) %>% 
  ggplot(aes(x = week_starting, y = count, fill = variable))+
  geom_bar(position="stack", stat="identity") +
  geom_line(data = dom_care_deaths_combined, 
            aes(x = week_starting, y = mean_deaths_2017_to_2019, fill = NULL, color = "average 2017-2019"),  stat="identity", size=1) +
  scale_x_date(breaks = seq(min(dom_care_deaths_combined$week_starting),max(dom_care_deaths_combined$week_starting),by="4 weeks"),
               date_labels = '%d %b %g') +
  annotate("segment", x=as.Date("2020-04-09"), xend=as.Date("2020-04-09"), y=0, yend=850, linetype="dashed", size=1, colour="black")+
  annotate("segment", x=as.Date("2020-06-15"), xend=as.Date("2020-06-15"), y=0, yend=850, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-04-11"), y=940, label=lab_firstwave, size=3.2, colour="black",hjust=0)+
  annotate("segment", x=as.Date("2020-09-02"), xend=as.Date("2020-09-02"), y=0, yend=850, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-09-02"), y=940, label=lab_secondwave, size=3.2, colour="black", hjust=0)+
  annotate("rect", xmin = as.Date("2020-04-09"), xmax =as.Date("2020-06-15"), ymin = 0, ymax = 850, alpha = .1,fill = "grey20")+
  annotate("rect", xmin = as.Date("2020-09-02"), xmax =as.Date("2021-03-31"), ymin = 0, ymax = 850, alpha = .1,fill = "grey20")+
  scale_fill_THF()+
  theme_THF() +
  scale_colour_manual(name="", values=c("average 2017-2019" = "grey20")) +  
  labs(x= "", y="")+
  scale_y_continuous(limits = c(0,1000), breaks = seq(0, 1000, by = 200))+
  theme(plot.title = element_text(size=16),legend.text=element_text(size=12),
        axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

ggsave(here::here('sprint_2','graphs', 'domcare_deaths.png'),dpi=300,
       width = 10, height = 6.5) 


ggsave(here::here('sprint_2','graphs', 'domcare_deaths.pdf'),dpi=300, device = "pdf",
       width = 10, height = 6.5) 


dom_care_deaths_combined %>% 
  write_csv(here::here('sprint_2','graphs', 'domcare_deaths.csv'))

