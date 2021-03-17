#Visualising and calculating care home residents data


# Libraries -----------------------------------------------------------------
library(ggtext)
library(tidyverse)
library(THFstyle)
# THFstyle is available on GitHub: https://github.com/THF-evaluative-analytics/THFstyle
library(here)

# Loading data ------------------------------------------------------------


df <- readRDS(here::here('sprint_2', 'data', 'clean', 'ONS_care_home_deaths_full_ENG.rds'))


# Calculating excess deaths and % of COVID deaths -------------------------


df <- df %>%
  mutate(wave = case_when(week_start >= as.Date('2020-03-14')& week_start<=as.Date('2020-06-13') ~ "first_wave",
                          week_start>=as.Date('2020-09-05') ~ "second_wave",
                          TRUE ~ NA_character_),
         ch_deaths_noncovid_england = ch_deaths_all_england - ch_covid_deaths_england)

calcs <-  df %>%
  filter(!is.na(wave)) %>% 
  group_by(wave) %>% 
  summarise(ch_deaths_all_england = sum(ch_deaths_all_england, na.rm = TRUE),
            ch_deaths_noncovid_england = sum(ch_deaths_noncovid_england, na.rm = TRUE),
            ch_covid_deaths_england = sum(ch_covid_deaths_england, na.rm = TRUE),
            covid_deaths_england = sum(covid_deaths_england, na.rm = TRUE),
            ch_deaths_all_avg_2015_2019_england = sum(ch_deaths_all_avg_2015_2019_england, na.rm = TRUE)) %>% 
  mutate(excess_ch_deaths = ch_deaths_all_england - ch_deaths_all_avg_2015_2019_england,
         prop_ch_covid_deaths = round(100*ch_covid_deaths_england / covid_deaths_england, 1))

# Visualising the data ----------------------------------------------------

lab_firstwave<-paste0("First wave (14 March - 19 June):\n",
                      format(calcs$ch_covid_deaths_england[calcs$wave == "first_wave"],big.mark = ","),
                      " COVID-19 deaths,\n", 
                      format(calcs$excess_ch_deaths[calcs$wave == "first_wave"],big.mark = ","),
                      " excess deaths, compared to 2015-2019 average")
lab_secondwave<-paste0("Second wave (from 5 September):\n",
                       format(calcs$ch_covid_deaths_england[calcs$wave == "second_wave"],big.mark = ","),
                       " COVID-19 deaths,\n", 
                       format(calcs$excess_ch_deaths[calcs$wave == "second_wave"],big.mark = ","),
                       " excess deaths, compared to 2015-2019 average")

df %>% 
  filter(week_start>as.Date('2020-03-06')) %>% 
  select(week_start, ch_deaths_noncovid_england, ch_covid_deaths_england) %>% 
  pivot_longer(-week_start, names_to="variable", values_to="count") %>% 
  mutate(variable = case_when(variable == "ch_deaths_noncovid_england" ~ "NON-COVID-19",
                              variable == "ch_covid_deaths_england" ~  "COVID-19")) %>% 
  ggplot(aes(x = week_start, y = count, fill = variable))+
  geom_bar(position="stack", stat="identity") +
  geom_line(data = df[df$week_start>as.Date('2020-03-06'),], 
            aes(x = week_start, y = ch_deaths_all_avg_2015_2019_england, fill = NULL, color = "average 2015-2019"),  stat="identity", size=1) +
  scale_x_date(date_breaks = '4 weeks', date_labels = '%d %b %g') +
  
  annotate("segment", x=as.Date("2020-03-11"), xend=as.Date("2020-03-11"), y=0, yend=9000, linetype="dashed", size=1, colour="black")+
  annotate("segment", x=as.Date("2020-06-15"), xend=as.Date("2020-06-15"), y=0, yend=9000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-03-11"), y=9800, label=lab_firstwave, size=4, colour="black",hjust=0)+
  annotate("segment", x=as.Date("2020-09-02"), xend=as.Date("2020-09-02"), y=0, yend=9000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-09-02"), y=9800, label=lab_secondwave, size=4, colour="black", hjust=0)+
  annotate("rect", xmin = as.Date("2020-03-11"), xmax =as.Date("2020-06-15"), ymin = 0, ymax = 9000, alpha = .1,fill = "grey20")+
  annotate("rect", xmin = as.Date("2020-09-02"), xmax =as.Date("2021-02-22"), ymin = 0, ymax = 9000, alpha = .1,fill = "grey20")+
  scale_fill_THF()+
  theme_THF() +
  scale_colour_manual(name="", values=c("average 2015-2019" = "grey20")) +  
  labs(x= "", y="")+
  scale_y_continuous(limits = c(0,11000), breaks = seq(0, 11000, by = 2000))+
  theme(plot.title = element_text(size=16),legend.text=element_text(size=12),
        axis.text.x=element_text(size=11),axis.text.y=element_text(size=11), plot.caption = element_markdown(hjust = 0, size=10))

  ggsave(here::here('sprint_2','graphs', 'care_home_residents_deaths_ONS.png'),dpi=300,
         width = 10, height = 6.5) 

  
  df %>% 
    filter(week_start>as.Date('2020-03-06')) %>% 
    select(week_start, ch_deaths_noncovid_england, ch_covid_deaths_england, ch_deaths_all_avg_2015_2019_england) %>% 
    pivot_longer(-week_start, names_to="variable", values_to="count") %>% 
    write_csv(here::here('sprint_2','graphs', 'care_home_residents_deaths_ONS.csv'))
  
