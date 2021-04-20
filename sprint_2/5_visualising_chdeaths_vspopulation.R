
# Loading library ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ISOweek)
library(janitor)
library(readxl)
library(THFstyle)
library(ggtext)

# Loading data ------------------------------------------------------------


df <- readRDS(here::here('sprint_2', 'data', 'clean', 'ONS_care_home_deaths_full_ENG.rds'))


# Visualise ---------------------------------------------------------------


df %>% 
  select(week_number, week_start, care_home = ch_covid_deaths_england, all = covid_deaths_england) %>% 
  mutate(other = all - care_home) %>% 
  select(-all) %>%
  filter(week_start >= ymd("2020-12-19")) %>%  
  pivot_longer(c(-week_number, - week_start), names_to = "setting", values_to = "deaths") %>% 
  ggplot(., aes(x=week_start, y=deaths, group=setting, colour=setting)) + 
  geom_line(size=1) + geom_point(size=3.5) + geom_point(colour='white') +
  scale_x_date(date_breaks = '1 weeks', date_labels = '%d %b %g')+
  scale_y_log10(limits = c(1, 10000))+
  scale_colour_THF() +
  theme_THF()+
  labs(x = "", y="Deaths")+
  theme(legend.text=element_text(size=11),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(colour = guide_legend(ncol = 1))


# Normalised to the peak
deaths <- df %>% 
  select(week_number, week_start, care_home = ch_covid_deaths_england, all = covid_deaths_england) %>% 
  mutate(other = all - care_home) %>% 
  select(-all) %>%
  filter(week_start >= ymd("2020-12-19")) %>%  
  mutate(care_home_norm = 100*care_home/max(care_home, na.rm = TRUE),
         other_norm = 100*other/max(other, na.rm = TRUE)) %>% 
  select(-care_home, - other) %>% 
  pivot_longer(c(-week_number, - week_start), names_to = "setting", values_to = "deaths") 
  
deaths  %>% 
  ggplot(., aes(x=week_start, y=deaths, group=setting, colour=setting)) + 
  geom_line(size=1) + geom_point(size=3.5) + geom_point(colour='white') +
  scale_x_date(breaks = seq(as.Date('2020-03-07'),max(df$week_start),by="1 weeks"),
               date_labels = '%d %b %g')+
  scale_y_log10(limits = c(1, 110))+
  scale_colour_THF() +
  theme_THF()+
  labs(x = "", y="Percent")+
  theme(legend.text=element_text(size=11),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(colour = guide_legend(ncol = 1))


ggsave(here::here('sprint_2', 'graphs', 'deaths_bysetting.png'), dpi=300,
       width = 10, height = 6.5)

df %>% 
  select(week_number, week_start, care_home = ch_covid_deaths_england, all = covid_deaths_england) %>% 
  mutate(other = all - care_home) %>% 
  select(-all) %>%
  filter(week_start >= ymd("2020-12-19")) %>%  
  mutate(care_home_norm = 100*care_home/max(care_home, na.rm = TRUE),
         other_norm = 100*other/max(other, na.rm = TRUE)) %>% 
  select(-week_number, -care_home, - other) %>% 
  write_csv(here::here('sprint_2','graphs', 'ENG_covid_deaths_ONS.csv'))