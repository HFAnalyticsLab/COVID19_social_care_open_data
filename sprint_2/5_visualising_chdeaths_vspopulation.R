
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


# Visualise for England ---------------------------------------------------------------

## FIGURE 1 ##
df %>% 
  select(week_number, week_start, care_home = ch_covid_deaths_england, all = covid_deaths_england) %>% 
  pivot_longer(c(-week_number, - week_start), names_to = "setting", values_to = "deaths") %>% 
  ggplot(., aes(x=week_start, y=deaths, group=setting, colour=setting)) + 
  geom_line(size=1) + geom_point(size=3.5) + geom_point(colour='white') +
  scale_x_date(breaks = seq( min(df$week_start), max(df$week_start),by="8 weeks"),
               date_labels = '%d %b %g')+ 
  annotate("text",x=as.Date("2020-02-01"), y=6000, label="First confirmed\nUK COVID-19\ncases", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-01-31"), xend=as.Date("2020-01-31"), y=0, yend=6000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-03-24"), y=7000, label="First lockdown", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-03-23"), xend=as.Date("2020-03-23"), y=0, yend=7000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-04-16"), y=6000, label="COVID-19\n socialcare\naction plan", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-04-15"), xend=as.Date("2020-04-15"), y=0, yend=6000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-05-16"), y=7000, label="Care home\nsupport package", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-05-15"), xend=as.Date("2020-05-15"), y=0, yend=7000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-06-09"), y=6000, label="Adult social care\ntaskforce announced", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-06-08"), xend=as.Date("2020-06-08"), y=0, yend=6000, linetype="dashed", size=1, colour="black")+
  
  annotate("text",x=as.Date("2020-07-05"), y=7000, label="Hospitality\nreopened", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-07-04"), xend=as.Date("2020-07-04"), y=0, yend=7000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-09-19"), y=6000, label="COVID winter\nplan for\nadult social care", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-09-18"), xend=as.Date("2020-09-18"), y=0, yend=6000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-11-06"), y=7000, label="Second\nlockdown", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-11-05"), xend=as.Date("2020-11-05"), y=0, yend=7000, linetype="dashed", size=1, colour="black")+
  
  annotate("text",x=as.Date("2020-12-03"), y=6000, label="Tier system\nintroduced", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-12-02"), xend=as.Date("2020-12-02"), y=0, yend=6000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-12-09"), y=7000, label="First vaccine", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-12-08"), xend=as.Date("2020-12-08"), y=0, yend=7000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-12-21"), y=7000, label="Tier 4\nintroduced", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-12-20"), xend=as.Date("2020-12-20"), y=0, yend=7000, linetype="dashed", size=1, colour="black")+
  
  annotate("text",x=as.Date("2020-12-22"), y=9000, label="Vaccine roll-out\nin care home", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2020-12-21"), xend=as.Date("2020-12-21"), y=0, yend=9000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2021-01-07"), y=8000, label="Third\nlockdown", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2021-01-06"), xend=as.Date("2021-01-06"), y=0, yend=8000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2021-02-23"), y=6000, label="Roadmap out\n out of lockdown", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2021-02-22"), xend=as.Date("2021-02-22"), y=0, yend=6000, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2021-03-19"), y=7000, label="Funding\nextended", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2021-03-18"), xend=as.Date("2021-03-18"), y=0, yend=7000, linetype="dashed", size=1, colour="black")+
  
  annotate("text",x=as.Date("2020-03-11"), y=8800, label="First wave\n14 March - 19 June", size=3.2, colour="black",hjust=0)+
  annotate("text",x=as.Date("2020-09-02"), y=8800, label="Second wave\nfrom 5 September", size=3.2, colour="black", hjust=0)+
  annotate("rect", xmin = as.Date("2020-03-11"), xmax =as.Date("2020-06-15"), ymin = 0, ymax = Inf, alpha = .1,fill = "grey20")+
  annotate("rect", xmin = as.Date("2020-09-02"), xmax =as.Date("2021-03-31"), ymin = 0, ymax = Inf, alpha = .1,fill = "grey20")+
  scale_colour_THF() +
  theme_THF()+
  ylim(0, 8800)+
  labs(x = "", y="COVID-19 deaths")+
  theme(legend.text=element_text(size=11),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(colour = guide_legend(nrow = 1))


ggsave(here::here('sprint_2', 'graphs', 'coviddeaths_ch_vs_all.png'), dpi=300,
       width = 10, height = 6.5)

ggsave(here::here('sprint_2', 'graphs', 'coviddeaths_ch_vs_all.pdf'), dpi=300, device = "pdf",
       width = 10, height = 6.5)
df %>% 
  select(week_number, week_start, care_home = ch_covid_deaths_england, all = covid_deaths_england) %>% 
  filter(week_start >= ymd("2020-02-29")) %>% 
  write_csv(here::here('sprint_2', 'graphs', 'coviddeaths_ch_vs_all.csv'))


# Normalised to the peak
deaths <- df %>% 
  select(week_number, week_start, care_home = ch_covid_deaths_england, all = covid_deaths_england) %>% 
  mutate(other = all - care_home) %>% 
  select(-all) %>%
  filter(week_start >= ymd("2020-12-19")) %>%  
  mutate(care_home_norm = 100*care_home/max(care_home, na.rm = TRUE),
         other_norm = 100*other/max(other, na.rm = TRUE)) %>% 
  select(-care_home, - other) %>% 
  pivot_longer(c(-week_number, - week_start), names_to = "group", values_to = "deaths_norm") 
  
deaths  %>% 
  ggplot(., aes(x=week_start, y=deaths_norm, group=group, colour=group)) + 
  geom_line(size=1) + geom_point(size=3.5) + geom_point(colour='white') +
  scale_x_date(breaks = seq(as.Date('2020-03-07'),max(df$week_start),by="1 weeks"),
               date_labels = '%d %b %g')+
  scale_y_log10(limits = c(1, 130))+
  scale_colour_THF(labels = c("care home", "other")) +
  theme_THF()+
  labs(x = "", y="Percent")+
  annotate("segment", x=as.Date("2020-12-21"), xend=as.Date("2020-12-21"), y=0, yend=110, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-12-22"), y=1.5, label="21 December 2020,\nvaccine roll-out in care homes", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2021-02-04"), xend=as.Date("2021-02-04"), y=0, yend=110, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2021-02-05"), y=1.5, label="4 February 2021,\n90.0% of residents\nhave received first dose", size=3.2, colour="black", hjust=0)+
  
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


# Visualise for England and Wales, by age group ---------------------------

CV_age_weekly <- readRDS(here::here('sprint_2', 'data', 'CV_age_ENGWALES_ONS.rds'))

CV_age_weekly <- CV_age_weekly %>% 
  clean_names() %>% 
  mutate(week_start = ISOweek2date(str_c(calendar_years , "-W", 
                                         str_pad(gsub("week-", "", week_number), 2, side = "left", pad = "0"), "-1")),
         week_start = week_start - days(2),
         age_group_70 = if_else(age_groups %in% c("70-74", "75-79", "80-84", "85-89", "90+"), "70+", "under 70")) %>% 
  group_by(week_start, age_group_70) %>% 
  summarise(deaths = sum(nr_deaths))
  
CV_age_weekly <- CV_age_weekly %>% 
  filter(week_start >= ymd("2020-12-19")) %>% 
  group_by(age_group_70) %>% 
  mutate(deaths_norm = 100*deaths/max(deaths, na.rm = TRUE))%>%  
  filter(!is.na(deaths_norm))

# Normalised to the peak
# FIGURE 4 ##
deaths2 <- df %>% 
  select(week_number, week_start, care_home = ch_covid_deaths_england_and_wales) %>% 
  filter(week_start >= ymd("2020-12-19")) %>%  
  mutate(deaths_norm = 100*care_home/max(care_home, na.rm = TRUE),
         age_group_70 = "care home resident") 

combined <- bind_rows(CV_age_weekly, deaths2) %>% 
  mutate(age_group_70 = factor(age_group_70, levels = c("under 70", "70+", "care home resident")))

combined  %>% 
  ggplot(., aes(x=week_start, y=deaths_norm, group=age_group_70, colour=age_group_70)) + 
  geom_line(size=1) + geom_point(size=3.5) + geom_point(colour='white') +
  scale_x_date(breaks = seq(as.Date('2020-03-07'),max(df$week_start),by="1 weeks"),
               date_labels = '%d %b %g')+
  scale_y_log10(limits = c(1, 130))+
  scale_colour_THF() +
  theme_THF()+
  labs(x = "", y="Percent")+
  annotate("segment", x=as.Date("2020-12-21"), xend=as.Date("2020-12-21"), y=0, yend=110, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2020-12-22"), y=1.5, label="21 December 2020,\nstart of vaccine roll-out in\nolder adult care homes", size=3.2, colour="black", hjust=0)+
  annotate("segment", x=as.Date("2021-02-04"), xend=as.Date("2021-02-04"), y=0, yend=110, linetype="dashed", size=1, colour="black")+
  annotate("text",x=as.Date("2021-02-05"), y=1.5, label="4 February 2021,\n90.0% of residents in older\nadult care homes in England\nhave received their first dose", size=3.2, colour="black", hjust=0)+
  
  theme(legend.text=element_text(size=11),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(0.5,01.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(colour = guide_legend(nrow = 2))


ggsave(here::here('sprint_2', 'graphs', 'deaths_bysetting_byage.png'), dpi=300,
       width = 10, height = 6.5)

ggsave(here::here('sprint_2', 'graphs', 'deaths_bysetting_byage.pdf'), dpi=300, device = "pdf",
       width = 10, height = 6.5)

combined %>% 
  select(week_start, age_group_70, deaths_norm)%>% 
  pivot_wider(names_from = "age_group_70", values_from = "deaths_norm") %>% 
  write_csv(here::here('sprint_2','graphs', 'ENGWALES_covid_deaths_age_norm.csv'))
