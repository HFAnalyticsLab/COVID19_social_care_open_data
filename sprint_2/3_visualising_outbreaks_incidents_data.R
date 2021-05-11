##Visualising the outbreaks and incidents data

library(tidyverse)
library(ggtext)
library(THFstyle)
# THFstyle is available on GitHub: https://github.com/THF-evaluative-analytics/THFstyle



#Loading the data set 
df <- readRDS(here::here('sprint_2','data', 'clean','care_home_outbreaks_incidents.rds'))

#Calculating the total outbreaks and incidents for the first and second wave 

df <- df %>%
    mutate(wave = case_when(week_start >= as.Date('2020-03-16')& week_start<as.Date('2020-06-22') ~ "first_wave",
                            week_start>=as.Date('2020-09-07') ~ "second_wave",
                            TRUE ~ NA_character_))

calcs <-  df %>%
  filter(!is.na(wave)) %>% 
  group_by(wave) %>% 
  summarise(CV_outbreaks = sum(CV_outbreaks, na.rm = TRUE),
            CV_incidents = sum(CV_incidents, na.rm = TRUE),
            num_ch = max(num_ch)) %>% # there is only one value
  mutate(CV_outbreaks_cum_pct = round(100*CV_outbreaks / num_ch, 1),
         CV_incidents_cum_pct = round(100*CV_incidents / num_ch, 1))
  
  
  
#extracting the calculations for the charts
lab_firstwave<-paste0("First wave (16 March - 21 June):\n",
                      format(calcs$CV_outbreaks[calcs$wave == "first_wave"],big.mark = ","),
             " confirmed or suspected outbreaks,\n", 
             format(calcs$CV_outbreaks_cum_pct[calcs$wave == "first_wave"],big.mark = ","),
             "% of care homes")
lab_secondwave<-paste0("Second wave (from 7 September):\n",
                       format(calcs$CV_incidents[calcs$wave == "second_wave"],big.mark = ","),
             " laboratory-confirmed COVID-19 incidents,\n", 
             format(calcs$CV_incidents_cum_pct[calcs$wave == "second_wave"],big.mark = ","),
             "% of care homes")

#creating the charts
## FIGURE 2##

df %>% 
  filter(week_start>as.Date('2020-02-10')) %>%
  pivot_longer(!c("week_number", "week_start", "week", "wave"), names_to="Metric", values_to="Count") %>% 
  mutate(lab=case_when(
    Metric== "CV_outbreaks"~ "COVID-19 outbreaks (>= confirmed or suspected 2 cases)",
    Metric=="CV_incidents" ~ "COVID-19 infection incidents (>= 1 confirmed  case)")) %>% 
  filter(!is.na(lab)) %>% 
  ggplot(., aes(x=week_start, y=Count, group=lab, colour=lab)) + 
  annotate("segment", x=as.Date("2020-03-14"), xend=as.Date("2020-03-14"), 
           y=0, yend=1100, linetype="dashed", size=1.5, colour="grey20",hjust=0)+
  annotate("segment", x=as.Date("2020-06-19"), xend=as.Date("2020-06-19"), 
           y=0, yend=1100, linetype="dashed", size=1.5, colour="grey20",hjust=0)+
  annotate("text",x=as.Date("2020-03-16"), y=1190, label= lab_firstwave, size=3.4, colour="black",hjust=0)+
  annotate("segment", x=as.Date("2020-09-05"), xend=as.Date("2020-09-05"), 
           y=0, yend=1000, linetype="dashed", size=1.5, colour="grey20")+
  annotate("text",x=as.Date("2020-09-04"), y=1100, label= lab_secondwave, size=3.4, colour="black", hjust=0)+
  annotate("rect", xmin = as.Date("2020-03-14"), xmax =as.Date("2020-06-19"), 
           ymin = 0, ymax = 1100, alpha = .1,fill = "grey20")+
  annotate("rect", xmin = as.Date("2020-09-05"), xmax =as.Date("2021-04-02"), 
           ymin = 0, ymax = 1000, alpha = .1,fill = "grey20")+
  geom_line(size=1) + geom_point(size=3.5) + geom_point(colour='white') +
  scale_x_date(date_breaks = '4 weeks', date_labels = '%d %b %g')+
  scale_y_continuous(limits = c(0,1350), breaks = seq(0, 1200, by = 200))+
  scale_colour_THF() +
  theme_THF()+
  labs(x = "", y="", caption = "Sources: PHE COVID-19 outbreaks from COVID-19: number of outbreaks in care homes - management information, <br> PHE National flu and COVID-19 surveillance reports")+
  theme(legend.text=element_text(size=11),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(colour = guide_legend(ncol = 1))

#saving the graph 
ggsave(here::here('sprint_2','graphs', 'care_outbreaks_incidents.png'), dpi=300,
       width = 10, height = 6.5) 

ggsave(here::here('sprint_2','graphs', 'care_outbreaks_incidents.pdf'), dpi=300, device  = "pdf",
       width = 10, height = 6.5) 


df %>% 
  filter(week_start>as.Date('2020-02-10')) %>%
  select(week_start, CV_outbreaks, CV_incidents) %>% 
  write_csv(here::here('sprint_2','graphs', 'care_outbreaks_incidents.csv'))
