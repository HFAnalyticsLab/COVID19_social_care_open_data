#Visualising and calculating care home residents data


# Libraries -----------------------------------------------------------------
library(ggtext)
library(ggplot2)
library(tidyverse)
library(plotly)
library(ISOweek)
library(THFstyle)

# Loading data ------------------------------------------------------------


df <- readRDS(here::here('sprint_2', 'data', 'clean', 'ONS_care_home_deaths_full_ENG.rds'))


# Calculating excess deaths and % of COVID deaths -------------------------

calcs<-df %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  filter(date >as.Date('2020-03-13')& date<as.Date('2020-06-19')) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(period="First Wave", date= "20 March 2020- 12 June 2020") %>% 
  bind_rows(df %>%
              mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
              filter(date>as.Date('2020-08-31')) %>%
              summarise_if(is.numeric, sum) %>% 
              mutate(period="Second Wave", date= "05 Sept 2020- 19 Feb 2021")) %>% 
  select(-(week_ended))%>% 
  mutate(prop_ch_cv_deaths=paste0(round((CV_ch_deaths_eng/CV_all)*100,1),"%"),
         excess_ch_deaths_ch=all_ch_deaths_eng-prev_ch_all_eng)


# Visualising the data ----------------------------------------------------

lab<-paste0(calcs$period,": ")
lab2<-paste0(format(calcs$excess_ch_deaths_ch,big.mark = ","),
             " excess deaths in care home residents compared to 2015-2019 average.")
lab3<-paste0("Care home residents accounted for ", calcs$prop_ch_cv_deaths, " of all COVID-19 registered deaths")


cap<-"Caption"

#p<- 
df %>% 
  filter(date>as.Date('2020-03-06')) %>% 
  pivot_longer(-c(week_ended,week2,date), names_to="Metric", values_to="Counts") %>% 
  mutate(lab= case_when(
    Metric== "all_ch_deaths_eng" ~ "All care home resident deaths", 
    Metric==  "CV_ch_deaths_eng"  ~ "COVID-19 related deaths",
    Metric== "other_ch_deaths_eng" ~ "Other causes",
    Metric== "prev_ch_all_eng" ~ "All causes, average corresponding week in 2015-2019",
    Metric== "CV_all" ~ "COVID-19 related deaths for all")) %>% 
  ggplot(.)+
  geom_bar(data=. %>%  filter (Metric %in% c('CV_ch_deaths_eng', 'other_ch_deaths_eng')),aes(x=date, y=Counts,fill=lab),position="stack", stat="identity") +
  geom_line(data=. %>%  filter(Metric=='prev_ch_all_eng'),aes(y=Counts, x=date, linetype=lab), stat="identity", size=2, colour="grey", hjust=0) +
  annotate("segment", x=as.Date("2020-03-16"), xend=as.Date("2020-03-16"), y=0, yend=8000, linetype="dashed", size=1, colour="grey",hjust=0)+
  annotate("segment", x=as.Date("2020-06-12"), xend=as.Date("2020-06-12"), y=0, yend=8000, linetype="dashed", size=1, colour="grey",hjust=0)+
  annotate("text",x=as.Date("2020-03-20"), y=9800, label=lab[1], size=3.4, colour="grey40", hjust=0)+
  annotate("text",x=as.Date("2020-03-20"), y=9500, label=lab2[1], size=3.4, colour="grey40",hjust=0)+
  annotate("text",x=as.Date("2020-03-20"), y=9200, label=lab3[1], size=3.4, colour="grey40",hjust=0)+
  annotate("segment", x=as.Date("2020-09-04"), xend=as.Date("2020-09-04"), y=0, yend=8000, linetype="dashed", size=1, colour="grey40")+
  annotate("text",x=as.Date("2020-09-11"), y=8300, label=lab[2], size=3.4, colour="black", hjust=0)+
  annotate("text",x=as.Date("2020-09-11"), y=8000, label=lab2[2], size=3.4, colour="black", hjust=0)+
  annotate("text",x=as.Date("2020-09-11"), y=7700, label=lab3[2], size=3.4, colour="black", hjust=0)+
  annotate("rect", xmin = as.Date("2020-03-16"), xmax =as.Date("2020-06-12"), ymin = 0, ymax = 10000, alpha = .1,fill = "grey20")+
  annotate("rect", xmin = as.Date("2020-09-04"), xmax =as.Date("2021-02-22"), ymin = 0, ymax = 10000, alpha = .1,fill = "grey40")+
  scale_x_date(date_breaks = '2 weeks', date_labels = '%d %b')+
  scale_fill_THF()+
  theme_THF()+
  labs(x= "", y="",title = "Care home residents deaths in England per week", caption=cap)+
  theme(plot.title = element_text(size=14),legend.text=element_text(size=11),axis.text.x=element_text(size=11),axis.text.y=element_text(size=11), plot.caption = element_markdown(hjust = 0))

ggsave(here::here('sprint_2','outputs', 'care_home_residents_deaths_ONS.png'), dpi=300)  





