##Visualising the outbreaks and incidents data
library(ggplot2)
library(ggtext)
library(THFstyle)
library(tidyverse)


#loading the data set 
df <- readRDS(here::here('sprint_2','data', 'clean','care_home_outbreaks_incidents.rds'))


#Creating the data notes and sources with formatting 

a<-"<br> Sources: ARI Incidents from National COVID-19 survellance reports (30 Sept 2019-27-Sept 2020) and <br>National flu and COVID-19 surveillance reports (28 Sept 2020-21 Feb 2021)."
b<-"<br> PHE COVID-19 outbreaks from COVID-19: number of outbreaks in care homes - management information (9 Mar 2020-19 July 2020)."
c<-"<br> Data Note: Data first published in April 2020, as surveillance of respiratory outbreaks (due to the season, it was assumed that outbreaks were COVID-19 related)."
d<- "<br> An outbreak can be confirmed or suspected, a suspected outbreak is defined as two or more people experiencing a similar illness, which appears to be linked to a particular setting. <br>Outbreaks can be confirmed using laboratory testing."
e<- "<br> From end of June onwards, outbreaks turned into the broader reporting of acute respiraroty infections (ARI) incidents."
f<-"<br> ARI Incidents are defined as suspected or confirmed outbreaks of acute respiratory infections (COVID, influenza or other respiratory pathogens)<br>linked to a particular setting (care homes)."
g<-"<br> COVID-19 infection incident is where there is at least 1 laboratory confirmed case of COVID-19, reporting started from 29th June 2020."
h<-"<br> Even though the reports uses the same PHE surveillance database, the change in the definition/reporting as well as the possibility of dismissal of some suspected outbreaks over time means <br>that the two waves are not directly comparable."

sub<-paste0(a,b)
cap<-paste0(c,d,e,f,g,h)

#Loading in the total from PHE management data set 

tot_ch<-readRDS(here::here('sprint_2','data', 'clean','care_home_total.rds'))

#Calculating the total outbreaks and incidents for the first and second wave 

calcs<- df %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  filter(date >as.Date('2020-03-13')& date<as.Date('2020-06-19')) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(period="First Wave", date= "16 March 2020- 21 June 2020 ") %>% 
  bind_rows(df %>%
              mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
              filter(date>as.Date('2020-08-31')) %>% 
              summarise_if(is.numeric, sum) %>% 
              mutate(period="Second Wave", date= "07 Sept 2020- 21 Feb 2021")) %>% 
  mutate(tot_care_homes=tot_ch) %>% 
  mutate(prop_ch_outbreaks=round((CV_outbreaks/tot_care_homes)*100,1),prop_ch_incidents=round((CV_incidents/tot_care_homes)*100,1),prop_ch_ARI=round((ARI/tot_care_homes)*100,1))

#extracting the calculations for the charts
lab<-paste0(calcs$period," : ")
lab2<-paste0(format(calcs$CV_outbreaks,big.mark = ","),
             " outbreaks of COVID-19 in care homes")
lab3<-paste0(format(calcs$ARI,big.mark = ","),
             " ARI incidents in care homes")
lab4<-paste0(format(calcs$CV_incidents,big.mark = ","),
             " care homes with at least one laboratory confirmed case of COVID-19")

#creating the charts 

df %>% 
  filter(date>as.Date('2020-02-10')) %>%
  select(-ARI) %>% 
  pivot_longer(!c("week_number", "date", "date2"), names_to="Metric", values_to="Count") %>% 
  mutate(lab=case_when(
    Metric== "CV_outbreaks"~ "PHE COVID-19 outbreaks",
    Metric=="CV_incidents" ~ "COVID-19 infection incidents")) %>% 
  ggplot(., aes(x=date, y=Count, group=lab, colour=lab)) + 
  geom_line(size=1) + geom_point(size=3.5) + geom_point(colour='white') +
  annotate("segment", x=as.Date("2020-03-16"), xend=as.Date("2020-03-16"), y=0, yend=1100, linetype="dashed", size=1.5, colour="grey",hjust=0)+
  annotate("segment", x=as.Date("2020-06-12"), xend=as.Date("2020-06-12"), y=0, yend=1100, linetype="dashed", size=1.5, colour="grey",hjust=0)+
  annotate("text",x=as.Date("2020-03-16"), y=1200, label=lab[1], size=3.4, colour="grey40", hjust=0)+
  #annotate("text",x=as.Date("2020-03-16"), y=1160, label=lab3[1], size=3.4, colour="grey40",hjust=0)+
  annotate("text",x=as.Date("2020-03-16"), y=1120, label=lab2[1], size=3.4, colour="grey40",hjust=0)+
  annotate("segment", x=as.Date("2020-09-04"), xend=as.Date("2020-09-04"), y=0, yend=1000, linetype="dashed", size=1.5, colour="grey40")+
  annotate("text",x=as.Date("2020-09-04"), y=1100, label=lab[2], size=3.4, colour="black", hjust=0)+
  #annotate("text",x=as.Date("2020-09-04"), y=1060, label=lab3[2], size=3.4, colour="black", hjust=0)+
  annotate("text",x=as.Date("2020-09-04"), y=1020, label=lab4[2], size=3.4, colour="black", hjust=0)+
  annotate("rect", xmin = as.Date("2020-03-16"), xmax =as.Date("2020-06-12"), ymin = 0, ymax = 1100, alpha = .1,fill = "grey20")+
  annotate("rect", xmin = as.Date("2020-09-04"), xmax =as.Date("2021-02-22"), ymin = 0, ymax = 1000, alpha = .1,fill = "grey40")+
  scale_x_date(date_breaks = '2 weeks', date_labels = '%d %b')+
  scale_colour_THF() +
  theme_THF()+
  labs(x= "",y="",title = "Care home outbreaks/incidents", caption=cap, subtitle=sub)+
  theme(legend.text=element_text(size=11),axis.text.x=element_text(size=14),axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=7.5), plot.subtitle = element_markdown(hjust=0, size=8))

#saving the graph 
ggsave(here::here('sprint_2','graphs', 'care_outbreaks_incidents.png'), dpi=300) 
