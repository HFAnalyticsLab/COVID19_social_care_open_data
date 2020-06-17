library(tidyverse)
library(THFstyle)
library(patchwork)

ch_deaths <- readRDS("data/CH_deaths_by_region.Rds")
ch_deaths <- ch_deaths %>% 
  mutate(deaths_per_1000_beds=deaths/beds*1000,
         deaths_cum_per_1000_beds=deaths_cum/beds*1000) %>% 
   filter(ch_region!='Unspecified')
regions <- unique(ch_deaths$ch_region)

regional_death_plot <- map(regions, ~ ch_deaths %>% 
       filter(ch_region==.x) %>% 
       ggplot(aes(x = date, y = deaths_per_1000_beds, color = type)) +
       geom_line() +
       geom_point(size=2) +
      # geom_point(colour='white') +
       theme_THF() +
      scale_y_continuous(limits = c(0,5)) +   
       scale_colour_THF(labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID")
       ) +
       theme(axis.title.x = element_blank(),
             legend.position = "none",
             plot.title.position = 'plot',
             legend.title = element_blank(),
             legend.justification= c(1,0),
             panel.grid = element_blank()) +
       guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
       labs(subtitle = paste0(.x),
                              x='', y='',
            fill = "COVID deaths"))

regional_death_plot[[1]] + regional_death_plot[[2]] +
   regional_death_plot[[3]] + regional_death_plot[[4]] +
   regional_death_plot[[5]] + regional_death_plot[[6]] +
   regional_death_plot[[7]] + regional_death_plot[[8]] +
   regional_death_plot[[9]] + plot_annotation(title='Daily deaths in care homes')
ggsave(here::here('output', 'regional_deaths_in_care_homes_daily.png'))  

## Weekly per 1000 beds
ch_deaths_weekly <- ch_deaths %>% 
   mutate(date=lubridate::floor_date(date, unit='week', week_start = 1)) %>% 
   group_by(ch_region, date, type, beds) %>% 
   summarise(deaths=sum(deaths), deaths_per_1000_beds=sum(deaths_per_1000_beds)) %>% 
   group_by(type, ch_region) %>% 
   arrange(date) %>% 
   mutate(deaths_cum_per_1000_beds = cumsum(deaths)/beds*1000) %>% 
   ungroup() 
   
regional_death_plot_weekly <- map(regions, ~ ch_deaths_weekly %>% 
                              filter(ch_region==.x) %>% 
                              ggplot(aes(x = date, y = deaths_per_1000_beds, color = type)) +
                              geom_line() +
                              geom_point(size=2) +
                              geom_point(colour='white') +
                              theme_THF() +
                              scale_y_continuous(limits = c(0,35)) +   
                              scale_colour_THF(labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID")
                              ) +
                              theme(axis.title.x = element_blank(),
                                    legend.position = "none",
                                    plot.title.position = 'plot',
                                    legend.title = element_blank(),
                                    legend.justification= c(1,0),
                                    panel.grid = element_blank()) +
                              guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
                              labs(subtitle = paste0(.x),
                                   x='', y='',
                                   fill = "COVID deaths"))

regional_death_plot_weekly[[1]] + regional_death_plot_weekly[[2]] +
   regional_death_plot_weekly[[3]] + regional_death_plot_weekly[[4]] +
   regional_death_plot_weekly[[5]] + regional_death_plot_weekly[[6]] +
   regional_death_plot_weekly[[7]] + regional_death_plot_weekly[[8]] +
   regional_death_plot_weekly[[9]] + plot_annotation(title='Weekly deaths in care homes')

 ggsave(here::here('output', 'regional_deaths_in_care_homes_weekly.png'))  
 
## Weekly COVID deaths ----
 
 ch_deaths_weekly %>% 
    filter(type=='covid' & date>'2020-02-17') %>% 
   ggplot(., aes(x = date, y = deaths_per_1000_beds, color = ch_region)) +
    geom_line() +
    geom_point(size=2) +
    geom_point(colour='white') +
    theme_THF() +
    scale_y_continuous(limits = c(0,15)) +   
    scale_colour_THF(labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID")
    ) +
    theme(axis.title.x = element_blank(),
          # legend.position = c(0.4,0),
          legend.position='right',
          plot.title.position = 'plot',
          legend.title = element_blank(),
          legend.justification= c(1,0),
          panel.grid = element_blank()) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(title = 'Weekly deaths per 1000 care home beds',
         x='', y='', caption = 'CQC data from 28 December 2019 to 1 May 2020',
         fill = "COVID deaths")
 
ggsave(here::here('output', 'regional_deaths_in_care_homes_weekly_covid_only.png'))  
 

## Weekly cummulative COVID deaths -----

ch_deaths_weekly %>% 
   filter(type=='covid' & date>'2020-02-17') %>% 
   ggplot(., aes(x = date, y = deaths_cum_per_1000_beds, color = ch_region)) +
   geom_line() +
   geom_point(size=2) +
   geom_point(colour='white') +
   theme_THF() +
   scale_y_continuous(limits = c(0,NA)) +   
   scale_colour_THF(labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID")
   ) +
   theme(axis.title.x = element_blank(),
         # legend.position = c(0.4,0),
         legend.position='top',
         plot.title.position = 'plot',
         legend.title = element_blank(),
         legend.justification= c(1,0),
         panel.grid = element_blank()) +
   guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
   labs(title = 'Weekly deaths per 1000 care home beds',
        x='', y='', caption = 'CQC data from 28 December 2019 to 1 May 2020',
        fill = "COVID deaths")

ggsave(here::here('output', 'regional_deaths_in_care_homes_weekly_covid_only.png'))  


