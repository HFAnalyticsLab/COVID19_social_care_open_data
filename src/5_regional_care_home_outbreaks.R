library(tidyverse)
library(THFstyle)

## Read in data

df <- readRDS(here::here('data', 'care_home_outbreaks_england.rds'))

df <- df %>% 
  mutate(outbreaks_cum_percent=outbreaks_cum/number_of_care_homes)
## cumulative

ggplot(df, aes(x=date, y=outbreaks_cum, group=phe_centre, colour=phe_centre)) + 
  geom_line() + geom_point(size=2) + geom_point(colour='white') +
  labs(x='', y='', title = "Cumulative number of outbreaks in care homes by region") +
  scale_colour_THF() +
  theme_THF() 

ggsave(here::here('output', 'cumulative_care_home_outbreaks.png'))
  

## cumulative percentage

ggplot(df, aes(x=date, y=outbreaks_cum_percent, group=phe_centre, colour=phe_centre)) + 
  geom_line() + geom_point(size=2) + geom_point(colour='white') +
  labs(x='', y='', title = "Cumulative number of outbreaks in care homes by region") +
  scale_colour_THF() +
  theme_THF() 

ggsave(here::here('output', 'cumulative_percent_care_home_outbreaks.png'))


