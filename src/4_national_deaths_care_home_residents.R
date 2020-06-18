library(THFstyle)
library(scales)
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#53a9cd'
df <- readRDS(here::here('data', 'care_home_deaths_england.rds'))
df %>% 
  pivot_longer(-date, names_to='type') %>% 
  filter(type %in% c('covid_daily', 'non_covid_daily')) %>% 
ggplot(., aes(x=date)) + 
  geom_col(aes(y=value, fill=fct_rev(type))) +
  geom_line(data=df, aes(y=non_covid_daily_2019), colour='grey20') +
  scale_y_continuous(labels = comma_format()) +
  scale_x_date(date_breaks = '2 weeks', date_labels = '%d %b') +
  scale_fill_THF() +
  theme_THF() +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(size=11, hjust = 0),
        plot.subtitle = element_text(size = 8)) +
  labs(x='', y='', 
       title = "Number of deaths of care home residents by date of death",
       subtitle = '28 December 2020 to 1 May 2020, registered up to 9 May 2020')

ggsave(here::here('output', 'national_deaths_ch_residents.png'))
