library(tidyverse)
library(THFstyle)

ch_deaths <- ch_deaths %>% 
  mutate(deaths_per_10_beds=deaths/beds*10,
         deaths_cum_per_10_beds=deaths_cum/beds*10)

(ch_deaths %>%
   ggplot(aes(x = date, y = deaths_per_10_beds, color = type)) +
   geom_line() +
   geom_point() +
   theme_bw() +
   scale_colour_THF(labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID")
   ) +
   theme(axis.title.x = element_blank(),
         legend.position = "top",
         legend.title = element_blank(),
         legend.justification= c(1,0),
         panel.grid = element_blank()) +
   guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
   ylab("Number of deaths") +
   labs(title = "Daily deaths in care homes",
        subtitle =  str_c("CQC data from ", min (ch_deaths_eng$date), " to ",
                          max(ch_deaths_eng$date),
                          x='', y=''),
        fill = "COVID deaths") + facet_wrap(vars(ch_region)))


# Daily deaths
(ch_deaths %>%
   ggplot(aes(x = date, y = deaths, group = ch_region, color = ch_region)) +
   facet_wrap("type", ncol = 2) +
   geom_line() +
   theme_bw() +
   theme(axis.title.x = element_blank(),
         legend.position = "top",
         legend.title = element_blank(),
         legend.justification= c(1,0),
         panel.grid = element_blank()) +
   guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
   ylab("Number of deaths") +
   labs(title = "Daily deaths in care homes",
        subtitle =  str_c("CQC data from ", min(ch_deaths$date), " to ",
                          max(ch_deaths$date)),
        fill = "COVID deaths"))
