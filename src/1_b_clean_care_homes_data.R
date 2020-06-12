####
## Visualise ONS data on COVID deaths in care homes
####

library(tidyverse) 
library(tidylog)
library(readxl)
library(janitor)

library(broom)
library(geojsonio)
library(maptools)

# Colors ------------------------------------------------------------------
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#aad3e5'
THF_1_purple <- '#744284'

# Import data -------------------------------------------------------------
# Deaths involving COVID-19 in the care sector, England and Wales: 
# deaths occurring up to 1 May 2020 and registered up to 9 May 2020 (provisional)
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/deathsinvolvingcovid19inthecaresectorenglandandwales/deathsoccurringupto1may2020andregisteredupto9may2020provisional


# mortality by region
ch_deaths <- read_xlsx(here::here('data', 'original data', "deathsinvolvingcovid19inthecaresectordataset.xlsx"), sheet = "Table 10", skip = 4, col_types = c("date", rep("numeric", 21)),
                       n_max = 126)

new_colnames <- gsub("...[0-9]{1,2}", "", colnames(ch_deaths))
new_colnames[1] <- "date"
new_colnames[2:11] <- str_c(new_colnames[2:11], "_all-cause")
new_colnames[12] <- "temp"
new_colnames[13:22] <- str_c(new_colnames[13:22], "_covid")

ch_deaths <- ch_deaths %>% 
  set_names(new_colnames) %>% 
  select(-temp) %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  pivot_longer(-date, names_to = "region_type", values_to = "deaths") %>% 
  separate(region_type, into = c("region", "type"), sep = "_") %>% 
  filter(region != "Wales") %>% 
  mutate(region=case_when(region=='East' ~ "East of England",
                          region=='Yorkshire and the Humber'~ 'Yorkshire and Humber',
         TRUE ~ region))

ch_deaths <- ch_deaths %>% 
  group_by(type, region) %>% 
  arrange(date) %>% 
  mutate(deaths_cum = cumsum(deaths)) %>% 
  ungroup()
ch_summary <- read.csv(here::here('data', 'original data' ,'CH_summary_2020-04-01_ch_region.csv'))
ch_deaths <- ch_deaths %>% 
  mutate(region=str_to_upper(region))
ch_deaths <- ch_summary %>% 
 left_join(ch_deaths, by=c('ch_region'='region')) %>% 
  mutate(ch_region=str_to_title(ch_region))  

saveRDS(ch_deaths, "data/CH_deaths_by_region.Rds")

ch_deaths %>% 
  filter(date == max(date) & type == "covid") %>% 
  select(-date, -deaths) %>% 
  write_csv("data/CH_deaths_by_region_covid_cumulative.csv")


ch_deaths_eng <- ch_deaths %>% 
  group_by(date, type) %>% 
  summarise(deaths = sum(deaths),
            deaths_cum = sum(deaths_cum))

saveRDS(ch_deaths_eng, "data/CH_deaths_england.Rds")


# Line graphs - England ---------------------------------------------------

# (ch_deaths_eng %>% 
#    ggplot(aes(x = date, y = deaths, group = type, color = type)) +
#    geom_line() +
#    geom_point() +
#    theme_bw() +
#    scale_color_manual(values = c(THF_red, THF_50pct_light_blue),
#                       labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID")
#    ) +
#    theme(axis.title.x = element_blank(),
#          legend.position = "top",
#          legend.title = element_blank(),
#          legend.justification= c(1,0),
#          panel.grid = element_blank()) +
#    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
#    ylab("Number of deaths") +
#    labs(title = "Daily deaths in care homes", 
#         subtitle =  str_c("CQC data from ", min (ch_deaths_eng$date), " to ", 
#                           max(ch_deaths_eng$date)),
#         fill = "COVID deaths")) %>% 
#   ggsave("graphs/sprint_2/Care_homes_deaths_England.png", ., width = 6, height = 5)
# 
# (ch_deaths_eng %>% 
#     ggplot(aes(x = date, y = deaths_cum, group = type, color = type)) +
#     geom_line() +
#     geom_point() +
#     theme_bw() +
#     scale_color_manual(values = c(THF_red, THF_50pct_light_blue),
#                        labels = c("Deaths in care homes, all-cause", "Deaths in care homes, COVID")) +
#     theme(axis.title.x = element_blank(),
#           legend.position = "top",
#           legend.title = element_blank(),
#           legend.justification= c(1,0),
#           panel.grid = element_blank()) +
#     guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
#     ylab("Number of deaths") +
#     labs(title = "Cumulative deaths in care homes", 
#          subtitle =  str_c("CQC data from ", min (ch_deaths_eng$date), " to ", 
#                            max(ch_deaths_eng$date)),
#          fill = "COVID deaths")) %>% 
#   ggsave("graphs/sprint_2/Care_homes_deaths_England_cumulative.png", ., width = 6, height = 5)
# 
# 
# # Regional aggregates ---------------------------------------------------
# 
# 
# # Daily deaths
# (ch_deaths %>% 
#    ggplot(aes(x = date, y = deaths, group = region, color = region)) +
#    facet_wrap("type", ncol = 2) +
#    geom_line() +
#    theme_bw() +
#    theme(axis.title.x = element_blank(),
#          legend.position = "top",
#          legend.title = element_blank(),
#          legend.justification= c(1,0),
#          panel.grid = element_blank()) +
#    guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
#    ylab("Number of deaths") +
#    labs(title = "Daily deaths in care homes", 
#         subtitle =  str_c("CQC data from ", min(ch_deaths$date), " to ", 
#                           max(ch_deaths$date)),
#         fill = "COVID deaths")) %>% 
#   ggsave("graphs/sprint_2/Care_homes_deaths_regions.png", ., width = 7, height = 5)
# 
# # Cumulative deaths
# 
# (ch_deaths %>% 
#     group_by(type, region) %>% 
#     arrange(date) %>% 
#     mutate(deaths_cum = cumsum(deaths)) %>% 
#     ggplot(aes(x = date, y = deaths_cum, group = region, color = region)) +
#     facet_wrap("type", ncol = 2) +
#     geom_line() +
#     theme_bw() +
#     theme(axis.title.x = element_blank(),
#           legend.position = "top",
#           legend.title = element_blank(),
#           legend.justification= c(1,0),
#           panel.grid = element_blank()) +
#     guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
#     ylab("Number of deaths") +
#     labs(title = "Cumulative deaths in care homes", 
#          subtitle =  str_c("CQC data from ", min(ch_deaths$date), " to ", 
#                            max(ch_deaths$date)),
#          fill = "COVID deaths")) %>% 
#   ggsave("graphs/sprint_2/Care_homes_deaths_regions_cumulative.png", ., width = 7, height = 5)
# 
# 
# region_order <- c("London", "South East", "South West", "East", "East Midlands", "West Midlands",
#                   "Yorkshire and the Humber",  "North East", "North West")
# 
# (ch_deaths %>% 
#     group_by(region, type) %>% 
#     summarise(deaths = sum(deaths)) %>% 
#     group_by(type) %>% 
#     mutate(pct = str_c(round(100* deaths / sum(deaths), 0), "%"),
#            ch_region_fct = factor(region, levels = rev(region_order))) %>% 
#     ggplot(aes(x = ch_region_fct, y = deaths)) +
#     facet_wrap("type", ncol = 2, scales = "free_x", labeller = as_labeller(c("all-cause" = "All-cause deaths",
#                                                                              "covid" = "Deaths related to COVID"))) +
#     geom_bar(stat = "identity", position = position_dodge(), fill = THF_red) +
#     geom_text(aes(label = pct, y = 0.9*deaths), size = 2, color = "white") +
#     coord_flip() +
#     theme_bw() +
#     theme(axis.title = element_blank(),
#           legend.position = "top",
#           legend.justification= c(1,0),
#           panel.grid.major.y = element_blank()) +
#     labs(title = "Cumulative deaths in care homes, by region", 
#          subtitle = str_c("CQC data from ", min(ch_deaths$date), " to ", 
#                           max(ch_deaths$date)))) %>% 
#   ggsave("graphs/sprint_2/Care_homes_deaths_by_region_bar.png", ., width = 6, height = 3)
# 
