# Loading library ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(THFstyle)



# Import data -------------------------------------------------------------
# Data from pre-print 
# Grimm, F.; Hodgson, K.; Brine, R.; Deeny, S.R. Hospital Admissions From Care Homes in England 
# During the COVID-19 Pandemic: A Retrospective, Cross-Sectional Analysis Using Linked Administrative Data. 
# Preprints 2021, 2021020593 (doi: 10.20944/preprints202102.0593.v1). 

electives <- read_csv(here::here('sprint_2', 'data', "Elective_causes_chnursing_March-May_countwide.csv"))


# Combined residential and nursing homes ----------------------------------


electives <- electives %>% 
  group_by(MainICD10Cat) %>% 
  summarise(Year_2019 = sum(Year_2019),
            Year_2020 = sum(Year_2020)) %>% 
  mutate(change = Year_2020 - Year_2019,
         pct_change = 100* change/Year_2019,
         combined = str_c(sprintf("%+d", change), " (", round(pct_change, 0), ")"),
         combined = gsub(")", "%)", combined))

electives <- electives %>% 
  mutate(ICD10_name = case_when(MainICD10Cat == "2" ~ "Neoplasms (II)",
                                MainICD10Cat == "3" ~ "Blood (III)",
                                MainICD10Cat == "5" ~ "Mental, behavioural (V)",
                                MainICD10Cat == "6" ~ "Nervous system (VI)",
                                MainICD10Cat == "7" ~ "Eye (VII)",
                                MainICD10Cat == "9" ~ "Circulatory system (IX)",
                                MainICD10Cat == "11" ~ "Digestive system (XI)",
                                MainICD10Cat == "12" ~ "Skin (XII)",
                                MainICD10Cat == "13" ~ "Musculoskeletal (XIII)",
                                MainICD10Cat == "14" ~ "Genitourinary system (XIV)",
                                MainICD10Cat == "18" ~ "Not elsewhere classified (XVIII)",
                                MainICD10Cat == "19" ~ "Injury, poisoning (XIX)",
                                MainICD10Cat == "21" ~ "Factors infl. health status (XXI)",
                                MainICD10Cat == "Unknown" ~  "Unknown",
                                MainICD10Cat == "Other" ~ "Other")) %>% 
  filter(ICD10_name != "Other" & ICD10_name != "Unknown") %>% 
  mutate(ICD10_name = fct_reorder(ICD10_name, Year_2019))


# Visualise ---------------------------------------------------------------
## FIGURE 6 ##

electives %>% 
  select(ICD10_name, Year_2019, Year_2020) %>% 
  pivot_longer(-ICD10_name, names_to = "year", values_to = "admissions") %>% 
  mutate(year = fct_rev(year)) %>% 
  ggplot(aes(x = ICD10_name, y = admissions, fill = year))+
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_THF(labels=c("March - May 2020", "March - May 2019"))+
  geom_segment(data = electives , aes(x = as.numeric(ICD10_name) - 0.25, xend = as.numeric(ICD10_name) + 0.25, y = Year_2019 + 15, yend = Year_2019 + 15),
                color = "black", size = 0.25, position = "dodge", inherit.aes = FALSE) +
  geom_text(data = electives , aes(x =  as.numeric(ICD10_name), y = Year_2019 + 48, label = str_c(round(pct_change,0), "%")), color = "black", 
            show.legend = FALSE, size = 3, inherit.aes = FALSE) +
  theme_THF() +
  labs(x= "", y="")+
  coord_flip() +
  theme(plot.title = element_text(size=16),
        legend.text=element_text(size=12),
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11, hjust = 1),
        panel.grid.major.y = element_blank())

ggsave(here::here('sprint_2','graphs', 'electives.png'),dpi=300,
       width = 10, height = 6.5) 


ggsave(here::here('sprint_2','graphs', 'electives.pdf'),dpi=300, device = "pdf",
       width = 10, height = 6.5) 

write_csv(electives, here::here('sprint_2','graphs', 'electives.csv'))
