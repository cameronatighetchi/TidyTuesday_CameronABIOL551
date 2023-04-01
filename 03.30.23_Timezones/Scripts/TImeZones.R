### Tidy Tuesday - Time Zones ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-03-31 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
library(ggmap)
library(maps)
library(mapdata)
#### Load Data ##########
timezones <- read_csv(here("03.30.23_Timezones", "Data", "timezones.csv")) # read in the csvs
countries_time <- read_csv(here("03.30.23_Timezones", "Data", "timezone_countries.csv"))
##### Functions/Plotting ###########################

## Data Wrangling ##############
df_combined <- inner_join(timezones, countries_time, by = "zone") %>% # join the two dataframes
  drop_na() %>% # drop all rows containing NA's
  filter(country_code == "US",  # only pick the US values
         longitude > -120) %>% # only choose the longitudes under -120
  mutate(comments = recode(comments, "Central - IN (Perry)" = "Central", # rename rows manually
                        "Central - IN (Starke)" = "Central",
                        "Central - MI (Wisconsin border)" = "Central",
                        "Central - ND (Mercer)" = "Central",
                        "Central - ND (Morton rural)" = "Central",
                        "Central - ND (Oliver)" = "Central",
                        "Central (most areas)" = "Central",
                        "Eastern - IN (Crawford)" = "Eastern",
                        "Eastern - IN (Da, Du, K, Mn)" = "Eastern",
                        "Eastern - IN (most areas)" = "Eastern",
                        "Eastern - IN (Pike)" = "Eastern",
                        "Eastern - IN (Pulaski)" = "Eastern",
                        "Eastern - IN (Switzerland)" = "Eastern",
                        "Eastern - KY (Louisville area)" = "Eastern",
                        "Eastern - KY (Wayne)" = "Eastern",
                        "Eastern - MI (most areas)" = "Eastern",
                        "Eastern (most areas)" = "Eastern",
                        "Mountain - ID (south); OR (east)" = "Mountain",
                        "Mountain (most areas)" = "Mountain",
                        "MST - Arizona (except Navajo), Creston BC" = "Mountain"))
  

### Use map data ############
usa<-map_data("usa") # load map data
states<-map_data("state") # load state data

### Plotting ################################
df_combined %>%
  ggplot(aes(x = longitude, y = latitude, color = comments))+ # use long and lat from combined dataframe as aesthetics
  geom_polygon(data = states, # plot map data as a geom_polygon
               aes(x = long, 
                   y = lat,
                   group = group),
               color = "black", 
               fill = "gray")+
  geom_point(size = 4) + # point size = 4
  guides(fill = FALSE)+ # get rid of legend
  scale_color_calc() + # change colors
  theme_minimal() +  # change theme
  labs(color = "Time Zone of Site", # plot titles and subtitles 
       title = "Time Zones Sites that participate in Day Light Savings measured in this study",
       subtitle = "Central, Pacific, Mountain, and Eastern times recorded in the U.S.") +
  theme(plot.background = element_rect(fill = "gray"), # change background color
        panel.grid = element_blank(), # get rid of grid panel
        axis.title = element_blank(), # get rid of axis titles
        axis.text = element_blank(), # get rid of axis text
        legend.background = element_rect(color = "black", size = .4, fill = NA), # put border around panel 
        plot.title = element_text(size = 15, face = "bold"), # change plot title size and font
        plot.subtitle = element_text(size = 12, face = "italic")) # change plot subtitle size and font
  
##ggsave(here("03.30.23_Timezones", "Output", "Timezone.png"))
