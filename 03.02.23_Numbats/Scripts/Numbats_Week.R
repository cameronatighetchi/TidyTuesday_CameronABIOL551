### Tidy Tuesday -  ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-16 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(lubridate)
### Load data ######
Numbats <- read.csv(here("03.02.23_Numbats","Data","numbats.csv" )) #read in numbats.csv data
head(Numbats) 
### Functions/Data Analysis #########################

numbats_mapping <- Numbats %>%
  drop_na(month) %>% #drop all NA values in month
  drop_na(decimalLongitude) %>% #drop all NA values in Longitude
  drop_na(decimalLatitude) %>% # drop all NA values in Latitude
  select(month, decimalLatitude, decimalLongitude) #select month, latitude, longitude columns

austra <- ne_countries(scale = "medium", returnclass = "sf") #retrieve a picture of Australia from rnaturalearth


ggplot(data = austra) + #plot on ggplot with australia as a background
  geom_sf(fill = "white") + #fill the australia plot white color
  geom_point(data = numbats_mapping, #loads data in
             aes(x = decimalLongitude, y = decimalLatitude, color = month),  #x-axis, y-axis data and group colors by month
             size = 2, position = "jitter") + #  changes size of points and jitter allows to see them better if clustered
  coord_sf(xlim = c(110, 155), ylim = c(-40, -11), expand = FALSE) + # converts our long-lat values into plottable values set with a limit that matches the limits of lat-long
  facet_wrap(~month) + #facet wrap the plots per month (one plot per month)
  labs(color = "Month", #change legend title to capital M
       title = "Numbats Spottings in Australia per Month") + #adds plot title
  theme_dark() + #dark theme
  theme(axis.text = element_blank(), #gets rid of axis titles and text
        axis.title = element_blank(),
        plot.background = element_rect(fill = "beige"), # changes background color
        plot.title = element_text(hjust = 0.6), # centers plot title
        panel.grid = element_blank(), #gets rid of panel grids in facets
        axis.ticks = element_blank()) #gets rid of scaled axis ticks 
  



ggsave(here("03.02.23_Numbats","Output","Australia-Numbat-Week3.png"))




