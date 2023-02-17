### Tidy Tuesday -  ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-02-16 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
library(ggthemr)  # New theme package
### Load data ######
Hollywood <- read.csv(here("02:16:23 - Hollywood Week","Data","hollywood_age.csv" ))
### Functions/Data Analysis #########################
ggthemr('chalk', type = "outer") # sets new theme first
Hollywood %>%
  filter(release_year <= 1980) %>% # filter out and display movies release on and before 1980
  group_by(release_year) %>% # group these movies by release_year 
  summarise(mean_agediff = mean(age_difference, na.rm = TRUE)) %>% # give me the mean age difference between actors in each year
  ggplot(aes(x = release_year, y= mean_agediff)) + # plot release year before 1980 versus mean age difference of actors
  geom_area(fill = "#feffe0", alpha = 0.6) + #create an area plot with opacity low
  geom_line(color="#f8faca", size=1.5) + # create a line on top of the area plot
  xlim(1935, 1980) + # set x-limit from lowest to highest year
  ylim(0,30) + #set y limit
  labs(x = "Release Year", #labels for plot title and axes
       y = "Average Age Difference per Year", 
       title = "Average Age Difference per Release Year (1980 and Prior)") +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) #change Plot title font size
        
ggsave(here("02:16:23 - Hollywood Week","Output","02/16/23Plot.png"))
  
  
  
  
  