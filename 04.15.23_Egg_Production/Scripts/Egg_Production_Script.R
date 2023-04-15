### Tidy Tuesday - Egg Production ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-04-15 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
library(patchwork)
library(lubridate)
#### Load Data ###################
eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
#### Functions ########################

# We want to create a plot where we see all table eggs means of production per year

tabled_eggs <- eggproduction %>%
  filter(prod_type == "table eggs") # filter out only table egg values

year_data <- tabled_eggs %>%
  mutate(year = year(ymd(observed_month))) %>% # Create a new column with only the year of each observation
  select(year, prod_type, prod_process) # select year, production type, and production process columns

egg_counts <- year_data %>%
  group_by(year, prod_process) %>% # group each row by year and production process
  summarise(count = n()) %>% # summarise by both of these grouped values by counting
  mutate(percent = count / sum(count) * 100) # turn this count into a percentage to be graphed


ggplot(egg_counts, aes(x = year, y = percent, fill = prod_process)) + # create a plot by year and percent and fill by produciton process
  geom_bar(stat = "identity", color = "black") + # create a bar graph using this statistic and put a black border around the bars
  scale_fill_manual(values = c("#fdee00", "#ba9f02", "#ffb900"), # manually fill in these colors for the values
                    labels = c("All", "Cage-Free (Non-Organic)", "Cage-Free (Organic)")) + # change the legend values
  labs(title = "Percentage of Table Eggs Produced by Different Processes (2016-2021)", # create a plot title
       x = "Year", y = "Percentage", # axis labels
       fill = "Production Method") + # fill legend label
  scale_x_continuous(breaks = seq(2016, 2021, by = 1)) + # change the scaler values for the x axis
  theme_clean() + # change the theme
  theme(plot.title = element_text(size = 13), # change the plot title size
        plot.background = element_rect(fill = "beige"))  # change the plot table background

ggsave(here("04.15.23_Egg_Production", "Output", "Egg_Production_Plot.png"))  # save the plot to outputs

  