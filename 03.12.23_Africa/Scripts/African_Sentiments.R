### Tidy Tuesday -  ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-03-12 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
library(ggmap)
library(ggsn)
#### Load Data ######################################
afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
#### Functions ######################################
afri_countries <- left_join(afrisenti, languages) %>% #join together two dataframes based on afrisenti common data
  group_by(language) %>% # group languages together
  mutate(language_counts = n()) %>% # add a column with counts of all data
  group_by(language, label) %>% # group this column by language and label
  summarize(value = n()) %>%  #create a new dataset with all three columns that summarize data with new column value with counts of all types of language and labels grouped by
  mutate(percent = value/sum(value)) %>%  # percentage calculation to new dataset of values
  group_by(language) # group these calculations by language

#### Plotting ##########

afri_countries %>%
  ggplot(aes(x = percent, y = language, fill = label)) + #create a plot with legend as a fill for column label and x and y as percent and languages
  geom_bar(stat = "identity") +  #creates a barplot based on the summarize values computed
  theme_solarized() +  #changes theme
  labs(x = "Percent",  # adds x, y axis titles and plot title
       y = "Language",
       title = "Sentiment Labels of Tweets in African Spoken Languages",
       fill = "Content of Tweet") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) # changes the plot title adjustments and size


ggsave(here("03.12.23_Africa","Output","Percent_Language_Sentiment.png")) #saves the plot to output

  
  



