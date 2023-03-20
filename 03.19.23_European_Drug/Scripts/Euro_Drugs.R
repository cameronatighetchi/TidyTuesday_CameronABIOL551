### Tidy Tuesday - European Drugs ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-03-19 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
#### Load Data ##########
euro_drugs <- read_csv(here("03.19.23_European_Drug","Data", "drugs.csv")) # reads in data from Data file in folder
#### Functions/Plotting ###################
euro_drugs %>%
  select("therapeutic_area", "active_substance", "authorisation_status") %>% # selects three rows
  filter(therapeutic_area == "HIV Infections") %>% # filters out all data that doesn't meet HIV Infections specifically
  group_by(authorisation_status) %>% # group by whether or not these drugs are approved
  summarize(drug_count = n()) %>% # count these authorisation status of these drugs
  ungroup() %>% # ungroup
  arrange(drug_count) %>% # arrange the values to each row 
  mutate(percent = drug_count / sum(drug_count) * 100) %>% # create a separate column for percent of either authorized or withdrawn
  ggplot(aes(x = authorisation_status, y = percent, fill = authorisation_status)) + # plot using x as author. and y as percent fill using the two values in author. status
  geom_bar(stat = "identity", position = "dodge") + # create a bar plot using percent of authorized versus not
  labs(x = "Authorization Status",   # create lables for x,y, title and legend
       y = "Percent", 
       title = "Authorized vs. Withdrawn Therapeutic Methods used to treat HIV Infection", 
       fill = "Authorization Status") +
  theme_clean() + # change theme to clean
  theme(axis.text.x = element_blank(),  # get rid of x axis text
        plot.title = element_text(size = "13")) + # change plot title size
  scale_fill_stata(name = "Authorization Status", # change the text for both legend title and values and change the color of the fill values
                      labels = c("Authorized", "Withdrawn"))


ggsave(here("03.19.23_European_Drug","Output","HIV_Infections.png")) #saves the plot to output

