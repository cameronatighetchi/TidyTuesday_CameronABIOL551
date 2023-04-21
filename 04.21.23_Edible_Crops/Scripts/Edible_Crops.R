### Tidy Tuesday - Egg Production ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-04-21 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
library(patchwork)
#### Load Data ###################
founder_crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-18/founder_crops.csv')
#### Functions ########################
edible_crops <- founder_crops %>%
  filter(!is.na(edibility)) %>%
  select(source_site_name, n, prop, edibility) %>%
  mutate(edibility = ifelse(edibility == "flowers", "Flowers, Stems", edibility),
         edibility = ifelse(edibility == "flowers, stems", "Flowers, Stems", edibility),
         edibility = ifelse(edibility == "leaves, root", "Leaves, Stems, Roots", edibility),
         edibility = ifelse(edibility == "leaves", "Leaves, Stems, Roots", edibility),
         edibility = ifelse(edibility == "leaves, stems", "Leaves, Stems, Roots", edibility),
         edibility = ifelse(edibility == "bulbs", "Bulbs", edibility),
         edibility = ifelse(edibility == "rhizomes, stems and leaves,", "Rhizomes/Stems/Leaves", edibility),
         edibility = ifelse(edibility == "stems", "Flowers, Stems", edibility)) %>%
  group_by(source_site_name, edibility) %>%
  summarize(total_n = sum(n)) %>%
  mutate(total_n_site = sum(total_n),
         edibility_percentage = total_n/total_n_site*100) %>%
  ungroup()


ggplot(edible_crops, aes(x = source_site_name, y = edibility_percentage, fill = edibility)) +
  geom_col(position = "stack") +
  labs(x = "Site Name", y = "Percentage of Edibility", fill = "Edibility") +
  ggtitle("Percentage of Edibility by Site") + 
  theme_clean() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "Site Distribution",
       y = "Percentage of Type of Edibility") +
  scale_fill_manual(values = c("gray", "lightblue", "#385457", "#176c96","#8e9dde"))


ggsave(here("04.21.23_Edible_Crops", "Output", "Edible_Crops.png"))

write.csv(founder_crops, file = here("04.21.23_Edible_Crops", "Data", "founder_crops.csv"))
          
          
          
          