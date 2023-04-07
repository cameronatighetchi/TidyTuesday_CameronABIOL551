### Tidy Tuesday - Premier League Data ####
### Created by: Cameron Atighetchi #############
### Updated on: 2023-04-07 ####################
#### Load Libraries ######
library(tidyverse)
library(here)
library(ggthemes)
library(patchwork)
#### Load Data ###################
soccer_premier <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv")
#### Data Wrangling/ Functions #################################
unique(soccer_premier$Referee)
ref_premier <- soccer_premier %>%
  filter(HomeTeam %in% c("West Ham", "Man City", "Arsenal", "Tottenham")) %>%
  select(HomeTeam, Referee, FTHG, FTR) %>%
  mutate(FTR = ifelse(FTR == "A", "L", FTR)) %>%
  mutate(FTR = ifelse(FTR == "H", "W", FTR))
################################################################
ref_Man <- ref_premier %>%
  filter(HomeTeam == "Man City") %>%
  group_by(Referee, FTR) %>%
  summarize(count = n()) %>%
  filter(count >= 2)

Man_City <- ggplot(ref_Man, aes(x = Referee, y = count, fill = FTR)) +
  geom_col(position = "dodge") +
  labs(x = "Referee", y = "Count", fill = "Result") +
  scale_fill_manual(values = "#6caadd", labels = c("Win")) +
  theme_classic() +
  labs(title = "Manchester City") + 
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


################################
ref_West <- ref_premier %>%
  filter(HomeTeam == "West Ham") %>%
  group_by(Referee, FTR) %>%
  summarize(count = n()) %>%
  filter(count >= 2)

West_Colors <- c("#7A263A", "#1BB1E7", "#F3D459")

West <- ggplot(ref_West, aes(x = Referee, y = count, fill = FTR)) +
  geom_col(position = "dodge") +
  labs(x = "Referee", y = "Count", fill = "Result") +
  scale_fill_manual(values = West_Colors, labels = c("Win", "Loss", "Draw")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 3)) +
  labs(title = "West Ham") + 
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

############################################

ref_Arsenal <- ref_premier %>%
    filter(HomeTeam == "Arsenal") %>%
    group_by(Referee, FTR) %>%
    summarize(count = n()) %>%
    filter(count >= 2)
  
Arsenal <- ggplot(ref_Arsenal, aes(x = Referee, y = count, fill = FTR)) +
  geom_col(position = "dodge") +
  labs(x = "Referee", y = "Count", fill = "Result") +
  scale_fill_manual(values = "#DB0007", labels = c("Win")) +
  theme_classic() +
  labs(title = "Arsenal") + 
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

##############################################

ref_Tottenham <- ref_premier %>%
  filter(HomeTeam == "Tottenham") %>%
  group_by(Referee, FTR) %>%
  summarize(count = n()) %>%
  filter(count >= 2)

Tottenham <- ggplot(ref_Tottenham, aes(x = Referee, y = count, fill = FTR)) +
  geom_col(position = "dodge") +
  labs(x = "Referee", y = "Count", fill = "Result") +
  scale_fill_manual(values = "#132257", labels = c("Win")) +
  theme_classic() +
  labs(title = "Tottenham") + 
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

######################################################

combined_teams <- Man_City + West + Arsenal + Tottenham

combined_teams + plot_annotation(title = "Refrees that Officiated More Than One game for these 4 teams & The Result of Each",
                                 subtitle = "These teams love these refs!!!!!")

ggsave(here("04.07.23_Premier_League", "Output", "Premier_league.png"))
  

