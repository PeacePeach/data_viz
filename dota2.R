library(tidyverse)

# ---------------------------------
# Dota 2 Prize Pool Trend
# ---------------------------------

# Load the data
prizepool <- read_csv("data/dota2_prizepool.csv")

# Create the plot
prizepool %>% 
  mutate(total = total/1000000) %>% 
  ggplot(aes(x = tournament, y = total)) +
  geom_line(group = 1, color = "#FFC30B", size = 1.05) +
  geom_point(color = "#F9A602", size = 3) +
  ggtitle("Dota 2 The International Prize Pool Trend", subtitle = "2011 - 2018") +
  labs(x = "Tournaments", y = "Total Prize Pool in millions (USD)") + 
  theme(panel.background = element_rect(fill = "White"),
        strip.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "#F0F0F0"),
        plot.title = element_text(face = 'bold', size = 22, hjust = 0.5),
        plot.subtitle = element_text(face = 'italic', size = 17, hjust = 0.5),
        strip.text = element_text(size = 13),
        axis.title = element_text(face = "bold", size = 16),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.text = element_text(size = 10))


# ---------------------------------
# Dota 2 Prize Pool Trend
# ---------------------------------

# Load the data
players <- read_csv("data/ti8_players.csv", col_types = cols(countries = col_factor(NULL)))

# Reorder the factor level for countries based on the number of players
fct_reorder(players$countries, -players$total)

# Select only the top 10 countries 
top10 <- players %>% top_n(10, total)

# Create the plot
top10 %>% 
  mutate(malaysia = factor(if_else(countries == "Malaysia", 1, 0))) %>% 
  ggplot(aes(x = fct_reorder(countries, total), y = total)) + 
  geom_bar(aes(fill = malaysia), stat = "identity", color = "#404040") + 
  coord_flip() +
  ggtitle("Top 10 Country of Origin of Dota2 Players\nat The International 8") +
  labs(x = "Countries", y = "Total Number of Players") +
  scale_fill_manual(values = c("#A9A9A9", "#7851A9")) +
  theme(panel.background = element_rect(fill = "White"),
        strip.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "#E0E0E0"),
        plot.title = element_text(face = 'bold', size = 22, hjust = 0.5),
        plot.subtitle = element_text(face = 'italic', size = 17, hjust = 0.5),
        strip.text = element_text(size = 13),
        axis.title = element_text(face = "bold", size = 16),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.text = element_text(size = 11.5),
        legend.position = "none")
