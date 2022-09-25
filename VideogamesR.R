library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(gganimate)
library(gifski)


setwd("/Users/alexchen/RProjects/MetaCritic Video Games")

data1 <- read.csv(file = "all_games.csv")

data <- data1 %>%
  mutate(release_date = mdy(release_date)) %>%
  mutate(year = as.integer(year(floor_date(release_date, "year")))) %>%
  mutate(user_review = as.double(user_review))



data$platform <- trimws(data$platform, which = c("left"))

nintendo <- c("3DS", "Dreamcast", "DS", "Game Boy Advance", "GameCube", "Nintendo 64", "Wii", "Wii U", "Switch")
playstation <- c("PlayStation", "PlayStation 2", "PlayStation 3", "PlayStation 4", "PlayStation 5", "PlayStation Vita", "PSP")
pc <- c("PC", "Stadia")
xbox <- c("Xbox", "Xbox 360", "Xbox One", "Xbox Series X")

data <- data %>% 
  mutate(platformtype = ifelse(platform %in% nintendo, "Nintendo", 
                        ifelse(platform %in% playstation, "Playstation",
                        ifelse(platform %in% pc, "PC & Stadia",
                        ifelse(platform %in% xbox, "Xbox", NA)))))

plotdata <- data %>%
  group_by(year, platformtype) %>%
  summarise(count = n())

graph1 <- plotdata %>%
  ggplot(aes(x = year, y = count, color = platformtype)) + 
  geom_line(size = 1.5, alpha = 0.8) +
  labs(title = "Numbers of Games Released on Each Platform By Year",
       x = "Year",
       y = "No. of Games Released",
       color = "Platform") +
  geom_point() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = 1995:2022)

graph1.animation = graph1 + 
  transition_reveal(year, keep_last = TRUE) +
  view_follow(fixed_y = TRUE) +
  labs(subtitle = "Year: {frame_along}")

animate(graph1.animation, fps = 30, end_pause = 120, res = 100, duration = 11, 
        width = 900, height = 450 )
anim_save("Games Graph.gif")
  
best_games <- data %>%
  group_by(year) %>%
  filter(meta_score == max(meta_score)) %>%
  filter(user_review == max(user_review)) %>%
  select(year, name, meta_score, user_review) %>%
  arrange(year, name)




                 