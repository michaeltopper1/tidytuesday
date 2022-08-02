library(tidyverse)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


office_ratings %>% 
  mutate(season_ep = glue::glue("{season}.{episode}"), .before = 1) %>% 
  group_by(season) %>% 
  mutate(first_date = min(air_date),
         max_date = max(imdb_rating)) %>% 
  ungroup() %>% 
  ggplot(aes(air_date, imdb_rating, color = factor(season), fill = factor(season),
             size = total_votes)) +
  geom_point() +
  geom_smooth(aes(air_date, imdb_rating), inherit.aes = F, color = "black") +
  labs(x = "", y = "IMDB Rating", title = "The Office Episode Ratings",
       color = "", size = "") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = "none")
office_ratings %>% 
  arrange(desc(imdb_rating)) %>% 
  head(20)
