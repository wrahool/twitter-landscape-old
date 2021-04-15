setwd("C:/Users/Subhayan Mukerjee/Work/twitter-landscape/")
library(tidyverse)
library(ggrepel)

elite_df <- read_csv("data/mturk_ideologies.csv")
elites <- elite_df$handle

elites <- c(elites, "realDonaldTrump", "BernieSanders")

load("data/master_edge_list.Rdata")

following_tbl <- following_df %>%
  as_tibble() %>%
  rename("user" = 1, "following" = 2) %>%
  mutate(user = as.character(user), following = as.character(following))

elite_following_tbl <- following_tbl %>%
  filter(tolower(following) %in% tolower(elites))

#top 20 elites
elite_following_tbl %>%
  pull(following) %>%
  table() %>%
  data.frame() %>%
  as_tibble() %>%
  arrange(-Freq) %>%
  head(20)

elite_following_tbl <- elite_following_tbl %>%
  filter(user != "Tip")

elite_genres <- read_csv("data/elite_classification.csv")

genres <- c("hard news", "meme", "organization", "political pundit", 
            "political figure",  "brand", "media outlet", "public figure",
            "sports", "entertainment")


elite_activity <- read_csv("data/elites_activity.csv")

all_genre_info <- NULL

for(genre in genres) {
  
  genre_elites <- elite_genres %>% 
    filter(sector1 == genre | sector2 == genre | sector3 == genre) %>%
    pull(handle) %>% 
    unique()
  
  n_elites <- length(genre_elites)
  
  unique_followers_genre <- elite_following_tbl %>%
    filter(following %in% genre_elites) %>%
    pull(user) %>%
    unique() %>%
    length()
  
  genre_activity <- elite_activity %>% 
    filter(handle %in% tolower(genre_elites)) %>%
    pull(numberoftweets) %>%
    sum()
  
  all_genre_info <- all_genre_info %>%
    rbind(c(genre = genre, elite_count = n_elites, unique_followers = unique_followers_genre, number_of_tweets = genre_activity)) %>%
    as_tibble()
}

all_genre_info <- all_genre_info %>%
  mutate(elite_count = as.numeric(elite_count),
         unique_followers = as.numeric(unique_followers),
         number_of_tweets = as.numeric(number_of_tweets)) %>%
  mutate(unique_followers_per_elite = unique_followers / elite_count,
         number_of_tweets_per_elite = number_of_tweets / elite_count)

all_genre_info %>%
  ggplot(aes(x=unique_followers, y=number_of_tweets)) +
  geom_point(shape = 21, color = "black", fill = "salmon", aes(size=elite_count)) +
  scale_size_continuous(range = c(4, 16.4)) +
  geom_text_repel(aes(label = genre),
                  size = 4.5) +
  labs(x="# unique followers", y="# tweets", size="# elites") +
  theme_bw()

all_genre_info %>%
  ggplot(aes(x=unique_followers, y=number_of_tweets)) +
  geom_point(shape = 21, color = "black", fill = "salmon", aes(size=elite_count)) +
  scale_size_continuous(range = c(4, 16.4)) +
  labs(x="# unique followers", y="# tweets", size="# elites") +
  theme_bw()

