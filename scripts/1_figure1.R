setwd("C:/Users/Subhayan Mukerjee/Work/twitter-landscape/")
library(tidyverse)
library(ggrepel)

elite_df = read.csv("data/weak_elite_ideologies.csv", as.is = T)
elites = elite_df$username

load("data/master_edge_list.Rdata")

names(following_df) = c("user", "following")

elite_following_df = following_df[tolower(following_df$following) %in% tolower(elites),]

#top 20 elites
df <- data.frame(table(following_df$following))
head(df[order(-df$Freq),],20)


elite_following_df = elite_following_df[elite_following_df$user != "Tip",]

elite_following_df$user = as.character(elite_following_df$user)
elite_following_df$following = as.character(elite_following_df$following)

elite_classes = read.csv("data/elite_classification.csv", as.is = T)

classes = c("hard news", "meme", "organization", "political pundit", 
            "political figure",  "brand", "media outlet", "public figure",
            "sports", "entertainment")


elite_activity <- read_csv("data/elites_activity.csv")

all_class_info <- NULL

for(class in classes) {
  
  class_elites <- elite_classes %>% 
                    filter(sector1 == class | sector2 == class | sector3 == class) %>%
                    pull(handle)
  
  n_elites <- length(class_elites)
  
  unique_followers_class <- elite_following_df %>%
                              filter(following %in% class_elites) %>%
                              pull(user) %>%
                              unique() %>%
                              length()
  
  class_activity <- elite_activity %>% 
                      filter(handle %in% tolower(class_elites)) %>%
                      pull(numberoftweets) %>%
                      sum()
  
  class_row <- c(class, n_elites, unique_followers_class, class_activity)
  all_class_info <- rbind(class_row, all_class_info)                        
}

all_class_info <- data.frame(all_class_info, row.names = NULL)
names(all_class_info) <- c("genre", "elite_count", "unique_followers", "number_of_tweets")

all_class_info$genre = as.character(all_class_info$genre)
all_class_info$elite_count = as.numeric(as.character(all_class_info$elite_count))
all_class_info$unique_followers = as.numeric(as.character(all_class_info$unique_followers))
all_class_info$number_of_tweets = as.numeric(as.character(all_class_info$number_of_tweets))

all_class_info$unique_followers_per_elite = all_class_info$unique_followers / all_class_info$elite_count
all_class_info$number_of_tweets_per_elite = all_class_info$number_of_tweets / all_class_info$elite_count

all_class_info_bkup <- all_class_info
all_class_info_bkup$number_of_tweets_scaled <- all_class_info_bkup$number_of_tweets/10000
all_class_info_bkup

all_class_info_long <- all_class_info_bkup %>% 
  select(-c(unique_followers_per_elite, number_of_tweets_per_elite)) %>%
  gather(key, value, c(elite_count, unique_followers, number_of_tweets_scaled))

ggplot(all_class_info, aes(x=unique_followers, y=number_of_tweets)) +
  geom_point(shape = 21, color = "black", fill = "salmon", aes(size=all_class_info$elite_count)) +
  scale_size_continuous(range = c(4, 16.4)) +
  geom_text_repel(aes(label = all_class_info$genre),
                  size = 4) +
  theme_bw() +
  theme(legend.position="none")

ggplot(all_class_info, aes(x=unique_followers, y=number_of_tweets)) +
  geom_point(shape = 21, color = "black", fill = "salmon", aes(size=all_class_info$elite_count)) +
  scale_size_continuous(range = c(4, 16.4)) +
  theme_bw() +
  theme(legend.position="none")
