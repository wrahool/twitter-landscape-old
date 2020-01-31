#wt anlaysis
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(moments)
setwd("C:/Users/Subhayan/Documents/Work/twitter-landscape/")

elite_df = read.csv("data/weak_elite_ideologies.csv", as.is = T)
elite_df[is.na(elite_df$ideology),]$ideology = 0
elite_df[elite_df$ideology == -Inf,]$ideology = -3
elite_df[elite_df$ideology == Inf,]$ideology = 3

names(elite_df)[2] = "ideology"

elite_freq = read.csv("data/elites_activity.csv", as.is = T)
names(elite_freq)[1] = "username"

elite_freq$numberoftweets_scaled = (elite_freq$numberoftweets - min(elite_freq$numberoftweets))/(max(elite_freq$numberoftweets) - min(elite_freq$numberoftweets))

elite_df$username = tolower(elite_df$username)
elite_freq_df = merge(elite_df, elite_freq)
elite_freq_df$corrected_ideology = elite_freq_df$ideology * elite_freq_df$numberoftweets_scaled

elite_followers_count = read.csv("data/elite_followers_count.csv")


load("data/walktrap_results.Rdata")
comm_membership <- data.frame(cbind(wt$names, wt$membership))
names(comm_membership) <- c("username", "community")

comm_membership$username <- tolower(as.character(comm_membership$username))

library(tidyverse)

elite_master_tbl <- elite_freq_df %>%
  inner_join(comm_membership) %>%
  inner_join(elite_followers_count)

median1 <- ddply(elite_master_tbl, "community", summarise, grp.median=median(ideology, na.rm = T))

median2 <- ddply(elite_master_tbl, "community", summarise, grp.median.w=median(corrected_ideology, na.rm = T))

elite_master_tbl %>% inner_join(median1) -> elite_master_tbl
elite_master_tbl %>% inner_join(median2) -> elite_master_tbl

median.tbl <- elite_master_tbl %>%
  group_by(community) %>%
  mutate(median.id = median(ideology))

p1 <- ggplot(elite_master_tbl, aes(ideology)) +
  geom_density(fill = "snow3", alpha = 0.6, colour="snow3") +
  facet_wrap(~community, nrow = 5, scales = "free") + 
  xlim(-3, 3) + 
  geom_vline(aes(xintercept=grp.median, colour = "red")) +
  geom_vline(xintercept=0, linetype = "dashed") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  theme_bw() +
  theme(legend.position = "none")

p2 = ggplot(elite_master_tbl, aes(corrected_ideology)) +
  geom_density(fill = "snow3", alpha = 0.6, colour="snow3") +
  facet_wrap(~community, nrow = 5, scales = "free") + 
  xlim(-3, 3) + 
  geom_vline(aes(xintercept=grp.median.w), color = "red") +
  geom_vline(xintercept=0, linetype = "dashed") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  theme_bw() +
  theme(legend.position = "none")

# figure 7
grid.arrange(p1, p2, nrow = 1)

comm_stats <- NULL

for(c in 1:max(wt$membership)) {

  c_ideologies <- elite_master_tbl %>%
    filter(community %in% c) %>%
    pull(ideology)
  
  c_corrected_ideologies <- elite_master_tbl %>%
    filter(community %in% c) %>%
    pull(corrected_ideology)
  
  c_tweetcount <- elite_master_tbl %>%
    filter(community %in% c) %>%
    pull(numberoftweets)
  
  c_followers <- elite_master_tbl %>%
    filter(community %in% c) %>%
    pull(followers)
  
  c_elite_count <- elite_master_tbl %>%
    filter(community %in% c) %>%
    pull(username)
  
  c_i_mean = mean(c_ideologies)
  c_i_sd = sd(c_ideologies)
  c_i_median = median(c_ideologies)
  c_i_skewness = skewness(c_ideologies)
  c_i_kurtosis = kurtosis(c_ideologies)
  
  c_ci_mean = mean(c_corrected_ideologies)
  c_ci_sd = sd(c_corrected_ideologies)
  c_ci_median = median(c_corrected_ideologies)
  c_ci_skewness = skewness(c_corrected_ideologies)
  c_ci_kurtosis = kurtosis(c_corrected_ideologies)
  
  total_tweets = sum(c_tweetcount)
  total_followers = sum(c_followers)
  elite_count = length(c_elite_count)
  
  c_row = c(c,
            c_i_mean, c_i_sd, c_i_median, c_i_skewness, c_i_kurtosis,
            c_ci_mean, c_ci_sd, c_ci_median, c_ci_skewness, c_ci_kurtosis,
            total_tweets, total_followers, elite_count)
  
  comm_stats <- rbind(comm_stats, c_row)
  
}

comm_stats <- data.frame(comm_stats, row.names = NULL)

names(comm_stats) = c("community",
                      "mean_id", "sd_id", "median_id", "skewness_id", "kurtosis_id",
                      "mean_cid", "sd_cid", "median_cid", "skewness_cid", "kurtosis_cid",
                      "total_tweets", "total_followers", "elite_count")

library(ggrepel)

#figure 8
ggplot(comm_stats, aes(x=total_followers, y=total_tweets)) +
  geom_point(color = "black", shape = 21, aes(fill = comm_stats$median_id,size=comm_stats$elite_count)) +
  geom_text_repel(aes(label = comm_stats$community),
                   size = 4) +
  scale_size_continuous(range = c(4, 11.4)) +
  scale_fill_gradient(low = "white", high = "salmon") +
  theme_bw() +
  theme(legend.position="none")

ggplot(comm_stats, aes(x=total_followers, y=total_tweets)) +
  geom_point(color = "black", shape = 21, aes(fill = comm_stats$median_id,size=comm_stats$elite_count)) +
  scale_size_continuous(range = c(4, 11.4)) +
  scale_fill_gradient(low = "white", high = "salmon") +
  theme_bw() +
  theme(legend.position="none")

comm_stats



