setwd("C:/Users/Subhayan Mukerjee/Work/twitter-landscape/")

library(tidyverse)
library(corrr)
library(cowplot)
library(broom)

# function to scale a vector to between [0,1]
scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}

mturk_tbl <- read_csv("data/mturk_ideologies.csv")
barbera_tbl <- read_csv("data/weak_elite_ideologies.csv")
elite_freq_tbl <- read_csv("data/elites_activity.csv")

barbera_tbl <- barbera_tbl %>%
  mutate(handle = tolower(username)) %>%
  mutate(recoded_ideology = ifelse(is.na(ideology), 0, ideology)) %>%
  mutate(recoded_ideology = ifelse(recoded_ideology == -Inf, -3, recoded_ideology)) %>%
  mutate(recoded_ideology = ifelse(recoded_ideology == Inf, 3, recoded_ideology)) %>%
  rename(barbera_ideology = recoded_ideology) %>%
  select(handle, barbera_ideology)

elite_freq_tbl <- elite_freq_tbl %>%
  mutate(handle = tolower(handle)) %>%
  rename(freq = numberoftweets) %>%
  mutate(freq_scaled = scale_01(freq))
  
full_tbl <- mturk_tbl %>%
  inner_join(barbera_tbl) %>%
  inner_join(elite_freq_tbl)

# different kinds of plots with only mturk
plot1 <- ggplot(full_tbl, aes(mean_rating_all)) +
  geom_density() +
  ggtitle("all annotations") +
  theme_bw()

plot2 <- ggplot(full_tbl, aes(mean_rating_attn)) +
  geom_density() +
  ggtitle("annotations with trump minus sanders > 0") +
  theme_bw()

plot3 <- ggplot(full_tbl, aes(mean_rating_twtr)) +
  geom_density() +
  ggtitle("annotations with twitter account == 1") +
  theme_bw()

plot4 <- ggplot(full_tbl, aes(mean_rating_pk)) +
  geom_density() +
  ggtitle("annotations all knowledge questions correctly answered") +
  theme_bw()

plot5 <- ggplot(full_tbl, aes(mean_rating_attn_twtr)) +
  geom_density() +
  ggtitle("correct answers AND trump - sanders > 0") +
  theme_bw()

mturk_only_plots <- plot_grid(plot1, plot2, plot3, plot4, plot5, ncol = 2)

# check correlations with barbera estimates

compare_tbl <- full_tbl %>%
  select(handle, mean_rating_all, mean_rating_attn, barbera_ideology) %>%
  mutate(scaled_mturk_ideology = scale_01(mean_rating_all),
         scaled_mturk_ideology_attn = scale_01(mean_rating_attn),
         scaled_barbera_ideology = scale_01(barbera_ideology)) %>%
  select(scaled_mturk_ideology, scaled_mturk_ideology_attn, scaled_barbera_ideology)

cor.test(compare_tbl$scaled_mturk_ideology, compare_tbl$scaled_barbera_ideology, method = "pearson") %>%
  tidy()

cor.test(compare_tbl$scaled_mturk_ideology_attn, compare_tbl$scaled_barbera_ideology, method = "pearson") %>%
  tidy()

###

# compare mturk vs barbera ideology

viz_tbl <- full_tbl %>%
  select(handle, mean_rating_attn, barbera_ideology) %>%
  mutate(mturk_ideology_shifted = mean_rating_attn - 4) %>%
  select(-mean_rating_attn) %>%
  rename(barbera = barbera_ideology,
         mturk = mturk_ideology_shifted) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "ideology")

medians <- viz_tbl %>%
  group_by(type) %>%
  summarise(type_median = median(ideology))

unweighted_distribution_plot <- viz_tbl %>%
  ggplot(aes(x=ideology, fill=type)) +
    geom_density(alpha = 0.4) +
    geom_vline(data=medians, aes(xintercept=type_median, color=type),
             linetype="dashed") +
  theme_bw() +
  labs(x="unweighted ideology")

NA_density <- full_tbl %>%
  select(NA_percent_all) %>%
  mutate(NA_percent_all = round(NA_percent_all)) %>%
  rename(NA_percent = NA_percent_all) %>%
  ggplot(aes(NA_percent)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5) +
  geom_density(alpha= 0.2, fill="#FF6666") +
  scale_x_continuous(breaks=seq(0, 100, by = 5)) +
  theme_bw() +
  labs(x = "Percent of MTurkers responded with 'don't know/can't say",
       y = "Frequency of opinion leaders")

# weighted

viz_tbl <- full_tbl %>%
  select(handle, mean_rating_attn, barbera_ideology, freq_scaled) %>%
  mutate(weighted_barbera_ideology = barbera_ideology * freq_scaled,
         weighted_mturk_ideology_shifted = (mean_rating_attn - 4) * freq_scaled) %>%
  select(-mean_rating_attn, -barbera_ideology, -freq_scaled) %>%
  rename(barbera = weighted_barbera_ideology,
         mturk = weighted_mturk_ideology_shifted) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "weighted_ideology")

medians <- viz_tbl %>%
  group_by(type) %>%
  summarise(type_median = median(weighted_ideology))

weighted_distribution_plot <- viz_tbl %>%
  ggplot(aes(x=weighted_ideology, fill=type)) +
  geom_density(alpha = 0.4) +
  geom_vline(data=medians, aes(xintercept=type_median, color=type),
             linetype="dashed") +
  theme_bw() +
  labs(x="weighted ideology")

########################################
# within deciles

qs <- quantile(full_tbl$freq, probs = seq(0, 1, by = 0.1))

all_decile_tbl = NULL
for(i in 1:10) {
  
  decile_tbl <- full_tbl %>%
    filter(freq >= qs[i] & freq <= qs[i+1]) %>%
    mutate(decile = i)

  all_decile_tbl <- all_decile_tbl %>% rbind(decile_tbl)
}

medians <- all_decile_tbl %>%
  group_by(decile) %>%
  summarize(barbera = median(barbera_ideology),
            mturk = median(mean_rating_attn-4)) %>%
  ungroup() %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "decile_median_ideology")

viz_tbl <- all_decile_tbl %>%
  select(handle, decile, mean_rating_attn, barbera_ideology) %>%
  mutate(mean_rating_attn = mean_rating_attn - 4) %>%
  rename(mturk = mean_rating_attn,
         barbera = barbera_ideology) %>%
  pivot_longer(cols = c(3,4), names_to = "type", values_to = "ideology") %>%
  inner_join(medians, by = c("decile", "type"))

decile_plot <- viz_tbl %>%
  ggplot(aes(x=ideology, fill = type)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = decile_median_ideology, colour = type), linetype = "dashed") +
  facet_wrap(~decile, nrow = 5, scales = "free_y") +
  theme_bw() +
  labs(x="unweighted ideology")

########################################
# within genres unweighted

elite_classes <- read_csv("data/elite_classification.csv")
classes <- elite_classes %>%
  pull(sector1) %>%
  unique()

classes <- classes[!classes %in% c("?", "X", "x")]

par(mfrow=c(5,2))

classes = c("hard news", "meme", "organization", "political pundit", 
            "political figure",  "brand", "media outlet", "public figure",
            "sports", "entertainment")

all_class_elites_ideologies <- NULL
for(class in classes) {
  message(class)
  class_elites <- elite_classes %>%
    filter(sector1 %in% class | sector2 %in% class | sector3 %in% class) %>%
    pull(handle) %>% tolower()
  
  class_elites_ideology <- full_tbl %>%
    select(handle, mean_rating_attn, barbera_ideology, freq_scaled) %>%
    filter(handle %in% class_elites) %>%
    mutate(genre = gsub(pattern = " ", "", class),
           mturk_ideology = mean_rating_attn - 4,
           weighted_mturk_ideology = mturk_ideology * freq_scaled,
           weighted_barbera_ideology = barbera_ideology * freq_scaled) %>%
    select(-mean_rating_attn)

  all_class_elites_ideologies <- all_class_elites_ideologies %>%
    rbind(class_elites_ideology)
}

class_medians <- all_class_elites_ideologies %>%
  group_by(genre) %>%
  summarize(median_mturk_ideology_unweighted = median(mturk_ideology),
            median_barbera_ideology_unweighted = median(barbera_ideology),
            median_mturk_ideology_weighted = median(weighted_mturk_ideology),
            median_barbera_ideology_weighted = median(weighted_barbera_ideology)) %>%
  ungroup()

all_class_elites_ideologies <- all_class_elites_ideologies %>%
  select(-freq_scaled)

unweighted_medians <- class_medians %>%
  select(genre, median_barbera_ideology_unweighted, median_mturk_ideology_unweighted) %>%
  rename(mturk = median_mturk_ideology_unweighted,
         barbera = median_barbera_ideology_unweighted) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "class_median_ideology")

viz_tbl <- all_class_elites_ideologies %>%
  select(handle, genre, mturk_ideology, barbera_ideology) %>%
  rename(mturk = mturk_ideology,
         barbera = barbera_ideology) %>%
  pivot_longer(cols = c(3,4), names_to = "type", values_to = "ideology") %>%
  inner_join(unweighted_medians, by = c("type", "genre"))
  
unweighted_genre_plot <- viz_tbl %>%
  ggplot(aes(x=ideology, fill = type)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = class_median_ideology, colour = type), linetype = "dashed") +
  facet_wrap(~genre, nrow = 5, scales = "free_y") +
  theme_bw() +
  labs(x="unweighted ideology")

# within genres weighted

weighted_medians <- class_medians %>%
  select(genre, median_barbera_ideology_weighted, median_mturk_ideology_weighted) %>%
  rename(mturk = median_mturk_ideology_weighted,
         barbera = median_barbera_ideology_weighted) %>%
  pivot_longer(cols = c(2,3), names_to = "type", values_to = "class_median_ideology")

viz_tbl <- all_class_elites_ideologies %>%
  select(handle, genre, weighted_mturk_ideology, weighted_barbera_ideology) %>%
  rename(mturk = weighted_mturk_ideology,
         barbera = weighted_barbera_ideology) %>%
  pivot_longer(cols = c(3,4), names_to = "type", values_to = "ideology") %>%
  inner_join(weighted_medians, by = c("type", "genre"))

weighted_genre_plot <- viz_tbl %>%
  ggplot(aes(x=ideology, fill = type)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = class_median_ideology, colour = type), linetype = "dashed") +
  facet_wrap(~genre, nrow = 5, scales = "free_y") +
  theme_bw() +
  labs(x="weighted ideology")

all_plots <- ls()[grep("plot", ls())]
