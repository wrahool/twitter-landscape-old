
#elites
elite_df = read.csv("data/weak_elite_ideologies.csv", as.is = T)

#those with -Inf and +Inf ideologies are assigned very low and very high values instead.
#+/-3 is chosen because all other elites have absolute ideologies less than 3.
elite_df[is.na(elite_df$ideology),]$ideology = 0
elite_df[elite_df$ideology == -Inf,]$ideology = -3
elite_df[elite_df$ideology == Inf,]$ideology = 3

names(elite_df)[2] = "ideology"
elites = elite_df$username

elite_freq = read.csv("data/elites_activity.csv", as.is = T)
names(elite_freq)[1] = "username"

elite_freq$numberoftweets_scaled = (elite_freq$numberoftweets - min(elite_freq$numberoftweets))/(max(elite_freq$numberoftweets) - min(elite_freq$numberoftweets))

elite_df$username = tolower(elite_df$username)
elite_freq_df = merge(elite_df, elite_freq)
elite_freq_df$corrected_ideology = elite_freq_df$ideology * elite_freq_df$numberoftweets_scaled

elite_classes = read.csv("data/elite_classification.csv", as.is = T)
classes = unique(elite_classes$sector1)

classes = classes[!classes %in% c("?", "X", "x")]

par(mfrow=c(5,2))

classes = c("hard news", "meme", "organization", "political pundit", 
            "political figure",  "brand", "media outlet", "public figure",
            "sports", "entertainment")

all_class_elites_ideologies = NULL

for(class in classes) {
  print(class)
  class_elites = elite_classes[elite_classes$sector1 %in% class |
                                 elite_classes$sector2 %in% class |
                                 elite_classes$sector3 %in% class,]$handle
  
  class_elites = tolower(class_elites)
  
  class_elites_ideology = elite_freq_df[elite_freq_df$username %in% class_elites,]
  
  d_ce = density(class_elites_ideology$corrected_ideology)
  
  plot(d_ce, xlab = "idelogy", main = paste0(class, " accounts (N=" , nrow(class_elites_ideology),") "))
  
  class_elites_ideology$class = class
  all_class_elites_ideologies = rbind(all_class_elites_ideologies, class_elites_ideology)
}

all_class_elites_ideologies$class_2 = gsub(pattern = " ", "", all_class_elites_ideologies$class)

# weighted elite distribution within genres
ggplot(all_class_elites_ideologies, aes(x=corrected_ideology)) +
  geom_density(fill = "snow3", alpha = 0.6, color = "snow3") +
  theme_bw() +
  facet_wrap(~class, nrow = 5, scales = "free")

class_freq = data.frame(table(all_class_elites_ideologies$class))
class_freq$percentage = 100*class_freq$Freq/sum(class_freq$Freq)

# # freq of genres
# ggplot(data = class_freq, aes(x=Var1, y = Freq)) +
#   geom_bar(stat = "identity", fill = "black") +
#   coord_flip()
