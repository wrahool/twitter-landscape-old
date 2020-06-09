# weighting and distribution within deciles.

library(reshape)
library(ggplot2)
library(plyr)

setwd("C:/Users/Subhayan/Documents/Work/twitter-landscape//")

#without NAs

#elites
elite_df = read.csv("data/weak_elite_ideologies.csv", as.is = T)
elite_df = elite_df[!is.na(elite_df$ideology),]

#those with -Inf and +Inf ideologies are assigned very low and very high values instead.
#+/-3 is chosen because all other elites have absolute ideologies less than 3.
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

elite_freq_viz = elite_freq_df[,c(1, 2, 5)]

elite_freq_viz_long = melt(elite_freq_viz, id.vars= "username")

mu <- ddply(elite_freq_viz_long, "variable", summarise, grp.mean=mean(value, na.rm = T))

p3_withoutNA = ggplot(elite_freq_viz_long, aes(x=value, fill=variable)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=variable),
             linetype="dashed") +
  scale_colour_manual(values = c("#E7B800", "#FC4E07")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07")) +
  theme_bw() +
  theme(legend.position = "none")

#elites without neutrals: weighted vs unweighted

#with NAs

library(reshape)

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

elite_freq_viz = elite_freq_df[,c(1, 2, 5)]

elite_freq_viz_long = melt(elite_freq_viz, id.vars= "username")

mu <- ddply(elite_freq_viz_long, "variable", summarise, grp.mean=mean(value, na.rm = T))

p4_withNA = ggplot(elite_freq_viz_long, aes(x=value, fill=variable)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=variable),
             linetype="dashed") +
  scale_colour_manual(values = c("#E7B800", "#FC4E07")) +
  scale_fill_manual(values = c("#E7B800", "#FC4E07")) +
  theme_bw() +
  theme(legend.position = "none") 

#elites with neutrals: weighted vs unweighted

library(gridExtra)

#figure 4
grid.arrange(p3_withoutNA, p4_withNA, nrow = 1)

# for MC article

MC_article_fig2_A = ggplot(elite_freq_viz_long[elite_freq_viz_long$variable == "ideology",],) +
  geom_density(alpha=0.4, aes(x=value), fill = "orange") +
  geom_vline(xintercept = mu[mu$variable == "ideology",]$grp.mean, linetype="dashed") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 10, 2))+
  ylim(c(0, 10.5))

MC_article_fig2_B = ggplot(elite_freq_viz_long[elite_freq_viz_long$variable == "corrected_ideology",],) +
  geom_density(alpha=0.4, aes(x=value), fill = "orange") +
  geom_vline(xintercept = mu[mu$variable == "corrected_ideology",]$grp.mean, linetype="dashed") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 10, 2))+
  ylim(c(0, 10.5))

grid.arrange(MC_article_fig2_A, MC_article_fig2_B, nrow = 1)

#distribution within deciles

qs = quantile(elite_freq_df$numberoftweets, probs = seq(0, 1, by = 0.1))

par(mfrow=c(5,2))

all_decile_df = NULL
grays = gray.colors(10, start = 0.1, end = 0.9, gamma = 2.2, alpha = NULL)
for(i in 1:10) {
  decile_df = elite_freq_df[elite_freq_df$numberoftweets >= qs[i] & 
                              elite_freq_df$numberoftweets <= qs[i+1],]
  
  d_de = density(decile_df$ideology)
  #plot(d_de, xlab = "ideology", paste0(main = "ideological distribution of (N=" , nrow(decile_df),") elites in dcile ", i))
  decile_df$decile = i
  all_decile_df = rbind(all_decile_df, decile_df)
}

#elite ideology with neutrals, distribution within deciles

medians <- ddply(all_decile_df, "decile", summarise, grp.median=median(ideology, na.rm = T))

all_decile_df <- all_decile_df %>% inner_join(medians)

#figure 5
ggplot(all_decile_df, aes(x=ideology)) +
  geom_density(fill="snow3", alpha = 0.6, color = "snow3") +
  facet_wrap(~decile, nrow = 5, scales = "free") +
  geom_vline(aes(xintercept=grp.median, colour = "red")) +
  geom_vline(xintercept=0, linetype = "dashed") +
  scale_fill_gradient(low="white", high="black") +
  theme_bw() +
  theme(legend.position = "none")



