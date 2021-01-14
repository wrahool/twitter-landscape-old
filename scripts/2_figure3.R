#ideologicial distribution elites vs non-elites
#weighted vs non-weighted
library(gridExtra)
library(ggplot2)
library(plyr)

setwd("C:/Users/Subhayan Mukerjee/Work/twitter-landscape//")

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
d_e = density(elite_df$ideology)

#ordinary users
ordinary_ideologies = read.csv("data/ordinary_ideologies.csv", as.is = T)
ordinary_ideologies = ordinary_ideologies[!is.na(ordinary_ideologies$ideology),]

#those with -Inf and +Inf ideologies are assigned very low and very high values instead.
#+/-3 is chosen because all other elites have absolute ideologies less than 3.
ordinary_ideologies[ordinary_ideologies$ideology == -Inf,]$ideology = -3
ordinary_ideologies[ordinary_ideologies$ideology == Inf,]$ideology = 3

d_p = density(ordinary_ideologies$ideology)

elite_df$type = "opinion leaders"
ordinary_ideologies$type = "ordinary users"
names(ordinary_ideologies)[1] = "username"
master_ideology = rbind(elite_df, ordinary_ideologies)

mu <- ddply(master_ideology, "type", summarise, grp.mean=mean(ideology, na.rm = T))

p1_withoutNA = ggplot(master_ideology, aes(x=ideology, fill=type)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=type),
             linetype="dashed") +
  theme_bw() +
  theme(legend.position = "none")

#recode NAs as 0

#repeat
#elites
elite_df = read.csv("data/weak_elite_ideologies.csv", as.is = T)

elite_df[is.na(elite_df$ideology),]$ideology = 0
elite_df[elite_df$ideology == -Inf,]$ideology = -3
elite_df[elite_df$ideology == Inf,]$ideology = 3

names(elite_df)[2] = "ideology"
elites = elite_df$username
d_e = density(elite_df$ideology)

#ordinary users
ordinary_ideologies = read.csv("data/ordinary_ideologies.csv", as.is = T)
#ordinary_ideologies = ordinary_ideologies[!is.na(ordinary_ideologies$ideology),]

ordinary_ideologies[is.na(ordinary_ideologies$ideology),]$ideology = 0
ordinary_ideologies[ordinary_ideologies$ideology == -Inf,]$ideology = -3
ordinary_ideologies[ordinary_ideologies$ideology == Inf,]$ideology = 3

d_p = density(ordinary_ideologies$ideology)

elite_df$type = "opinion leader"
ordinary_ideologies$type = "ordinary user"
names(ordinary_ideologies)[1] = "username"
master_ideology = rbind(elite_df, ordinary_ideologies)

mu <- ddply(master_ideology, "type", summarise, grp.mean=mean(ideology, na.rm = T))

p2_withNA = ggplot(master_ideology, aes(x=ideology, fill=type)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=type),
             linetype="dashed") +
  theme_bw() +
  theme(legend.position = "none")


#figure 3: elites vs plebs, with vs without neutrals
grid.arrange(p1_withoutNA, p2_withNA, nrow = 1)