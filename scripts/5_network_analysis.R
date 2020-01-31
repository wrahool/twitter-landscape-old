setwd("C:/Users/Subhayan/Documents/Work/twitter-landscape/")
library(igraph)

load("data/elite_network.Rdata")

elite_df = read.csv("data/weak_elite_ideologies.csv", as.is = T)
elites = elite_df$username

elite_followers_df = read.csv("data/elite_followers.csv", as.is = T)

edgelist = as.data.frame(get.edgelist(elite_network))
names(edgelist) = c("from", "to")
edgelist$weight = E(elite_network)$weight

edgelist = merge(edgelist, elite_followers_df, by.x =  "from", by.y = "elite")
names(edgelist)[4] = "from_followers"

edgelist = merge(edgelist, elite_followers_df, by.x =  "to", by.y = "elite")
names(edgelist)[5] = "to_followers"


N = 8672 # number of ordinary users who follow elites

edgelist$from_followers = as.numeric(as.character(edgelist$from_followers))
edgelist$to_followers = as.numeric(as.character(edgelist$to_followers))

edgelist$phi_num = (edgelist$weight*N) - (edgelist$to_followers*edgelist$from_followers)
edgelist$phi_denom = sqrt(edgelist$from_followers * edgelist$to_followers * 
                             (N - edgelist$from_followers) * (N - edgelist$to_followers))

edgelist$phi = edgelist$phi_num / edgelist$phi_denom

edgelist$t_num = edgelist$phi * sqrt((max(edgelist$from_followers, edgelist$to_followers)) - 2)
edgelist$t_denom = sqrt(1 - (edgelist$phi) ^ 2)

edgelist$t = edgelist$t_num / edgelist$t_denom

edgelist_filtered = edgelist[abs(edgelist$t) > 2.58,]

filtered_elite_network = graph_from_edgelist(as.matrix(edgelist_filtered[,1:2]), directed = F)
E(filtered_elite_network)$weight = edgelist_filtered$weight

#########################
# walktrap on filtered, unaugmented, network
set.seed(42)
wt = walktrap.community(filtered_elite_network)
filtered_elite_network_bkup = filtered_elite_network

save(wt, file = "data/walktrap_results.Rdata")

#########################
# extract subgraphs for wt

cstats_df = NULL
for(i in 1:length(wt)) {
  c = induced_subgraph(filtered_elite_network_bkup, 
                       vids = which(V(filtered_elite_network_bkup)$name %in% wt[[i]]),
                       impl = "auto")
  
  cstats = c(i, length(V(c)), edge_density(c, loops = FALSE), centr_degree(c)$centralization)
  print(cstats)
  cstats_df = data.frame(rbind(cstats_df, cstats))
}

names(cstats_df) = c("community", "size", "density", "degree_centr")
rownames(cstats_df)= NULL

cstats_df