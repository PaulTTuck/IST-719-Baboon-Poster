###############################################################################
#
# Author: Paul Tuck
# IST 719
# Purpose: Code for Project Poster Plots
#
###############################################################################

# Needed libraries

library(ggplot2)
library(igraph)
library(RColorBrewer)
library(plotrix)

# Loading the data

fname <- file.choose()
baboon <- read.csv(fname, header = T, stringsAsFactors = F)

# Backup of data

backup <- baboon

# Inspecting and cleaning the data

head(baboon)
str(baboon)
summary(baboon)

# Checking for missing values (yay! none found)
cbind(lapply(lapply(baboon, is.na), sum))

###############################################################################
# Question 1: Of consorting and conceiving pairs, is there a relationship 
# between male and female hybrid score?
###############################################################################

# Isolating conceptive and conceiving interactions

Interact <- baboon[baboon$consort == 1 | baboon$conceptive == 1,]

# Extracting female and male id's

Female <- unique(baboon$female_id)
Male <- sort(unique(baboon$male_id))
All <- c(Female, Male)

# Creating links btw female and male partners

length(baboon[baboon$female_id == 'ABB',])

baboon_links <- matrix(nrow = length(All), ncol = length(All))

rownames(baboon_links) <- All
colnames(baboon_links) <- All


for (f in Female){
  for (m in Male){
    if (any(Interact[Interact$female_id == f,]$male_id == m) == TRUE){
      baboon_links[f, m] = 1
      baboon_links[m, f] = 1
    }
    else{
      baboon_links[f, m] = 0
      baboon_links[m, f] = 0
    }
  }
}

# Filling in remaining nulls with 0

baboon_links[is.na(baboon_links)] <- 0

# Creating hybrid node data for females and males

test <- aggregate(Interact$female_hybridscore, by = list(Interact$female_id), FUN = max)
test2 <- aggregate(Interact$male_hybridscore, by = list(Interact$male_id), FUN = max)

test$sex <- 'female'
test2$sex <- 'male'

colnames(test) <- c('id', 'hybrid_score', 'sex')
colnames(test2) <- c('id', 'hybrid_score', 'sex')

baboon_nodes <- rbind(test, test2)

# Creating network plot

bg <- graph_from_adjacency_matrix(baboon_links)

# How many nodes and edges?

vcount(bg)
ecount(bg)

# Plotting network

par(mar = c(0,0,0,0))
bg <- simplify(bg)

V(bg)$size <- 6
V(bg)$label.cex <- 0
plot.igraph(bg)

# Shape nodes based on sex

V(bg)$sex <- baboon_nodes$sex
V(bg)$shape <- 'circle'
V(bg)$shape[V(bg)$sex == 'female'] <- 'square'
plot.igraph(bg)

# Color nodes based on hybrid scores

V(bg)$hybrid <- baboon_nodes$hybrid_score
my_pal <- colorRampPalette(c('blue', 'orange'))
V(bg)$color <- rev(my_pal(200))[round(1 + rescale(V(bg)$hybrid, c(1, 199)), 0)]
V(bg)$arrow.size <- 0.5
plot.igraph(bg)

# Histograms of female and male hybrid scores

par(mar = c(4,4,4,4))

hist(baboon_nodes$hybrid_score[baboon_nodes$sex == 'female']
     , main = 'Female Hybridization'
     , xlab = 'Percent Anubis Baboon DNA')

hist(baboon_nodes$hybrid_score[baboon_nodes$sex == 'male']
     , main = 'Male Hybridization'
     , xlab = 'Percent Anubis Baboon DNA')

###############################################################################
# Question 2: does genetic distance affect whether a pair consorts or conceives?
###############################################################################

# Separating cycles that have consorted and/or concieved
baboon.consort <- baboon[baboon$consort == 1 & baboon$conceptive == 0,]
baboon.concieve <- baboon[baboon$conceptive == 1 & baboon$consort == 0,]
baboon.both <- baboon[baboon$consort == 1 & baboon$conceptive == 1,]
baboon.none <- baboon[baboon$consort == 0 & baboon$conceptive == 0,]

# Placing boxplots on same plot
par(mfrow = c(1, 1))
boxplot(baboon.consort$gen_distance
        , baboon.concieve$gen_distance
        , baboon.both$gen_distance
        , baboon.none$gen_distance
        , main = 'Genetic Distance of Interacting Pairs'
        , ylab = 'Genetic Distance'
        , names = c('consort', 'concieving', 'both', 'no interaction'))
# It does not appear so

###############################################################################
# Question 3: Does relatedness affect combined social rank?
###############################################################################

# Linear model for plot

Rank_lm <- predict(lm(baboon$rank_interact ~ baboon$gen_distance))

ggplot(baboon) +
  aes(x = gen_distance, y = rank_interact) +
  geom_point() +
  geom_line(aes(y = Rank_lm), color = 'orange', lwd = 2) +
  xlab('Genetic Distance Between Pairs') +
  ylab('Combined Social Rank of Pair') +
  ggtitle('Relatedness and Social Rank')

###############################################################################
# Question 4: Are female age and hybridization related (i.e., has hybridization
# occurred at a specific time?)
###############################################################################

plot(baboon$female_age, baboon$female_hybridscore
     , pch = 16
     , xlab = 'Age (years)'
     , ylab = 'Hybrid Score'
     , main = 'Female age and hybrid score')

###############################################################################
# Other plots:
# Number of consorting and conceiving pairs
###############################################################################

consort.count <- nrow(baboon.consort)
conceive.count <- nrow(baboon.concieve)
both.count <- nrow(baboon.both)
neither.count <- nrow(baboon.none)
counts <- c(consort.count, conceive.count, both.count, neither.count)
count.names <- c('consort', 'conceive', 'both', 'neither')

count.df <- data.frame(names = count.names, counts = counts)
barplot(count.df$counts, names.arg = count.df$names
        , main = 'Consorting and Conceiving Cycles')


