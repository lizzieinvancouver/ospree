## Started July 9 2016 ##
## by Lizzie ##

## Playing around with labgroups ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(igraph)
library(rgexf) # install.packages("rgexf") # for write.gexf
library(plyr)

# Set working directory: 
setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/labgroups")
#setwd("~/Documents/git/ospree")


authorfile <- read.csv("output/aut.sm.csv", header=TRUE)
names(authorfile) <- c("combo", "freq", "name1", "name2")

auts <- subset(authorfile, select=c("name1", "name2", "freq"))

# following along from here ... http://www.vesnam.com/Rblog/viznets2/

# now make an igraph object and simplify it
# simplify removes all self loops and makes sure edges are unique
gD <- simplify(graph.data.frame(auts, directed=FALSE))

# Print number of nodes and edges
vcount(gD)
ecount(gD)

#############
# Calculate some node properties and node similarities that will be used to illustrate 

# Calculate degree for all nodes
degAll <- degree(gD, v = V(gD), mode = "all")

# Calculate betweenness for all nodes
betAll <- betweenness(gD, v = V(gD), directed = FALSE) / (((vcount(gD) - 1) * (vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
rm(betAll)

# Calculate Dice similarities between all pairs of nodes
dsAll <- similarity.dice(gD, vids = V(gD), mode = "all")

#############
# Add new node/edge attributes based on the calculated node properties/similarities

gD <- set.vertex.attribute(gD, "degree", index = V(gD), value = degAll)
gD <- set.vertex.attribute(gD, "betweenness", index = V(gD), value = betAll.norm)

# Check the attributes
# summary(gD)

F1 <- function(x) {data.frame(V4 = dsAll[which(V(gD)$name == as.character(x$V1)),
    which(V(gD)$name == as.character(x$V2))])}
dataSet.ext <- ddply(auts, .variables=c("name1", "name2", "freq"), function(x) data.frame(F1(x)))

gD <- set.edge.attribute(gD, "weight", index = E(gD), value = 0)
gD <- set.edge.attribute(gD, "similarity", index = E(gD), value = 0)

# The order of interactions in gD is not the same as it is in dataSet or as it is in the edge list,
# and for that reason these values cannot be assigned directly

E(gD)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$weight <- as.numeric(dataSet.ext$V3)
E(gD)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$similarity <- as.numeric(dataSet.ext$V4)

# Check the attributes
summary(gD)

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(gD)), NAME = V(gD)$name)

# Trying with first letter capitalization
nodes_df_cap <- nodes_df

# Strip leading blanks
nodes_df_cap$NAME <-  sub("+ ", "", nodes_df_cap$NAME)

# Capitalize first letter
nodes_df_cap$NAME <- paste(toupper(substr(nodes_df_cap$NAME, 1, 1)), substr(nodes_df_cap$NAME, 2, nchar(nodes_df_cap$NAME)), sep="")

# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(gD, c(1:ecount(gD))))

# Define node and edge attributes - these attributes won't be directly used for network visualization, but they
# may be useful for other network manipulations in Gephi
#
# Create a dataframe with node attributes: 1st column - attribute 1 (degree), 2nd column - attribute 2 (betweenness)
nodes_att <- data.frame(DEG = V(gD)$degree, BET = V(gD)$betweenness) 
#
# Create a dataframe with edge attributes: 1st column - attribute 1 (weight), 2nd column - attribute 2 (similarity)
edges_att <- data.frame(WGH = E(gD)$weight, SIM = E(gD)$similarity) 

# Define node/edge visual attributes - these attributes are the ones used for network visualization
#
# Calculate node coordinate - needs to be 3D
#nodes_coord <- as.data.frame(layout.fruchterman.reingold(gD, weights = E(gD)$similarity, dim = 3, niter = 10000))
# We'll cheat here, as 2D coordinates result in a better (2D) plot than 3D coordinates
nodes_coord <- as.data.frame(layout.fruchterman.reingold(gD, weights = E(gD)$similarity, dim = 2, niter = 10000))
nodes_coord <- cbind(nodes_coord, rep(0, times = nrow(nodes_coord)))
#
# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
approxVals <- approx(c(1, 5), n = length(unique(V(gD)$betweenness)))
# And we will assign a node size for each node based on its betweenness centrality
nodes_size <- sapply(V(gD)$betweenness, function(x) approxVals$y[which(sort(unique(V(gD)$betweenness)) == x)])


# Define node color
# We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library
library("grDevices")

# This function returns a function corresponding to a color palete of "bias" number of elements
F2 <- colorRampPalette(c("#F5DEB3", "#FF0000"), bias = length(unique(V(gD)$degree)), space = "rgb", interpolate = "linear")

# Now we'll create a color for each degree
colCodes <- F2(length(unique(V(gD)$degree)))

# And we will assign a color for each node based on its degree
nodes_col <- sapply(V(gD)$degree, function(x) colCodes[which(sort(unique(V(gD)$degree)) == x)])
# Transform it into a data frame (we have to transpose it first)
nodes_col_df <- as.data.frame(t(col2rgb(nodes_col, alpha = FALSE)))
# And add alpha (between 0 and 1). The alpha from "col2rgb" function takes values from 0-255, so we cannot use it
nodes_col_df <- cbind(nodes_col_df, alpha = rep(1, times = nrow(nodes_col_df)))
# Assign visual attributes to nodes (colors have to be 4dimensional - RGBA)
nodes_att_viz <- list(color = nodes_col_df, position = nodes_coord, size = nodes_size)

# Assign visual attributes to edges using the same approach as we did for nodes
F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(E(gD)$weight)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(E(gD)$weight)))
edges_col <- sapply(E(gD)$weight, function(x) colCodes[which(sort(unique(E(gD)$weight)) == x)])
edges_col_df <- as.data.frame(t(col2rgb(edges_col, alpha = FALSE)))
edges_col_df <- cbind(edges_col_df, alpha = rep(1, times = nrow(edges_col_df)))
edges_att_viz <-list(color = edges_col_df)


# Write the network into a gexf (Gephi) file
write.gexf(nodes = nodes_df, edges = edges_df, nodesAtt = nodes_att, edgesWeight = E(gD)$weight, edgesAtt = edges_att, nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, defaultedgetype = "undirected", output = "gephi/lagroups_weights.gexf")
# And without edge weights
write.gexf(nodes = nodes_df_cap, edges = edges_df, nodesAtt = nodes_att, edgesAtt = edges_att, nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, defaultedgetype = "undirected", output = "gephi/lagroups.gexf")

# also write out the authors names as formatted here
write.csv(nodes_df, "output/aut.names.csv", row.names=FALSE)






