## Started 6 July 2016 ##
## By Lizzie (so far) ###

## looking at author clustering in OSPREE ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# library(plyr)
library(reshape)
library(cluster) # for agnes command

setwd("~/Documents/git/projects/treegarden/budreview/budreview/")

## useful f(x)s
stripwhite <- function(x) {
    if (!is.character(x)) {
        stop("x must be a character vector")
    }
    sub("^ *([^ ]*) *$", "\\1", x)
}

# get the data
dater <- read.csv("refs/ospreebib.csv", header=TRUE)

# and make it long format
aut.sm <- subset(dater, select=c("aut1", "aut2", "aut3", "aut4", "aut5", "aut6", "aut7",  "Custom3"))
aut.long <- melt(aut.sm, id.var="Custom3")
aut.long.sm <- subset(aut.long, value!="")
aut.long.sm$author <- gsub( ",.*$", "", aut.long.sm$value)
names(aut.long.sm) <- c("datasetid", "authornum", "fullname", "author")

# also while I am here, clean up wide format
aut.sm$aut1 <- gsub( ",.*$", "", aut.sm$aut1)
aut.sm$aut2 <- gsub( ",.*$", "", aut.sm$aut2)
aut.sm$aut3 <- gsub( ",.*$", "", aut.sm$aut3)
aut.sm$aut4 <- gsub( ",.*$", "", aut.sm$aut4)
aut.sm$aut5 <- gsub( ",.*$", "", aut.sm$aut5)
aut.sm$aut6 <- gsub( ",.*$", "", aut.sm$aut6)
aut.sm$aut7 <- gsub( ",.*$", "", aut.sm$aut7)


## clustering analysis
autmat <- table(aut.long.sm$author, aut.long.sm$datasetid)
aut.clust <- agnes(dist(autmat), diss=TRUE, method="average") # this should be applying UPGMA method
aut.hclust <- hclust(dist(autmat), method="average") # also this should be applying UPGMA method

plot(aut.clust)
cutree(aut.hclust, k=4)


aut.hclust.1 <- hclust(dist(autmat)) # cannot use average method and cutree, I don't think
cutree(aut.hclust, h=c(1.5, 2, 2.5, 3))

## wait, do we want the reverse?
autmat.rev <- table(aut.long.sm$datasetid,aut.long.sm$author)

aut.rev.hclust <- hclust(dist(autmat.rev), method="average") # also this should be applying UPGMA method

plot(aut.rev.clust)
cutree(aut.rev.hclust, k=6)

aut.rev.hclust <- hclust(dist(autmat.rev)) # cannot use average method and cutree, I don't think
chopchop <- cutree(aut.rev.hclust, h=c(1, 1.5, 2, 2.5, 3))
chopchop <- cutree(aut.rev.hclust, k=4)

## try kmeans
k.means.fit <- kmeans(autmat.rev, 6)
plot(autmat.rev, col = k.means.fit$cluster)
points(k.means.fit$centers, col = 1:5, pch = 8)

## Below taken from: https://www.biostars.org/p/86563/

library(gplots)

# get a color palette equal to the number of clusters
clusterCols <- rainbow(length(unique(chopchop)))

# create vector of colors for side bar
myClusterSideBar <- clusterCols[chopchop]

# choose a color palette for the heat map
myheatcol <- rev(redgreen(75))

# draw the heat map
heatmap.2(autmat.rev, main="Hierarchical Cluster", Rowv=as.dendrogram(aut.rev.hclust), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none")

