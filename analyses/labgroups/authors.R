## Started 6 July 2016 ##
## By Lizzie (so far) ###

## looking at author clustering in OSPREE ##


## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# library(plyr)
library(reshape)
library(cluster) # for agnes command
library(plyr)
library(dplyr)


setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/labgroups")

## useful f(x)s
stripwhite <- function(x) {
    if (!is.character(x)) {
        stop("x must be a character vector")
    }
    sub("^ *([^ ]*) *$", "\\1", x)
}

# get the data
dater <- read.csv("input/ospreebib.csv", header=TRUE)

# fix some missing datasetIDs (matching to datasetID in ospree.csv)
dater[which(dater$Custom3==""),] # here's the list
dater$Custom3[which(dater$Custom3=="" & dater$Identifier=="zohner2016")] <- "zohner16"
dater[which(dater$Custom3=="" & dater$Identifier=="Sonsteby:2009aa"),] <- "sonsteby09a" # note, there is a sonsteby09a and a sonsteby09b, but this is a strawberry one so I didn't try to figure it out for sure, since it will disappear later 

# and make it long format
aut.sm <- subset(dater, select=c("aut1", "aut2", "aut3", "aut4", "aut5", "aut6", "aut7",  "Custom3"))

aut.long.sm <- melt(aut.sm, id.var="Custom3")
aut.long.sm$author <- gsub( ",.*$", "", aut.long.sm$value)
names(aut.long.sm) <- c("datasetid", "authornum", "fullname", "author")

write.csv(aut.long.sm, "output/aut.long.sm.csv", row.names=FALSE) # we need this for back-conversion



## count up interactions for each paper
datersets <- unique(aut.long.sm$datasetid)
subset(aut.long.sm, datasetid=="")
output <- matrix(ncol=2, nrow=0)

aut.long.smer <- subset(aut.long.sm, fullname !="")


# during the below we lose lamb37, nienstaedt66, webb78 because they are sole-author papers
# and the combn command requires at least two authors
for (i in c(1:length(datersets))){
    subby <- subset(aut.long.smer, datasetid==datersets[i])
    ifelse((nrow(subby)>1)==TRUE,
        output <- rbind(output, t(combn(sort(tolower(subby$author)), m=2))),
        output <- output)
}
output <- stripwhite(output)
output.df <- as.data.frame(output)

# output.df$combos <- paste(output.df[,1], output.df[,2], sep="")

# get column for combinations -- regardless of order of names across columns
output.df.comb <- data.frame(output.df, stringsAsFactors = FALSE) %>% 
   mutate(key = paste0(pmin(V1, V2), pmax(V1, V2), sep = ""))

output.df.comb.nodups <- output.df.comb[!duplicated(output.df.comb$key),]

countz <- ddply(output.df.comb,.(key), summarize, freq=length(key))

fulldf <- merge(countz, output.df.comb.nodups, by="key")

# write the wide format out to try in gephi
write.csv(fulldf, "output/aut.sm.csv", row.names=FALSE)



########################
## clustering analysis #
########################
autmat <- table(aut.long.sm$author, aut.long.sm$datasetid)
aut.clust <- agnes(dist(autmat), diss=TRUE, method="average") # this should be applying UPGMA method
aut.hclust <- hclust(dist(autmat), method="average") # also this should be applying UPGMA method

plot(aut.clust)
cutree(aut.hclust, k=4)

aut.hclust.1 <- hclust(dist(autmat)) # cannot use average method and cutree, I don't think

## wait, do we want the reverse?
autmat.rev <- table(aut.long.sm$datasetid,aut.long.sm$author)

aut.rev.hclust <- hclust(dist(autmat.rev), method="average") # also this should be applying UPGMA method

plot(aut.rev.hclust)
cutree(aut.rev.hclust, k=6)

aut.rev.hclust <- hclust(dist(autmat.rev)) # cannot use average method and cutree, I don't think
chopchop <- cutree(aut.rev.hclust, h=c(1, 1.5, 2, 2.5, 3))
chopchop <- cutree(aut.rev.hclust, k=4)

## try kmeans (it worries me that I get way too many different answers with this each run of it) 
k.means.fit <- kmeans(autmat.rev, 6)
plot(autmat.rev, col = k.means.fit$cluster)
points(k.means.fit$centers, col = 1:5, pch = 8)

# write.csv(as.data.frame(k.means.fit$cluster), "output/kmeans6.csv")


############################
## end clustering analysis #
############################
