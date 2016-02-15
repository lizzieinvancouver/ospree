# Fixing merge issues
library(gdata)

setwd("~/Documents/git/budreview")

d <- read.csv("growthchambers_litreview-CLEANING.csv") 

# Look at each cleaning block. Make unique identifiers for rows. Make master list, and put only unique rows in it.

# Then double-check to see if any papers ended up in master twice.

# unique id's for things which should not have changed between the entry periods.

d$uniqeid = 
  apply(d[,c(2,3,5,6,7,8,9,18,19,20,21,22,23,24,25,26,27,28,50,51)], 1,
        function(x)
          paste(x
              , collapse = "_")
                  )

# Start with the last block. Add any from previous blocks which are unique

a <- d[d$CleanBlock == max(d$CleanBlock),]


a1 <- d[d$CleanBlock == 1,]
a2 <- d[d$CleanBlock == 2,]
a3 <- d[d$CleanBlock == 3,]
a4 <- d[d$CleanBlock == 4,]

# test with first 500 rows
a1test = a1[1:500,]
a2test = a2[1:500,]
a3test = a3[1:500,]
a4test = a4[1:500,]
atest = a[1:500,]

atest <- rbind(atest, a1test[!atest$uniqeid %in% a1test$uniqeid,])
summary(!atest$uniqeid %in% a2test$uniqeid)
atest <- rbind(atest, a2test[!atest$uniqeid %in% a2test$uniqeid,])
summary(!atest$uniqeid %in% a3test$uniqeid)
atest <- rbind(atest, a2test[!atest$uniqeid %in% a2test$uniqeid,])
summary(!atest$uniqeid %in% a4test$uniqeid)

summary(!a$uniqeid %in% a1$uniqeid)
summary(!a$uniqeid %in% a2$uniqeid)
summary(!a$uniqeid %in% a3$uniqeid)
summary(!a$uniqeid %in% a4$uniqeid)

a <- rbind(a, a1[!a$uniqeid %in% a1$uniqeid,])
a <- rbind(a, a2[!a$uniqeid %in% a2$uniqeid,])
a <- rbind(a, a3[!a$uniqeid %in% a3$uniqeid,])
a <- rbind(a, a4[!a$uniqeid %in% a4$uniqeid,])

dim(a)

a <- a[a$datasetID != "<<<<<<< HEAD" & a$datasetID != "=======" & a$datasetID != "<<<<<<< Updated upstream" & !is.na(a$datasetID),]
    
write.csv(a, "growthchambers_litreview-CLEANED.csv", row.names=F)
