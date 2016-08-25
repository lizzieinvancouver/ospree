# Fixing merge issues

setwd("~/Documents/git/budreview")

d <- read.csv("growthchambers_litreview_CLEANING.csv") 

# Look at each cleaning block. Make unique identifiers for rows. Make master list, and put only unique rows in it.

# Then double-check to see if any papers ended up in master twice.

# unique id's for things which should *not* have changed between the entry periods. Don't add too many variables, or won't find actual duplicates.
names(d)

d$uniqueid = 
  apply(d[,c("datasetID","study","genus","species","population","population.detail",
              "forcetemp","photoperiod_day","chilltemp","chilldays",
                "respvar","response","response.time"
             #, "n","response..pre.treatment."
             #  , "error.type","resp_error", "number.longdays","dormancy_induction_temp_day","freeze.treatment.time"
             
                      )], 1,
        function(x)
          paste(x
              , collapse = "_")
                  )

summary(duplicated(d$uniqueid)) # should be >10k, would rather still have some duplicates than throw out non-duplicates by mistake


# Start with the last block. Add any from previous blocks which are unique
nrow(d) # with many duplicates

d$CleanBlock = d$Group

a <- d[d$CleanBlock == max(d$CleanBlock,na.rm = T),]

nrow(a)

# duplicates in the last block still!? Manually remove later.
summary(duplicated(a$uniqueid)) # 

a1 <- d[d$CleanBlock == 1,]
#a2 <- d[d$CleanBlock == 2,]
#a3 <- d[d$CleanBlock == 3,]
#a4 <- d[d$CleanBlock == 4,]

# look at duplicates  
#summary(!a4$uniqueid %in% a$uniqueid)
#summary(!a3$uniqueid %in% a$uniqueid)
#summary(!a2$uniqueid %in% a$uniqueid)
summary(!a1$uniqueid %in% a$uniqueid)

a <- rbind(a, a1[!a1$uniqueid %in% a$uniqueid,])
#a <- rbind(a, a2[!a2$uniqueid %in% a$uniqueid,])
#a <- rbind(a, a3[!a3$uniqueid %in% a$uniqueid,])
#a <- rbind(a, a4[!a4$uniqueid %in% a$uniqueid,])

dim(a)

a <- a[a$datasetID != "<<<<<<< HEAD" & a$datasetID != "=======" & a$datasetID != "<<<<<<< Updated upstream" & !is.na(a$datasetID),]

length(unique(as.character(a$datasetID)))

write.csv(a, "growthchambers_litreview-CLEANED.csv", row.names=F)
