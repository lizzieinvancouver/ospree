# cleanup script for budburst review

library(gdata)

# Slow! read.xls calls a perl script, might take a while

d  <- read.csv("~/Dropbox/Work/Harvard/Budburst_Review/growthchambers_litreview_2016-01-15.csv") 
# Find wide characters, marked by backslash.
# Which columns have wide characters?
apply(d, 2, function(X) any(grepl("[\\]",as.character(x))))
apply(d, 2, function(X) any(grepl("[\x?]",as.character(x))))

d <- d.source <- read.xls("~/Dropbox/Work/Harvard/Budburst_Review/growthchambers_litreview_2016-01-15.xlsx", 2)
d.study <- read.xls("~/Dropbox/Work/Harvard/Budburst_Review/growthchambers_litreview_2016-01-15.xlsx", 3)
d.data_simple <- read.xls("~/Dropbox/Work/Harvard/Budburst_Review/growthchambers_litreview_2016-01-15.xlsx", 4)
d.data_detailed <- read.xls("~/Dropbox/Work/Harvard/Budburst_Review/growthchambers_litreview_2016-01-15.xlsx", 5)

# Make a summary table, checking for completeness of datasets entered in all four sheets

sourceID <- d.source$DatasetID # change to lowercase in next round
studyID <- unique(d.study$datasetID)
simpleID <- unique(d.data_simple$datasetID)
detailedID <- unique(d.data_detailed$datasetID)

done <- data.frame(sourceID)

done$study <- sourceID %in% studyID
done$simple <- sourceID %in% simpleID
done$detailed <- sourceID %in% detailedID

done[done==F] = ""
done[done==T] = "X"

write.csv(done, file="~/Dropbox/Work/Harvard/Budburst_Review/Summary_of_Completed.csv", row.names=F)

################# For cleaning up excluded references

tomove <- d[d$Status == "n", "Filename"]

for(i in 1:length(tomove)){
	
system(paste(paste("mv ~/Dropbox/Work/Harvard/Budburst_Review/Budburst_Refs/", tomove[i], sep = ""), 
				paste("~/Dropbox/Work/Harvard/Budburst_Review/Excluded_Refs/", tomove[i], sep = ""))
				)
				}

##### random other checks

head(d)

hist(d$Year, breaks = 50)

sort(summary(d$Journal))

head(d.data_detailed)
detailedID

detailedID_exp <- unique(paste(d.data_detailed$datasetID, d.data_detailed$study))
unique(detailedID_exp)


#########

head(d)

length(unique(paste(d.data_detailed$genus, d.data_detailed$species)))
sort(summary(factor(paste(d.data_detailed$genus, d.data_detailed$species))), decreasing = T)


unique(d.data_detailed$respvar)
sort(summary(d.data_detailed$respvar))


library(maps)
library(scales)
plot(world)


map(col = "lightgrey")
points(
      jitter(
      as.numeric(as.character(d.data_detailed$population.long))), 
      jitter(
      as.numeric(as.character(d.data_detailed$population.lat))),
      pch = 16, cex = 1,
      col = alpha("midnightblue", 0.1)
      )

