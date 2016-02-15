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

hist(as.numeric(as.character(d$year)), breaks = 50)

sort(summary(d$Journal))

head(d.data_detailed)
detailedID

detailedID_exp <- unique(paste(d.data_detailed$datasetID, d.data_detailed$study))
unique(detailedID_exp)


#############################
# Mapping 

head(d)

#d.data_detailed = d

length(unique(paste(d.data_detailed$genus, d.data_detailed$species)))
sort(summary(factor(paste(d.data_detailed$genus, d.data_detailed$species))), decreasing = T)


unique(d.data_detailed$respvar)
sort(summary(d.data_detailed$respvar))


library(maps)
library(scales)


# summary data by study, with n responses

nd <- data.frame(n=tapply(d$datasetID, d$datasetID, length))
nd$lat <- as.numeric(as.character(d[match(rownames(nd), d$datasetID), "provenance.lat"]))
nd$long <- as.numeric(as.character(d[match(rownames(nd), d$datasetID), "provenance.long"]))
nd$resp <- d[match(rownames(nd), d$datasetID), "respvar"]

# colors for responses
colz = alpha(c("midnightblue", "darkred","darkgreen"), 0.6)
ccolz = rep(colz[1], nrow(nd))
ccolz[nd$resp == "percentbudburst"] = colz[2]
ccolz[nd$resp == "daystobudburst"] = colz[3]


map(fill = F, col = "grey60" )
points(
    nd$long, 
    nd$lat,
    lwd = 3,
  pch = 1, cex = log(nd$n)/1.5,
  col = ccolz
  )

dev.print(pdf, "figures/Big Map.pdf", width = 15, height = 10)
system("open 'figures/Big Map.pdf' -a /Applications/Preview.app")


       
# europe
par(xpd=F)
map(fill = F, col = "grey60",
    xlim = c(-13, 40), ylim = c(34, 72))
points(
  nd$long, 
  nd$lat,
  lwd = 4,
  pch = 1, cex = log(nd$n)/1.5,
  col = ccolz
)


levs <- hist(log(nd$n)/1.5, breaks = 3, plot=F)

par(xpd=T)

legend(-30, 62, bty = "n",
       #title = "N",
       pch = 1, pt.lwd = 4,
       legend = ceiling(exp(levs$mids*1.5)),
        pt.cex = levs$mids,
       y.intersp = 2
        ) 

legend(-30, 52, bty = "n",
       #title = "N",
       pch = 1, pt.lwd = 4, pt.cex = 3,
       legend = c("Other","Percent budburst","Days to budburst"),
       y.intersp = 2,
       col = colz)

dev.print(pdf, "figures/Euro Map.pdf", width = 10, height = 8)
system("open 'figures/Euro Map.pdf' -a /Applications/Preview.app")