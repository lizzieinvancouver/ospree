# cleanup script for budburst review
# Also makes summaries of what data we have

library(gdata) # for read.xls
library(maps)
library(scales) # for alpha
library(lme4)
library(sjPlot) # visualizing fixed and random effects

setwd("~/Documents/git/ospree") # setwd("~/Documents/git/projects/treegarden/ospree/ospree")

# Run the prep script (slow, uncomment if haven't run in a while)
# source("Prep_all_data.R") 

d <- read.csv("ospree_clean1.csv") 

d <- d[d$woody=="yes",]
head(d)

yr <- as.numeric(as.character(d$year))

hist(yr, breaks = 50, xaxt="n", col = "lightblue", main = "Years of Publication")
axis(1, at = seq(min(yr, na.rm=T), max(yr, na.rm=T), by = 3))

#############################
# Counting
d$latbi <- paste(d$genus, d$species)
length(unique(d$latbi)) # unique species
d$datasetIDstudy <- paste(d$datasetID, d$study)
length(unique(d$datasetIDstudy)) # unique studies

# ranges
range(as.numeric(as.character(d$forcetemp)), na.rm=TRUE) 
range(as.numeric(as.character(d$chilltemp)), na.rm=TRUE)
range(as.numeric(as.character(d$photoperiod_day)), na.rm=TRUE)
hist(as.numeric(as.character(d$photoperiod_day)),
     main = "Photoperiod frequency", xlab = "Hour",
     )


#############################
# Which do we need to revisit?

# do we have dates for all fieldchill = Y studies?

nodate <- unique(d[d$fieldchill == 'yes' & d$fieldsample.date == "",'datasetID'])

revisit <- d[match(nodate, d$datasetID),1:3]
revisit$getfieldsampledate = 'x'

# Fix missing years

noyear <- unique(d[d$year == "",'datasetID'])

revisit2 <- d[match(noyear, d$datasetID),1:3]
revisit2$getyear = "x"

revisit <- merge(revisit2, revisit, all = T)

# Fix missing lat long

nolat <- unique(d[is.na(as.numeric(as.character(d$provenance.lat))),'datasetID'])
revisit3 <- d[match(nolat, d$datasetID),1:3]
revisit3$getlatlong = "x"

revisit <- merge(revisit3, revisit, all = T)

# Fix response or response.time 
fixresp <- unique(d[d$response == "" & d$response.time == "" | 
                  d$response == "no response" & d$response.time == "no response" | 
                    is.na(d$response) & is.na(d$response.time),'datasetID'])

revisit4 <- d[match(fixresp, d$datasetID),1:3]
revisit4$fixresponse = "x"
revisit <- merge(revisit4, revisit, all = T)

revisit[is.na(revisit)] = ""

write.csv(revisit, file = "Papers to Revisit.csv", row.names = F)


#############################
# Mapping 

head(d)

#d.data_detailed = d

length(unique(d$datasetID))
length(unique(paste(d$datasetID, d$study)))


length(unique(paste(d$genus, d$species)))
sort(summary(factor(paste(d$genus, d$species))), decreasing = T)

unique(d$respvar)
sort(summary(d$respvar))


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

  pdf("figures/Big_Map_0.pdf", width = 15, height = 8)
  
  # nd$long[nd$long < -52 & nd$long > -55]
  # nd[nd$long == -52.35,]
  # nd[nd$lat == 31.683,] # Fixing one latitude in Atlantic Ocean..
  nd$lat[nd$lat == 31.683 & !is.na(nd$lat)] = -31.683
  
  # ylim = c(-62, 90) for full version
  map(fill = F, col = "grey60", interior = F, ylim = c(16, 90))
  points(
      nd$long, 
      nd$lat,
      lwd = 3,
    pch = 1, cex = log(nd$n)/1.2,
    col = ccolz
    )
  
  levs <- hist(log(nd$n)/1.2, breaks = 3, plot=F)
  
  par(xpd=T)
  
  legend(-100, 15, bty = "n",# x = -180, y = 0 for full version
         title = "N",
         pch = 1, pt.lwd = 4,
         legend = c(5, 50, 500),#ceiling(exp(levs$mids*1.2)),
         pt.cex = levs$mids,
         y.intersp = 1.75,
         x.intersp = 2
         
  ) 
  
  legend(25, 15, bty = "n", # x = -150, y = 0 for full version
         title = "Response",
         pch = 1, pt.lwd = 4, pt.cex = 3,
         legend = c("Other","Percent budburst","Days to budburst"),
         y.intersp = 1.75,
         x.intersp = 2,
         col = colz)
  
  dev.off();system("open 'figures/Big_Map_0.pdf' -a /Applications/Preview.app")


       
# europe
pdf("figures/Euro_Map.pdf", width = 10, height = 8)
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
       y.intersp = 2,
       x.intersp = 2
       
        ) 

legend(-30, 52, bty = "n",
       #title = "N",
       pch = 1, pt.lwd = 4, pt.cex = 3,
       legend = c("Other","Percent budburst","Days to budburst"),
       y.intersp = 2,
       x.intersp = 2,
       col = colz)

dev.off()
system("open 'figures/Euro_Map.pdf' -a /Applications/Preview.app")


################# For cleaning up excluded references. Moves PDFs for studies marked in the study file as 'n' for 'no, do not include' into a different folder
# tomove <- d[d$Status == "n", "Filename"]
# 
# for(i in 1:length(tomove)){
# 	
# system(paste(paste("mv ~/Dropbox/Work/Harvard/Budburst_Review/Budburst_Refs/", tomove[i], sep = ""), 
# 				paste("~/Dropbox/Work/Harvard/Budburst_Review/Excluded_Refs/", tomove[i], sep = ""))
# 				)
# 				}
# 

# For study file
# sort(summary(d$Journal))
# 
# head(d.data_detailed)
# detailedID
# 
# detailedID_exp <- unique(paste(d.data_detailed$datasetID, d.data_detailed$study))
# unique(detailedID_exp)
