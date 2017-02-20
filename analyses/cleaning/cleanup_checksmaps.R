## Started by DBFlynn in summer 2016 ##
## Small updated by Lizzie in 2017 ##

# This file does two main things:
# (1) Makes some summaries of how clean things are (including making 'papers to revisit' lists) 
# (2) Also makes summaries of what data we have including maps of data!

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(maps)
library(scales) # for alpha
library(lme4)
#library(sjPlot) # visualizing fixed and random effects

# set to DF working directory if DF computer.
if(length(grep("danflynn", getwd())>0)) { # set to DF working directory if DF computer. 
  setwd("~/Documents/git/ospree") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

# Run the prep script (slow, uncomment if haven't run in a while)
# source("source/Prep_all_data.R") 

d <- read.csv("output/ospree_clean.csv") 

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
     main = "Photoperiod frequency", xlab = "Hour"
     )



#############################
# Which do we need to revisit?
# Note that as of Feb 2017 -- we think we have looked at ALL of these
# So I (Lizzie) stopped the files from writing out (since we tossed those folders because ...
# we looked at all that). Whoop!

# do we have dates for all fieldchill = Y studies?

nodate <- unique(d[d$fieldchill == 'yes' & d$fieldsample.date == "",'datasetID'])

revisit <- d[match(nodate, d$datasetID),1:3]
revisit$getfieldsampledate = 'x'

# Fix missing years

noyear <- unique(d[d$year == "",'datasetID'])

revisit2 <- d[match(noyear, d$datasetID),1:3]
revisit2$getyear = "x"

revisit <- merge(revisit2, revisit, all = TRUE)

# Fix missing lat long

nolat <- unique(d[is.na(as.numeric(as.character(d$provenance.lat))),'datasetID'])
revisit3 <- d[match(nolat, d$datasetID),1:3]
revisit3$getlatlong = "x"

revisit <- merge(revisit3, revisit, all = TRUE)

# Fix response or response.time 
fixresp <- unique(d[d$response == "" & d$response.time == "" | 
    d$response == "no response" & d$response.time == "no response" | 
    is.na(d$response) & is.na(d$response.time),'datasetID'])

revisit4 <- d[match(fixresp, d$datasetID),1:3]
revisit4$fixresponse = "x"
revisit <- merge(revisit4, revisit, all = TRUE)

revisit[is.na(revisit)] = ""

# write.csv(revisit, file = "revisit.csv", row.names = FALSE)

# ambient or range for forcetemp
revisit.amb <- d[which(grepl("ambi", d$forcetemp)==TRUE),]
revisit.hyp <- d[which(grepl("-", d$forcetemp)==TRUE),]
revisit.com <- d[which(grepl(",", d$forcetemp)==TRUE),] # Check; mainly Skuterud paper here, Fig 4 is mean across temps, but Fig 3 has data across forcing temps....

forcetempissues <- rbind(revisit.amb, revisit.hyp, revisit.com)

forcetempissues.print <- subset(forcetempissues, select=c("datasetID", "study", "forcetemp"))
forcetempissues.print <- forcetempissues.print[!duplicated(forcetempissues.print),]
# write.csv(forcetempissues.print, file = "revisiting/revisit_temp.csv", row.names = FALSE)


#############################
# More thinking about what's missing
d.nolat <- subset(d, is.na(provenance.lat)==TRUE | provenance.lat=="") # only 10 rows (was 399 in July 2016!)
d.noforce <- subset(d, is.na(forcetemp)==TRUE | forcetemp=="") # only 257 rows (was 521)
d.nophoto <- subset(d, is.na(photoperiod_day)==TRUE | photoperiod_day=="") # 315 (was 628)
d$latbi <- paste(d$genus, d$species, sep="_")
d.nolatbi <- subset(d, is.na(latbi)==TRUE | latbi=="") # all studies are done on some named thing!

# Now let's make them numeric and see what happens ...
d.num <- d

d.num$provenance.lat <- as.numeric(d.num$provenance.lat)
d.num$forcetemp <- as.numeric(d.num$forcetemp)
d.num$photoperiod_day <- as.numeric(d.num$photoperiod_day)

d.num.nolat <- subset(d.num, is.na(provenance.lat)==TRUE | provenance.lat=="") # only 10 (was 479) rows
d.num.noforce <- subset(d.num, is.na(forcetemp)==TRUE | forcetemp=="") # 0 (was 3124) rows ... really?
d.num.nophoto <- subset(d.num, is.na(photoperiod_day)==TRUE | photoperiod_day=="") # 0 (was 3770) rows ... really?

unique(d$provenance.lat)
unique(d$photoperiod_day)
unique(d$forcetemp)
unique(d$latbi)

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
       col = colz)# for(i in 1:length(tomove)){
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
