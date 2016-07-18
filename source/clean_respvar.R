## Started Thursday January 21 ##
## by Jehane, Lizzie and others ##

## An R script to clean the "respvar" column of the bud burst data
## Updated 7 July 2016 for more cleaning! ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("danflynn", getwd())>0)) { # set to DF working directory if DF computer. 
  setwd("~/Documents/git/ospree") 
  } else setwd("~/Documents/git/projects/treegarden/budreview/ospree")

# Name data frame:
d <- read.csv("ospree.csv")

sort(table(d$respvar), TRUE)
names(table(d$respvar)) # 87 values now

sort(with(d[d$respvar=="daystobudburst",], table(datasetID)), TRUE)
sort(with(d[d$respvar=="percentbudburst",], table(datasetID)), TRUE)

##
##  Convert some inverted stuff
##

d$response.time[d$respvar=="1/daysto25%budburst"] <-
    1/as.numeric(d$response[d$respvar=="1/daysto25%budburst"])
d$response.time[d$respvar=="1/daysto50%budburst"] <-
    1/as.numeric(d$response[d$respvar=="1/daysto50%budburst"])
d$response[d$respvar=="1/daysto25%budburst"] <- "25per"
d$response[d$respvar=="1/daysto50%budburst"] <- "50per"

d$respvar[d$respvar=="1/daysto25%budburst"] <- "daysto25%budburst"
d$respvar[d$respvar=="1/daysto50%budburst"] <- "daysto50%budburst"

# Still need to deal with :
# "dateofbudburst"
# for above, values of 95 to 137 it looks like ... but no field sampling date
# "percentbudset"   

# for now, dayofbudbreak is equivalent to daystobudbreak ... 
subset(d$fieldsample.date, d$respvar=="dayofbudbreak")
subset(d$response.time, d$respvar=="dayofbudbreak")

##
## Fixing obvious typos and synonmyms
##

# Days to budburst
d$respvar[d$respvar == "days to budbreak (on  50% of plants)"] <- "daysto50%budburst"
d$respvar[d$respvar == "daysto50perbudburst"] <- "daysto50%budburst"
d$respvar[d$respvar == "daystodudburst"] <- "daystobudburst"
d$respvar[d$respvar == "daystobudbust"] <- "daystobudburst"
d$respvar[d$respvar == "daystobudset"] <- "daystobudburst"
d$respvar[d$respvar == "baystobudset"] <- "daystobudburst"
d$respvar[d$respvar == "baystobudburst"] <- "daystobudburst"
d$respvar[d$respvar == "daysto50percentbudburst"] <- "daysto50%budburst"

# Days to something else
d$respvar[d$respvar == "daysto50percentflowering"] <- "daysto50flowering"

  
# Percent something
d$respvar[d$respvar == "percentbudsburst"] <- "percentbudburst"
d$respvar[d$respvar == "percent_apicalbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "% plants with budburst"] <- "percentbudburst"
d$respvar[d$respvar == "perbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "percent_lateralbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "percentlateralbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "percentapicalbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "percentbloom"] <- "percentbudburst"
d$respvar[d$respvar == "percentflowering"] <- "percentbudburst"
d$respvar[d$respvar == "mean percent budbreak at end of study"] <- "percentbudburst"
d$respvar[d$respvar == "percentunfolding"] <- "percentbudburst"

# Growth of some sort
d$respvar[d$respvar == "growth rate 1/days to 25 pct budburst"] <- "1/daysto25%budburst"
d$respvar[d$respvar == "cumulative growth increment"] <- "cumulativegrowthincrement"
d$respvar[d$respvar == "cumulative leaf no. increment"] <- "cumulativeleafincrement"
d$respvar[d$respvar == "cumulative leaf increment"] <- "cumulativeleafincrement"
d$respvar[d$respvar == "numofnewleaves"] <- "numberofleaves"

#responses reported as scores (for examples, scores on the BBCH scale) should be reported as "budstage"
d$respvar[d$respvar == "leafemergencescore"] <- "budstage"
#"budphenology" seems to be integers from 1 through 8, so included in "budstage"
d$respvar[d$respvar == "budphenology"] <- "budstage"
d$respvar[d$respvar == "daystoleafunfolding"] <- "daystoleafout"

#"elongation_heightgrowth" has values from 20-95 for populus seedlings, "elongation_height" has values from 0-9 for betula seedlings. Can we assume all elongation is in centimeters, and combine them? Unclear.
d$respvar[d$respvar == "elongation (cm)"] <- "elongation"
d$respvar[d$respvar == "elongation_height"] <- "elongation"
d$respvar[d$respvar == "elongation_heightgrowth"] <- "elongation"
d$respvar[d$respvar == "shootelongation"] <- "elongation"
d$respvar[d$respvar == "shootlength"] <- "elongation"
d$respvar[d$respvar == "shootlengthcm"] <- "elongation"

# more cleaning
d$respvar[d$respvar == "daystoantithesis"] <- "daystoanthesis"
d$respvar[d$respvar == "elongationcm"] <- "elongation"
d$respvar[d$respvar == "heightcm"] <- "height"
d$respvar[d$respvar ==  "inflorescencesperplant"] <- "inflorescenceperplant"
d$respvar[d$respvar ==  "petiolelengthcm"] <- "petiolelength"


d$respvar[d$respvar == ""] <- NA
d$respvar[d$respvar == ""] <- NA
d$respvar[d$respvar == ""] <- NA

##
## Assigning 87 response variables to a higher level
## AKA, making fewer response variables!
##

d$respvar.simple <- NA

# thermal time
d$respvar.simple[d$respvar == ">2_ degreedaystobudburst"] <- "thermaltime"
d$respvar.simple[d$respvar == "degreedays>0tobudburst"] <- "thermaltime"
d$respvar.simple[d$respvar == "degreedays>0tobudburst"] <- "thermaltime"
d$respvar.simple[d$respvar == "thermaltimetobudburst"] <- "thermaltime"
d$respvar.simple[d$respvar == "degreedaystobudburst"] <- "thermaltime"


# daystobudburst
# "dateofbudburst" still not dealt with ... 
d$respvar.simple[d$respvar == "daystobudburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daysto25%budburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daysto50%budburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "dayofbudbreak"] <- "daystobudburst"  #see notes above
d$respvar.simple[d$respvar == "daysto10percentbudburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daysto20%budburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daysto50%budburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daystoleafout"] <- "daystobudburst"
d$respvar.simple[d$respvar == "leafunfoldingdate"] <- "daystobudburst"

# daystoflower
d$respvar.simple[d$respvar == "daystoflower"] <- "daystoflower"
d$respvar.simple[d$respvar == "daystoflowering"] <- "daystoflower"
d$respvar.simple[d$respvar == "daysto10flowering"] <- "daystoflower"
d$respvar.simple[d$respvar == "daysto50flowering"] <- "daystoflower"
d$respvar.simple[d$respvar == "daystoanthesis"] <- "daystoflower"
d$respvar.simple[d$respvar == "daystopanicle"] <- "daystoflower"

# percentbudburst
d$respvar.simple[d$respvar == "percentbudburst"] <- "percentbudburst"

# percentflower
d$respvar.simple[d$respvar == "percentflower"] <- "percentflower"
d$respvar.simple[d$respvar == "percentcuttingswithflowerbuds"] <- "percentflower"
d$respvar.simple[d$respvar == "percentnodesflowering"] <- "percentflower"
d$respvar.simple[d$respvar == "percentflowerbuds"] <- "percentflower"

# growth
d$respvar.simple[d$respvar == "averagegrowth"] <- "growth"
d$respvar.simple[d$respvar == "apicalbudgrowth"] <- "growth"
d$respvar.simple[d$respvar == "budlength"] <- "growth"
d$respvar.simple[d$respvar == "budwidth"] <- "growth"
d$respvar.simple[d$respvar == "elongation"] <- "growth"
d$respvar.simple[d$respvar == "lengthofinternodescm"] <- "growth"
d$respvar.simple[d$respvar == "leafincrement"] <- "growth"
d$respvar.simple[d$respvar == "mmper14days"] <- "growth"
d$respvar.simple[d$respvar == "lengthoftrusscm"] <- "growth"
d$respvar.simple[d$respvar == "totalgrowth"] <- "growth"
d$respvar.simple[d$respvar == "stemelongationcm"] <- "growth"
d$respvar.simple[d$respvar == "shootelongationcm"] <- "growth"
d$respvar.simple[d$respvar == "shootgrowthcm"] <- "growth"
d$respvar.simple[d$respvar == "petiolelength"] <- "growth"
d$respvar.simple[d$respvar == "yieldgrams"] <- "growth"

# phenstage
d$respvar.simple[d$respvar == "growthstage"] <- "phenstage"
d$respvar.simple[d$respvar == "budstage"] <- "phenstage"

# phenstageper.probonestudy
d$respvar.simple[d$respvar == "percentstage01"] <- "phenstageper.probonestudy"
d$respvar.simple[d$respvar == "percentstage02"] <- "phenstageper.probonestudy"
d$respvar.simple[d$respvar == "percentstage03"] <- "phenstageper.probonestudy"
d$respvar.simple[d$respvar == "percentstage04"] <- "phenstageper.probonestudy"
d$respvar.simple[d$respvar == "percentstage06"] <- "phenstageper.probonestudy"

# flowernumber
d$respvar.simple[d$respvar == "flowerperplant"] <- "flowernumber"
d$respvar.simple[d$respvar == "flowers"] <- "flowernumber"
d$respvar.simple[d$respvar == "flowersperinflorescence"] <- "flowernumber"
d$respvar.simple[d$respvar == "flowersperplant"] <- "flowernumber"
d$respvar.simple[d$respvar == "flowerspertruss"] <- "flowernumber"
d$respvar.simple[d$respvar == "growthincrement"] <- "flowernumber"
d$respvar.simple[d$respvar == "height"] <- "flowernumber"
d$respvar.simple[d$respvar == "inflorescenceperplant"] <- "flowernumber"
d$respvar.simple[d$respvar == "plantsflowering"] <- "flowernumber"
d$respvar.simple[d$respvar == "inflorescences"] <- "flowernumber"
d$respvar.simple[d$respvar == "numberflowerbuds"] <- "flowernumber"

# othernums
d$respvar.simple[d$respvar == "numberofbuds"] <- "othernums"
d$respvar.simple[d$respvar == "numberofleaves"] <- "othernums"
d$respvar.simple[d$respvar == "numberofnodes"] <- "othernums"
d$respvar.simple[d$respvar == "numberofstolons"] <- "othernums"
d$respvar.simple[d$respvar == "stolonsperplant"] <- "othernums"

# otherpercents
d$respvar.simple[d$respvar == "percentfruiting"] <- "otherpercents"
d$respvar.simple[d$respvar == "percentrooting"] <- "otherpercents"
d$respvar.simple[d$respvar == "percentrunnering"] <- "otherpercents"

# fruitmass
d$respvar.simple[d$respvar == "freshfruitg"] <- "fruitmass"
d$respvar.simple[d$respvar == "fruitmassperplant"] <- "fruitmass"

# notsureabout
d$respvar.simple[d$respvar == "budprojectedarea"] <- "notsureabout"
d$respvar.simple[d$respvar == "critical.daylength.hrs"] <- "notsureabout"
d$respvar.simple[d$respvar == "cumulativegrowthincrement"] <- "notsureabout"
d$respvar.simple[d$respvar == "cumulativeleafincrement"] <- "notsureabout"
d$respvar.simple[d$respvar == "leaves"] <- "notsureabout"
d$respvar.simple[d$respvar == "nodes"] <- "notsureabout"
d$respvar.simple[d$respvar == "plantheightatflowerbudappearance"] <- "notsureabout"
   
# other
d$respvar.simple[d$respvar == "percentbudburst_dormancy"] <- "other"
d$respvar.simple[d$respvar == "weeksofleafproduction"] <- "other"
d$respvar.simple[d$respvar == "survival"] <- "other"
d$respvar.simple[d$respvar == "DARD"] <- "other"
d$respvar.simple[d$respvar == ""] <- "other"

# check your work .... 
checking <- subset(d, is.na(respvar.simple)==TRUE)
unique(checking$respvar)




#############################
# Which studies have multiple respvar but only one respvar.simple?
d$datasetIDstudy <- paste(d$datasetID, d$study)

studyresp <- with(d, paste(datasetIDstudy, respvar))
studyresps <- with(d, paste(datasetIDstudy, respvar.simple))

# unique(studyresp)
# unique(studyresps)

# which studies have multiple respvars for one respvar.simple?
xx <- tapply(studyresp, studyresps, function(x)
  length(unique(x)) > 1)

multiresp <- names(xx)[xx==T]

# make a flag for this 
d$multiresp <- !is.na(match(studyresps, multiresp))

# which studies have multiple original respvars and each one is a separate respvar.simple?
xx <- data.frame(datasetIDstudy = d$datasetIDstudy, studyresp, studyresps)

# i="yazdaniha64 exp1"
# i = "falusi90 exp1"
# i = "ashby62 exp1"

multibothresp <- vector()

for(i in unique(d$datasetIDstudy)){
  xz <- xx[xx$datasetIDstudy == i,]
  
  ta <- table(xz$studyresp, xz$studyresps)
  
  multibothresp <- 
    c(multibothresp, 
      identical(nrow(ta), ncol(ta)) & nrow(ta) > 1
    )
  
}
# these are the studies fit this criterion
(mb <- unique(d$datasetIDstudy)[multibothresp])

d$multibothresp <- !is.na(match(d$datasetIDstudy, mb))


write.csv(d, file = "ospree_clean.csv", row.names=FALSE)



# scratch
incaseneeded <- format(as.Date(d$fieldsample.date, format="%Y-%m-%d"), "%j")
# subset(d[,1:20], d$respvar=="dayofbudbreak")


