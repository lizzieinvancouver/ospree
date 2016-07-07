## Started Thursday January 21 ##
## by Jehane, Lizzie and others ##

## An R script to clean the "respvar" column of the bud burst data
## Updated 7 July 2016 for more cleaning! ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/git/projects/treegarden/budreview/ospree")

# Name data frame:
scrapedata <- read.csv ("ospree.csv")

sort(table(scrapedata$respvar), TRUE)
names(table(scrapedata$respvar)) # 87 values now

sort(with(scrapedata[scrapedata$respvar=="daystobudburst",], table(datasetID)), TRUE)
sort(with(scrapedata[scrapedata$respvar=="percentbudburst",], table(datasetID)), TRUE)

##
##  Convert some inverted stuff
##

scrapedata$response.time[scrapedata$respvar=="1/daysto25%budburst"] <-
    1/as.numeric(scrapedata$response[scrapedata$respvar=="1/daysto25%budburst"])
scrapedata$response.time[scrapedata$respvar=="1/daysto50%budburst"] <-
    1/as.numeric(scrapedata$response[scrapedata$respvar=="1/daysto50%budburst"])
scrapedata$response[scrapedata$respvar=="1/daysto25%budburst"] <- "25per"
scrapedata$response[scrapedata$respvar=="1/daysto50%budburst"] <- "50per"

scrapedata$respvar[scrapedata$respvar=="1/daysto25%budburst"] <- "daysto25%budburst"
scrapedata$respvar[scrapedata$respvar=="1/daysto50%budburst"] <- "daysto50%budburst"

# Still need to deal with :
# "dateofbudburst"
# for above, values of 95 to 137 it looks like ... but no field sampling date
# "percentbudset"   

# for now, dayofbudbreak is equivalent to daystobudbreak ... 
subset(scrapedata$fieldsample.date, scrapedata$respvar=="dayofbudbreak")
subset(scrapedata$response.time, scrapedata$respvar=="dayofbudbreak")

##
## Fixing obvious typos and synonmyms
##

# Days to budburst
scrapedata$respvar[scrapedata$respvar == "days to budbreak (on  50% of plants)"] <- "daysto50%budburst"
scrapedata$respvar[scrapedata$respvar == "daysto50perbudburst"] <- "daysto50%budburst"
scrapedata$respvar[scrapedata$respvar == "daystodudburst"] <- "daystobudburst"
scrapedata$respvar[scrapedata$respvar == "daystobudbust"] <- "daystobudburst"
scrapedata$respvar[scrapedata$respvar == "daystobudset"] <- "daystobudburst"
scrapedata$respvar[scrapedata$respvar == "baystobudset"] <- "daystobudburst"
scrapedata$respvar[scrapedata$respvar == "baystobudburst"] <- "daystobudburst"
scrapedata$respvar[scrapedata$respvar == "daysto50percentbudburst"] <- "daysto50%budburst"

# Days to something else
scrapedata$respvar[scrapedata$respvar == "daysto50percentflowering"] <- "daysto50flowering"

  
# Percent something
scrapedata$respvar[scrapedata$respvar == "percentbudsburst"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "percent_apicalbudburst"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "% plants with budburst"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "perbudburst"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "percent_lateralbudburst"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "percentlateralbudburst"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "percentapicalbudburst"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "percentbloom"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "percentflowering"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "mean percent budbreak at end of study"] <- "percentbudburst"
scrapedata$respvar[scrapedata$respvar == "percentunfolding"] <- "percentbudburst"

# Growth of some sort
scrapedata$respvar[scrapedata$respvar == "growth rate 1/days to 25 pct budburst"] <- "1/daysto25%budburst"
scrapedata$respvar[scrapedata$respvar == "cumulative growth increment"] <- "cumulativegrowthincrement"
scrapedata$respvar[scrapedata$respvar == "cumulative leaf no. increment"] <- "cumulativeleafincrement"
scrapedata$respvar[scrapedata$respvar == "cumulative leaf increment"] <- "cumulativeleafincrement"
scrapedata$respvar[scrapedata$respvar == "numofnewleaves"] <- "numberofleaves"

#responses reported as scores (for examples, scores on the BBCH scale) should be reported as "budstage"
scrapedata$respvar[scrapedata$respvar == "leafemergencescore"] <- "budstage"
#"budphenology" seems to be integers from 1 through 8, so included in "budstage"
scrapedata$respvar[scrapedata$respvar == "budphenology"] <- "budstage"
scrapedata$respvar[scrapedata$respvar == "daystoleafunfolding"] <- "daystoleafout"

#"elongation_heightgrowth" has values from 20-95 for populus seedlings, "elongation_height" has values from 0-9 for betula seedlings. Can we assume all elongation is in centimeters, and combine them? Unclear.
scrapedata$respvar[scrapedata$respvar == "elongation (cm)"] <- "elongation"
scrapedata$respvar[scrapedata$respvar == "elongation_height"] <- "elongation"
scrapedata$respvar[scrapedata$respvar == "elongation_heightgrowth"] <- "elongation"
scrapedata$respvar[scrapedata$respvar == "shootelongation"] <- "elongation"
scrapedata$respvar[scrapedata$respvar == "shootlength"] <- "elongation"
scrapedata$respvar[scrapedata$respvar == "shootlengthcm"] <- "elongation"

# more cleaning
scrapedata$respvar[scrapedata$respvar == "daystoantithesis"] <- "daystoanthesis"
scrapedata$respvar[scrapedata$respvar == "elongationcm"] <- "elongation"
scrapedata$respvar[scrapedata$respvar == "heightcm"] <- "height"
scrapedata$respvar[scrapedata$respvar ==  "inflorescencesperplant"] <- "inflorescenceperplant"
scrapedata$respvar[scrapedata$respvar ==  "petiolelengthcm"] <- "petiolelength"


scrapedata$respvar[scrapedata$respvar == ""] <- NA
scrapedata$respvar[scrapedata$respvar == ""] <- NA
scrapedata$respvar[scrapedata$respvar == ""] <- NA

##
## Assigning 87 response variables to a higher level
## AKA, making fewer response variables!
##

scrapedata$respvar.simple <- NA

# thermal time
scrapedata$respvar.simple[scrapedata$respvar == ">2_ degreedaystobudburst"] <- "thermaltime"
scrapedata$respvar.simple[scrapedata$respvar == "degreedays>0tobudburst"] <- "thermaltime"
scrapedata$respvar.simple[scrapedata$respvar == "degreedays>0tobudburst"] <- "thermaltime"
scrapedata$respvar.simple[scrapedata$respvar == "thermaltimetobudburst"] <- "thermaltime"
scrapedata$respvar.simple[scrapedata$respvar == "degreedaystobudburst"] <- "thermaltime"


# daystobudburst
# "dateofbudburst" still not dealt with ... 
scrapedata$respvar.simple[scrapedata$respvar == "daystobudburst"] <- "daystobudburst"
scrapedata$respvar.simple[scrapedata$respvar == "daysto25%budburst"] <- "daystobudburst"
scrapedata$respvar.simple[scrapedata$respvar == "daysto50%budburst"] <- "daystobudburst"
scrapedata$respvar.simple[scrapedata$respvar == "dayofbudbreak"] <- "daystobudburst"  #see notes above
scrapedata$respvar.simple[scrapedata$respvar == "daysto10percentbudburst"] <- "daystobudburst"
scrapedata$respvar.simple[scrapedata$respvar == "daysto20%budburst"] <- "daystobudburst"
scrapedata$respvar.simple[scrapedata$respvar == "daysto50%budburst"] <- "daystobudburst"
scrapedata$respvar.simple[scrapedata$respvar == "daystoleafout"] <- "daystobudburst"
scrapedata$respvar.simple[scrapedata$respvar == "leafunfoldingdate"] <- "daystobudburst"

# daystoflower
scrapedata$respvar.simple[scrapedata$respvar == "daystoflower"] <- "daystoflower"
scrapedata$respvar.simple[scrapedata$respvar == "daystoflowering"] <- "daystoflower"
scrapedata$respvar.simple[scrapedata$respvar == "daysto10flowering"] <- "daystoflower"
scrapedata$respvar.simple[scrapedata$respvar == "daysto50flowering"] <- "daystoflower"
scrapedata$respvar.simple[scrapedata$respvar == "daystoanthesis"] <- "daystoflower"
scrapedata$respvar.simple[scrapedata$respvar == "daystopanicle"] <- "daystoflower"

# percentbudburst
scrapedata$respvar.simple[scrapedata$respvar == "percentbudburst"] <- "percentbudburst"

# percentflower
scrapedata$respvar.simple[scrapedata$respvar == "percentflower"] <- "percentflower"
scrapedata$respvar.simple[scrapedata$respvar == "percentcuttingswithflowerbuds"] <- "percentflower"
scrapedata$respvar.simple[scrapedata$respvar == "percentnodesflowering"] <- "percentflower"
scrapedata$respvar.simple[scrapedata$respvar == "percentflowerbuds"] <- "percentflower"

# growth
scrapedata$respvar.simple[scrapedata$respvar == "averagegrowth"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "apicalbudgrowth"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "budlength"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "budwidth"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "elongation"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "lengthofinternodescm"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "leafincrement"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "mmper14days"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "lengthoftrusscm"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "totalgrowth"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "stemelongationcm"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "shootelongationcm"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "shootgrowthcm"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "petiolelength"] <- "growth"
scrapedata$respvar.simple[scrapedata$respvar == "yieldgrams"] <- "growth"

# phenstage
scrapedata$respvar.simple[scrapedata$respvar == "growthstage"] <- "phenstage"
scrapedata$respvar.simple[scrapedata$respvar == "budstage"] <- "phenstage"

# phenstageper.probonestudy
scrapedata$respvar.simple[scrapedata$respvar == "percentstage01"] <- "phenstageper.probonestudy"
scrapedata$respvar.simple[scrapedata$respvar == "percentstage02"] <- "phenstageper.probonestudy"
scrapedata$respvar.simple[scrapedata$respvar == "percentstage03"] <- "phenstageper.probonestudy"
scrapedata$respvar.simple[scrapedata$respvar == "percentstage04"] <- "phenstageper.probonestudy"
scrapedata$respvar.simple[scrapedata$respvar == "percentstage06"] <- "phenstageper.probonestudy"

# flowernumber
scrapedata$respvar.simple[scrapedata$respvar == "flowerperplant"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "flowers"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "flowersperinflorescence"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "flowersperplant"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "flowerspertruss"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "growthincrement"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "height"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "inflorescenceperplant"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "plantsflowering"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "inflorescences"] <- "flowernumber"
scrapedata$respvar.simple[scrapedata$respvar == "numberflowerbuds"] <- "flowernumber"

# othernums
scrapedata$respvar.simple[scrapedata$respvar == "numberofbuds"] <- "othernums"
scrapedata$respvar.simple[scrapedata$respvar == "numberofleaves"] <- "othernums"
scrapedata$respvar.simple[scrapedata$respvar == "numberofnodes"] <- "othernums"
scrapedata$respvar.simple[scrapedata$respvar == "numberofstolons"] <- "othernums"
scrapedata$respvar.simple[scrapedata$respvar == "stolonsperplant"] <- "othernums"

# otherpercents
scrapedata$respvar.simple[scrapedata$respvar == "percentfruiting"] <- "otherpercents"
scrapedata$respvar.simple[scrapedata$respvar == "percentrooting"] <- "otherpercents"
scrapedata$respvar.simple[scrapedata$respvar == "percentrunnering"] <- "otherpercents"

# fruitmass
scrapedata$respvar.simple[scrapedata$respvar == "freshfruitg"] <- "fruitmass"
scrapedata$respvar.simple[scrapedata$respvar == "fruitmassperplant"] <- "fruitmass"

# notsureabout
scrapedata$respvar.simple[scrapedata$respvar == "budprojectedarea"] <- "notsureabout"
scrapedata$respvar.simple[scrapedata$respvar == "critical.daylength.hrs"] <- "notsureabout"
scrapedata$respvar.simple[scrapedata$respvar == "cumulativegrowthincrement"] <- "notsureabout"
scrapedata$respvar.simple[scrapedata$respvar == "cumulativeleafincrement"] <- "notsureabout"
scrapedata$respvar.simple[scrapedata$respvar == "leaves"] <- "notsureabout"
scrapedata$respvar.simple[scrapedata$respvar == "nodes"] <- "notsureabout"
scrapedata$respvar.simple[scrapedata$respvar == "plantheightatflowerbudappearance"] <- "notsureabout"
   
# other
scrapedata$respvar.simple[scrapedata$respvar == "percentbudburst_dormancy"] <- "other"
scrapedata$respvar.simple[scrapedata$respvar == "weeksofleafproduction"] <- "other"
scrapedata$respvar.simple[scrapedata$respvar == "survival"] <- "other"
scrapedata$respvar.simple[scrapedata$respvar == "DARD"] <- "other"
scrapedata$respvar.simple[scrapedata$respvar == ""] <- "other"

# check your work .... 
checking <- subset(scrapedata, is.na(respvar.simple)==TRUE)
unique(checking$respvar)

write.csv(scrapedata, file = "ospree_clean.csv", row.names=FALSE)


stop(print("cleaning response variables done for now, but we need to check how many times one study has multiple variables ...."))

# scratch
incaseneeded <- format(as.Date(scrapedata$fieldsample.date, format="%Y-%m-%d"), "%j")
# subset(scrapedata[,1:20], scrapedata$respvar=="dayofbudbreak")


