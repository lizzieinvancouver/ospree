## Started Thursday January 21 ##
## by Jehane, Lizzie and others ##

## An R script to clean the "respvar" column of the bud burst data
## Updated 7 July 2016 for more cleaning! ##

## Edits made to respvar.simple column by Cat 
## Updated 1 August 2016 ##

## Updates by Lizzie on 27 Jan 2017 ##
## As of 27 Jan 2017 this file is now SOURCED from cleanmerge_all.R ##
## So to run this you need to start there ##

# See cleanmerge_all.R for started text #
sort(table(d$respvar), TRUE)
names(table(d$respvar)) # 88 values as of 2 Feb 2017

sort(with(d[d$respvar=="daystobudburst",], table(datasetID)), TRUE)
sort(with(d[d$respvar=="percentbudburst",], table(datasetID)), TRUE)

## 
##  Convert some inverted stuff

d$response.time[d$respvar=="1/daysto25%budburst"] <-
    1/as.numeric(d$response[d$respvar=="1/daysto25%budburst"])
d$response.time[d$respvar=="1/daysto50%budburst"] <-
    1/as.numeric(d$response.time[d$respvar=="1/daysto50%budburst"])
d$response[d$respvar=="1/daysto25%budburst"] <- "25per"
d$response[d$respvar=="1/daysto50%budburst"] <- "50per"

d$respvar[d$respvar=="1/daysto25%budburst"] <- "daysto25%budburst"
d$respvar[d$respvar=="1/daysto50%budburst"] <- "daysto50%budburst"

# Still need to deal with :
# for above, values of 95 to 137 it looks like ... but no field sampling date

# for now, dayofbudbreak is equivalent to daystobudbreak ... 
# ailene 1 oct 2012: the only study with this respvar is hawkins12, and it is actually 50% budbreak and day of year (not days to)

subset(d$fieldsample.date, d$respvar=="dayofbudbreak")
subset(d$response.time, d$respvar=="dayofbudbreak")
# "dateofbudburst" has been converted to dayofbudbreak so will treat as daystobudbreak for now

##
## Fixing obvious typos and synonmyms
##

# Days to budburst
d$respvar[d$respvar == "days to budbreak (on  50% of plants)"] <- "daysto50%budburst"
d$respvar[d$respvar == "daysto50perbudburst"] <- "daysto50%budburst"
d$respvar[d$respvar == "daystodudburst"] <- "daystobudburst"
d$respvar[d$respvar == "daystobudbust"] <- "daystobudburst"
d$respvar[d$respvar == "baystobudset"] <- "daystobudset"
d$respvar[d$respvar == "baystobudburst"] <- "daystobudburst"
d$respvar[d$respvar == "daysto50percentbudburst"] <- "daysto50%budburst"
d$respvar[d$respvar == "days to budbust"] <- "daystobudburst"
d$respvar[d$respvar == "dateofbudburst"] <- "daystobudburst"

# Days to something else
d$respvar[d$respvar == "daysto50percentflowering"] <- "daysto50flowering"

  
# Percent something
d$respvar[d$respvar == "percentoftwigswithbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "percentbudsburst"] <- "percentbudburst"
d$respvar[d$respvar == "percent_apicalbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "% plants with budburst"] <- "percentbudburst"
d$respvar[d$respvar == "perbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "percent_lateralbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "percentlateralbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "percentapicalbudburst"] <- "percentbudburst"
d$respvar[d$respvar == "mean percent budbreak at end of study"] <- "percentbudburst"
d$respvar[d$respvar == "percentunfolding"] <- "percentbudburst"
d$respvar[d$respvar == ""] <- "percentbudburst" ## ?This is selecting out hawerroth13, not sure why there is no response variable for this one.
d$respvar[d$datasetID=="junttila12" & d$figure.table..if.applicable. == "fig1" & d$respvar=="percentbudburst"] <- "percentbudburst_dormancy"#mistake we noticed in the database (all other rows from this study and figure are entered as "percentbudburst_dormancy)

# Growth of some sort
d$respvar[d$respvar == "growth rate 1/days to 25 pct budburst"] <- "1/daysto25%budburst"
d$respvar[d$respvar == "cumulative growth increment"] <- "cumulativegrowthincrement"
d$respvar[d$respvar == "cumulative leaf no. increment"] <- "cumulativeleafincrement"
d$respvar[d$respvar == "cumulative leaf increment"] <- "cumulativeleafincrement"
d$respvar[d$respvar == "numofnewleaves"] <- "numberofleaves"
d$respvar[d$respvar == "leaves"] <- "numberofleaves"

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

# fix some specific studies
# Lizzie deleted code that changed viheraaarnio06 (percentbudset) respvar.simple from NA to percentbudset
# ...since below it should turn into 'otherpercents' (14 Mar 2018)

# howe 95: data that is about budset, not budburst
d$respvar[which(d$datasetID=="howe95" & d$figure.table..if.applicable.=="fig1a")] <- "daystobudset"

#### Added 25 July 2017 - Cat ### 
## two rows of Sonsteby14 data were entered incorrectly
d$response.time[d$datasetID=="sonsteby14"& d$figure.table..if.applicable.=="table 3" &
             d$respvar=="daystobudburst" & d$response==7.3] <- 7.3
d$response[d$datasetID=="sonsteby14"& d$figure.table..if.applicable.=="table 3" &
             d$respvar=="daystobudburst" & d$response==7.3] <- 1
d$response.time[d$datasetID=="sonsteby14"& d$figure.table..if.applicable.=="table 4" &
                  d$respvar=="daystobudburst" & d$response==0] <- 0
d$response[d$datasetID=="sonsteby14"& d$figure.table..if.applicable.=="table 4" &
             d$respvar=="daystobudburst" & d$response==0] <- 1

# guerriero90 issues - changed respvar.simple from 'phenstageper.probonestudy' to 'leaves'
d$respvar[d$datasetID== 'guerriero90' & d$respvar == 'percentstage06'] <- 'leaves'

# jones12 issues
# phenstage01 - percentbudburst
# phenstage02 - leaves to othernums
# phenstage03 - fruits to othernums
# phenstage04 - flowers to percentflower
d$respvar[d$datasetID=='jones12' & d$respvar == 'percentstage01'] <- 'percentbudburst'
d$respvar[d$datasetID== 'jones12' & d$respvar == 'percentstage02'] <- 'leaves'
d$respvar[d$datasetID== 'jones12' & d$respvar == 'percentstage03'] <- 'fruits'
d$respvar[d$datasetID== 'jones12' & d$respvar == 'percentstage04'] <- 'flowers'


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
d$respvar.simple[d$respvar == "daystobudburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daysto25%budburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daysto50%budburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "dayofbudbreak"] <- "daystobudburst"  #see notes above
d$respvar.simple[d$respvar == "daysto10percentbudburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daysto20%budburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daysto50%budburst"] <- "daystobudburst"
d$respvar.simple[d$respvar == "daystoleafout"] <- "daystobudburst"
d$respvar.simple[d$respvar == "leafunfoldingdate"] <- "daystobudburst"
d$respvar.simple[d$respvar == "dateofbudburst"] <- "daystobudburst"
## Convert DARD Calculation to daystobudburst
d$response.time[d$respvar=="DARD"] <-
  100/as.numeric(d$response.time[d$respvar=="DARD"])#
d$respvar.simple[d$respvar == "DARD"] <- "daystobudburst"#daily average rate of development.

# daystoflower
d$respvar.simple[d$respvar == "daystoflower"] <- "daystoflower"
d$respvar.simple[d$respvar == "daystoflowering"] <- "daystoflower"
d$respvar.simple[d$respvar == "daysto10flowering"] <- "daystoflower"
d$respvar.simple[d$respvar == "daysto50flowering"] <- "daystoflower"
d$respvar.simple[d$respvar == "daystoanthesis"] <- "daystoflower"
d$respvar.simple[d$respvar == "daystopanicle"] <- "daystoflower"

# percentbudburst
d$respvar.simple[d$respvar == "percentbudburst"] <- "percentbudburst"
d$respvar.simple[d$respvar == "percentbudburst_dormancy"] <- "percentbudburst"

# percentflower
d$respvar.simple[d$respvar == "percentflower"] <- "percentflower"
d$respvar.simple[d$respvar == "percentcuttingswithflowerbuds"] <- "percentflower"
d$respvar.simple[d$respvar == "percentnodesflowering"] <- "percentflower"
d$respvar.simple[d$respvar == "percentflowerbuds"] <- "percentflower"

# growth
d$respvar.simple[d$respvar == "budprojectedarea"] <- "growth"
d$respvar.simple[d$respvar == "averagegrowth"] <- "growth"
d$respvar.simple[d$respvar == "apicalbudgrowth"] <- "growth"
d$respvar.simple[d$respvar == "budlength"] <- "growth"
d$respvar.simple[d$respvar == "budwidth"] <- "growth"
d$respvar.simple[d$respvar == "cumulativegrowthincrement"] <- "growth"
d$respvar.simple[d$respvar == "nodes"] <- "growth"
d$respvar.simple[d$respvar == "elongation"] <- "growth"
d$respvar.simple[d$respvar == "growthincrement"] <- "growth"
d$respvar.simple[d$respvar == "height"] <- "growth"
d$respvar.simple[d$respvar == "lengthofinternodescm"] <- "growth"
d$respvar.simple[d$respvar == "leafincrement"] <- "growth"
d$respvar.simple[d$respvar == "mmper14days"] <- "growth"
d$respvar.simple[d$respvar == "lengthoftrusscm"] <- "growth"
d$respvar.simple[d$respvar == "plantheightatflowerbudappearance"] <- "growth"
d$respvar.simple[d$respvar == "totalgrowth"] <- "growth"
d$respvar.simple[d$respvar == "stemelongationcm"] <- "growth"
d$respvar.simple[d$respvar == "shootelongationcm"] <- "growth"
d$respvar.simple[d$respvar == "shootgrowthcm"] <- "growth"
d$respvar.simple[d$respvar == "petiolelength"] <- "growth"
d$respvar.simple[d$respvar == "yieldgrams"] <- "growth"

# phenstage
d$respvar.simple[d$respvar == "growthstage"] <- "phenstage"
d$respvar.simple[d$respvar == "budstage"] <- "phenstage"

# phenstageper
d$respvar.simple[d$respvar == "percentstage01"] <- "phenstageper" # from jones12
d$respvar.simple[d$respvar == "percentstage02"] <- "phenstageper" # from jones12
d$respvar.simple[d$respvar == "percentstage03"] <- "phenstageper" # from jones12
d$respvar.simple[d$respvar == "percentstage04"] <- "phenstageper" # from jones12
d$respvar.simple[d$respvar == "percentstage06"] <- "phenstageper" # from guerriero90

# flowernumber
d$respvar.simple[d$respvar == "flowerperplant"] <- "flowernumber"
d$respvar.simple[d$respvar == "flowers"] <- "flowernumber"
d$respvar.simple[d$respvar == "flowersperinflorescence"] <- "flowernumber"
d$respvar.simple[d$respvar == "flowersperplant"] <- "flowernumber"
d$respvar.simple[d$respvar == "flowerspertruss"] <- "flowernumber"
d$respvar.simple[d$respvar == "inflorescenceperplant"] <- "flowernumber"
d$respvar.simple[d$respvar == "plantsflowering"] <- "flowernumber"
d$respvar.simple[d$respvar == "inflorescences"] <- "flowernumber"
d$respvar.simple[d$respvar == "numberflowerbuds"] <- "flowernumber"

# othernums
d$respvar.simple[d$respvar == "cumulativeleafincrement"] <- "othernums"
d$respvar.simple[d$respvar == "leaves"] <- "othernums"
d$respvar.simple[d$respvar == "fruits"] <- "othernums"
d$respvar.simple[d$respvar == "numberofbuds"] <- "othernums"
d$respvar.simple[d$respvar == "numberofleaves"] <- "othernums"
d$respvar.simple[d$respvar == "numberofnodes"] <- "othernums"
d$respvar.simple[d$respvar == "numberofstolons"] <- "othernums"
d$respvar.simple[d$respvar == "stolonsperplant"] <- "othernums"

# otherpercents
d$respvar.simple[d$respvar == "percentfruiting"] <- "otherpercents"
d$respvar.simple[d$respvar == "percentrooting"] <- "otherpercents"
d$respvar.simple[d$respvar == "percentrunnering"] <- "otherpercents"
d$respvar.simple[d$respvar == "percentbudset"] <- "otherpercents"

# fruitmass
d$respvar.simple[d$respvar == "freshfruitg"] <- "fruitmass"
d$respvar.simple[d$respvar == "fruitmassperplant"] <- "fruitmass"

# notsureabout
d$respvar.simple[d$respvar == "critical.daylength.hrs"] <- "notsureabout" ## Not Woody

# other
d$respvar.simple[d$respvar == "daystobudset"] <- "other"
d$respvar.simple[d$respvar == "weeksofleafproduction"] <- "other"
d$respvar.simple[d$respvar == "survival"] <- "other"



# check your work .... 
checking <- subset(d, is.na(respvar.simple)==TRUE)
unique(checking$respvar) 




# fixing respvar issues where daystobudburst was really DOY to budburst
# see bb_analysis/cleaning/checkresponsetime for more info
# daytobudburstdayofyear
d$respvar[d$respvar == "daystobudburst" & d$datasetID=="gomory15"] <- "dayofyeartobudburst"
d$respvar[d$respvar == "daystobudburst" & d$datasetID=="skre08"] <- "dayofyeartobudburst" 
d$respvar[d$respvar == "daystobudburst" & d$datasetID=="gunderson12"] <- "dayofyeartobudburst"
d$respvar[d$respvar == "budstage" & d$datasetID=="gunderson12"] <- "budstage_dayofyear"
d$respvar[which(d$respvar=="daystoleafout" & d$datasetID=="fu13")] <- "dayofyeartoleafout"
d$respvar[d$respvar == "daystobudburst" & d$datasetID=="skre08"] <- "dayofyeartobudburst"

if(FALSE){
d$respvar[d$respvar == "percentbudburst" & d$datasetID=="Sanz-Perez09"] <- "percentbudburst_dayofyear"
d$respvar[d$respvar == "percentbudburst" & d$datasetID=="sanzperez10"] <- "percentbudburst_dayofyear"
}



#############################
# Which studies have multiple respvar but only one respvar.simple?
d$datasetIDstudy <- paste(d$datasetID, d$study)

studyresp <- with(d, paste(datasetIDstudy, respvar))
studyresps <- with(d, paste(datasetIDstudy, respvar.simple))

# unique(studyresp)
# unique(studyresps)

# which studies have multiple respvars for one respvar.simple?
#this makes a new column with a flag (=="TRUE") for rows that had multiple respvars but only one respvar.simple
#this is dealt with in multresp.R
xx <- tapply(studyresp, studyresps, function(x)
  length(unique(x)) > 1)

multiresp <- names(xx)[xx==T]

# make a flag for this 
d$multiresp <- !is.na(match(studyresps, multiresp))

# which studies have multiple original respvars and each one is a separate respvar.simple?
#the code below creates a column for "multibothresp"
#As of 2 Feb 2017 we decided that the column for "multibothresp"
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

#On June 12, 2017 we decided that we need to add two columns that do the following, slightly different thing:
#multi_respvar: identifies studies that have multiple response variables in the respvar column
#multi_respvar.simple: identifies studies that have multiple response variables in the respvar.simple column
#However, my code identified no studies with multiple response variables of either type within a study...
yy <- tapply(d$respvar, d$datasetIDstudy,function(x)
  length(unique(x)) > 1)

multi_respvar <- names(yy)[yy==T]

yz <- tapply(d$respvar.simple, d$datasetIDstudy, function(x)
  length(unique(x))> 1 )

multi_respvar.simple <- names(yz)[yz==T]

# make a flag for these 
d$multi_respvar <- !is.na(match(d$datasetIDstudy,multi_respvar))
d$multi_respvar.simple <- !is.na(match(d$datasetIDstudy,multi_respvar.simple))


ospree_clean_respvar <- d

rm(checking,ospree_clean_respvar,xx,xz,yy,yz)
stop("Not an error, just stopping here to say we're now done cleaning respvar. The d item in your workspace is now all cleaned up for its respvar. Yay!")

##
#write.csv(d, file = "output/ospree_clean_respvar.csv", row.names=FALSE)
simple.count<-as.data.frame(table(d$respvar.simple))


