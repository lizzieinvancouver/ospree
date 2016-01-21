#Started Thursday January 21 by Jehane
#An R script to clean the "respvar" column of the bud burst data (currently growthchambers_litreview_2016-01-15.csv, with most updated version in github>tsavas>budreview

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#JS set working directory: 
setwd("/Users/jsamaha/Documents/git/budreview")

#name data frame:
scrapedata <- read.csv ("growthchambers_litreview_2016-01-15.csv")

names(table(scrapedata$respvar))

#Fixing obvious typos and synonmyms:
scrapedata$respvar[scrapedata$respvar == "days to budbreak (on  50% of plants)"] <- "daysto50%budburst"
scrapedata$respvar[scrapedata$respvar == "daysto50perbudburst"] <- "daysto50%budburst"
scrapedata$respvar[scrapedata$respvar == "daystodudburst"] <- "daystobudburst"
scrapedata$respvar[scrapedata$respvar == "daystobudbust"] <- "daystobudburst"
scrapedata$respvar[scrapedata$respvar == "daystobudset"] <- "daystobudburst"

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

scrapedata$respvar[scrapedata$respvar == ""] <- ""
scrapedata$respvar[scrapedata$respvar == ""] <- ""
scrapedata$respvar[scrapedata$respvar == ""] <- ""
