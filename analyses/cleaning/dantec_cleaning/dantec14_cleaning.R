######## cleaning for dantec14. Mira Garner 22 April, 2020 ########

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/git/ospree/analyses/cleaning/dantec_cleaning/")

#-----------------------------------------------------------------------#
# read in MDG and FJ datasets for cleaning and merge
mdg <- read.csv("cleaning/dantec_cleaning/Dantec14_MG.csv", header = TRUE)
fj <- read.csv("cleaning/dantec_cleaning/Dantec14_fj.csv", header = TRUE)
dan <- rbind(mdg, fj)

# correct datasetID for entry 39 MDG dataset from "dantec15"
dan$datasetID[which(dan$datasetID == "dantec15")] <- "dantec14"

# change sample date of "15-Oct-10" to "unclear 15-Oct-10 to 15-Apr-11"
dan$fieldsample.date[which(dan$datasetID == "dantec14")] <- "unclear Oct-10 to Apr-11"

# add figure numbers for MDG (3b) data and FJ (3a) data.
dan$figure.table..if.applicable.[which(dan$datasetID == "dantec14" & dan$Entered.By == "MDG")] <- "3b"
dan$figure.table..if.applicable.[which(dan$datasetID == "dantec14" & dan$Entered.By == "FJ")] <- "3a"

# for F. sylvatica, MDG switched the first low elevation and high elevation entries for population.altitude.m (high elevation entry labeled as low elevation but at top of high ele)
dan$population.altitude.m[which(dan$Entered.By == "MDG" & dan$chilldays == "1.084")] <- "1604"
dan$population.altitude.m[which(dan$Entered.By == "MDG" & dan$chilldays == "0")] <- "131"

# MG entered chilling in chill.days not field.chill.units. Delete this info from chill.days and move to field.chill.units
dan$field.chill.units[which(dan$Entered.By == "MDG")] <- dan$chilldays[which(dan$Entered.By == "MDG")]
dan$chilldays[which(dan$Entered.By == "MDG")] <- "NA"

# add more info to cu.model and change FJ threshold to 10 deg
dan$cu.model[which(dan$cu.model == "days-5")] <- "chillcalc,10degthreshold,see paper"
dan$cu.model[which(dan$cu.model == "days-10")] <- "chillcalc,10degthreshold,see paper"

# add GDD temperature thresholds to respvar (5 deg for FJ, 10 deg for MDG)
dan$respvar[which(dan$Entered.By == "MDG")] <- "GDDabove10"
dan$respvar[which(dan$Entered.By == "FJ")] <- "GDDabove5"

# remove empty row
dan2 <- dan[which(dan$datasetID != ""),]
head(dan2)

write.csv(dan2, file = "../../input/dantec_merge.csv")
