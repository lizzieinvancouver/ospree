######## cleaning for dantec14. Mira Garner 22 April, 2020 ########

rm(list = ls())
options(stringsAsFactors = FALSE)
setwd("~/Documents/git/ospree/analyses/")

#subset data already in ospree from DF
d <- read.csv("output/ospree_clean.csv", header=TRUE)

df <- d[which(d$datasetID == "dantec14"), ]
head(df)

# read in MDG and FJ datasets
mdg <- read.csv("cleaning/dantec_cleaning/Dantec14_MG.csv", header = TRUE)
fj <- read.csv("cleaning/dantec_cleaning/Dantec14_fj.csv", header = TRUE)

# merge datasets
dan <- rbind(mdg, fj)

### dan has more columns than new data - why???

# correct datasetID for entry 39 MDG dataset from "dantec15"
dan$datasetID[which(dan$datasetID == "dantec15")] <- "dantec14"

# change sample date of "15-Oct-10" to "unclear 15-Oct-10 to 15-Apr-11"
dan$fieldsample.date[which(dan$datasetID == "dantec14")] <- "unclear Oct-10 to Apr-11"
df$fieldsample.date[which(df$datasetID == "dantec14")] <- "unclear Oct-10 to Apr-11"

# add figure numbers for DF data (3c), MDG (3b) data and FJ (3a) data.
df$figure.table..if.applicable.[which(df$datasetID == "dantec14" & df$Entered.By == "DF")] <- "3c"
dan$figure.table..if.applicable.[which(dan$datasetID == "dantec14" & dan$Entered.By == "MDG")] <- "3b"
dan$figure.table..if.applicable.[which(dan$datasetID == "dantec14" & dan$Entered.By == "FJ")] <- "3a"

# for F. sylvatica, MDG switched the first low elevation and high elevation entries for population.altitude.m (high elevation entry labeled as low elevation but at top of high ele)
dan$population.altitude.m[which(dan$Entered.By == "MDG" & dan$chilldays == "1.084")] <- "1604"
dan$population.altitude.m[which(dan$Entered.By == "MDG" & dan$chilldays == "0")] <- "131"

# FJ entered chill.days in field.chill.units. Move this info to chill.days and delete from field.chill.units
dan$chilldays[which(dan$Entered.By == "FJ")] <- dan$field.chill.units[which(dan$Entered.By == "FJ")]
dan$field.chill.units[which(dan$Entered.By == "FJ")] <- "NA" ## IS IT OKAY TO HAVE NAs

# add more info to cu.model
dan$cu.model[which(dan$cu.model == "day-5")] <- "chillcalc,5degthreshold,see paper"
dan$cu.model[which(dan$cu.model == "day-10")] <- "chillcalc,10degthreshold,see paper"

# same for DF data
df$cu.model <- "chillcalc,10degthreshold,see paper"

# remove empty row
dan2 <- dan[which(dan$datasetID != ""),]

# merge them together?? MG and FJ data has less columns - not sure where the other columns are coming from in ospree
