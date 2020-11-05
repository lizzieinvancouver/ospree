## No one likes factors
options(stringsAsFactors = FALSE)


##setwd("C:\\Users\\Faith Jones\\Documents\\ubc\\OspreeTraits")

## Load libraries
library(tidyr)
library(dplyr)
library(data.table)

## Read the data (modify path as needed) 
tryData <- fread("~/Downloads/TRYtraitdataNov2019.txt")
#tryData <- fread("TRYtraitdataNov2019.txt")

## Drop some columns to keep things simpler
tryData$V28 <- NULL
tryData$Comment <- NULL
tryData$ObsDataID <- NULL
tryData$Reference <- NULL 

## What traits do we have data on?
traitN <- unique(tryData$TraitName)
traitN
## Let's clean this up a bit
### Drop rows where TraitName is blank (these are location or reference data; these can be merged later)
tryData <- subset(tryData, TraitName != "")
### Skip plant biomass data (only 104 measurements)
tryData <- subset(tryData, TraitName != c("Plant biomass and allometry: Leaves per plant (emergent, mature, senescent)"))
### Lump together all SLA measurements
tryData$TraitName <- gsub(pattern = "Leaf area per leaf dry mass.*uded", replacement = "Specific leaf area", x = tryData$TraitName)
### Lump together all Leaf area measurements
tryData$TraitName <- gsub(pattern = "Leaf area .*)", replacement = "Leaf area", x = tryData$TraitName)
### Rename Stem specific density
tryData$TraitName <- gsub(pattern = "Stem specific density.*)", replacement = "Stem specific density", x = tryData$TraitName)
### Rename Leaf dry mass per leaf
tryData$TraitName <- gsub(pattern = "Leaf dry mass per leaf.*)", replacement = "Leaf dry matter content", x = tryData$TraitName)
### Rename Crown height
tryData$TraitName <- gsub(pattern = "Crown.*)", replacement = "Crown height", x = tryData$TraitName)
### Now how we do look?
traitN <- unique(tryData$TraitName)
traitN # much nicer

## What type of trait values do we have?
traitT <- unique(tryData$ValueKindName)
traitT
### How many measurements for each one?
aggregate(StdValue ~ ValueKindName, data = tryData, FUN = length)
### Keep only "Single"
tryData <- subset(tryData, ValueKindName == c("Single"))
## Keep only data with standardized values (StdValue)
tryData <- tryData[!is.na(tryData$StdValue), ]

## What kind of units are we dealing with?
aggregate(UnitName ~ TraitName, data = tryData, unique) # should only be 1 per TraitName
### Remove "g/m2/d" measurements for Leaf photosynthesis (only 81 measurements)
tryData <- subset(tryData, UnitName != c("g/m2/d"))
aggregate(UnitName ~ TraitName, data = tryData, unique) # much better

## Generate sample plots of key traits (hopefully not much variation)
testspecies <- subset(tryData, SpeciesName == c("Abies alba"))
trait.list <- c("Stem specific density", "Leaf dry matter content", "Crown height", "Specific leaf area")
par(mfrow = c(2, 2), mar = c(5, 5, 2, 1))
for(i in 1:length(trait.list)){
    temp <- subset(testspecies, TraitName == trait.list[i])
    plot(temp$StdValue, main = trait.list[i], xlab = "Observation", ylab = "Trait value")
}
