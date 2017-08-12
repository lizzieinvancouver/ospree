## Started 24 July 2017 ##
## By Lizzie ##

## This file preps the prepped PMP data ##
## That is, it formats it in the correct order and makes things numeric ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
    (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
setwd("~/Documents/git/ospree/analyses")

## get data
clim1 <- read.delim("output/pmp/percbb_clim_pmpA.txt")
clim2 <- read.delim("output/pmp/percbb_clim_pmpB.txt")
phen <- read.delim("output/pmp/percbb_bb_pmp.txt")

## for now, try just one of the clim files (do the smaller one to start)
phenB <- phen[which(phen$stn %in% unique(clim2$stn)),]

## make fit file, should be each unique stn, population, year
## we also need to make the stn and provenance numeric ...
## we could check this works correctly someday if needed (it relies on the stn and provenances to be consistent across files)
fitfile <-subset(phenB, select=c("stn", "species", "year"))
fitfileB <- fitfile[!duplicated(fitfile),]
fitfileB$Station <- fitfileB$stn
fitfileB$Provenance <- fitfileB$species
fitfileB$Station <- as.numeric(as.factor(fitfileB$Station))
fitfileB$Provenance <- as.numeric(as.factor(fitfileB$Provenance))
fitfileB.out <- subset(fitfileB, select=c("Station", "Provenance", "year"))

## format the phen file
phenB$Station <- phenB$stn
phenB$Provenance <- phenB$species
phenB$Station <- as.numeric(as.factor(phenB$Station))
phenB$Provenance <- as.numeric(as.factor(phenB$Provenance))
phenB.out <- subset(phenB, select=c("Station", "Provenance", "year", "doy"))

## format the clim data file
clim2$Station <- clim2$stn
clim2$Station <- as.numeric(as.factor(clim2$Station))
climB.out <- subset(clim2, select=c("Station", "latitude", "year", "doy2", "Tmin", "Tmax", "Tmean"))

# Need files written out as Windows-formatted text files.
# Below doesn't work, so I just adjust the .csv files in Excel (I know, ugh!)
# write.table(fitfileB.out, "output/pmp/pmp24Jul2017/phenB_fit.txt", sep="/t", row.names=FALSE)
# write.table(phenB.out, "output/pmp/pmp24Jul2017/phenB.txt", sep="/t", row.names=FALSE)
# write.table(climB.out, "output/pmp/pmp24Jul2017/phenB_clim.txt", sep="/t", row.names=FALSE)

write.csv(fitfileB.out, "output/pmp/bbdata/phenB_fit.csv", row.names=FALSE)
write.csv(phenB.out, "output/pmp/bbdata/phenB.csv", row.names=FALSE)
write.csv(climB.out, "output/pmp/bbdata/phenB_clim.csv", row.names=FALSE)
