# Started 18 January 2017 - Cat
## Removing duplicate rows and errors

## An R script to clean the "forcetemp" column of the bud burst data
## Updated 31 Jan 2017 for more cleaning! ##

## As of 31 Jan 2017 this file is now SOURCED from cleanmerge_all.R ##
## So to run this you need to start there ##

# See cleanmerge_all.R for started text #

# Using work from Liz Stebbins


# charrier11 - change ambient to 25 for both forcetemp and forcetemp_night
d <- within(d, forcetemp[datasetID== 'charrier11' & forcetemp == 'ambient'] <- 25)
# cronje03 - change blank to 25
d <- within(d, forcetemp[datasetID== 'cronje03' & forcetemp == ''] <- 25)
# falusi96 - change exp2 from blank to 16
d <- within(d, forcetemp[datasetID== 'falusi96' & study == 'exp2'] <- 16)
d <- within(d, forcetemp[datasetID== 'falusi96' & study == 'exp3'] <- 24)
# gomory15 - change from ambient to 4.9 (Svarin - 49 deg lat) or 9.2 (Hladomer - 48 deg lat)
d <- within(d, forcetemp[datasetID== 'gomory15' & growing.lat == 48.448201] <- 9.2)
d <- within(d, forcetemp[datasetID== 'gomory15' & growing.lat == 49.017914] <- 4.9)
# guerriero90 - changed blanks to 23
d <- within(d, forcetemp[datasetID== 'guerriero90' & forcetemp == ''] <- 23)
# schnabel87 - change ambient to 24
d <- within(d, forcetemp[datasetID== 'schnabel87' & forcetemp == 'ambient'] <- 24)
# yazdaniha64 - change ambient to ambient to 17
d <- within(d, forcetemp[datasetID== 'yazdaniha64' & forcetemp == 'ambient'] <- 17)

stop("Not an error, just stopping here to say we're now done cleaning forcetemp. The d item in your workspace is now all cleaned up for its forcetemp. Yay!")

write.csv(d, "~/Documents/git/ospree/analyses/output/ospree_clean_forcetemp.csv", row.names = FALSE)
