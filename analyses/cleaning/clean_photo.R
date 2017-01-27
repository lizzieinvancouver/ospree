## Started 3 January 2017 - Cat ##
## An R script to clean the "photoperiod_day" and "photoperiod_night" columns of the bud burst data

## Updates by Lizzie on 27 Jan 2017 ##
## As of 27 Jan 2017 this file is now SOURCED from cleanmerge_all.R ##
## So to run this you need to start there ##

# See cleanmerge_all.R for started text #

# Fixing by datasetID -- basler12
d$photoperiod_day[d$photoperiod_day == "shortday"] <- 9.5
d$photoperiod_day[d$photoperiod_day == "longday"] <- 11
d$photoperiod_night[d$photoperiod_night == "shortday"] <- 14.5
d$photoperiod_night[d$photoperiod_night == "longday"] <- 13

## Blanks
# cannell83 - does not specify for Figure 3, can assume ambient
# chavarria09 - doesn't specify, can't assume ambient
# falusi96 - doesn't specify for exp2 but does specify for exp3 - SD is 9, LD is 13
# gianfagna85 - doesn't specify, can't assume ambient
# hawerroth13 - "in fitotrons without light.." - changed to 0
d <- within(d, photoperiod_day[datasetID== 'hawerroth13'] <- 0)
# manson91 - doesn't specify, can't assume ambient
# nishimoto95 - doesn't specify, can't assume ambient

## Should we change 'constant' to 24 h?
# cronje03 - fixed to constant - irradiance 215 umol m-2 s2
d<-within(d, photoperiod_day[datasetID=='cronje03']<-'constant')
# devries82 - constant - 8, 16, 24 Wm-2
d<-within(d, photoperiod_day[datasetID=='devries82' & respvar=='plantheightatflowerbudappearance']<-'constant') # Lizzie changed this constant to 'constant' because the code was not running, need Cat to check

d$photoperiod_day[d$photoperiod_day == "constant"] <- 24
d$photoperiod_night[d$photoperiod_day==24]<-0

## Ambient
# gansert02
# hawkins12 - ambient for exp1

stop("Not an error, just stopping here to say we're now done cleaning photo. The d item in your workspace is now all cleaned up for its photoperiod_day and photoperiod_night columns. Zippitydoodah ...")

# Make new data sheet
# write.csv(d, file = "ospree_clean_photo.csv", row.names=FALSE)
