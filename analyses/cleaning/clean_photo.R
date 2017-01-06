## Started 3 January 2017 - Cat ##
## An R script to clean the "photoperiod_day" and "photoperiod_night" columns of the bud burst data

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/git/ospree/analyses/input")

# Name data frame:
d <- read.csv("ospree_clean_respvar.csv")

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
# manson91 - doesn't specify, can't assume ambient
# nishimoto95 - doesn't specify, can't assume ambient

## Should we change 'constant' to 24 h?
# cronje03 - fixed to constant - irradiance 215 umol m-2 s2
# devries82 - constant - 8, 16, 24 Wm-2

d$photoperiod_day[d$photoperiod_day == "constant"] <- 24

## Ambient
# gansert02
# hawkins12 - ambient for exp1

# Make new data sheet
write.csv(d, file = "ospree_clean_photo.csv", row.names=FALSE)
