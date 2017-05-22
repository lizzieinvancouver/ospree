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
# gansert02 - change "" to "ambient"
d<- within(d, photoperiod_day[datasetID == 'gansert02'] <- 'ambient')
# gianfagna85 - doesn't specify, can't assume ambient
# hawerroth13 - "in fitotrons without light.." - changed to 0
d <- within(d, photoperiod_day[datasetID == 'hawerroth13'] <- 0)
# hawkins12 - changed "" to "ambient"
d<- within(d, photoperiod_day[datasetID == 'hawkins12'] <- 'ambient')
# linkosalo06 - changed "" to "ambient"
d<- within(d, photoperiod_day[datasetID == 'linkosalo06'] <- 'ambient')
# manson91 - doesn't specify, can't assume ambient
# morin10 - changed "" to "ambient"
d<- within(d, photoperiod_day[datasetID == 'morin10'] <- 'ambient')
# nishimoto95 - doesn't specify, can't assume ambient
# guerriero90 has missing photoperiod data for some rows, change to 12
d <- within(d, photoperiod_day[datasetID == 'guerriero90'] <- 12)
d <- within(d, photoperiod_night[datasetID == 'guerriero90']<- 12)


## Should we change 'constant' to 24 h?
# cronje03 - fixed to constant - irradiance 215 umol m-2 s2
d<-within(d, photoperiod_day[datasetID=='cronje03']<-'constant')
# devries82 - constant - 8, 16, 24 Wm-2
d<-within(d, photoperiod_day[datasetID=='devries82' & respvar=='plantheightatflowerbudappearance']<-'constant') # Lizzie changed this constant to 'constant' because code was not running

d$photoperiod_day[d$photoperiod_day == "constant"] <- 24
d$photoperiod_night[d$photoperiod_day==24]<-0

## Ambient
# hawkins12 - ambient for exp1

# heide15 - remove duplicate rows and adjust photoperiod_day accordingly!!
d <- d[!(d$datasetID == "heide15" & d$photoperiod_day == "10, 15"), ]
d <- d[!(d$datasetID == "heide15" & d$photoperiod_day == "15, 10"), ]
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 14 & response == 22.2] <- 11)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 21 & response == 22.6] <- 12)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 28 & response == 22.6] <- 13)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 35 & response == 22.6] <- 14)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 42 & response == 22.6] <- 15)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 14 & response == 30.7] <- 14)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 21 & response == 33.3] <- 13)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 28 & response == 33.3] <- 12)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 35 & response == 33.3] <- 11)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'shootgrowthcm'
                               & response.time == 42  & response == 33.3] <- 10)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 14 & response == 4.6] <- 11)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 21 & response == 4.6] <- 12)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 28 & response == 4.6] <- 13)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 35 & response == 4.6] <- 14)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 42  & response == 4.6] <- 15)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 14 & response == 6.2] <- 14)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 21 & response == 6.7] <- 13)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 28 & response == 7.1] <- 12)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 35 & response == 7.1] <- 11)
d <- within(d, photoperiod_day[datasetID == 'heide15' & respvar == 'nodes'
                               & response.time == 42  & response == 7.1] <-10)
#Dan B steps in for a moment to adjust night time in accordance to day time (if he can)
View(subset(d,datasetID=="heide15"))
d<- within(d, genus[datasetID=="calme94" & species=="rubra"]<-"Betula")d <- within(d, photoperiod_night[datasetID == 'heide15' & photoperiod_day== 11])<-13
d <- within(d, photoperiod_night[datasetID == 'heide15' & photoperiod_day== 12]<-12)
d <- within(d, photoperiod_night[datasetID == 'heide15' & photoperiod_day== 13]<-11)
d <- within(d, photoperiod_night[datasetID == 'heide15' & photoperiod_day== 10]<-14)
d <- within(d, photoperiod_night[datasetID == 'heide15' & photoperiod_day== 14]<-10)
d <- within(d, photoperiod_night[datasetID == 'heide15' & photoperiod_day== 11]<-13)
d <- within(d, photoperiod_night[datasetID == 'heide15' & photoperiod_day== 15]<-9)
##back to Cat's code:

# thielges75 - fix one row
d <- within(d, photoperiod_day[datasetID == 'thielges75' & photoperiod_day == 
                                 'half in 16, half ambient(9.5-10.5)'] <- 16)


# Remove extra lines for ghelardini10 added by Dan that do not correlate to paper
d[-which(d$datasetID=='ghelardini10' & d$Entered.By == 'DF'), ]

stop("Not an error, just stopping here to say we're now done cleaning photo. The d item in your workspace is now all cleaned up for its photoperiod_day and photoperiod_night columns. Zippitydoodah ...")

# Make new data sheet
# write.csv(d, file = "ospree_clean_photo.csv", row.names=FALSE)

