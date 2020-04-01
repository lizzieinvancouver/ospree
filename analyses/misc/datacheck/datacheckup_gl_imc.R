## Started 24 March 2020 ##
## Let's review the new data! ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)
library(RColorBrewer)
library(ggplot2)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("CatherineChamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/")


# Grab the data, all of it
dall <- read.csv("output/ospree_clean.csv", header=TRUE)
dbb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

# Get your subset ... 
sort(unique(dbb$datasetID))
medata <- c("anzanello16", "fu19", "prevey18")

mall <- dall[which(dall$datasetID %in% medata),]
mdbb <- dbb[which(dbb$datasetID %in% medata),]

# check if you lose data between all and bb (if you do, find out what is lost!)
dim(mall)
dim(mdbb) ## 62 lines are lost


str(mall)
str(mdbb)

#why (and which) lines are lost 
stu_exp_sps = paste(mall$datasetID,mall$study,mall$species,mall$figure.table..if.applicable.)
stu_exp_sps_dbb = paste(mdbb$datasetID,mdbb$study,mdbb$species,mdbb$figure.table..if.applicable.)

table(stu_exp_sps)
table(stu_exp_sps_dbb)

# 60 lines of percentbudburst are removed from Anzanello16 in mdbb
# 2 lines are removed from Fu19 


## bb stuff... this code poached from:
# models_stan.R, models_stan_plotting.R, bbstanleadin.R, and bbdataplease.R 
## just the bb data ...
d <- mdbb
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),]
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)
bb.resp$forceday <- as.numeric(bb.resp$forcetemp)
bb.resp$forcenight <- as.numeric(bb.resp$forcetemp_night)
bb.resp$photonight <- as.numeric(bb.resp$photoperiod_night)

# Correct forcing for day/night differences
bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$force <- bb.resp$forceday
bb.resp$force[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] <-
    (bb.resp$forceday[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photo[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] +
    bb.resp$forcenight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photonight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE])/24

bb.resp$chill <- as.numeric(bb.resp$Total_Utah_Model) 
bb.resp$chill.hrs <- as.numeric(bb.resp$Total_Chilling_Hours)
bb.resp$chill.ports <- as.numeric(bb.resp$Total_Chill_portions) 

bb.resp$resp <- as.numeric(bb.resp$response.time)

bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(chill)==FALSE & is.na(resp)==FALSE)
dim(bb.noNA) ### 0 rows of data
unique(bb.noNA$datasetID) ## all rows are removed because all chill=NA


########################################################################################
###################### Let's check on some things individually #########################
########################################################################################

### Before moving on, let's check out why we are losing 1) richardson18 and 2) man17 
# 1) fu19
fudf <- mdbb[(mdbb$datasetID=="fu19"),]
dim(fudf) ## 26 rows of data
unique(fudf$respvar.simple) # daystobudburst, so that checks out... 
fudf.simple <- subset(fudf, select=c("genus", "species", "provenance.lat", "forcetemp", 
                                         "photoperiod_day", "Total_Utah_Model", "response.time"))

head(fudf.simple)
#         genus       species provenance.lat forcetemp photoperiod_day Total_Utah_Model response.time
#2534 Aesculus  hippocastanum          50.78      <NA>              13               NA            89
#2535 Aesculus  hippocastanum          50.78      <NA>              10               NA            81
#2536 Aesculus  hippocastanum          50.78      <NA>            <NA>               NA            NA
#2537     Fagus     sylvatica          50.78      <NA>              13               NA           125
#2538     Fagus     sylvatica          50.78      <NA>              13               NA           127
#2539     Fagus     sylvatica          50.78      <NA>              13               NA           123

## For fu19 we are missing both forcing and chilling. 

## We should have both chilling and forcing information... 
## It seems like temperature of the treatments is explicited in the paper (Fig. 1) 
## This information has not been input in ospree yet


# 2) anzanello16
anzanellodf <- mdbb[(mdbb$datasetID=="anzanello16"),] ## 60 observations
dim(anzanellodf) 
unique(anzanellodf$respvar.simple) # daystobudburst

## The paper is missing percentbudburst observations from dall to dbb
## this is half the observations in the paper


## no rows are removed from Prevey18



########################################################################################
########################################################################################

# Fix species... so it plays well with rstan
bb.noNA$latbi <- paste(bb.noNA$genus, bb.noNA$species)
bb.noNA$complex <- as.numeric(as.factor(bb.noNA$latbi))


# Start looking at the data ...
ggplot(bb.noNA, aes(chill, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) ## multiple chill but have one abberrant chiling! Probably from exp1, I am assuming it is a misentry
ggplot(bb.noNA, aes(force, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) # only one forcing
ggplot(bb.noNA, aes(photo, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) # only one photoperiod
# Compare it to  the paper ....
comppaper <- subset(bb.noNA, select=c("datasetID", "latbi", "chill", "force",
    "photo", "resp", "figure.table..if.applicable."))
comppaperfig <- subset(bb.noNA, select=c("datasetID", "latbi", "figure.table..if.applicable."))
comppaperfig[!duplicated(comppaperfig), ]

if(FALSE){
# rstan
datalist.bb <- with(bb.noNA, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.noNA),
                         n_sp = length(unique(bb.noNA$complex))
                    ))

modhere = stan('bb_analysis/stan/nointer_2level.stan', data = datalist.bb,
    iter = 4000, warmup=3000)

library(shinystan)
launch_shinystan(modhere) # please go to: Explore -> Click on 'bivariate' on right and make sure y-axis is log-posterior
}
# At this point I would try a different model formulation! Which ones would you try?

# Some plotting (if you have a good model!)
bb.stan <- bb.noNA
post.mohere <- extract(modhere)
sumer.here <- summary(modhere)$summary
whichmodel <- sumer.here
spp <- sort(unique(bb.noNA$complex))
n <- length(spp)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colv = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

par(mfrow=c(1,3))
plot(resp~chill, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~chill, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
    whichmodel[grep("mu_b_chill_sp", rownames(whichmodel)),1], col="black", lwd=3)

plot(resp~force, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~force, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_force", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
    whichmodel[grep("mu_b_force_sp", rownames(whichmodel)),1], col="black", lwd=3)
plot(resp~photo, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~photo, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_photo", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col=colv[sp])
    }
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
           whichmodel[grep("mu_b_photo_sp", rownames(whichmodel)),1], col="black", lwd=3)

##### plotting raw data
# Grab the data, all of it
dall <- read.csv("output/ospree_clean.csv", header=TRUE)
dbb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

# Get your subset ... 
sort(unique(dbb$datasetID))
medata <- c("richardson18", "man17", "nanninga17")

mall <- dall[which(dall$datasetID %in% medata),]
mdbb <- dbb[which(dbb$datasetID %in% medata),]

# check if you lose data between all and bb (if you do, find out what is lost!)
dim(mall)
dim(mdbb) 

str(mall)
str(mdbb)

# Fix species... so it plays well with rstan
mdbb$latbi <- paste(mdbb$genus, mdbb$species)
mdbb$complex <- as.numeric(as.factor(mdbb$latbi))

## make a bunch of things numeric (eek!)
mdbb$forceday <- as.numeric(mdbb$forcetemp)
mdbb$forcenight <- as.numeric(mdbb$forcetemp_night)
mdbb$photonight <- as.numeric(mdbb$photoperiod_night)

# Correct forcing for day/night differences
mdbb$photo <- as.numeric(mdbb$photoperiod_day)
mdbb$force <- mdbb$forceday
mdbb$force[is.na(mdbb$forcenight)==FALSE & is.na(mdbb$photo)==FALSE &
                is.na(mdbb$photonight)==FALSE] <-
  (mdbb$forceday[is.na(mdbb$forcenight)==FALSE & is.na(mdbb$photo)==FALSE &
                      is.na(mdbb$photonight)==FALSE]*
     mdbb$photo[is.na(mdbb$forcenight)==FALSE & is.na(mdbb$photo)==FALSE &
                     is.na(mdbb$photonight)==FALSE] +
     mdbb$forcenight[is.na(mdbb$forcenight)==FALSE & is.na(mdbb$photo)==FALSE &
                          is.na(mdbb$photonight)==FALSE]*
     mdbb$photonight[is.na(mdbb$forcenight)==FALSE & is.na(mdbb$photo)==FALSE &
                          is.na(mdbb$photonight)==FALSE])/24

mdbb$chill <- as.numeric(mdbb$Total_Utah_Model) 
mdbb$chill.hrs <- as.numeric(mdbb$Total_Chilling_Hours)
mdbb$chill.ports <- as.numeric(mdbb$Total_Chill_portions) 

mdbb$resp <- as.numeric(mdbb$response.time)


path <- unique(mdbb$datasetID)


mdbb2.0<- subset(mdbb, datasetID == path[3])

ggplot(mdbb, aes(chill, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) ## multiple chill but have one abberrant chiling! Probably from exp1, I am assuming it is a misentry
ggplot(mdbb, aes(force, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) # only one forcing
ggplot(mdbb, aes(photo, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
  
