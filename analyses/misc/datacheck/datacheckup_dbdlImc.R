## Started 24 March 2020 ##
## DL edits March 27, 2020
## Let's review the new data! ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)
library(RColorBrewer)
library(ggplot2)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("deirdreloughnan", getwd()))>0) { 
  setwd("~/Documents/github/ospree/analyses/")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/")


# Grab the data, all of it
dall <- read.csv("output/ospree_clean.csv", header=TRUE)
dbb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)#fundamental file used in bb analysis

# Get your subset ... 
sort(unique(dbb$datasetID))
medata <- c("richardson18", "fu18","malyshev18","vitra17") #our papers

mall <- dall[which(dall$datasetID %in% medata),]
mdbb <- dbb[which(dbb$datasetID %in% medata),]

# check if you lose data between all and bb (if you do, find out what is lost!)
dim(mall) #229, 61
dim(mdbb) #67, 87
#me data bb - subsets down to bb data only; goes through additional cleaning to remove treatments that were double entered

#may find data size decreases if things get deleted, report what gets deleted so know only good stuff gets deleted

## bb stuff... this code poached from:
# models_stan.R, models_stan_plotting.R, bbstanleadin.R, and bbdataplease.R 
## just the bb data ...
d <- mdbb # renaming the subset
respvars.thatwewant <- c("daystobudburst", "percentbudburst") #subset down to the two variables using
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),] #double checking dont have thermal time
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)
bb.resp$forceday <- as.numeric(bb.resp$forcetemp)
bb.resp$forcenight <- as.numeric(bb.resp$forcetemp_night)
bb.resp$photonight <- as.numeric(bb.resp$photoperiod_night)

# Correct forcing for day/night differences #correcting for fact that sometimes temperature during the day is different from the night 
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

bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE 
                 # & is.na(chill)==FALSE 
                  & is.na(resp)==FALSE)
#issue is with the chill factor

dim(bb.noNA) #without chilling have 18 and 96 columns

#extract chilling from the field and calc utah chilling etc and delete all the NA in the critical models, loosing data, check what gets lost
# Fix species... so it plays well with rstan
bb.noNA$latbi <- paste(bb.noNA$genus, bb.noNA$species) #paste together to get a latin binomial
bb.noNA$complex <- as.numeric(as.factor(bb.noNA$latbi)) #rstan arm can use character values, stan only uses numeric


# Start looking at the data ...
ggplot(bb.noNA, aes(chill, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) #plots chilling by study id, should have plot for each study, now think about response values and one chilling value, look for odd datapoints
ggplot(bb.noNA, aes(force, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) #repeat for other factors
ggplot(bb.noNA, aes(photo, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
#Think about how many levels, if only one value of photoperiod, need model that doesn't run photoperiod. possible wont have enough sp or a lot of variation in c f p. 
## Compare it to  the paper ....makes 2 smaller datasets, go back to figures
comppaper <- subset(bb.noNA, select=c("datasetID", "latbi", "chill", "force",
    "photo", "resp", "figure.table..if.applicable."))
comppaperfig <- subset(bb.noNA, select=c("datasetID", "latbi", "figure.table..if.applicable."))
comppaperfig[!duplicated(comppaperfig), ] #quick way to look at unique sp by figure
# are these the only two figures or is there other data that we should have extracted? 
# Hmm, chilling units seem to vary in malyshev18 figure 2, they are plotting phenological data as a function of chilling, but said there was only one level of chilling. How are they plotting it is we didnt see that they are varying chilling; unless can make sense of it in the paper, would need to remove the data --> create a new data issue, with problem, our interpretation, if things simply not sure of just keep track of them

# rstan : using un zcored, usually center and standardize c f p, but better to look at raw data bc checking 
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

#get 400 divergent transitions and initial warnings about tree depth and chains; suggests it will look BAD!
library(shinystan)
launch_shinystan(modhere) # please go to: Explore -> Click on 'bivariate' on right and make sure y-axis is log-posterior

#Start by looking at main page, see really bad chains, varying wildly and vary a lot for bb date. bivariate: mu_ looks like a gunshot, fine, sigma looks BAD, skewed to one side, bunched at low values, may see concavity; suggests models have more moderate problems, getting bunched up and divergingif parameter rich model you would rather do a pairs plot, good if dont have too many parameters, shape is often diagnosic enough 
#fact that sigmas are failing you could start with a noncentered parameterization, noncenter the places you have problems, center the data, replot what asking the model to do, may just be asking too much of the model bc just 2 levels, don't have a lot of photoperiod inferrence. could drop it from the model
# At this point I would try a different model formulation! Which ones would you try?

# Some plotting (if you have a good model!)
bb.stan <- bb.noNA
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

