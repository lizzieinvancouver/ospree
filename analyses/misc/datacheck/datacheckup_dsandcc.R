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
medata <- c("richardson18", "man17", "nanninga17")

mall <- dall[which(dall$datasetID %in% medata),]
mdbb <- dbb[which(dbb$datasetID %in% medata),]

# check if you lose data between all and bb (if you do, find out what is lost!)
dim(mall)
dim(mdbb) 

str(mall)
str(mdbb)

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
dim(bb.noNA) ### super tiny... only 49 rows of data
unique(bb.noNA$datasetID) ## just includes nanninga17

dim(d[(d$datasetID=="nanninga17"),]) ### so we are removing 11 rows from nanninga17, plus man17 and richardson17

########################################################################################
###################### Let's check on some things individually #########################
########################################################################################

### Before moving on, let's check out why we are losing 1) richardson18 and 2) man17 
# 1) Richardson18
richdf <- mdbb[(mdbb$datasetID=="richardson18"),]
dim(richdf) ## 17 rows of data
unique(richdf$respvar.simple) # daystobudburst, so that checks out... 
richdf.simple <- subset(richdf, select=c("genus", "species", "provenance.lat", "forcetemp", 
                                         "photoperiod_day", "Total_Utah_Model", "response.time"))

head(richdf.simple)
#     genus   species provenance.lat forcetemp photoperiod_day Total_Utah_Model response.time
#6527 Larix laricinia       47.50285      <NA>              14               NA       113.239
#6528 Larix laricinia       47.50285      <NA>              14               NA       106.555
#6529 Picea   mariana       47.50285      <NA>              11               NA        64.615
#6530 Picea   mariana       47.50285      <NA>              11               NA        67.692
#6531 Larix laricinia       47.50285      <NA>              14               NA       103.985
#6532 Picea   mariana       47.50285      <NA>              12               NA        68.205

## Okay so for richardson18 we are missing both forcing and chilling. 
# Let's go back through the paper to make sure that is true.

# 2) man17
mandf <- mdbb[(mdbb$datasetID=="man17"),] ## 112 observations
dim(mandf) 
unique(mandf$respvar.simple) # othernums - so that's why! 

## Let's double check the paper to make sure the response variable is correct!

#### Alright now let's see why we are losing those 11 rows of data from nanninga17
nanndf <- mdbb[(mdbb$datasetID=="nanninga17"),] ## 112 observations
dim(nanndf)  ## 60 rows of data
unique(nanndf$respvar.simple) ## okay daystobudburst so that checks out...
colnames(nanndf)
nanndf.simple <- subset(nanndf, select=c("study", "genus", "species", "woody", "provenance.lat", "forcetemp", 
                                         "photoperiod_day", "Total_Utah_Model", "response.time"))

head(nanndf.simple)
# Looks like we have some NAs in the Total_Utah_Model
nanndf.exp1 <- nanndf.simple[(nanndf.simple$study=="exp1"),]
## Okay, so there are 12 observations and all have no chilling reported except for one observation
# CHECK THIS OUT! Exp1 needs to be revisited for nanninga17

nanndf.exp2 <- nanndf.simple[(nanndf.simple$study=="exp2"),] ## This checks out! We can continue on with this study below.

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
