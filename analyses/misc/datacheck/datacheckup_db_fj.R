## Started 24 March 2020 ##
## Let's review the new data! ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)
library(RColorBrewer)
library(ggplot2)

# Setting working directory. Add in your own path in an if statement for your file structure

if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")



# Grab the data, all of it
dall <- read.csv("..//output/ospree_clean.csv", header=TRUE)
dbb <- read.csv("..//output/ospree_clean_withchill_BB.csv", header=TRUE)

# Get your subset ... 
sort(unique(dbb$datasetID))

medata <- c("malyshev18", "flynn18","ramos17")

mall <- dall[which(dall$datasetID %in% medata),]
mdbb <- dbb[which(dbb$datasetID %in% medata),]
unique(mall$datasetID)
unique(mdbb$datasetID) ### ramos17 dissapears in cleaing note: the paper is ramos 18 on google drive

# check if you lose data between all and bb (if you do, find out what is lost!)
dim(mall) #1332
dim(mdbb) #1006
table(mall$datasetID)
1006+326 ####see row differences between the two dataframes result from the loss of ramos.

#########################################################################################
############# task 1 Find out why ramos17 goes away.##################################
#1. Run chillmergeall.R after every step in cleaning type table(d$datasetID=="ramos17")
#hmmmm it still appear to be in the dataset at the end of chillmergeall.R
#2. run bb_cleanmergall.R
## We lose half of ramos17 (163 rows in cleaning/multirespr) (perceptbudburst reproductive)
### we lose the rest in clean_bbperctodays.R. We checked this before, and Lizzie was going to follow up
#see double check clean_bbperctodays.R  issue #310


## bb stuff... this code poached from:
# models_stan.R, models_stan_plotting.R, bbstanleadin.R, and bbdataplease.R 
## just the bb data ...
d <- mdbb
respvars.thatwewant <- c("daystobudburst", "percentbudburst")

table(d$respvar.simple)
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),] ### removes 110 rows of flowering
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)

# values before check making everythig numberic
unique(bb.resp$forcetemp)
bb.resp$forceday <- as.numeric(bb.resp$forcetemp)
unique(bb.resp$forceday)

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
####do to thermoperiodicity, this calculation makes it so there are 4 forcing treamtnets instead of 2 for

bb.resp$chill <- as.numeric(bb.resp$Total_Utah_Model) 
bb.resp$chill.hrs <- as.numeric(bb.resp$Total_Chilling_Hours)
bb.resp$chill.ports <- as.numeric(bb.resp$Total_Chill_portions) 

bb.resp$resp <- as.numeric(bb.resp$response.time)

bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(chill)==FALSE & is.na(resp)==FALSE)
dim(bb.noNA) ### nothing is lost by making things numeric. Yay!


# Fix species... so it plays well with rstan
bb.noNA$latbi <- paste(bb.noNA$genus, bb.noNA$species)
bb.noNA$complex <- as.numeric(as.factor(bb.noNA$latbi))


# Start looking at the data ...
ggplot(bb.noNA, aes(chill, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
ggplot(bb.noNA, aes(force, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
ggplot(bb.noNA, aes(photo, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
# Compare it to  the paper ....
comppaper <- subset(bb.noNA, select=c("datasetID", "latbi", "chill", "force",
    "photo", "resp", "figure.table..if.applicable."))
comppaperfig <- subset(bb.noNA, select=c("datasetID", "latbi", "figure.table..if.applicable."))
comppaperfig[!duplicated(comppaperfig), ]

# Hmm, chilling units seem to vary in malyshev18 figure 2 agreed

### st up z.score
bb.noNA$chill.z<-(bb.noNA$chill-mean(bb.noNA$chill))/sd(bb.noNA$chill)
bb.noNA$force.z<-(bb.noNA$force-mean(bb.noNA$force))/sd(bb.noNA$force)
bb.noNA$photo.z<-(bb.noNA$photo-mean(bb.noNA$photo))/sd(bb.noNA$photo)
# rstan
datalist.bb <- with(bb.noNA, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.noNA),
                         n_sp = length(unique(bb.noNA$complex))
                    ))

modhere = stan('stan/datacheckup_db_fj_intonly.stan', data = datalist.bb,
    iter = 4000, warmup=3000,control=list(adapt_delta=.99))



library(shinystan)
launch_shinystan(modhere) # please go to: Explore -> Click on 'bivariate' on right and make sure y-axis is log-posterior
##signa force is the worst, but so is sigma chill, photo not great either
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
