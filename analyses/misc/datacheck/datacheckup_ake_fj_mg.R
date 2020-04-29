## Started 24 March 2020 ##
## Let's review the new data! ##

#Faith and Mira adding to Aileen's code, so we chan check the other three datasest
#richardson18, fu18, prevey18

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)
library(RColorBrewer)
library(ggplot2)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("deirdreloughnan", getwd()))>0) { 
  setwd("~/Documents/github/ospree/analyses/") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else if (length(grep("faith", getwd()))>0) {
  setwd("~/Documents/github/ospree/analyses/")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


# Grab the data, all of it
dall <- read.csv("output/ospree_clean.csv", header=TRUE)
dbb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
dchill<-read.csv("output/ospree_clean_withchill.csv",header=TRUE)
# Get your subset ... 
sort(unique(dbb$datasetID))
medata <- c("prevey18","fu18","richardson18")
mall <- dall[which(dall$datasetID %in% medata),]
mdbb <- dbb[which(dbb$datasetID %in% medata),]
mchill<-dchill[which(dchill$datasetID%in% medata),]
# check if you lose data between all and bb (if you do, find out what is lost!)
#losing 1 row of fu18 and 1 of richardson18. Not sure why 
names(mall)
names(mdbb)

dim(mall) #59, 60
dim(mdbb) #56, 89
head(mall)

table(mall$datasetID)
table(mdbb$datasetID)#losing 2 from fu18 and 1 from richardson

#how do I track down which rows I am missing? what is the unique ID row for mall that connects it to mdbb??
mdbb$ID_fieldsample.date2
names(mdbb) %in% names(mall)

## bb stuff... this code poached from:
# models_stan.R, models_stan_plotting.R, bbstanleadin.R, and bbdataplease.R 
## just the bb data ...

d <- mdbb
tail(d)
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),]
d$respvar.simple #remove days to flower and other numbers 
bb.resp <- subset(bb.resp, respvar != "thermaltime") 
tail(bb.resp)
nrow(bb.resp)
str(bb.resp)
table(bb.resp$datasetID)#12 fu, 18 richardson, no prevey18. why?
mdbb[mdbb$datasetID == "prevey18",]#there is forcing data, but not for the respvars selected below. Only days to flower


## make a bunch of things numeric (eek!)
#Question: why are there so few resonse variables???
bb.resp$forceday <- as.numeric(bb.resp$forcetemp) #NAs in the datasets we have to look at?
#is this possible, or a mistake in my code? WHy are there no values for forcetemp?

#convert ambinet night temps using code from dsccgl.R
### And now we compare to the cleaned code...
## make a bunch of things numeric - and change ambient to 100 just to see

#bb.resp$forcenight <- as.numeric(bb.resp$forcetemp_night) # "ambient + 1", so cannot be converted to a number?
bb.resp$forcenight.conv <- bb.resp$forcetemp_night
bb.resp$forcenight.conv <- ifelse(bb.resp$forcetemp_night=="ambient", 100, as.numeric(bb.resp$forcetemp_night))
bb.resp$forcenight.conv <- ifelse(bb.resp$forcetemp_night=="ambient+1", 101, bb.resp$forcenight.conv)
bb.resp$forcenight.conv <- ifelse(bb.resp$forcetemp_night=="ambient+2", 102, bb.resp$forcenight.conv)
bb.resp$forcenight.conv <- ifelse(bb.resp$forcetemp_night=="ambient+3", 103, bb.resp$forcenight.conv)
bb.resp$forcenight.conv <- ifelse(bb.resp$forcetemp_night=="ambient+4", 104, bb.resp$forcenight.conv)
bb.resp$forcenight.conv <- ifelse(bb.resp$forcetemp_night=="ambient+5", 105, bb.resp$forcenight.conv)

bb.resp$photonight <- as.numeric(bb.resp$photoperiod_night) #ome na's 

# Correct forcing for day/night differences
bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$force <- bb.resp$forceday#but forceday is na?
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
bb.resp$chill <- as.numeric(bb.resp$Total_Utah_Model) #some NAs here as well. 
bb.resp$chill.hrs <- as.numeric(bb.resp$Total_Chilling_Hours)
bb.resp$chill.ports <- as.numeric(bb.resp$Total_Chill_portions) 

bb.resp$resp <- as.numeric(bb.resp$response.time)
bb.resp$respvar.simple
#there are sponses above 100, but is that a problem when not all data is about budburst?

#bb.resp<-bb.resp[bb.resp$resp<100,]
bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(resp)==FALSE)
dim(bb.noNA)[1]  #no rows where there are no NAs. For some resaon forcing is NA thoughout  
dim(bb.resp)
# Fix species... so it plays well with rstan
bb.noNA$latbi <- paste(bb.noNA$genus, bb.noNA$species)
bb.noNA$complex <- as.numeric(as.factor(bb.noNA$latbi))

# Start looking at the data ...
ggplot(bb.noNA, aes(chill, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
ggplot(bb.noNA, aes(force, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
ggplot(bb.noNA, aes(photo, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
#Weird high resp
#bb.noNA$datasetID[which(bb.noNA$resp==25273.312000)]
#Yikes- weird very high resp  in bb.noNA from  "man17"

# Compare it to  the paper ....
comppaper <- subset(bb.noNA, select=c("datasetID", "latbi", "chill", "force",
    "photo", "resp", "figure.table..if.applicable."))
comppaperfig <- subset(bb.noNA, select=c("datasetID", "latbi", "figure.table..if.applicable."))
comppaperfig[!duplicated(comppaperfig), ]
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
