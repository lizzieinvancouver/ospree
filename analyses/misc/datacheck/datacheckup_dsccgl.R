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
medata <- c("fu19", "anzanello18", "nanninga17", "prevey18", "flynn18", "man17", "anzanello16", "vitra17") ### round 1

mall <- dall[which(dall$datasetID %in% medata),]
mdbb <- dbb[which(dbb$datasetID %in% medata),]

# check if you lose data between all and bb (if you do, find out what is lost!)
dim(mall) ## 1364
dim(mdbb) ## 1122; We lose 242 rows of data

sort(unique(mdbb$datasetID)) ## We lose anzanello18
dim(mall[(mall$datasetID=="anzanello18"),]) ## 180 rows, still missing 62 rows

dim(mall[(mall$datasetID=="flynn18"),]) ## 830
dim(mdbb[(mdbb$datasetID=="flynn18"),]) ## 830

dim(mall[(mall$datasetID=="fu19"),]) ## 28
dim(mdbb[(mdbb$datasetID=="fu19"),]) ## 26 ### Two rows removed are listed below... I have no idea why these are being dropped
 #####forcetemp: ambient+4;   photo: ambient; response.time: 89; SD: 2; Fig: S1
 #####forcetemp: ambient+4;   photo: ambient-2; response.time: 91; SD: 1; Fig: S1
 #### Forcing conditions are not being converted due to provenance.lat issues

dim(mall[(mall$datasetID=="anzanello16"),]) ## 120
dim(mdbb[(mdbb$datasetID=="anzanello16"),]) ## 60 ## Final 60 accounted for yay! ### Dropping duplicates from percent budburst entries

str(mall)
str(mdbb)

## bb stuff... this code poached from:
# models_stan.R, models_stan_plotting.R, bbstanleadin.R, and bbdataplease.R 
## just the bb data ...
d <- mdbb
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),]
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

dim(bb.resp) ### 1000; losing 122 rows

dim(mdbb[(mdbb$datasetID=="flynn18"),]) ## 830
dim(bb.resp[(bb.resp$datasetID=="flynn18"),]) ## 720 ## accounts for 110 rows due to 'daystoflower' respvar

dim(mdbb[(mdbb$datasetID=="man17"),]) ## 112
dim(bb.resp[(bb.resp$datasetID=="man17"),]) ## 112

dim(mdbb[(mdbb$datasetID=="anzanello16"),]) ## 60
dim(bb.resp[(bb.resp$datasetID=="anzanello16"),]) ## 60

dim(mdbb[(mdbb$datasetID=="prevey18"),]) ## 12
dim(bb.resp[(bb.resp$datasetID=="prevey18"),]) ## 0 ## yay!! Last 12 rows due to 'daystoflower' respvar


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
dim(bb.noNA) ### 892 ## Losing 108 rows
unique(bb.noNA$datasetID) ## "flynn18"    "man17"      "nanninga17"

## Okay so we're losing anzanello16, fu19, and vitra17... why?

fu19 <- bb.resp[(bb.resp$datasetID=="fu19"),] ### No chilling data and can't calculate forcing = 26 rows

anzanello16 <- bb.resp[(bb.resp$datasetID=="anzanello16"),] ## can't calculate chilling in South America

vitra17 <- bb.resp[(bb.resp$datasetID=="vitra17"),] ### Losing due to photoperiod issues! Can fix.


########################################################################################
###################### Let's check on some things individually #########################
########################################################################################

### Before moving on, let's check out why we are losing 1) richardson18 and 2) man17 
# 1) Richardson18
richdf <- mdbb[(mdbb$datasetID=="richardson18"),]
dim(richdf) ## 17 rows of data
unique(richdf$respvar.simple) # daystobudburst, so that checks out... 
richdf.simple <- subset(richdf, select=c("genus", "species", "provenance.lat", "forcetemp", 
                                         "photoperiod_day", "Total_Utah_Model", "response.time", 
                                         "fieldsample.date", "chilltemp"))

head(richdf.simple)
#     genus   species provenance.lat forcetemp photoperiod_day Total_Utah_Model response.time   fieldsample.date    chilltemp
#6527 Larix laricinia       47.50285      <NA>              14               NA       113.239                     ambient + 2.25
#6528 Larix laricinia       47.50285      <NA>              14               NA       106.555                     ambient + 2.25
#6529 Picea   mariana       47.50285      <NA>              11               NA        64.615                     ambient + 2.25
#6530 Picea   mariana       47.50285      <NA>              11               NA        67.692                     ambient + 2.25
#6531 Larix laricinia       47.50285      <NA>              14               NA       103.985                     ambient + 4.5
#6532 Picea   mariana       47.50285      <NA>              12               NA        68.205                     ambient + 4.5

## Okay so for richardson18 we are missing both forcing and chilling. 
# Let's go back through the paper to make sure that is true.

## We should have both chilling and forcing information... they were both entered as ambient in the 
# initial push so it is being deleted somewhere in the code early on for forcing. Need to check on chilling.
# I think it's because we are missing a field sample date since this takes place in a chamber outside. 

# 2) man17
mandf <- mdbb[(mdbb$datasetID=="man17"),] ## 112 observations
dim(mandf) 
unique(mandf$respvar.simple) # othernums - so that's why! 

## Let's double check the paper to make sure the response variable is correct!

#### CJC checked man17 on 27 March 2020: response variable is cumulative chilling hours 
# which is correctly reported in the ospree database.

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

### Experiment 1: we should be getting field chilling. There must be a flaw in chilling code somewhere.
## This needs to be updated!

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
                         sp = complex,
                         N = nrow(bb.noNA),
                         n_sp = length(unique(bb.noNA$complex))
                    ))

modhere = stan('bb_analysis/stan/datacheckup_cc_ds_chillonly.stan', data = datalist.bb,
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

plot(resp~chill, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~chill, data=subby, main=subby$complex.wname[1], col=colv[sp])
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1]
    abline(intercepthere, slopehere, col=colv[sp])
    }
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
    whichmodel[grep("b_chill", rownames(whichmodel)),1], col="black", lwd=3)


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
mdbb.noman <- mdbb[!(mdbb$datasetID=="man17"),]


path <- unique(mdbb$datasetID)


mdbb2.0<- subset(mdbb, datasetID == path[3])

ggplot(mdbb.noman, aes(chill, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) ## multiple chill but have one abberrant chiling! Probably from exp1, I am assuming it is a misentry
ggplot(mdbb.noman, aes(force, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.) # only one forcing
ggplot(mdbb.noman, aes(photo, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
  

##### Alright, now let's checkout what is happening in our cleaning code.
my.pal <- brewer.pal(n = 8, name = "Dark2")

## make a bunch of things numeric - and change ambient to 100 just to see
ambs <- c("ambient","ambient + 2.25","ambient + 4.5","ambient + 6.75","ambient + 9")
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient", 100, as.numeric(mall$forcetemp))
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient + 2.25", 102.25, mall$forcetemp.conv)
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient + 4.5", 104.5, mall$forcetemp.conv)
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient + 6.75", 106.75, mall$forcetemp.conv)
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient + 9", 109, mall$forcetemp.conv)

chills <- c("ambient","ambient + 2.25","ambient + 4.5","ambient + 6.75","ambient + 9","")
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient", 100, as.numeric(mall$chilltemp))
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient + 2.25", 102.25, mall$chilltemp.conv)
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient + 4.5", 104.5, mall$chilltemp.conv)
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient + 6.75", 106.75, mall$chilltemp.conv)
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient + 9", 109, mall$chilltemp.conv)
mall$chilltemp.conv <- ifelse(mall$chilltemp=="", 0, mall$chilltemp.conv)

mall$photo.conv <-  ifelse(mall$photoperiod_day=="ambient", 30, as.numeric(mall$photoperiod_day))


fc <- ggplot(mall, aes(chilltemp.conv, forcetemp.conv, colour=datasetID)) + geom_point() +
  scale_color_manual(name="Dataset", values = my.pal, labels=sort(unique(mall$datasetID)))

cp <- ggplot(mall, aes(chilltemp.conv, photo.conv, colour=datasetID)) + geom_point() +
  scale_color_manual(name="Dataset", values = my.pal, labels=sort(unique(mall$datasetID)))

pf <- ggplot(mall, aes(photo.conv, forcetemp.conv, colour=datasetID)) + geom_point() +
  scale_color_manual(name="Dataset", values = my.pal, labels=sort(unique(mall$datasetID)))

library(gridExtra)
quartz()
grid.arrange(fc, cp, pf)

### And now we compare to the cleaned code...
## make a bunch of things numeric - and change ambient to 100 just to see
ambs <- c("ambient","ambient + 2.25","ambient + 4.5","ambient + 6.75","ambient + 9")
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient", 100, as.numeric(mall$forcetemp))
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient + 2.25", 102.25, mall$forcetemp.conv)
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient + 4.5", 104.5, mall$forcetemp.conv)
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient + 6.75", 106.75, mall$forcetemp.conv)
mall$forcetemp.conv <- ifelse(mall$forcetemp=="ambient + 9", 109, mall$forcetemp.conv)

chills <- c("ambient","ambient + 2.25","ambient + 4.5","ambient + 6.75","ambient + 9","")
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient", 100, as.numeric(mall$chilltemp))
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient + 2.25", 102.25, mall$chilltemp.conv)
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient + 4.5", 104.5, mall$chilltemp.conv)
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient + 6.75", 106.75, mall$chilltemp.conv)
mall$chilltemp.conv <- ifelse(mall$chilltemp=="ambient + 9", 109, mall$chilltemp.conv)
mall$chilltemp.conv <- ifelse(mall$chilltemp=="", 0, mall$chilltemp.conv)

mall$photo.conv <-  ifelse(mall$photoperiod_day=="ambient", 30, as.numeric(mall$photoperiod_day))


fc.clean <- ggplot(mdbb, aes(chill, force, colour=datasetID)) + geom_point() +
  scale_color_manual(name="Dataset", values = my.pal, labels=sort(unique(mall$datasetID)))

cp.clean <- ggplot(mdbb, aes(chill, photo, colour=datasetID)) + geom_point() +
  scale_color_manual(name="Dataset", values = my.pal, labels=sort(unique(mall$datasetID)))

pf.clean <- ggplot(mdbb, aes(photo, force, colour=datasetID)) + geom_point() +
  scale_color_manual(name="Dataset", values = my.pal, labels=sort(unique(mall$datasetID)))

grid.arrange(fc.clean, cp.clean, pf.clean)


#### More model output
quartz()
par(mar=c(4,7,3,4))
plot(x=NULL,y=NULL, xlim=c(-15, 60), yaxt='n', ylim=c(0,2),
     xlab="Model estimate change in days to budburst", ylab="")
axis(2, at=1:2, labels=rev(c("Intercept", "Chill")), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("mu_a_sp", "b_chill")
ppeffects <- c("mu_a_sp", "b_chill") 
for(i in 1:2){
  pos.y<-(2:1)[i]
  pos.x<-whichmodel[rownameshere[i],"mean"]
  lines(whichmodel[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="black")
  for(spsi in 1:6){
    pos.sps.i<-which(grepl(paste0("a_sp", "[",spsi,"]"),rownames(whichmodel),fixed=TRUE))
    jitt<-(spsi/20) + 0.1
    pos.y.sps.i<-pos.y-jitt
    pos.x.sps.i<-whichmodel[pos.sps.i[i],"mean"]
    lines(whichmodel[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
          col=my.pal[spsi])
    points(pos.x.sps.i,pos.y.sps.i,cex=0.8, col=my.pal[spsi])
    
  }
}
