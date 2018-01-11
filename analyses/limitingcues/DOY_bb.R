## Estimate the DOY for each species across lat/long sites ##
## Using PEP data for Limiting Cues paper ##
## Updates by Lizzie ##

## Lizzie worked off some code from projects/misc/pep725/pep725spp.R ##
## In meeting with Andrew on 21 Dec 2017 he confirmed that Stan will:
# (1) reorganize your grouping factor (e.g., our SITE) into a numeric
# (2) present it to you consequetively

## TO DO! ##
# work on code starting at: compare model fits
# Check what year I should predict for and write out predicted DOY values for Nacho
# add some code showing shinystan
# add priors to models and rerun!

## Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

## Say whether or not you want to run stan!
runstan = FALSE
mapsandsummaries = TRUE # these are just summaries for mapping (they're slow but not terribly), note that right now they run for the 10+ year dataset
use20yrs = FALSE # Otherwise it uses all data with 10 or more years
shinystancheck = FALSE # If you want to look at output in shiny stan
ncores = 2 # how many cores to use, only applies when runstan=TRUE

## Load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggplot2)
library(lubridate)
library(rstan)

## Set working directory
if(length(grep("Lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/limitingcues") 
} else
  setwd("~/Documents/git/ospree/analyses/limitingcues")

## Grab the data
betall <- read.csv("input/PEP_betpen.csv", header=TRUE)
fagall <- read.csv("input/PEP_fagsyl.csv", header=TRUE)

betall <- betall[order(betall$PEP_ID),]
fagall <- fagall[order(fagall$PEP_ID),]

#################################
## Look at the data and format ##
#################################

## Let's figure out sites and stages data ...
betagg <- aggregate(betall[("YEAR")], betall[c("PEP_ID", "BBCH", "National_ID", "LAT", "LON", "ALT")],
    FUN=length)
fagagg <- aggregate(fagall[("YEAR")], fagall[c("PEP_ID", "BBCH", "National_ID", "LAT", "LON", "ALT")],
    FUN=length)
if(FALSE){
ggplot(betagg, aes(x=YEAR, fill=as.factor(BBCH))) +
    geom_histogram(alpha=0.2, position="identity")
ggplot(fagagg, aes(x=YEAR, fill=as.factor(BBCH))) +
    geom_histogram(alpha=0.2, position="identity")
}
# Well, I guess we will go with 11!
nrow(betagg)
nrow(fagagg)
nrow(subset(betagg, YEAR>19))/nrow(betagg)
nrow(subset(fagagg, YEAR>19))/nrow(fagagg)

nrow(subset(betagg, YEAR>9))/nrow(betagg)
nrow(subset(fagagg, YEAR>9))/nrow(fagagg)
# Hmm, I guess we will start with 20 years ...
bet20 <- subset(betagg, YEAR>19)
fag20 <- subset(fagagg, YEAR>19)

# Trying 10 years ...
bet10 <- subset(betagg, YEAR>9)
fag10 <- subset(fagagg, YEAR>9)

# Annoying detour to figure out which info is needed for unique ID
betagg$ID1 <- paste(betagg$PEP_ID, betagg$National_ID)
betagg$ID2 <- paste(betagg$PEP_ID, betagg$National_ID, betagg$LAT, betagg$LON, betagg$ALT)

length(unique(betagg$PEP_ID))
length(unique(betagg$National_ID))
length(unique(betagg$ID1))
length(unique(betagg$ID2))
# PEP_ID seems unique

# Subset the data based on the above for now ...
bet11 <- subset(betall, BBCH==11)
betuse10 <- bet11[which(bet11$PEP_ID %in% bet10$PEP_ID),]
fag11 <- subset(fagall, BBCH==11)
faguse10 <- fag11[which(fag11$PEP_ID %in% fag10$PEP_ID),]
betuse20 <- bet11[which(bet11$PEP_ID %in% bet20$PEP_ID),]
faguse20 <- fag11[which(fag11$PEP_ID %in% fag20$PEP_ID),]


####################################
## Get mean at each site and plot ##
####################################

## summarizing data
if(mapsandsummaries){
meanbet <-
      ddply(betall, c("PEP_ID", "LON", "LAT"), summarise,
      mean = mean(DAY),
      mean.yr = mean(YEAR),
      sd = sd(DAY),
      sem = sd(DAY)/sqrt(length(DAY)))

meanfag <-
      ddply(fagall, c("PEP_ID", "LON", "LAT"), summarise,
      mean = mean(DAY),
      mean.yr = mean(YEAR),
      sd = sd(DAY),
      sem = sd(DAY)/sqrt(length(DAY)))

if(use20yrs){
meanbetuse <-
      ddply(betuse20, c("PEP_ID", "LON", "LAT"), summarise,
      mean = mean(DAY),
      mean.yr = mean(YEAR),
      sd = sd(DAY),
      sem = sd(DAY)/sqrt(length(DAY)))

meanfaguse <-
      ddply(faguse20, c("PEP_ID", "LON", "LAT"), summarise,
      mean = mean(DAY),
      mean.yr = mean(YEAR),
      sd = sd(DAY),
      sem = sd(DAY)/sqrt(length(DAY)))
}

if(!use20yrs){
meanbetuse <-
      ddply(betuse10, c("PEP_ID", "LON", "LAT"), summarise,
      mean = mean(DAY),
      mean.yr = mean(YEAR),
      sd = sd(DAY),
      sem = sd(DAY)/sqrt(length(DAY)))

meanfaguse <-
      ddply(faguse10, c("PEP_ID", "LON", "LAT"), summarise,
      mean = mean(DAY),
      mean.yr = mean(YEAR),
      sd = sd(DAY),
      sem = sd(DAY)/sqrt(length(DAY)))
}


# get the map and set the theme
wmap <- readOGR("..//..//..//..//..//general/maps/ne_110m_land", layer="ne_110m_land")
wmap.df <- fortify(wmap)
theme.tanmap <- list(theme(panel.grid.minor = element_blank(),
                        # panel.grid.major = element_blank(),
                        panel.background = element_rect(fill = "grey90",colour = NA),
                        # plot.background = element_rect(fill=NA),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22),
                        legend.position = "left"))

ggplot() + 
  geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
  coord_cartesian(ylim=c(30, 75), xlim=c(-15, 40)) +
  geom_point(data=meanfag, 
             aes(x=LON, y=LAT, fill=mean), 
             colour="dodgerblue4", pch=21) +
  theme.tanmap
}

#################################################
## Fit hinge models for each species (in Stan) ##
#################################################

# f(x) create new column for PEP_ID ordering
pepnumber <- function(dat, sitecolname){
   df <- data.frame(PEP_ID=unique(as.numeric(unlist(dat[sitecolname]))),
       peporder=c(1:length(unique(unlist(dat[sitecolname])))))
   datmerge <- merge(dat, df, by=sitecolname)
   return(datmerge)
}

# Option to run the 20 year data
if(use20yrs){
betuse <- pepnumber(betuse20, "PEP_ID")
faguse <- pepnumber(faguse20, "PEP_ID")
}

if(!use20yrs){
betuse <- pepnumber(betuse10, "PEP_ID")
faguse <- pepnumber(faguse10, "PEP_ID")
}

# now add hinge
betuse$YEAR.hin <- betuse$YEAR
betuse$YEAR.hin[which(betuse$YEAR.hin<1980)] <- 1980
betuse$YEAR.hin <- betuse$YEAR.hin-1980
    # Note that I tried centering and scaling doy and it did not speed up much. The 20 yrs model still said: 1000 transitions using 10 leapfrog steps per transition would take 215.36 seconds.

faguse$YEAR.hin <- faguse$YEAR
faguse$YEAR.hin[which(faguse$YEAR.hin<1980)] <- 1980
faguse$YEAR.hin <- faguse$YEAR.hin-1980

# Note to self: for betula lmer will fit random intercepts but not random slopes

if(runstan) {
# betula
N <- nrow(betuse)
y <- betuse$DAY
J <- length(unique(betuse$peporder))
sites <- betuse$peporder
year <- betuse$YEAR.hin
# nVars <-1
# Imat <- diag(1, nVars)

fit.hinge.bet <- stan("stan/hinge_randslopesint.stan",
    data=c("N","J","y","sites","year"), iter=2500, warmup=1500,
    chains=4, cores=ncores)
    # control = list(adapt_delta = 0.95, max_treedepth = 15))

if(!use20yrs){
save(fit.hinge.bet, file="stan/output/fit.hinge.bet.Rda")
}
if(use20yrs){
save(fit.hinge.bet, file="stan/output/fit.hinge.20yr.bet.Rda")
}

# the above model was returning a few divergent transitions (model ran fast but led to 52 div transition and obvious issues in fitting sigma_b) when I ran it on this subset of the data though:
# betuse <- betuse[1:5000,]
# This NCP model (below) on the 5K data above returned ALL divergent transitions!
# fit.hinge.ncp <- stan("stan/hinge_randslopesint_ncp.stan",
#    data=c("N","J","y","sites","year"), iter=500, chains=4, cores=4)


# Now do fagus
Nf <- nrow(faguse)
yf <- faguse$DAY
Jf <- length(unique(faguse$peporder))
sitesf <- faguse$peporder
yearf <- faguse$YEAR.hin
# nVars <-1
# Imat <- diag(1, nVars)

fit.hinge.fag <- stan("stan/hinge_randslopesint.stan",
    data=list(N=Nf, J=Jf, y=yf, sites=sitesf, year=yearf), iter=2000, chains=4, cores=ncores)

if(!use20yrs){
save(fit.hinge.fag, file="stan/output/fit.hinge.fag.Rda")
}
if(use20yrs){
save(fit.hinge.fag, file="stan/output/fit.hinge.20yr.fag.Rda")
}

}

# If not running stan, then we load the stan runs here ...
if(!runstan) {
# Versions using data with only 20 or more years
load("stan/output/fit.hinge.20yr.bet.Rda") # mu of 113.4 and -0.35, 5400 sites ("fit.hinge")
load("stan/output/fit.hinge.20yr.fag.Rda") # mu of 121.4 and -0.34, 6600 sites
fit.hinge.bet20 <- fit.hinge.bet
fit.hinge.fag20 <- fit.hinge.fag
# Below versions use data with 10 years or more
load("stan/output/fit.hinge.bet.Rda") # mu of 113.5 and -0.35, 9700 sites
load("stan/output/fit.hinge.fag.Rda") # mu of 121.4 and -0.33, 8300 sites
}

if(!runstan) {
if(shinystancheck) {
library(shinystan)
# for both betula models: could run warmup longer and try to get neff up for sigma_b and logposterior
launch_shinystan(fit.hinge.bet) 
launch_shinystan(fit.hinge.fag)
launch_shinystan(fit.hinge.bet20) 
launch_shinystan(fit.hinge.fag20) # looks good
    }
}

###################################
## Get predictions for each species ##
###################################
# Okay, now get predictions, there seems to be no easy way to do this in base Stan:http://discourse.mc-stan.org/t/best-way-to-do-prediction-on-new-data-r-rstan-stanfit/1772/5

# Stan does sort my sites, so need to re-sort to match to PEP_ID
sumerf <- summary(fit.hinge.fag)$summary
sumerf[grep("mu_", rownames(sumerf)),]
mean(faguse$YEAR)
# ... and Betula
mean(betuse$YEAR)
sumerb <- summary(fit.hinge.bet)$summary
sumerb[grep("mu_", rownames(sumerb)),]


getstanpred <- function(dat, sitecolname, stansummary, predyear){
    siteslist <- unlist(unique(dat[sitecolname]))
    sumer.ints <- stansummary[grep("a\\[", rownames(stansummary)),]
    sumer.slopes <- stansummary[grep("b\\[", rownames(stansummary)),]
    stanfit <- data.frame(m=as.numeric(rep(NA, length(siteslist))),
        pred=as.numeric(rep(NA, length(siteslist))), site=siteslist)
    for (sitehere in c(1:length(siteslist))){
        stanfit$m[sitehere] <- sumer.slopes[sitehere]
        stanfit$pred[sitehere] <- sumer.ints[sitehere]+sumer.slopes[sitehere]*predyear
    }
    return(stanfit)
    }


# Reminder, 1980 is 0 in our model...
fagpred <- getstanpred(faguse, "PEP_ID", sumerf, 3)
betpred <- getstanpred(betuse, "PEP_ID", sumerb, 3)

# add species to be safe
fagpred$sp <- "fagsyl"
betpred$sp <- "betpen"

# Compare model fits between 10 and 20 year cut-off 
if(FALSE){
sumerb20 <- summary(fit.hinge.bet20)$summary
sumerf20 <- summary(fit.hinge.fag20)$summary
fagpred20 <- getstanpred(faguse20, "PEP_ID", sumerf20, 3)
betpred20 <- getstanpred(betuse20, "PEP_ID", sumerb20, 3)
fagm <- merge(fagpred20, fagpred, by="site", suffixes=c(20, 10))
betm <- merge(betpred20, betpred, by="site", suffixes=c(20, 10))
plot(pred20~pred10, data=fagm)
plot(m20~m10, data=fagm)
summary(lm(m20~m10, data=fagm))
plot(m20~m10, data=betm)
summary(lm(m20~m10, data=betm))
}


# Now do linear fits for each site
getlinpred <- function(dat, sitecolname, predyear){
    siteslist <- unique(dat[sitecolname])
    linfit <- data.frame(m=as.numeric(rep(NA, nrow(siteslist))),
        pred=as.numeric(rep(NA, nrow(siteslist))))
    for (sitehere in c(1:nrow(siteslist))){
        subby <- subset(dat, PEP_ID==siteslist[sitehere,])
        mod <- lm(DAY~YEAR.hin, data=subby)
        linfit$m[sitehere] <- coef(mod)[2]
        linfit$pred[sitehere] <- coef(mod)[1]+coef(mod)[2]*predyear
    }
    return(linfit)
}

fagpred.lin <- getlinpred(faguse, "PEP_ID", 3)
betpred.lin <- getlinpred(betuse, "PEP_ID", 3)

plot(fagpred$pred~fagpred.lin$pred, asp=1)
abline(lm(fagpred$pred~fagpred.lin$pred))

plot(betpred$pred~betpred.lin$pred, asp=1)
abline(lm(betpred$pred~betpred.lin$pred))

# Compare with mean values
fagpred.wsite <- data.frame(fagpred=fagpred$pred, PEP_ID=unique(faguse$PEP_ID))
fagpred.wsite <- fagpred.wsite[with(fagpred.wsite, order(PEP_ID, fagpred)),]
plot(fagpred.wsite$fagpred~meanfaguse$mean, asp=1) # need to run this above (commented out just now)

# an example of an outlier from above, so you can look at why some things are pooled:
if(FALSE){
# here's a high one (160)  
    which(fagpred.lin$pred>160)
    unique(faguse$PEP_ID)[1044]
    subset(faguse, PEP_ID=="1707")
    goo <- subset(faguse, PEP_ID=="1707") # note that only one year is later than 1980 and it is a really late year....
    mean(goo$DAY)
    mod <- lm(DAY~YEAR.hin, data=goo)
    summary(mod)
}

# Now make a map (adjust datplot for each species)
if(mapsandsummaries){
sumerb.df <- data.frame(PEP_ID=meanbetuse$PEP_ID, lat=meanbetuse$LAT, lon=meanbetuse$LON,
   pred1983=betpred)
sumerf.df <- data.frame(PEP_ID=meanfaguse$PEP_ID, lat=meanfaguse$LAT, lon=meanfaguse$LON,
   pred1983=fagpred)

datplot <- sumerf.df
ggplot() + 
  geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
  coord_cartesian(ylim=c(30, 75), xlim=c(-15, 40)) +
  geom_point(data=datplot, 
             aes(x=lon,lat, fill=pred1983.pred), 
             colour="dodgerblue4", pch=21) +
  theme.tanmap
}

## Write out the data for the 10 year models ##

write.csv(sumerb.df, "output/betpred.wlatlong.csv", row.names=FALSE)
write.csv(sumerf.df, "output/fagpred.wlatlong.csv", row.names=FALSE)

###################
###################
###################

## Code from Cat ##
## Removes anything without a BBCH, makes things into date, take first observation at each site ##
if(FALSE){
    
bet<-betall%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(bet$YEAR, bet$DAY)
bet$date<-as.Date(strptime(x, format="%Y %j"))
bet$year<-as.numeric(substr(bet$date, 0,4))
bet$month<-as.numeric(substr(bet$date, 6, 7))
bet$day<-as.numeric(substr(bet$date, 9,10))
bet$prov<-paste(bet$lat, bet$long)
bet <- bet[order(bet$prov, bet$date), ]

bet$grow<-ifelse(is.na(bet$BBCH), NA, TRUE)
bet$count <- ave(
  bet$grow, bet$PEP_ID, bet$year,
  FUN=function(x) cumsum(c(1, head(x, -1)))
)

bet$count<-as.numeric(as.character(bet$count))
bet<-filter(bet, count==1)
bet$year<-bet$YEAR
bet$bb<-bet$DAY

db<-dplyr::select(bet, year, BBCH, bb, lat, long, species, date)

fag<-fagall%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
x<-paste(fag$YEAR, fag$DAY)
fag$date<-as.Date(strptime(x, format="%Y %j"))
fag$year<-as.numeric(substr(fag$date, 0,4))
fag$month<-as.numeric(substr(fag$date, 6, 7))
fag$day<-as.numeric(substr(fag$date, 9,10))
fag$prov<-paste(fag$lat, fag$long)
fag <- fag[order(fag$prov, fag$date), ]

fag$grow<-ifelse(is.na(fag$BBCH), NA, TRUE)
fag$count <- ave(
  fag$grow, fag$PEP_ID, fag$year,
  FUN=function(x) cumsum(c(1, head(x, -1)))
)

fag$count<-as.numeric(as.character(fag$count))
fag<-filter(fag, count==1)
fag$year<-fag$YEAR
fag$bb<-fag$DAY

df<-dplyr::select(fag, year, BBCH, bb, lat, long, species, date)

d<- full_join(df, db)

}
