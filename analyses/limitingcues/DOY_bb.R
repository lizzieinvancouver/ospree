## Estimate the DOY for each species across lat/long sites ##
## Using PEP data for Limiting Cues paper ##
## Updates by Lizzie ##

## Lizzie worked off some code from projects/misc/pep725/pep725spp.R ##

## TO DO! ##
# add priors to models and rerun!

## Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

## Say whether or not you want to run stan!
runstan = FALSE
mapsandsummaries = TRUE # these are just summaries for mapping (they're slow but not terribly)
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
betuse <- bet11[which(bet11$PEP_ID %in% bet10$PEP_ID),]
fag11 <- subset(fagall, BBCH==11)
faguse <- fag11[which(fag11$PEP_ID %in% fag10$PEP_ID),]


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

meanbetuse <-
      ddply(betuse, c("PEP_ID", "LON", "LAT"), summarise,
      mean = mean(DAY),
      mean.yr = mean(YEAR),
      sd = sd(DAY),
      sem = sd(DAY)/sqrt(length(DAY)))

meanfaguse <-
      ddply(faguse, c("PEP_ID", "LON", "LAT"), summarise,
      mean = mean(DAY),
      mean.yr = mean(YEAR),
      sd = sd(DAY),
      sem = sd(DAY)/sqrt(length(DAY)))



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

betuse$YEAR.hin <- betuse$YEAR
betuse$YEAR.hin[which(betuse$YEAR.hin<1980)] <- 1980
betuse$PEP_ID <- as.character(betuse$PEP_ID)
betuse$YEAR.hin <- betuse$YEAR.hin-1980

faguse$YEAR.hin <- faguse$YEAR
faguse$YEAR.hin[which(faguse$YEAR.hin<1980)] <- 1980
faguse$PEP_ID <- as.character(faguse$PEP_ID)
faguse$YEAR.hin <- faguse$YEAR.hin-1980
# Note to self: for betula lmer will fit random intercepts but not random slopes

if(runstan) {
# betula
N <- nrow(betuse)
y <- betuse$DAY
J <- length(unique(betuse$PEP_ID))
sites <- as.numeric(as.factor((betuse$PEP_ID)))
year <- betuse$YEAR.hin
# nVars <-1
# Imat <- diag(1, nVars)

# Whoa! I think the model runs when I use all data ... must check more!
fit.hinge.bet <- stan("stan/hinge_randslopesint.stan",
    data=c("N","J","y","sites","year"), iter=2000, chains=4, cores=ncores)
    # control = list(adapt_delta = 0.95, max_treedepth = 15))

save(fit.hinge.bet, file="stan/output/fit.hinge.bet.Rda")

# the above model was returning a few divergent transitions (model ran fast but led to 52 div transition and obvious issues in fitting sigma_b) when I ran it on this subset of the data though:
# betuse <- betuse[1:5000,]
# This NCP model (below) on the 5K data above returned ALL divergent transitions!
# fit.hinge.ncp <- stan("stan/hinge_randslopesint_ncp.stan",
#    data=c("N","J","y","sites","year"), iter=500, chains=4, cores=4)


# Now do fagus
Nf <- nrow(faguse)
yf <- faguse$DAY
Jf <- length(unique(faguse$PEP_ID))
sitesf <- as.numeric(as.factor((faguse$PEP_ID)))
yearf <- faguse$YEAR.hin
# nVars <-1
# Imat <- diag(1, nVars)

fit.hinge.fag <- stan("stan/hinge_randslopesint.stan",
    data=list(N=Nf, J=Jf, y=yf, sites=sitesf, year=yearf), iter=2000, chains=4, cores=ncores)

save(fit.hinge.fag, file="stan/output/fit.hinge.fag.Rda")
}

# If not running stan, then we load the stan runs here ...
if(!runstan) {
# Below versions use data with 10 years or more
load("stan/output/fit.hinge.bet.Rda") # mu of 113.5 and -0.35, 9700 sites
load("stan/output/fit.hinge.fag.Rda") # mu of 121.4 and -0.33, 8300 sites
# Versions using data with only 20 or more years
# load("stan/output/fit.hinge.20yr.bet.Rda") # mu of 113.4 and -0.35, 5400 sites
# load("stan/output/fit.hinge.20yr.fag.Rda") # mu of 121.4 and -0.34, 6600 sites
}


###################################
## Get predictions for each species ##
###################################
# Okay, now get predictions, there seems to be no easy way to do this in base Stan:http://discourse.mc-stan.org/t/best-way-to-do-prediction-on-new-data-r-rstan-stanfit/1772/5

# To Do! Below assumes that Stan does not sort my sites, should check this!!!
sumerf <- summary(fit.hinge.fag)$summary # mu_a is 121 and mu_b is -0.34
sumerf[grep("mu_", rownames(sumerf)),]
mean(faguse$YEAR)
# ... and Betula
mean(betuse$YEAR)
sumerb <- summary(fit.hinge.bet)$summary
sumerb[grep("mu_", rownames(sumerb)),]

getstanpred <- function(dat, sitecolname, stansummary, predyear){
    siteslist <- unique(dat[sitecolname])
    sumer.ints <- stansummary[grep("a\\[", rownames(stansummary)),]
    sumer.slopes <- stansummary[grep("b\\[", rownames(stansummary)),]
    predhere <- c()
    for (sitehere in c(1:nrow(siteslist))){
        predhere[sitehere] <- sumer.ints[sitehere]+sumer.slopes[sitehere]*predyear
    }
    return(predhere)
    }

# Reminder, 1980 is 0 in our model...
fagpred <- getstanpred(faguse, "PEP_ID", sumerf, 3)
betpred <- getstanpred(betuse, "PEP_ID", sumerb, 3)

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

plot(fagpred~fagpred.lin$pred, asp=1)
abline(lm(fagpred~fagpred.lin$pred))

plot(betpred~betpred.lin$pred, asp=1)
abline(lm(betpred~betpred.lin$pred))

# Compare with mean values
fagpred.wsite <- data.frame(fagpred=fagpred, PEP_ID=unique(faguse$PEP_ID))
fagpred.wsite <- fagpred.wsite[with(fagpred.wsite, order(PEP_ID, fagpred)),]
plot(fagpred.wsite$fagpred~meanfaguse$mean, asp=1)


# an example of a couple outlier from above:
if(FALSE){
# here's a high one (160)  
    which(fagpred.lin$pred>160)
    unique(faguse$PEP_ID)[103]
    subset(faguse, PEP_ID=="19666")
    goo <- subset(faguse, PEP_ID=="19666") # note that only one year is later than 1980 and it is a really late year....
    mean(goo$DAY)
    mod <- lm(DAY~YEAR.hin, data=goo)
    summary(mod)
# and a low one
    which(fagpred.lin$pred>160)
    unique(faguse$PEP_ID)[378]
    subset(faguse, PEP_ID=="4365")
    goo <- subset(faguse, PEP_ID=="4365") # note that (again) only one year is later than 1980
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

datplot <- sumerb.df
ggplot() + 
  geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
  coord_cartesian(ylim=c(30, 75), xlim=c(-15, 40)) +
  geom_point(data=datplot, 
             aes(x=lon,lat, fill=pred1983), 
             colour="dodgerblue4", pch=21) +
  theme.tanmap
}







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
