## Estimate the DOY for each species across lat/long sites ##
## Using PEP data for Limiting Cues paper ##
## Updates by Lizzie ##

## Lizzie worked off some code from projects/misc/pep725/pep725spp.R ##

## TO DO! ##
# Clean up the code below so the stuff that is identical for bet and fag is called as a f(x) or such
# Then need to look at the betula
# Also check the outliers in fagus (linear model predictions of <70 or >160 make sense)

## Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

## Load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggplot2)
library(lubridate)
library(rstan)

## Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/limitingcues") 
} else
  setwd("~/Documents/git/ospree/analyses/limitingcues")

betall <- read.csv("input/PEP_betpen.csv", header=TRUE)
fagall <- read.csv("input/PEP_fagsyl.csv", header=TRUE)

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
betuse <- bet11[which(bet11$PEP_ID %in% bet20$PEP_ID),]
fag11 <- subset(fagall, BBCH==11)
faguse <- fag11[which(fag11$PEP_ID %in% fag20$PEP_ID),]


##
## Get mean at each site and plot
## summarizing data
if(FALSE){
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
## NEXT! 
# fit hinge to each site, then predict a common year
# plot mean value for each site against common year estimate ...
# also plot a linear fit to each site (no hierarchical model) and compare to that...

betuse$YEAR.hin <- betuse$YEAR
betuse$YEAR.hin[which(betuse$YEAR.hin<1980)] <- 1980
betuse$PEP_ID <- as.character(betuse$PEP_ID)
betuse$YEAR.hin <- betuse$YEAR.hin-1980

faguse$YEAR.hin <- faguse$YEAR
faguse$YEAR.hin[which(faguse$YEAR.hin<1980)] <- 1980
faguse$PEP_ID <- as.character(faguse$PEP_ID)
faguse$YEAR.hin <- faguse$YEAR.hin-1980

# Note to self: lmer will fit random intercepts but not random slopes
N <- nrow(betuse)
y <- betuse$DAY
J <- length(unique(betuse$PEP_ID))
sites <- as.numeric(as.factor((betuse$PEP_ID)))
year <- betuse$YEAR.hin
# nVars <-1
# Imat <- diag(1, nVars)

# Whoa! I think the model runs when I use all data ... must check more!
fit.hinge.bet <- stan("stan/hinge_randslopesint.stan",
    data=c("N","J","y","sites","year"), iter=2000, chains=4, cores=2)
    # control = list(adapt_delta = 0.95, max_treedepth = 15))

save(fit.hinge, file="stan/output/fit.hinge.bet.Rda")

if(FALSE){
# the above model was returning a few divergent transitions (model ran fast but led to 52 div transition and obvious issues in fitting sigma_b) when I ran it on this subset of the data though:
# betuse <- betuse[1:5000,]
# This NCP model (below) on the 5K data above returned ALL divergent transitions!
fit.hinge.ncp <- stan("stan/hinge_randslopesint_ncp.stan",
    data=c("N","J","y","sites","year"), iter=500, chains=4, cores=4)
# Should add these priors to CP...
  // Other priors
  mu_a ~ normal(0, 100);
  mu_b ~ normal(0, 10);
  sigma_y ~ normal(0, 30);
  sigma_b ~ normal(0, 30);
}

# Now do fagus
Nf <- nrow(faguse)
yf <- faguse$DAY
Jf <- length(unique(faguse$PEP_ID))
sitesf <- as.numeric(as.factor((faguse$PEP_ID)))
yearf <- faguse$YEAR.hin
# nVars <-1
# Imat <- diag(1, nVars)

fit.hinge.fag <- stan("stan/hinge_randslopesint.stan",
    data=list(N=Nf, J=Jf, y=yf, sites=sitesf, year=yearf), iter=2000, chains=4, cores=2)

save(fit.hinge.fag, file="stan/output/fit.hinge.fag.Rda")

#load("stan/output/fit.hinge.bet.Rda")
#load("stan/output/fit.hinge.fag.Rda")


# Okay, now get predictions, there seems to be no easy way to do this in base Stan:http://discourse.mc-stan.org/t/best-way-to-do-prediction-on-new-data-r-rstan-stanfit/1772/5

# To Do! Below assumes that Stan does not sort my sites, should check this!!!
sitesfag <- unique(faguse$PEP_ID)

sumerf <- summary(fit.hinge.fag)$summary
sumerf[grep("mu_", rownames(sumerf)),]
sumerfints <- sumerf[grep("a\\[", rownames(sumerf)),]
sumerfslopes <- sumerf[grep("b\\[", rownames(sumerf)),]

# Reminder, 1980 is 0 in our model...
mean(faguse$YEAR)

fagpred <- c()
for (sitehere in c(1:length(sitesfag))){
    fagpred[sitehere] <- sumerfints[sitehere]+sumerfslopes[sitehere]*3
    }

# Now do linear fits for each site
linfit.fagpred <- c()
for (sitehere in c(1:length(sitesfag))){
    subby <- subset(faguse, PEP_ID==sitesfag[sitehere])
    mod <- lm(DAY~YEAR.hin, data=subby)
    linfit.fagpred[sitehere] <- coef(mod)[1]+coef(mod)[2]*3
    }

plot(fagpred~linfit.fagpred, asp=1)

# an example of one outlier from above:
if(FALSE){
    which(linfit.fagpred>160)
    unique(faguse$PEP_ID)[103]
    subset(faguse, PEP_ID=="19666")
    goo <- subset(faguse, PEP_ID=="19666") # note that only one year is later than 1980 and it is a really late year....
    mean(goo$DAY)
    mod <- lm(DAY~YEAR.hin, data=goo)    summary(mod)

    summary(mod)
}

# ... and Betula
sitesbet <- unique(betuse$PEP_ID)
mean(betuse$YEAR)

betpred <- c()
for (sitehere in c(1:length(sitesbet))){
    betpred[sitehere] <- sumerbints[sitehere]+sumerbslopes[sitehere]*3
    }

# Now do linear fits for each site
linfit.betpred <- c()
for (sitehere in c(1:length(sitesbet))){
    subby <- subset(betuse, PEP_ID==sitesbet[sitehere])
    mod <- lm(DAY~YEAR.hin, data=subby)
    linfit.betpred[sitehere] <- coef(mod)[1]+coef(mod)[2]*3
    }

plot(betpred~linfit.betpred, asp=1)

# Now make a map
sumerb <- summary(fit.hinge)$summary
sumerb[grep("mu_", rownames(sumerb)),]
sumerbints <- sumerb[grep("a\\[", rownames(sumerb)),]
sumerbslopes <- sumerb[grep("b\\[", rownames(sumerb)),]

sumerb.df <- data.frame(PEP_ID=meanbetuse$PEP_ID, lat=meanbetuse$LAT, lon=meanbetuse$LON,
    m=sumerbslopes, pred1983=betpred)

ggplot() + 
  geom_polygon(dat=wmap.df, aes(long, lat, group=group), fill="grey80") +
  coord_cartesian(ylim=c(30, 75), xlim=c(-15, 40)) +
  geom_point(data=sumerb.df, 
             aes(x=lon,lat, fill=pred1983), 
             colour="dodgerblue4", pch=21) +
  theme.tanmap



## Code from Cat, need to go through ##
## Also need to change bet to betall ... and fag to fagall ##

## Removes anything without a BBCH, makes things into date, take first observation at each site ##

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

fag<-fag%>%dplyr::rename("lat" = LAT)%>%dplyr::rename("long"=LON)
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

