#### 18 February 2021 - Cat
## Building a muplot for the pop model results. The main focus should be on the sigmas.

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# libraries
library(shinystan)
library(reshape2)
library(rstan)
library(rstanarm)
library(viridis)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges") 
}else setwd("~/Documents/git/ospree/analyses/ranges") 

# Our flags for ranges, for now ... (see issue #379)
use.chillports = FALSE
use.zscore = TRUE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE


setwd("..//bb_analysis")
source("source/bbstanleadin.R")


# Species complex for ranges, without crops and need species that do not only have field chilling, z-scored
use.rangespp = TRUE
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE & use.rangespp==TRUE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == FALSE 
    & use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.alltypes.ranges
  
  source("source/bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

setwd("..//ranges")

bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")
bb.stan$site <-  paste(bb.stan$provenance.lat, bb.stan$provenance.long)

### find only studies with 2 or more latitudes
multisites<-bb.stan %>% group_by(datasetID) %>% dplyr::summarise(unique_sites = n_distinct(site))
multisites<-filter(multisites, unique_sites>=2)
bb.stan.site<-filter(bb.stan,datasetID %in% c(multisites$datasetID)) ###### this is the datasheet for the intra/inter model

## Do some population stuff, by latitude
getpopz1 <- subset(bb.stan.site, select=c("latbi", "site")) # "datasetID", "study",
getpopz2 <- getpopz1[!duplicated(getpopz1), ]
getpopz <- aggregate(getpopz2["site"], getpopz2["latbi"], FUN=length)
getpopz5 <- subset(getpopz, site>4) # 3
getpopz4 <- subset(getpopz, site>3) # 3
getpopz3 <- subset(getpopz, site>2) # 12
getpopz2 <- subset(getpopz, site>1) # 28


bb.stan$latbinum <- as.numeric(as.factor(bb.stan$latbi))
bb.stan.site$latbinum <- as.numeric(as.factor(bb.stan.site$latbi))

bb.stan.pop5 <- bb.stan.site[which(bb.stan.site$latbi %in% getpopz5$latbi),] # 3 species!
bb.stan.pop4 <- bb.stan.site[which(bb.stan.site$latbi %in% getpopz4$latbi),] # 12 species
bb.stan.pop3 <- bb.stan.site[which(bb.stan.site$latbi %in% getpopz3$latbi),] # 12 species
bb.stan.pop2 <- bb.stan.site[which(bb.stan.site$latbi %in% getpopz2$latbi),] # 28 species

bb.stan.here <- bb.stan.pop3 ##lets do the 3 pop
getpop <- paste(bb.stan.here$latbinum, bb.stan.here$site)
bb.stan.here$pophere <- as.numeric(as.factor(getpop))
bb.stan.here$latbinum <- as.numeric(as.factor(bb.stan.here$latbi))
bb.stan.here$datasetnum <- as.numeric(as.factor(bb.stan.here$datasetID))


load("~/Desktop/forcephoto_popmodel.Rdata")

modelhere <- m3l.ni 
mod.sum <- summary(modelhere)$summary

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <-rep(viridis_pal(option="viridis")(9),2)
my.pch <- rep(15:18, each=10)
alphahere = 0.4

modoutput <- summary(m3l.ni)$summary
noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
labs <- c("Species", "Forcing \n(Species)", "Photoperiod \n(Species)", "Population", "Forcing \n(Population)", 
                                 "Photoperiod \n(Population)")

modelhere <- m3l.ni
bball <- bb.stan.here
spnum <- length(unique(bball$latbi))
quartz()
par(xpd=FALSE)
par(mar=c(5,8,3,3))
plot(x=NULL,y=NULL, xlim=c(-20,20), yaxt='n', ylim=c(0,6),
     xlab="Model estimate change in days to budburst", ylab="")
axis(2, at=1:6, labels=rev(labs), las=1)
abline(v=0, lty=2, col="darkgrey")
rownameshere <- c("sigma_a_sp", "sigma_b_force_sp", "sigma_b_photo_sp", "sigma_a_pop",
                  "sigma_b_force_sppop", "sigma_b_photo_sppop")
for(i in 1:6){
  pos.y<-(6:1)[i]
  pos.x<-noncps[rownameshere[i],"mean"]
  lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
  
}
par(xpd=TRUE) # so I can plot legend outside
#legend(22, 6, sort(unique(gsub("_", " ", bball$latbi))), pch=my.pch[1:spnum],
 #      col=alpha(my.pal[1:spnum], alphahere),
  #     cex=1, bty="n", text.font=3)



