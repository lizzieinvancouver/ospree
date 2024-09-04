## Started 4 September 2024 ##
## By Lizzie ##

## Trying to make the main ranger figures, adapted from traitors ##
## REMEMBER: We should also make the decompose figures ##

# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

library(rstan)
library(ggplot2) # alpha!


setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

#####################
### READ IN DATA ####
#####################
## Lizzie says: I doubt that  we need ALL this! 
## Would be nicer to WRITE out what we need in jointmodlingranges.R 

######################################
# Flags to choose for bbstanleadin.R #
######################################

# Our flags for ranges, for now ... (see issue #379)
use.chillports = FALSE
use.zscore = TRUE
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
use.expramptypes.fp = FALSE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

run.models = TRUE

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

bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species, sep="_")
bb.stan$latbinum <- as.numeric(as.factor(bb.stan$latbi))


setwd("..//ranges")

source("source/cleanranges.R")##clean up the ranges data file (now call fulldat)

goodsp<-intersect(unique(bb.stan$complex.wname),fulldat$complex.wname) ##get species in ospree and ranges

bb.stan<-dplyr::filter(bb.stan,complex.wname %in% goodsp)
fulldat<-dplyr::filter(fulldat,complex.wname %in% goodsp)

bb.stan<-left_join(bb.stan,fulldat)

##seperate out the continents
bb.stan.nam <- filter(bb.stan, continent=="N. America")
bb.stan.eu <- filter(bb.stan, continent!="N. America")

##numierate latbinum
bb.stan.nam$latbinum <- as.numeric(as.factor(bb.stan.nam$latbi))
bb.stan.eu$latbinum <- as.numeric(as.factor(bb.stan.eu$latbi))


###make datalists
bb.lf.nam <- with(bb.stan.nam, 
                     list(yPhenoi = resp, 
                          forcingi = force.z,
                          photoi = photo.z,
                          chillingi = chill.z,
                          species = latbinum,
                          N = nrow(bb.stan.nam),
                          n_spec = length(unique(bb.stan.nam$complex.wname)),
                          climvar=unique(bb.stan.nam$SD.lastfrost)
                     ))

bb.lf.eu <- with(bb.stan.eu, 
                    list(yPhenoi = resp, 
                         forcingi = force.z,
                         photoi = photo.z,
                         chillingi = chill.z,
                         species = latbinum,
                         N = nrow(bb.stan.eu),
                         n_spec = length(unique(bb.stan.eu$complex.wname)),
                         climvar=unique(bb.stan.eu$SD.lastfrost)
                         
                    ))


bb.stv.nam <- with(bb.stan.nam, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.nam),
                       n_spec = length(unique(bb.stan.nam$complex.wname)),
                       climvar=unique(bb.stan.nam$STV)
                  ))

bb.stv.eu <- with(bb.stan.eu, 
                 list(yPhenoi = resp, 
                      forcingi = force.z,
                      photoi = photo.z,
                      chillingi = chill.z,
                      species = latbinum,
                      N = nrow(bb.stan.eu),
                      n_spec = length(unique(bb.stan.eu$complex.wname)),
                      climvar=unique(bb.stan.eu$STV)
                      
                 ))


bb.GDD.nam <- with(bb.stan.nam, 
                   list(yPhenoi = resp, 
                        forcingi = force.z,
                        photoi = photo.z,
                        chillingi = chill.z,
                        species = latbinum,
                        N = nrow(bb.stan.nam),
                        n_spec = length(unique(bb.stan.nam$complex.wname)),
                        climvar=unique(bb.stan.nam$Temp.Mean.GDD)
                   ))

bb.GDD.eu <- with(bb.stan.eu, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.eu),
                       n_spec = length(unique(bb.stan.eu$complex.wname)),
                       climvar=unique(bb.stan.eu$Temp.Mean.GDD)
                       
                  ))


bb.CP.nam <- with(bb.stan.nam, 
                   list(yPhenoi = resp, 
                        forcingi = force.z,
                        photoi = photo.z,
                        chillingi = chill.z,
                        species = latbinum,
                        N = nrow(bb.stan.nam),
                        n_spec = length(unique(bb.stan.nam$complex.wname)),
                        climvar=unique(bb.stan.nam$Temp.Mean.CP)
                   ))

bb.CP.eu <- with(bb.stan.eu, 
                  list(yPhenoi = resp, 
                       forcingi = force.z,
                       photoi = photo.z,
                       chillingi = chill.z,
                       species = latbinum,
                       N = nrow(bb.stan.eu),
                       n_spec = length(unique(bb.stan.eu$complex.wname)),
                       climvar=unique(bb.stan.eu$Temp.Mean.CP)
                       
                  ))

### END OF READ IN DATA 


# Following Deirdre's code from cue_slope_plot_sm.pdf

if(FALSE){ # for testing .. 
     rangevar <- "Temp.Mean.CP"
     betasp <- "betaChillSp"
     intercepthere  <- mean(post$muChillSp)
     slopehere  <- betaTraitChilleff
     intpostlines <- "muChillSp"
     slopepostlines <- "betaTraitxChill"
}


makecuebyrangeplot.eu <- function(posthere, name, maintext, rangevar, betasp, intercepthere, slopehere, 
     intpostlines, slopepostlines){
     plot( x= unique(bb.stan.eu[[rangevar]]), y = apply(posthere[[betasp]], MARGIN = 2, FUN = mean), type="n", 
          xlim = c(min(unique(bb.stan.eu[[rangevar]])), max(unique(bb.stan.eu[[rangevar]]))), 
          ylim = c(quantile(posthere[[betasp]], probs=c(0.1)), quantile(posthere[[betasp]], probs=c(0.9))), 
          ylab = expression("Response to cue (standardized)"), 
          xlab = name, cex.lab = 1.5,
          main=maintext)  # blank plot with x range 
     # Now add the fit lines ... 
     # for(j in 1:1000){
     #    abline(a = posthere[[intpostlines]][j], b = posthere[[slopepostlines]][j], col=alpha("lightpink", 0.15)) 
     #  }
     arrows(
         unique(bb.stan.eu[[rangevar]]), # x mean
          apply(posthere[[betasp]], MARGIN = 2, FUN = quantile)[2,], 
          unique(bb.stan.eu[[rangevar]]),
         apply(posthere[[betasp]], MARGIN = 2, FUN = quantile)[4,], 
         length = 0
       )
     abline(a = quantile(posthere[[intpostlines]], probs=c(0.25)), b = quantile(posthere[[slopepostlines]], probs=(0.25)), col="lightpink") 
     abline(a = quantile(posthere[[intpostlines]], probs=c(0.75)), b = quantile(posthere[[slopepostlines]], probs=(0.75)), col="lightpink") 
     abline(a=intercepthere, b=slopehere, col = "grey")
}

makecuebyrangeplot.nam <- function(posthere, name, maintext, rangevar, betasp, intercepthere, slopehere, 
     intpostlines, slopepostlines){
     plot( x= unique(bb.stan.nam[[rangevar]]), y = apply(posthere[[betasp]], MARGIN = 2, FUN = mean), type="n", 
          xlim = c(min(unique(bb.stan.nam[[rangevar]])), max(unique(bb.stan.nam[[rangevar]]))), 
          ylim = c(quantile(posthere[[betasp]], probs=c(0.1)), quantile(posthere[[betasp]], probs=c(0.9))), 
          ylab = expression("Response to cue (standardized)"), 
          xlab = name, cex.lab = 1.5,
          main=maintext)  # blank plot with x range 
     # Now add the fit lines ... 
     # for(j in 1:1000){
     #    abline(a = posthere[[intpostlines]][j], b = posthere[[slopepostlines]][j], col=alpha("lightpink", 0.15)) 
     #  }
     arrows(
         unique(bb.stan.nam[[rangevar]]), # x mean
          apply(posthere[[betasp]], MARGIN = 2, FUN = quantile)[2,], 
          unique(bb.stan.nam[[rangevar]]),
         apply(posthere[[betasp]], MARGIN = 2, FUN = quantile)[4,], 
         length = 0
       )
     abline(a = quantile(posthere[[intpostlines]], probs=c(0.25)), b = quantile(posthere[[slopepostlines]], probs=(0.25)), col="lightpink") 
     abline(a = quantile(posthere[[intpostlines]], probs=c(0.75)), b = quantile(posthere[[slopepostlines]], probs=(0.75)), col="lightpink") 
     abline(a=intercepthere, b=slopehere, col = "grey")
}

# For now, read in one model ..
load("output/CP_jnt_nam.Rda")
load("output/CP_jnt_eu.Rda")

load("output/lf_jnt_nam.Rda")
load("output/lf_jnt_eu.Rda")


if(FALSE){
whichmodel=CP_jnt_nam
colname="Temp.Mean.CP"
namegraph="Temp.Mean.CP NAM"
}

dosomething.nam <- function(whichmodel, colname, namegraph){
     post <- rstan::extract(whichmodel)
     forceeff <- apply(post[["betaForcingSp"]], MARGIN = 2, FUN = mean)
     chilleff <- apply(post[["betaChillSp"]], MARGIN = 2, FUN = mean)
     photoeff <- apply(post[["betaPhotoSp"]], MARGIN = 2, FUN = mean)
     betaTraitForceeff <- mean(post[["betaTraitxForcing"]])
     betaTraitChilleff <- mean(post[["betaTraitxChill"]]) 
     betaTraitPhotoeff <- mean(post[["betaTraitxPhoto"]]) 
     pdf(paste("figures/helpme/cuebyrangepredict", namegraph, ".pdf"), width=8, height=4)
     par(mfrow=c(1,3))
     makecuebyrangeplot.nam(posthere=post, namegraph, "chill", colname, "betaChillSp", mean(post$muChillSp), betaTraitChilleff, 
          "muChillSp", "betaTraitxChill")
     makecuebyrangeplot.nam(posthere=post, namegraph, "force", colname, "betaForcingSp", mean(post$muForceSp), betaTraitForceeff,
          "muForceSp", "betaTraitxForcing")
     makecuebyrangeplot.nam(posthere=post, namegraph, "photo", colname, "betaPhotoSp", mean(post$muPhotoSp), betaTraitPhotoeff, 
          "muPhotoSp", "betaTraitxPhoto")
     dev.off()
}

dosomething.eu <- function(whichmodel, colname, namegraph){
     post <- rstan::extract(whichmodel)
     forceeff <- apply(post[["betaForcingSp"]], MARGIN = 2, FUN = mean)
     chilleff <- apply(post[["betaChillSp"]], MARGIN = 2, FUN = mean)
     photoeff <- apply(post[["betaPhotoSp"]], MARGIN = 2, FUN = mean)
     betaTraitForceeff <- mean(post[["betaTraitxForcing"]])
     betaTraitChilleff <- mean(post[["betaTraitxChill"]]) 
     betaTraitPhotoeff <- mean(post[["betaTraitxPhoto"]]) 
     pdf(paste("figures/helpme/cuebyrangepredict", namegraph, ".pdf"), width=8, height=4)
     par(mfrow=c(1,3))
     makecuebyrangeplot.eu(posthere=post, namegraph, "chill", colname, "betaChillSp", mean(post$muChillSp), betaTraitChilleff, 
          "muChillSp", "betaTraitxChill")
     makecuebyrangeplot.eu(posthere=post, namegraph, "force", colname, "betaForcingSp", mean(post$muForceSp), betaTraitForceeff,
          "muForceSp", "betaTraitxForcing")
     makecuebyrangeplot.eu(posthere=post, namegraph, "photo", colname, "betaPhotoSp", mean(post$muPhotoSp), betaTraitPhotoeff, 
          "muPhotoSp", "betaTraitxPhoto")
     dev.off()
}




dosomething.nam(lf_jnt_nam, colname="SD.lastfrost", namegraph="SD.lastfrost NAM")
dosomething.eu(lf_jnt_eu, colname="SD.lastfrost", namegraph="SD.lastfrost Europe")

dosomething.nam(CP_jnt_nam, colname="Temp.Mean.CP", namegraph="Temp.Mean.CP NAM")
dosomething.eu(CP_jnt_eu, colname="Temp.Mean.CP", namegraph="Temp.Mean.CP Europe")

if(FALSE){
whichmodel=lf_jnt_eu
colname="SD.lastfrost"
namegraph="SD.lastfrost Europe"
}


