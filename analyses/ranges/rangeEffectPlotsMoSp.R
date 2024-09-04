## Started 4 September 2024 ##
## By Lizzie ##

## Trying to make the main ranger figures, adapted from traitors ##
## REMEMBER: We should also make the decompose figures ##

# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

library(rstan)

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

# For now, read in one model ..
load("output/CP_jnt_nam.Rda")
post <- rstan::extract(CP_jnt_nam)

if(FALSE){
colnames(post$alphaPhenoSp) <- specieslist
colnames(post$alphaForcingSp) <- specieslist
colnames(post$alphaChillSp) <- specieslist
colnames(post$alphaPhotoSp) <- specieslist
}

## Obtain mean effect of forcing, chilling, photoperiod, interaction
forceeff <- apply(post$betaForcingSp, MARGIN = 2, FUN = mean)
chilleff <- apply(post$betaChillSp, MARGIN = 2, FUN = mean)
photoeff <- apply(post$betaPhotoSp, MARGIN = 2, FUN = mean)
# mugrandeff <- apply(post$mu_grand_sp, MARGIN = 2, FUN = mean)
betaTraitForceeff <- mean(post$betaTraitxForcing)
betaTraitChilleff <- mean(post$betaTraitxChill) 
betaTraitPhotoeff <- mean(post$betaTraitxPhoto) 

# Following Deirdre's code from cue_slope_plot_sm.pdf

plot( x= unique(bb.stan.nam$Temp.Mean.CP), y = chilleff, type="n", 
	xlim = c(min(unique(bb.stan.nam$Temp.Mean.CP)), max(unique(bb.stan.nam$Temp.Mean.CP))), 
	ylim = c(quantile(post$betaChillSp, probs=c(0.25)), quantile(post$betaChillSp, probs=c(0.75))), 
	ylab = expression("Response to cue (standardized)"), 
	xlab = "Temp.Mean.CP", cex.lab = 1.5) # blank plot with x range 
arrows(
    unique(bb.stan.nam$Temp.Mean.CP), # x mean
	apply(post$betaChillSp, MARGIN = 2, FUN = quantile)[2,], 
	unique(bb.stan.nam$Temp.Mean.CP),
    apply(post$betaChillSp, MARGIN = 2, FUN = quantile)[4,], 
    length = 0
  )

# Now add the fit line ... but should limit to quantiles!
# Also, not clear to me why the light pink with alpha does now work ... 
for(j in 1:1000){
    abline(a = post$muChillSp[j], b = post$betaTraitxChill[j], col="lightpink") # alpha("lightpink", 0.015))
  }
abline(a=mean(post$muChillSp), b=betaTraitChilleff, col = "grey")


if(FALSE){ # for testing .. 
	rangevar <- "Temp.Mean.CP"
	betasp <- "betaChillSp"
	intercepthere  <- mean(post$muChillSp)
	slopehere  <- betaTraitChilleff
}

makecuebyrangeplot <- function(rangevar, betasp, intercepthere, slopehere){
	plot( x= unique(bb.stan.nam[[rangevar]]), y = chilleff, type="n", 
		xlim = c(min(unique(bb.stan.nam[[rangevar]])), max(unique(bb.stan.nam[[rangevar]]))), 
		ylim = c(quantile(post[[betasp]], probs=c(0.25)), quantile(post[[betasp]], probs=c(0.75))), 
		ylab = expression("Response to cue (standardized)"), 
		xlab = "Temp.Mean.CP", cex.lab = 1.5) # blank plot with x range 
	arrows(
	    unique(bb.stan.nam[[rangevar]]), # x mean
		apply(post[[betasp]], MARGIN = 2, FUN = quantile)[2,], 
		unique(bb.stan.nam[[rangevar]]),
	    apply(post[[betasp]], MARGIN = 2, FUN = quantile)[4,], 
	    length = 0
	  )

	# Now add the fit line ... but should limit to quantiles!
	# Also, not clear to me why the light pink with alpha does now work ... 
	#for(j in 1:1000){
	#    abline(a = post$muChillSp[j], b = post$betaTraitxChill[j], col="lightpink") # alpha("lightpink", 0.015))
	#  }
	abline(a=intercepthere, b=slopehere, col = "grey")

}

par(mfrow=c(1,3))
makecuebyrangeplot("Temp.Mean.CP", "betaChillSp", mean(post$muChillSp), betaTraitChilleff)
makecuebyrangeplot("Temp.Mean.CP", "betaForcingSp", mean(post$muForceSp), betaTraitForceeff)
makecuebyrangeplot("Temp.Mean.CP", "betaPhotoSp", mean(post$muPhotoSp), betaTraitPhotoeff)


