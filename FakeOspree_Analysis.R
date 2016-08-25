# Fake data testing of Ospree 
dostan = FALSE # Set to false to use stored runs

library(rstan)
library(ggplot2)
library(shinystan)

setwd("~/Documents/git/ospree")
source('stan/savestan.R')
# get latest .Rdata file

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("FakeOspreeNoLab.RData")

# If very slow, thin the data. But first try with whole data set
# fake <- fake[sample(1:nrow(fake), 999),]

datalist.f <- with(fake, 
    list(y = bb, 
         chill = as.numeric(chill), 
         force = as.numeric(force), 
         photo = as.numeric(photo),
         lat = as.numeric(lat),
         sp = as.numeric(sp),
#         lab = as.numeric(lab),
         N = nrow(fake),
         n_sp = length(unique(sp))
#        n_lab = length(unique(lab))
         )
)

if(dostan){ # M2: no labgroup
  osp.f <- stan('stan/ospreeM4.stan', data = datalist.f, 
                 iter = 2882
                  ) 
  sf <- summary(osp.f)$summary
  
  ssm.f <- as.shinystan(osp.f)
  # launch_shinystan(ssm.f)
  savestan("Fake NoLab")
}


# Load lastest fake data output. Grep for both Fake and Stan Output.
if(!exists('osp.f')){
  
  fakes <- na.omit(grep("Stan Output", dir())[match(grep("Fake", dir()), grep("Stan Output", dir()))])

  load(sort(dir()[fakes], T)[1])
}

sf <- summary(osp.f)$summary
ssm.f <- as.shinystan(osp.f)

launch_shinystan(ssm.f) 

sf[grep("^a_sp\\[", rownames(sf)),"mean"] # 

summary(sf[grep("^b_chill\\[", rownames(sf)),"mean"] ) # should be near -3
sf[grep("^b_chill", rownames(sf)),] # slopes at species level

summary(sf[grep("^b_force\\[", rownames(sf)),"mean"]) # should be near -2
sf[grep("^b_force", rownames(sf)),] # 

summary(sf[grep("^b_photo\\[", rownames(sf)),"mean"] ) #  near -1
sf[grep("^b_photo", rownames(sf)),] # 

# summary(sf[grep("^b_inter_fc\\[", rownames(sf)),"mean"] )# correct, near 0.5
# sf[grep("^b_inter_fc", rownames(sf)),] # 

# plotlet function at bottom of script.
# This displays the species-level parameter estimates from the model
plotlet("b_force", "b_photo", # not structured as correlated in these data
#         xlim = c(-24, -23),
#         ylim = c(-18, -17),
        dat = sf)

plot(density(sf[grep("^a_sp\\[", rownames(sf)),'mean']),
     main = "Species level intercepts",
     col = "midnightblue")

plot(density(sf[grep("^b_force\\[", rownames(sf)),'mean']),
     main = "Species level forcing effects")

###### Posterior predictive checks -- 
# pull out coefficients at each level

# nlab = length(unique(fake$lab)) # 20 lab groups
nsp = length(unique(fake$sp)) # 100 species
ntot = 50
# Extracting fitted values from the stan fit object
chillcoef = sf[grep("^b_chill", rownames(sf)),'mean']
forcecoef = sf[grep("^b_force", rownames(sf)),'mean']
photocoef = sf[grep("^b_photo", rownames(sf)),'mean']
latcoef =  sf[grep("^b_lat", rownames(sf)),'mean']
# chillforce =  sf[grep("^b_inter_fc", rownames(sf)),'mean']
# chillphoto =  sf[grep("^b_inter_pc", rownames(sf)),'mean']
# chilllat =  sf[grep("^b_inter_lc", rownames(sf)),'mean']
# forcephoto =  sf[grep("^b_inter_fp", rownames(sf)),'mean']
# forcelat =  sf[grep("^b_inter_fl", rownames(sf)),'mean']
# photolat =  sf[grep("^b_inter_pl", rownames(sf)),'mean']

######## SD for each treatment
chillcoef.sd = sf[grep("^b_chill", rownames(sf)),'sd'] 
forcecoef.sd = sf[grep("^b_force", rownames(sf)),'sd']  
photocoef.sd = sf[grep("^b_photo", rownames(sf)),'sd'] 
latcoef.sd =  sf[grep("^b_lat", rownames(sf)),'sd'] 
# chillforce.sd = sf[grep("^b_inter_fc", rownames(sf)),'sd'] 
# chillphoto.sd = sf[grep("^b_inter_pc", rownames(sf)),'sd'] 
# chilllat.sd = sf[grep("^b_inter_lc", rownames(sf)),'sd'] 
# forcephoto.sd = sf[grep("^b_inter_fp", rownames(sf)),'sd'] 
# forcelat.sd = sf[grep("^b_inter_fl", rownames(sf)),'sd'] 
# photolat.sd = sf[grep("^b_inter_pl", rownames(sf)),'sd'] 


############ !
spint <- sf[grep("^a_sp\\[", rownames(sf)),'mean'] # Was centered on 80 in fake data generating, perfect here: mean(spint)

poster <- vector() # to hold the posterior predictive checks

for(i in 1:nsp){ # loop over species, as these are the random effect modeled. 
  # Within species, have a loop for individuals
  # i = 1
  
  # initialize with random normal values
  chill = rnorm(ntot, 0, 1)
  force = rnorm(ntot, 0, 1)
  photo = rnorm(ntot, 0, 1)
  lat = rnorm(ntot, 0, 1)
  
  # Set up the model matrix. This will have ntot rows (number of values per species in this case)
  # add ^2 to right side of parenthetical formula for two-way interactions
  mm <- model.matrix(~(chill+force+photo+lat), data.frame(chill, force, photo, lat))
  
  # For species i, pull out the model estimates for intercept, chilling, forcing, photoperiod, and latitude effects.
  coeff <- c(
      sf[rownames(sf) %in% paste("a_sp[", i, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_chill[", i, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_force[", i, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_photo[", i, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_lat[", i, "]", sep = ""),'mean']
  )
    
  # make posterior predictions for each value per species. Matrix multiplication %*% takes the initial random values and multiplies by the posterior estimates of our predictors in the coeff object
      bb <- rnorm(n = length(force), mean = mm %*% coeff, sd = 0.1)
    
      # Save the values for this species in a small data frame
      posterx <- data.frame(bb, sp = i, 
                        chill, force, photo, lat)
      
    # Put these together with the results for all species
    poster <- rbind(poster, posterx)  
}


# Should be on 1:1 line

plot(fake$bb, poster$bb,
     xlab = "Simulated data",
     ylab = "Posterior predictive check",
     xlim = c(0, 150),
     ylim = c(0, 150),
     pch = 16,
     col = alpha('midnightblue', 0.2)
     )
abline(a=0, b=1, lty=3)

identical(fake$sp, poster$sp)

fp <- data.frame(fake, posterior = poster$bb)
fp <- fp[!is.na(match(fp$sp, sample(1:100, 5))),] # take 5 random species to look at

# across species doing a very good job, within species still variable but ok
ggplot(fp,
       aes(bb, posterior, color = chill)) +
  facet_grid(.~sp) +
  geom_point() 


##################################################################################################################################################################################################


plotlet <- function(x, y, xlab=NULL, ylab=NULL, dat, groups = NULL, ...){
  if(is.null(xlab)) xlab = x; if(is.null(ylab)) ylab = y
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
  else {
    colz = c("brown", "blue3")
    ccolz = rep(colz[1], length(groups))
    ccolz[groups == 2] = colz[2]
    col.pch = ccolz
    col.lines = alpha(ccolz, 0.4)
  }
  
  plot(
    dat[grep(paste("^", x,"\\[",sep=""), rownames(dat)),1],
    dat[grep(paste("^", y,"\\[",sep=""), rownames(dat)),1],
    pch = "+",
    ylab = ylab,
    xlab = xlab,
    col = col.pch,
    ...
  )
  
  abline(h=0, lty = 3, col = "grey60")
  abline(v=0, lty = 3, col = "grey60")
  
  arrows(
    dat[grep(paste("^", x,"\\[",sep=""), rownames(dat)),"mean"],
    dat[grep(paste("^", y,"\\[",sep=""), rownames(dat)),"25%"],
    dat[grep(paste("^", x,"\\[",sep=""), rownames(dat)),"mean"],
    dat[grep(paste("^", y,"\\[",sep=""), rownames(dat)),"75%"],
    length = 0, col = col.lines)
  
  arrows(
    dat[grep(paste("^", x,"\\[",sep=""), rownames(dat)),"25%"],
    dat[grep(paste("^", y,"\\[",sep=""), rownames(dat)),"mean"],
    dat[grep(paste("^", x,"\\[",sep=""), rownames(dat)),"75%"],
    dat[grep(paste("^", y,"\\[",sep=""), rownames(dat)),"mean"],
    length = 0, col = col.lines)
  

}