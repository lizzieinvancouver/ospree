# Fake data testing of Ospree 
dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)

setwd("~/Documents/git/ospree")
source('stan/savestan.R')
# get latest .Rdata file

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("FakeOspree.RData")

# Thin the data, see if goes faster
fake <- fake[sample(1:nrow(fake), 999),]

datalist.f <- with(fake, 
    list(y = bb, 
         chill = as.numeric(chill), 
         force = as.numeric(force), 
         photo = as.numeric(photo),
         lat = as.numeric(lat),
         sp = as.numeric(sp),
         lab = as.numeric(lab),
         N = nrow(fake),
         n_sp = length(unique(sp)),
         n_lab = length(unique(lab))
         )
)
if(dostan){
  osp.f <- stan('stan/ospreeM1.stan', data = datalist.f, 
                 iter = 2882
                  ) 
  sf <- summary(osp.f)$summary
  
  ssm.f <- as.shinystan(osp.f)
  # launch_shinystan(ssm.f)
  savestan("Fake")
}

# Load lastest fake data output. Grep for both Fake and Stan Output.
if(!exists('osp.f')){
  
  fakes <- na.omit(grep("Stan Output", dir())[match(grep("Fake", dir()), grep("Stan Output", dir()))])

  load(sort(dir()[fakes], T)[1])
}

launch_shinystan(ssm.f) 

sf[grep("^a_sp\\[", rownames(sf)),"mean"] # 
sf[grep("^a_0", rownames(sf)),] # 

summary(sf[grep("^b_chill_sp\\[", rownames(sf)),"mean"] ) # this is correct, near -3
sf[grep("^b_chill_0", rownames(sf)),] # 

summary(sf[grep("^b_force_sp\\[", rownames(sf)),"mean"]) # correct, near -2
sf[grep("^b_force_0", rownames(sf)),] # 
sf[grep("^mu_b_force_sp\\[", rownames(sf)),"mean"] # near 0

summary(sf[grep("^b_photo_sp\\[", rownames(sf)),"mean"] ) # correct, near -1
sf[grep("^b_photo_0", rownames(sf)),] # 

summary(sf[grep("^b_inter_fc_sp\\[", rownames(sf)),"mean"] )# correct, near 0.5
sf[grep("^b_inter_fc_0", rownames(sf)),] # 

plotlet("b_force_sp", "b_photo_sp", # not structured as correlated in these data
#         xlim = c(-24, -23),
#         ylim = c(-18, -17),
        dat = sf)

plot(density(sf[grep("^a_sp\\[", rownames(sf)),'mean']),
     main = "Species level intercepts",
     col = "midnightblue")

plot(density(sf[grep("^b_force_sp\\[", rownames(sf)),'mean']),
     main = "Species level forcing effects")



###### Posterior predictive checks -- 
# pull out coefficients at each level

nlab = length(unique(fake$lab)) # 20 lab groups
nsp = length(unique(fake$sp)) # 100 species
ntot = 
# Extracting fitted values from the stan fit object
chillcoef = sf[grep("^b_chill_0", rownames(sf)),'mean']
forcecoef = sf[grep("^b_force_0", rownames(sf)),'mean']
photocoef = sf[grep("^b_photo_0", rownames(sf)),'mean']
latcoef =  sf[grep("^b_lat_0", rownames(sf)),'mean']
chillforce =  sf[grep("^b_inter_fc_0", rownames(sf)),'mean']
chillphoto =  sf[grep("^b_inter_pc_0", rownames(sf)),'mean']
chilllat =  sf[grep("^b_inter_lc_0", rownames(sf)),'mean']
forcephoto =  sf[grep("^b_inter_fp_0", rownames(sf)),'mean']
forcelat =  sf[grep("^b_inter_fl_0", rownames(sf)),'mean']
photolat =  sf[grep("^b_inter_pl_0", rownames(sf)),'mean']

######## SD for each treatment
chillcoef.sd = sf[grep("^b_chill_0", rownames(sf)),'sd'] 
forcecoef.sd = sf[grep("^b_force_0", rownames(sf)),'sd']  
photocoef.sd = sf[grep("^b_photo_0", rownames(sf)),'sd'] 
latcoef.sd =  sf[grep("^b_lat_0", rownames(sf)),'sd'] 
chillforce.sd = sf[grep("^b_inter_fc_0", rownames(sf)),'sd'] 
chillphoto.sd = sf[grep("^b_inter_pc_0", rownames(sf)),'sd'] 
chilllat.sd = sf[grep("^b_inter_lc_0", rownames(sf)),'sd'] 
forcephoto.sd = sf[grep("^b_inter_fp_0", rownames(sf)),'sd'] 
forcelat.sd = sf[grep("^b_inter_fl_0", rownames(sf)),'sd'] 
photolat.sd = sf[grep("^b_inter_pl_0", rownames(sf)),'sd'] 


############ !
spint <- sf[grep("^a_sp\\[", rownames(sf)),'mean'] # Was centered on 80 in fake data generating, now on 40

poster <- vector() # to hold the posterior predictive checks

for(i in 1:nsp){ # loop over species, as these are the random effect modeled. 
  # Within species, have a loop for individuals
  
  indx <- which(splookup == i)
  
  force = rnorm(ntot, 0, 1)
  photo = rnorm(ntot, 0, 1)
  chill = rnorm(ntot, 0, 1)
  lat = rnorm(ntot, 0, 1)
  
  mm <- model.matrix(~(chill+force+photo+lat)^2, data.frame(chill, force, photo, lat))
  
  coeff <- data.frame(
      sf[rownames(sf) %in% paste("a_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_chill_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_force_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_photo_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_inter_ws_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_inter_ps_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_inter_wp_sp_ind[", indx, "]", sep = ""),'mean']
  )
    
    for(j in 1:nind){ # simulate data for each individual, using these coefficients
      
      bb <- rnorm(n = length(force), mean = mm %*% as.numeric(coeff[j,]), sd = 0.1)
    
      
      posterx <- data.frame(bb, sp = i, ind = paste(i, j, sep="_"),
                        chill, force, photo)
    
    poster <- rbind(poster, posterx)  
  }
}


plot(fake$bb, poster$bb,
     xlab = "Simulated data",
     ylab = "Posterior predictive check",
     xlim = c(-10, 90),
     ylim = c(-10, 90),
     pch = 16,
     col = alpha('midnightblue', 0.2)
     )
abline(a=0, b=1, lty=3)



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