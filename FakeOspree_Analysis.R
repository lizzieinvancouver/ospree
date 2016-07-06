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
  osp.f <- stan('stan/ospree1.stan', data = datalist.f, 
                 iter = 3883
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

sf[grep("^a_sp\\[", rownames(sf)),] # 
sf[grep("^a_sp\\[", rownames(sf)),] # 
sf[grep("^a_0", rownames(sf)),] # 

sf[grep("^b_force_sp\\[", rownames(sf)),] # 
sf[grep("^b_force_0", rownames(sf)),] # 
sf[grep("^mu_b_force_sp\\[", rownames(sf)),] # 0

sf[grep("^b_photo_sp\\[", rownames(sf)),] # 
sf[grep("^b_photo_0", rownames(sf)),] # 

sf[grep("^b_chill_sp\\[", rownames(sf)),] #
sf[grep("^b_chill_0", rownames(sf)),] # 

sf[grep("^b_inter_fc_sp\\[", rownames(sf)),] # 
sf[grep("^b_inter_fc_0", rownames(sf)),] # 

plot(sf['b_force_0','mean'], sf['b_chill_0','mean'])

plotlet("b_force_sp", "b_photo_sp", 
#         xlim = c(-24, -23),
#         ylim = c(-18, -17),
        dat = sf)

plot(density(sf[grep("^a_sp\\[", rownames(sf)),'mean']),
     main = "Species level intercepts",
     col = "midnightblue")

plot(density(sf[grep("^b_force_sp\\[", rownames(sf)),'mean']),
     main = "Species level forcing effects")




###### Posterior predictive checks -- Todo. Below is based on bud burst experiment checks.
# pull out coefficients at each level

nlab = length(unique(fake$lab)) # 20 lab groups
nsp = length(unique(fake$sp)) # 100 species

# (ntot = nsite*nwarm*nphoto) # 8 rows. But will be looping over individuals and species below
# 
# # Build up the data frame
# site = gl(nsite, rep, length = ntot)

warm = gl(nwarm, rep*nsite, length = ntot)
photo = gl(nphoto, rep*nsite*nwarm, length = ntot)

treatcombo = paste(warm, photo, sep = "_")

(d <- data.frame(site, warm, photo, treatcombo))

# Extracting fitted values from the stan fit object
sitediff = sf[grep("^b_site_0", rownames(sf)),'mean']
warmdiff = sf[grep("^b_warm_0", rownames(sf)),'mean']
photodiff = sf[grep("^b_photo_0", rownames(sf)),'mean']

# interactions. 9 two-way interactions
sitewarm = sf[grep("^b_inter_ws_0", rownames(sf)),'mean']
sitephoto = sf[grep("^b_inter_ps_0", rownames(sf)),'mean']
warmphoto = sf[grep("^b_inter_wp_0", rownames(sf)),'mean']

######## SD for each treatment
sitediff.sd = sf[grep("^b_site_0", rownames(sf)),'sd'] 
warmdiff.sd = sf[grep("^b_warm_0", rownames(sf)),'sd']  
photodiff.sd = sf[grep("^b_photo_0", rownames(sf)),'sd'] 
sitewarm.sd = sf[grep("^b_inter_ws_0", rownames(sf)),'sd'] 
sitephoto.sd = sf[grep("^b_inter_ps_0", rownames(sf)),'sd'] 
warmphoto.sd = sf[grep("^b_inter_wp_0", rownames(sf)),'sd'] 

mm <- model.matrix(~(site+warm+photo)^2, data.frame(warm, photo))

############ !
spint <- sf[grep("^a_sp\\[", rownames(sf)),'mean'] # Was centered on 35 in fake data, why now 61?

poster <- vector() # to hold the posterior predictive checks

for(i in 1:nsp){ # loop over species, as these are the random effect modeled. 
  # Within species, have a loop for individuals
  
  indx <- which(splookup == i)
  
  coeff <- data.frame(
      sf[rownames(sf) %in% paste("a_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_site_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_warm_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_photo_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_inter_ws_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_inter_ps_sp_ind[", indx, "]", sep = ""),'mean'],
      sf[rownames(sf) %in% paste("b_inter_wp_sp_ind[", indx, "]", sep = ""),'mean']
  )
    
    for(j in 1:nind){ # simulate data for each individual, using these coefficients
      
      bb <- rnorm(n = length(warm), mean = mm %*% as.numeric(coeff[j,]), sd = 0.1)
    
      
      posterx <- data.frame(bb, sp = i, ind = paste(i, j, sep="_"),
                        site, warm, photo)
    
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