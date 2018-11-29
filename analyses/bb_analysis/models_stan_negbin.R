## Started 20 November 2018 ##
## By Lizzie ##

## Based off models_stan.R ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
  }else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# dostan = TRUE
# Flags to choose for bbstanleadin.R
use.chillunits = FALSE # change to true for testing chill units
use.allspp = FALSE
use.allphoto = FALSE
source("source/bbstanleadin.R")

# Flags to choose for this here file
use.zscore = TRUE # change to TRUE to use centered and scaled data

########################
## Z-scored data here ##
########################
#write.csv(bb.stan.nocrops, "~/Desktop/nocrops_nointer.csv", row.names=FALSE)

if(use.zscore){
datalist.bb <- with(bb.stan.nocrops, 
                    list(y = round(resp), # for integers!
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan.nocrops),
                         n_sp = length(unique(bb.stan.nocrops$complex))
                    )
)
}



########################
## ALERT! Below is just starter code ...
## Please generally follow models_stan_negbin.R
## Below is just some code from Lizzie if useful....
########################

m2l.winsp.nb = stan('stan/nointer_2level_negbin.stan', data = datalist.bb,
               iter = 4000, warmup=2500) 


m2l.nb.sum <- summary(m2l.winsp.nb )$summary
m2l.nb.sum[grep("mu_", rownames(m2l.nb.sum)),]

y_pred <- extract(m2l.winsp.nb, 'y_ppc')
par(mfrow=c(1,2))
hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="")
hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="Group intercept only")


########################
## ALERT! Below is just starter code ...
## Please generally follow models_stan_negbin.R
## Below is just some code from Lizzie if useful....
########################
m2l.winsp = stan('stan/winternosp_2level_negbin.stan', data = datalist.bb,
                 iter = 4000, warmup=2500) 


m2l.winsp.sum <- summary(m2l.winsp.nb)$summary 
#d<-as.data.frame(m2l.winsp.sum)
#write.csv(d, file="~/Desktop/output.csv", row.names=TRUE)
m2l.winsp.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
                "b_cf","b_cp","b_fp"),]
#y_pred <- extract(m2l.winsp, 'y_ppc')
#y_pred<- as.vector(y_pred$y_ppc)
#launch_shinystan(m2l.winsp)
y_pred

# PPC 
if(FALSE){
  y_pred <- extract(m2l.winsp, 'y_ppc')
  par(mfrow=c(1,2))
  hist(bb.stan$response.time, breaks=40, xlab="real data response time", main="")
  hist(y_pred[[1]][1,], breaks=40, xlab="PPC response time", main="With intxn model")
}

check_all_diagnostics(m2l.winsp)
# launch_shinystan(m2l.winsp)

