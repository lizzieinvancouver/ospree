###models Stan exp+ramp. This is R code to model OSPREE bud burst data with both experimental and ramped data.
##Began 27-Nov-2018 by Dan B

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
use.allphoto=FALSE

source("source/bbstanleadin.R")

use.zscore = TRUE 
####This code is meant to model with ramped and exp photo and ramped and exp forcing. So, if testing all species the data should be
#bb.stan
######Lizzie already ran 6 models with all species and species complex
###my models should be with spcomplex.onecue species; spcomplex.nocrops

##check things
unique(bb.stan.onecue$complex.wname) #41
unique(bb.stan$complex.wname) #38
setdiff(bb.stan$complex.wname,bb.stan.onecue$complex.wname)

sort(unique(bb.stan.nocrops$complex.wname))#35
sort(unique(bb.stan.nocrops$complex))#35 no 5 or 19

#goo<-dplyr::select(bb.stan.nocrops, complex,complex.wname)
#goob<-as.data.frame(unique(goo[,c('complex','complex.wname')]))
#intersect(bb.stan.nocrops$complex.wname,goob$complex.wname)


if(use.zscore){
  datalist.bb.onecue <- with(bb.stan.onecue, 
                      list(y = resp, 
                           chill = chill.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan.onecue),
                           n_sp = length(unique(bb.stan.onecue$complex))
                      )
  )

datalist.bb.nocrops <- with(bb.stan.nocrops, 
                           list(y = resp, 
                                chill = chill.z, 
                                force = force.z, 
                                photo = photo.z,
                                sp = complex,
                                N = nrow(bb.stan.nocrops),
                                n_sp = length(unique(bb.stan.nocrops$complex))
                           )
)
}
########################################################
# real data on 2 level model (sp) with no interactions 
# Note the notation: nointer_2level.stan: m2l.ni
########################################################
m2l.ni=stan('stan/nointer_2level.stan', data = datalist.bb.onecue,
             iter = 2500, warmup=1500)

m2l.ni2= stan('stan/nointer_2level.stan', data = datalist.bb.nocrops,
             iter = 2500, warmup=1500)

check_all_diagnostics(m2l.ni)
check_all_diagnostics(m2l.ni2)
#launch_shinystan(m2l.)


m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),]
m2lni.sum[grep("sigma_", rownames(m2lni.sum)),]
speffs<-c(m2lni.sum[grep("a_sp", rownames(m2lni.sum)),1][3:43],
          m2lni.sum[grep("b_chill", rownames(m2lni.sum)),1][3:43],
          m2lni.sum[grep("b_photo", rownames(m2lni.sum)),1][3:43],
          m2lni.sum[grep("b_force", rownames(m2lni.sum)),1][3:43])

write.csv(speffs,"~/Desktop/quick.csv")

m2lni2.sum <- summary(m2l.ni2)$summary
m2lni2.sum[grep("mu_", rownames(m2lni2.sum)),]
m2lni2.sum[grep("alpha", rownames(m2lni2.sum)),]
write.csv(m2lni2.sum, "~/Desktop/quick4.csv")







nrow(bb.stan.onecue)#1953
length(unique(bb.stan.onecue$complex))#41
length(unique(bb.stan.onecue$datasetID))#38
nrow(bb.stan.nocrops)#1731
length(unique(bb.stan.nocrops$complex))#35
length(unique(bb.stan.nocrops$datasetID))#30

########################################################
# real data on 2 level model (sp) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: winternosp_2level.stan: m2l.winsp
########################################################

m2l.winsp = stan('stan/winternosp_2level.stan', data = datalist.bb.onecue,
                 iter = 4000, warmup=2500)

m2l.winsp2 = stan('stan/winternosp_2level.stan', data = datalist.bb.nocrops,
                 iter = 4000, warmup=2500) 

check_all_diagnostics(m2l.winsp)
check_all_diagnostics(m2l.winsp2)

m2l.winsp.sum <- summary(m2l.winsp)$summary 
m2l.winsp.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
                "b_cf","b_cp","b_fp"),]

m2l.winsp.sum[grep("mu_", rownames(m2l.winsp.sum)),]
m2l.winsp.sum[grep("sigma_", rownames(m2l.winsp.sum)),]
speffs2<-c(m2l.winsp.sum[grep("a_sp", rownames(m2l.winsp.sum)),1][3:43],
          m2l.winsp.sum[grep("b_chill", rownames(m2l.winsp.sum)),1][3:43],
          m2l.winsp.sum[grep("b_photo", rownames(m2l.winsp.sum)),1][3:43],
          m2l.winsp.sum[grep("b_force", rownames(m2l.winsp.sum)),1][3:43])
write.csv(speffs2, "~/Desktop/quick2.csv")

m2l.winsp.sum2 <- summary(m2l.winsp2)$summary 
m2l.winsp.sum2[grep("mu_", rownames(m2l.winsp.sum2)),]
m2l.winsp.sum2[grep("sigma_", rownames(m2l.winsp.sum2)),]
 write.csv(m2l.winsp.sum2, "~/Desktop/quick5.csv")

########################################################
# real data on 2 level model (sp and study) with 2 two-way interactions but no partial pooling on interactions
# Note the notation: nointer_2level_studyint: m2l.wstudy
########################################################
 if(use.zscore){
datalist.bb.onecue2 <- with(bb.stan.onecue, 
                    list(y = resp, 
                         chill = chill.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         study = as.numeric(as.factor(bb.stan.onecue$datasetID)),
                         N = nrow(bb.stan.onecue),
                         n_sp = length(unique(bb.stan.onecue$complex)),
                         n_study = length(unique(bb.stan.onecue$datasetID))
                    )
)

datalist.bb.nocrops2 <- with(bb.stan.nocrops, 
                            list(y = resp, 
                                 chill = chill.z, 
                                 force = force.z, 
                                 photo = photo.z,
                                 sp = complex,
                                 study = as.numeric(as.factor(bb.stan.nocrops$datasetID)),
                                 N = nrow(bb.stan.nocrops),
                                 n_sp = length(unique(bb.stan.nocrops$complex)),
                                 n_study = length(unique(bb.stan.nocrops$datasetID))
                            )
                            )
}
##########

 m2l.wstudy = stan('stan/nointer_2level_studyint_ncp.stan', data = datalist.bb.onecue2, 
                    iter = 5000, warmup=3500) 

 m2l.wstudy2 = stan('stan/nointer_2level_studyint_ncp.stan', data = datalist.bb.nocrops2, 
                    iter = 5000, warmup=3500) 

check_all_diagnostics( m2l.wstudy2)

m2l.wstudy2.sum2 <- summary(m2l.wstudy2)$summary 
m2l.wstudy2.sum2[grep("mu_", rownames(m2l.wstudy2.sum2)),]
m2l.wstudy2.sum2[grep("sigma_", rownames(m2l.wstudy2.sum2)),]
write.csv(m2l.wstudy2.sum2, "~/Desktop/quick6.csv")


