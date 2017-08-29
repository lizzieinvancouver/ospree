library(lme4)
summary(lme1 <- lmer(bb ~ chill+force+photo +lat+ (1|sp), data = testdat2)) 
ranef(lme1)
fixef(lme1)
#head(testdat)
#head(list.coeffs)

##
# try the model
datalist.td <- with(testdat2, 
                    list(y = bb, 
                         chill = as.numeric(chill), 
                         force = as.numeric(force), 
                         photo = as.numeric(photo),
                         lat= as.numeric(lat),
                         sp = as.numeric(sp),
                         N = nrow(testdat2),
                         n_sp = length(unique(sp))
                    )
)



## running model with fake data, centered no interactions
library(rstan)
osp.td2.fake = stan("stan/lat/LAT_daysBBnointer_2level.stan", data = datalist.td, 
               iter = 2000,warmup=1500,control=list(adapt_delta=0.90)) 
###504 divergent transitions Yikes. Data is probably bad.

### try with interactions
datalist.td2 <- with(testdat3, 
                    list(y = bb, 
                         chill = as.numeric(chill), 
                         force = as.numeric(force), 
                         photo = as.numeric(photo),
                         lat= as.numeric(lat),
                         sp = as.numeric(sp),
                         N = nrow(testdat3),
                         n_sp = length(unique(sp))
                    )
)

###### fake data- uncentered. interactions
osp.td4.fake = stan('stan/lat/LAT_daysBBwinter_2level.stan', data = datalist.td2,
               iter = 4000,warmup=3000,control=list(adapt_delta=0.99)) 
