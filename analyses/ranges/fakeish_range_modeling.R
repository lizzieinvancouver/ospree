### a sample code for modeling models
##Overview, extract many posterior sample and model them as a function of predictors of interests
##started by Dan B on 14 Nov 2019 based on Lizzie's corr_posteriors.R 
rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()


library(rstan)
library(rstanarm)
library(reshape2)
library(plyr)
library(dplyr)
set.seed(73)
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")


nsp = 30 # number of species

ntot = 50 # numbers of obs per species. 

#  with species  (note to self: This is not the best, better to draw from a distribution)

#  with species  (note to self: This is not the best, better to draw from a distribution)
intermean <- 30 # mean for selecting intercept (days to BB) across all species
intersd <- 3 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)  # different intercepts by species

# now start building ...
testdat2 <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) only 2-way interactions between  force + photo + chill
# (d) each obs is a different set of treatments

# and some important points ...
# (z) the below draws treatments from distribution in such a way that there is a lot more variation than we have


for(i in 1:nsp){ # loop over species. i = 1
  
  # continuous predictors, generate level (if you will) for each observation
  force = rnorm(ntot, 0, 2)
  photo = rnorm(ntot, 0, 2)
  chill = rnorm(ntot, 0, 5)
  
  # set up effect sizes
  chillcoef = -5 # steep slope for earlier day with higher chilling
  forcecoef = -3 # less steep for forcing
  photocoef = -1
  
  # SD for each treatment
  chillcoef.sd = 3
  forcecoef.sd = 1 
  photocoef.sd = 0.5
  
  # set interaction effects. 3 two-way interactions

  
  # build model matrix 
  mm <- model.matrix(~(chill+force+photo), data.frame(chill, force, photo))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo
  coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd)
  )
  
  bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx2 <- data.frame(bb, sp = i, 
                          chill, force, photo)
  
  testdat2 <- rbind(testdat2, testdatx2)  
}

sapply(testdat2,class)

datalist.test <- with(testdat2, 
                    list(y = bb, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = sp,
                         N = as.numeric(nrow(testdat2)),
                         n_sp = length(unique(testdat2$sp))
                    )
)


test.mod=stan("stan/nointer_2level.stan",data=datalist.test,iter=4000,warmup = 3000)
summary(test.mod) #good### paramenter values

##now estract paramenter parements
sample.l <- rstan::extract(test.mod)

sample.lo.warm <- melt(sample.l$b_force)

sample.lo.chill <- melt(sample.l$b_chill)

names(sample.lo.warm) <- c("iter", "sp", "b_force")
names(sample.lo.chill) <- c("iter", "sp", "b_chill")

lo.df <- left_join(sample.lo.warm, sample.lo.chill)
lo.df1K <- subset(lo.df, iter>3800)### probably you should use the actualy samplint iters, but to make model smaller ill subsety

###now make a new data set about range size 

meanbeata<-lo.df1K %>% group_by(sp) %>% dplyr::summarise(mean.chill=mean(b_chill))
### this is the mean estimate for each species
eff_chill<--10
alpha<-0
sigma<-0.01
df2<-data.frame(sdchill=numeric(),sp=factor()) 
for (i in seq_along(meanbeata$sp)){
  y<-eff_chill*meanbeata$mean.chill[i]+alpha
  
  dfhere2 <- data.frame(sdrangechill=rnorm(length(y),y,sigma),sp=meanbeata$sp[i])
  
  df2 <- rbind(df2, dfhere2) ## rbind it here for safty
}

# now lets combind our two data sheets
rangey<-merge(lo.df1K,df2,sort=FALSE)
?merge()
rangey<-dplyr::select(rangey,sp,iter,b_chill,sdrangechill)
head(rangey)
tail(rangey)

## so I think essentiall the model runs like this but baysian. we wont be able to extract the exact
#params as fromulated because they rangesd is based on means with a very small sd
#but general trend should hold.

mod<-lm(b_chill~sdrangechill, data=rangey)
summary(mod)
