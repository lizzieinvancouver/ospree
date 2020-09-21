### Started 22 April 2020 - Cat
## Building stan models using 90% cred intervals

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(viridis)
library(rstan)
library(dplyr)
library(broom)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/chillfreeze/analyses")

## load the model
load("output/stan/m3l_ni_brms_testdata.Rda")

#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "testdata_brms_popmodel" ### change based on model
modelhere <- modpop3.force

xlab <- "Model estimate of change in days to budburst (doy)"

df <- read.csv("output/testdata_popmodels.csv")

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- viridis_pal(option="plasma")(10)
my.pch <- rep(10:18, each=10)
alphahere = 0.4

sp <- unique(df$species)
npop <- unique(df$pop)

force <- coef(modelhere, prob=c(0.10, 0.90))$species[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`10%` = Q10) %>%
  rename(`90%` = Q90) %>%
  dplyr::select(mean, `10%`, `90%`) 
new.names<-NULL
for(i in 1:length(sp)){
  new.names[i]<-paste("force", "[", i, "]", sep="")
}
force$parameter<-new.names

mod.ranef <- force

### Now to add on 50% cred intervals on top
force50 <- coef(modelhere, prob=c(0.25, 0.75))$species[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(sp)){
  new.names[i]<-paste("force", "[", i, "]", sep="")
}
force50$parameter<-new.names

forcepop <- coef(modelhere, prob=c(0.10, 0.90))$pop[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`10%` = Q10) %>%
  rename(`90%` = Q90) %>%
  dplyr::select(mean, `10%`, `90%`) 
new.names<-NULL
for(i in 1:length(npop)){
  new.names[i]<-paste("force", "[", i, "]", sep="")
}
forcepop$parameter<-new.names

mod.ranefpop <- forcepop

### Now to add on 50% cred intervals on top
force50pop <- coef(modelhere, prob=c(0.25, 0.75))$`species:pop`[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(npop)){
  new.names[i]<-paste("force", "[", i, "]", sep="")
}
force50pop$parameter<-new.names

mod.ranefpop <- left_join(mod.ranefpop, force50pop)

samples1 <- posterior_samples(modelhere)
head(samples1)
modoutput <- tidy(modelhere, prob=c(0.9))
mod50 <- tidy(modelhere, prob=c(0.5))
names(mod50) <- c("term", "estimate", "std.error", "low50", "high50")
modoutput <- left_join(modoutput, mod50)
#quartz()
source("~/Documents/git/ospree/analyses/ranges/source/exp_muplot_brms90.R")

muplotfx(modelhere, "", 8, 8, c(0,5), c(-15, 15) , 16, 3.5) ## forcing

