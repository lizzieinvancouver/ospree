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
setwd("~/Documents/git/ospree/analyses/ranges/")

## load the model
#load("output/stan/m3l_ni_brms_testdata.Rda")
#load("~/Desktop/m3l_ni_brms_testdata.Rdata")
load("~/Desktop/m3l_ni_brms_realdata.Rdata")

#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "testdata_brms_popmodel" ### change based on model
modelhere <- modpop3.force.real

xlab <- "Model estimate of change in days to budburst (doy)"

#df <- read.csv("output/testdata_popmodels.csv")
#df <- read.csv("~/Desktop/testing123.csv")
df <- read.csv("~/Desktop/realdata.csv")

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- viridis_pal(option="plasma")(10)
my.pch <- c(10:18)
alphahere = 0.4

sp <- unique(df$latbi)
npop <- unique(df$pophere)

force <- coef(modelhere, prob=c(0.10, 0.90))$latbi[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  dplyr::rename(mean = Estimate) %>%
  dplyr::rename(`10%` = Q10) %>%
  dplyr::rename(`90%` = Q90) %>%
  dplyr::select(mean, `10%`, `90%`) 
new.names<-NULL
for(i in 1:length(sp)){
  new.names[i]<-paste("force", "[", i, "]", sep="")
}
force$parameter<-new.names

mod.ranef <- force

### Now to add on 50% cred intervals on top
force50 <- coef(modelhere, prob=c(0.25, 0.75))$latbi[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  dplyr::rename(mean = Estimate) %>%
  dplyr::rename(`25%` = Q25) %>%
  dplyr::rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(sp)){
  new.names[i]<-paste("force", "[", i, "]", sep="")
}
force50$parameter<-new.names

mod.ranef <- left_join(force, force50)

forcepop <- coef(modelhere, prob=c(0.10, 0.90))$`latbi:pophere`[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  dplyr::rename(mean = Estimate) %>%
  dplyr::rename(`10%` = Q10) %>%
  dplyr::rename(`90%` = Q90) %>%
  dplyr::select(mean, `10%`, `90%`) 
new.names<-NULL
for(i in 1:length(npop)){
  new.names[i]<-paste("forcepop", "[", i, "]", sep="")
}
forcepop$parameter<-new.names

mod.ranefpop <- forcepop

### Now to add on 50% cred intervals on top
force50pop <- coef(modelhere, prob=c(0.25, 0.75))$`latbi:pophere`[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  dplyr::rename(mean = Estimate) %>%
  dplyr::rename(`25%` = Q25) %>%
  dplyr::rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(npop)){
  new.names[i]<-paste("forcepop", "[", i, "]", sep="")
}
force50pop$parameter<-new.names

mod.ranefpop <- left_join(mod.ranefpop, force50pop)
mod.ranefpop <- rbind(mod.ranef, mod.ranefpop)
mod.ranefpop$term <- rep(c("force", "forcepop"), c(10, 80))
mod.ranefpop$species <- NA
mod.ranefpop$species <- ifelse(mod.ranefpop$term=="force", 
                               rep(c("a","b","c","d","e","f","g","h","i","j"), 10), mod.ranefpop$species)
mod.ranefpop$species[11:90] <- rep(c("a","b","c","d","e","f","g","h","i","j"), each=8)
mod.ranefpop$pop <- NA
mod.ranefpop$pop[11:90] <- rep(c("A","B","C","D","E","F","G","H"), 10)

modoutput <- tidy(modelhere, prob=c(0.9))
mod50 <- tidy(modelhere, prob=c(0.5))
names(mod50) <- c("term", "estimate", "std.error", "low50", "high50")
modoutput <- left_join(modoutput, mod50)
#quartz()
source("~/Documents/git/ospree/analyses/ranges/source/exp_muplot_brms90.R")

muplotfx(modelhere, "", 8, 8, c(0,1), c(-30, 15) , 16, 1) ## forcing


