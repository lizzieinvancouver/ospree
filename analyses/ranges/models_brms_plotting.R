### Started 22 April 2020 - Cat
## Building stan models using 90% cred intervals

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(viridis)
library(RColorBrewer)
library(rstan)
library(dplyr)
library(broom)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd("~/Documents/git/ospree/analyses/ranges/")

## load the model
#load("output/stan/m3l_ni_brms_testdata.Rda")
load("~/Desktop/m3l_ni_brms_testdata.Rdata")
#load("~/Desktop/m3l_ni_brms_realdata.Rdata")

#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
figpath <- "figures"
figpathmore <- "testdata_brms_popmodel" ### change based on model
modelhere <- modpop3.force

xlab <- "Model estimate of change in days to budburst (doy)"

#df <- read.csv("output/testdata_popmodels.csv")
df <- read.csv("~/Desktop/testing123.csv")
#df <- read.csv("~/Desktop/realdata.csv")

#sp <- unique(df$latbi)
sp <- unique(df$species)
nsp <- length(sp)
#pop <- unique(df$pophere)
pop <- unique(df$pop)
npop <- length(pop)

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
#my.pal <- viridis_pal(option="plasma")(nsp)
my.pal <- rep(brewer.pal(8, "Dark2"), (ceiling(nsp/8)))
my.pch <- rep(c(10:18), ceiling(npop/8))
alphahere = 0.4

force <- coef(modelhere, prob=c(0.10, 0.90))$species[, c(1, 3:4), 2] %>%
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

### Now to add on 50% cred intervals on top
force50 <- coef(modelhere, prob=c(0.25, 0.75))$species[, c(1, 3:4), 2] %>%
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

forcepop <- coef(modelhere, prob=c(0.10, 0.90))$`species:pop`[, c(1, 3:4), 2] %>%
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

### Now to add on 50% cred intervals on top
force50pop <- coef(modelhere, prob=c(0.25, 0.75))$`species:pop`[, c(1, 3:4), 2] %>%
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
forcepop$names <- rownames(forcepop)
force50pop$names <- rownames(force50pop)

mod.ranefpop <- left_join(force50pop, forcepop, by="names")
mod.ranefpop$names <- mod.ranefpop$parameter.y <- mod.ranefpop$mean.y <- NULL
colnames(mod.ranefpop) <- c("mean", "25%", "75%", "parameter", "10%", "90%")
mod.ranefpop <- rbind(mod.ranef, mod.ranefpop)
mod.ranefpop$term <- rep(c("force", "forcepop"), c(nsp, npop))
listofspps <- unique(df$species)
listofspsites <- unique(paste0(formatC(df$pop, width=2, flag="0"), df$species))
restofsps <- substr(listofspsites, 3, nchar(listofspsites))
popspecies<-sapply(1:length(restofsps),function(x)sum(restofsps[1:x]==restofsps[x]))
#popslist <- substr(listofspsites, 0, 2)
mod.ranefpop$species <- NA
mod.ranefpop$species <- as.vector(c(listofspps, restofsps))
mod.ranefpop$pop <- NA
mod.ranefpop$pop[(nsp+1):nrow(mod.ranefpop)] <- popspecies

modoutput <- tidy(modelhere, prob=c(0.9))
mod50 <- tidy(modelhere, prob=c(0.5))
names(mod50) <- c("term", "estimate", "std.error", "low50", "high50")
modoutput <- left_join(modoutput, mod50)
#quartz()
source("~/Documents/git/ospree/analyses/ranges/source/exp_muplot_brms90.R")

muplotfx(modelhere, "", 9, 8, c(0,2), c(-30, 10) , 11, 2) ## forcing


