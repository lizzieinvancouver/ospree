## Started 6 July 2016 ##
## By Lizzie, and Dan and others ##

## Try to run REAL Ospree data ##
## With Stan! ##

## Take 2: February 2017! ##
## Take 3: July 2017! ## Nacho's clean code to run stan models on Ospree
############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)
library(brms)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

setwd("~/Documents/git/ospree/analyses")
source('stan/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


######################## OLD CODE, SKIP###############################
#### get the data
#source("bb_analysis/source/bbdataplease.R")
#source("bb_analysis/source/speciescomplex.R")
###below is covered in above source##########
#bb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)
#taxon <- read.csv("output/bb_analysis/taxon/complex_levels.csv", header=TRUE)
## Old code to remove Olea, probably the new subset of species is getting rid of it 
#bb[bb$genus=="Olea",]
#bb<-subset(bb,genus!="Olea")
## subsetting for experimental chilling
#bb<-subset(bb,!is.na(as.numeric(chilltemp)))
# merge in labgroup (we could do this elsewhere someday)
#bb.wlab <- merge(bb, taxon, by=c("genus","species"), all.x=TRUE)

######Above is all old code. Below is what formats the data properly in accordance with other models
source("lat_analysis/source/bbdataplease.R")
## (2) Deal with species
dim(bb.noNA)
d <- bb.noNA
source("bb_analysis/source/speciescomplex.R")
bb.noNA.wtaxa <- d
dim(bb.noNA.wtaxa)
unique(bb.noNA.wtaxa$complex)
bb.wlab<-bb.noNA.wtaxa

###filter for species of interest
tt <- table(bb.wlab$complex)
bb.wlab <- subset(bb.wlab, complex %in% names(tt[tt > 100])) 
#myspp<-c("Betula_pendula", "Betula_pubscens", "Fagus_sylvatica", "Picea_abies",
         #"Ribes_nigrum", "Corylus_avellana", "Quercus_robur", "Larix_decidua")
#bb.wlab<-dplyr::filter(bb.noNA.wtaxa, complex%in%myspp)

####below is handled in source i think
columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
                   "photoperiod_day", "response", "response.time", "Total_Chilling_Hours",
                   "complex", "provenance.lat")

bb.wlab.sm <- bb.wlab
unique(bb.wlab.sm$complex)
table(bb.wlab.sm$complex)
myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica", "Picea_abies", "Picea_glauca",
         "Pseudotsuga_menziesii", "Ribes_nigrum")
bb.wlab.sm<-dplyr::filter(bb.wlab.sm, complex%in%myspp)

## make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$chill <- as.numeric(bb.wlab.sm$Total_Chilling_Hours)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)
bb.wlab.sm$lat<-as.numeric(bb.wlab.sm$provenance.lat)

## In case we decide to center data, not doing it for now
#bb.wlab.sm$cphoto <- scale(bb.wlab.sm$photo, center=TRUE, scale=FALSE)
#bb.wlab.sm$cforce <- scale(bb.wlab.sm$force, center=TRUE, scale=FALSE)
#bb.wlab.sm$cchill <- scale(bb.wlab.sm$chill, center=TRUE, scale=FALSE)
#bb.wlab.sm$clat<- scale(bb.wlab.sm$lat, center=TRUE, scale=FALSE)


## subsetting data, preparing genus variable, removing NAs
ospr.prepdata <- subset(bb.wlab.sm, select=c("resp", "chill", "photo", "force", "complex", "lat"))
dim(subset(bb.wlab.sm, is.na(chill)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE 
           & is.na(lat)==FALSE))
ospr.stan <- ospr.prepdata[complete.cases(ospr.prepdata),]
ospr.stan$complex <- as.numeric(as.factor(ospr.stan$complex))

#lat.stan<-stan_glmer(resp~chill+photo+force+lat+(1|complex), data=ospr.stan) - ugly pp_checks
ospr.stan$resp<-as.integer(ospr.stan$resp+1)
ospr.stan<-ospr.stan[(ospr.stan$resp!=1000),]

lat.pois<-stan_glmer(resp~chill+photo+force+lat+(1|complex), data=ospr.stan, family=poisson)

ospr.stan$sm.chill<-ospr.stan$chill/240

lat.pois<-stan_glmer(resp~sm.chill+photo+force+lat+(1|complex), data=ospr.stan, family=poisson)
lat.pois.oneinter<-stan_glmer(resp~sm.chill+photo+force+lat+photo:lat+(1|complex), data=ospr.stan, family=poisson)

lat.brm.inter<-brm(resp~sm.chill+photo+force+lat+photo:lat+(1|complex), data=ospr.stan, family=poisson)

lat.pois.oneinter<-stan_glmer(resp~sm.chill+photo+force+lat+photo:lat+(1|complex)+
                                (sm.chill-1|complex)+(photo-1|complex)+
                                (force-1|complex)+(lat-1|complex)+
                                (photo:lat-1|complex), data=ospr.stan, family=neg_binomial_2)

lat.brm<-brm(resp~sm.chill+photo+force+lat+photo:lat+(1|complex)+
                                (sm.chill-1|complex)+(photo-1|complex)+
                                (force-1|complex)+(lat-1|complex)+
                                (photo:lat-1|complex), data=ospr.stan, family=negbinomial, chains=2)

m<-mod_rate 
# This is an Rstanarm object, and so behaves differently than th previous brms object. Thus the coeffs have to be extracted 
#differently.  There's probably a better way to do this, but I already had this code written up for something else, so used it again.
sum.m <-
  summary(
    m,
    pars = c(
      "(Intercept)",
      "origin",
      "strat",
      "temp1",
      "temp2",
      "temp3",
      "origin:strat",
      "origin:temp1",
      "origin:temp2",
      "origin:temp3",
      "strat:temp1",
      "strat:temp2",
      "strat:temp3",
      "origin:strat:temp1",
      "origin:strat:temp2",
      "origin:strat:temp3"
      ,
      "b[origin sp:1]",
      "b[strat sp:1]",
      "b[temp1 sp:1]",
      "b[temp2 sp:1]",
      "b[temp3 sp:1]",
      "b[origin:strat sp:1]",
      "b[origin:temp1 sp:1]",
      "b[origin:temp2 sp:1]",
      "b[origin:temp3 sp:1]",
      "b[strat:temp1 sp:1]",
      "b[strat:temp2 sp:1]",
      "b[strat:temp3 sp:1]",
      "b[origin:strat:temp1 sp:1]",
      "b[origin:strat:temp2 sp:1]",
      "b[origin:strat:temp3 sp:1]"
      ,
      "b[origin sp:2]",
      "b[strat sp:2]",
      "b[temp1 sp:2]",
      "b[temp2 sp:2]",
      "b[temp3 sp:2]",
      "b[origin:strat sp:2]",
      "b[origin:temp1 sp:2]",
      "b[origin:temp2 sp:2]",
      "b[origin:temp3 sp:2]",
      "b[strat:temp1 sp:2]",
      "b[strat:temp2 sp:2]",
      "b[strat:temp3 sp:2]",
      "b[origin:strat:temp1 sp:2]",
      "b[origin:strat:temp2 sp:2]",
      "b[origin:strat:temp3 sp:2]"
      ,
      "b[origin sp:3]",
      "b[strat sp:3]",
      "b[temp1 sp:3]",
      "b[temp2 sp:3]",
      "b[temp3 sp:3]",
      "b[origin:strat sp:3]",
      "b[origin:temp1 sp:3]",
      "b[origin:temp2 sp:3]",
      "b[origin:temp3 sp:3]",
      "b[strat:temp1 sp:3]",
      "b[strat:temp2 sp:3]",
      "b[strat:temp3 sp:3]",
      "b[origin:strat:temp1 sp:3]",
      "b[origin:strat:temp2 sp:3]",
      "b[origin:strat:temp3 sp:3]"
      ,
      "b[origin sp:4]",
      "b[strat sp:4]",
      "b[temp1 sp:4]",
      "b[temp2 sp:4]",
      "b[temp3 sp:4]",
      "b[origin:strat sp:4]",
      "b[origin:temp1 sp:4]",
      "b[origin:temp2 sp:4]",
      "b[origin:temp3 sp:4]",
      "b[strat:temp1 sp:4]",
      "b[strat:temp2 sp:4]",
      "b[strat:temp3 sp:4]",
      "b[origin:strat:temp1 sp:4]",
      "b[origin:strat:temp2 sp:4]",
      "b[origin:strat:temp3 sp:4]"
      ,
      "b[origin sp:5]",
      "b[strat sp:5]",
      "b[temp1 sp:5]",
      "b[temp2 sp:5]",
      "b[temp3 sp:5]",
      "b[origin:strat sp:5]",
      "b[origin:temp1 sp:5]",
      "b[origin:temp2 sp:5]",
      "b[origin:temp3 sp:5]",
      "b[strat:temp1 sp:5]",
      "b[strat:temp2 sp:5]",
      "b[strat:temp3 sp:5]",
      "b[origin:strat:temp1 sp:5]",
      "b[origin:strat:temp2 sp:5]",
      "b[origin:strat:temp3 sp:5]"
      ,
      "b[origin sp:6]",
      "b[strat sp:6]",
      "b[temp1 sp:6]",
      "b[temp2 sp:6]",
      "b[temp3 sp:6]",
      "b[origin:strat sp:6]",
      "b[origin:temp1 sp:6]",
      "b[origin:temp2 sp:6]",
      "b[origin:temp3 sp:6]",
      "b[strat:temp1 sp:6]",
      "b[strat:temp2 sp:6]",
      "b[strat:temp3 sp:6]",
      "b[origin:strat:temp1 sp:6]",
      "b[origin:strat:temp2 sp:6]",
      "b[origin:strat:temp3 sp:6]"
      ,
      "b[origin sp:7]",
      "b[strat sp:7]",
      "b[temp1 sp:7]",
      "b[temp2 sp:7]",
      "b[temp3 sp:7]",
      "b[origin:strat sp:7]",
      "b[origin:temp1 sp:7]",
      "b[origin:temp2 sp:7]",
      "b[origin:temp3 sp:7]",
      "b[strat:temp1 sp:7]",
      "b[strat:temp2 sp:7]",
      "b[strat:temp3 sp:7]",
      "b[origin:strat:temp1 sp:7]",
      "b[origin:strat:temp2 sp:7]",
      "b[origin:strat:temp3 sp:7]"
    )
  )

cri.f<-sum.m[c(2:121),c(1,4,8)] #just selecting the mean and 95% CI. Removing the intercept 
fdf<-data.frame(cri.f)
#binding 
fdf2<-as.data.frame(
  cbind(
    (c(rownames(fdf)[1:15], rep(rev(rownames(fdf)[1:15]), each=7))), #stdarzing the parameter  names 
    as.numeric(as.character(fdf$mean)),  # the estimate 
    as.numeric(as.character(fdf$X2.5.)), #lower bound, 95% CI
    as.numeric(as.character(fdf$X97.5.)),  #upper bound, 95% CI
    as.numeric(c(rep(1, 15), rep(2, 105))),  # A variable to signify if the corresponding row is a fixed  or random effect. 1=global, 2=rndm
    as.numeric( c(rep(0,15), rep(seq(1:7),15 ))))) #sp variable. Zero when a factor 

names(fdf2)<-c("var", "Estimate", colnames(cri.f)[c(2,3)], "rndm", "sp") #renaming. 

fdf2$Estimate<-as.numeric(fdf2$Estimate)      
fdf2$`2.5%`<-as.numeric(fdf2$`2.5%`)
fdf2$`97.5%`<-as.numeric(fdf2$`97.5%`)      


#Fixed effect estimates:
fixed<-c(rep(0, 15), rep(as.numeric(rev(fdf2[c(1:15),2])), each=7))
dff<-fdf2

#adding the fixef estiamtes to the random effect values:
dff$Estimate<-fdf2$Estimate+fixed
dff$`2.5%`<-fdf2$`2.5%`+fixed
dff$`97.5%`<-fdf2$`97.5%`+fixed

dff$var <- fct_inorder(dff$var) #so that the categorical variables plot in the right order 

## plotting

pd <- position_dodgev(height = -0.5)


fig2<-ggplot(dff, aes(x=Estimate, y=var, color=factor(sp), size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd, size=4)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`97.5%`)), position=pd, size=.5, height =0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(labels = c("Fixed effects", "CAPBUR", "CHEMAJ", "DACGLO", "PLALAN", "PLAMAJ", "RUMCRI", "TAROFF"),
                      values=c("blue", "red", "orangered1", "sienna4", "green4", "green1", "purple2", "magenta2"))+  scale_alpha_manual(values=c(1, 0.5))+
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5))+
  guides(alpha=FALSE) + #removes the legend 
  ggtitle(label = "Germination rate")+
  scale_y_discrete(limits = rev(unique(sort(dff$var))))
#pdf(file.path(figpath, "Fig2.pdf"), width = 7, height = 8)
fig2

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
#photoperiod, and where we have a response in days and total chilling. There are NA in each 
#of these columns, including labgroup!

## remove NAs individually .... (not needed currently)
ospr.stan$resp<-ospr.stan$resp[which(is.na(ospr.stan$resp)==FALSE)]
ospr.stan$chill<-ospr.stan$chill[which(is.na(ospr.stan$chill)==FALSE)]
ospr.stan$force<-ospr.stan$force[which(is.na(ospr.stan$force)==FALSE)]
ospr.stan$photo<-ospr.stan$photo[which(is.na(ospr.stan$photo)==FALSE)]
ospr.stan$lat<-ospr.stan$lat[which(is.na(ospr.stan$lat)==FALSE)]
ospr.stan$complex<-ospr.stan$complex[which(is.na(ospr.stan$complex)==FALSE)]


y = ospr.stan$resp
chill = ospr.stan$chill
force = ospr.stan$force
photo = ospr.stan$photo
lat = ospr.stan$lat
sp = ospr.stan$complex
N = length(y)
n_sp = length(unique(sp))



## remove outliers: some values of y > 600 - maybe driving huge estimated values for alpha
chill = chill[which(y<300)]
force = force[which(y<300)]
photo = photo[which(y<300)]
sp = sp[which(y<300)]
lat = lat[which(y<300)]
y = y[which(y<300)]
N = length(y)
n_sp = length(unique(sp))


# making a list out of the processed data. It will be input for the model
datalist.td <- list(y=y,chill=chill, force=force,photo=photo,sp=sp,N=N,n_sp=n_sp, lat=lat)

# we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
datalist.td$chill<-datalist.td$chill/240


## real data with only experimental chilling (no field chilling)
#osp.td3 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td,
 #              iter = 2000,warmup=1500,control=list(adapt_delta=0.95)) 


##############################
###### real data all chilling
osp.td4 = stan('stan/lat/LAT_daysBBwinter_2level.stan', data = datalist.td,
               iter = 4000,warmup=3000,control=list(adapt_delta=0.99)) 

betas <- as.matrix(osp.td4, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","mu_b_lat_sp",
"b_force", "b_photo", "b_chill", "b_lat"))
mcmc_intervals(betas[,1:4])


launch_shinystan(osp.td4)
load("/Users/CatherineChamberlain/Downloads/shinystan-multiparam-gg.RData")
shinystan_multiparam_gg_4

load("/Users/CatherineChamberlain/Downloads/shinystan-multiparam-gg-4.RData")
#td4 <- summary(osp.td4)$summary
#preds.4<-td4[grep("yhat", rownames(td4)),]



######################################
###### real data all chilling sigmoid
osp.td5 = stan('stan/bb/M1_daysBBnointer_2level_interceptonly_sigmoid.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.95)) 

betas.td5 <- as.matrix(osp.td5, pars = c("b_force", "b_photo","a_chill", "b_chill"))
mcmc_intervals(betas[,1:5])

#td5 <- summary(osp.td5)$summary
#preds.5<-td5[grep("yhat", rownames(td5)),]



########### Running the models with fake data
#setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
source("bb_analysis/bb_testdata_generate.R")

# lme version
summary(lme1 <- lmer(bb ~ chill+force+photo + (1|sp), data = testdat)) 
ranef(lme1)
fixef(lme1)
#head(testdat)
#head(list.coeffs)

##
# try the model
datalist.td <- with(testdat, 
                    list(y = bb, 
                         chill = as.numeric(chill), 
                         force = as.numeric(force), 
                         photo = as.numeric(photo),
                         sp = as.numeric(sp),
                         N = nrow(testdat),
                         n_sp = length(unique(sp))
                    )
)



## running model with fake data
osp.td2 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td, 
             iter = 2000,warmup=1500,control=list(adapt_delta=0.90)) 

