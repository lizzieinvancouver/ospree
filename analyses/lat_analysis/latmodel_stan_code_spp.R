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
library(ggstance)

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


########################
#### get the data

# make sure this is the correct file (we're still cleaning as I write this!) 
bb <- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)

taxon <- read.csv("output/bb_analysis/taxon/complex_levels.csv", header=TRUE)


## Old code to remove Olea, probably the new subset of species is getting rid of it 
#bb[bb$genus=="Olea",]
#bb<-subset(bb,genus!="Olea")

## subsetting for experimental chilling
#bb<-subset(bb,!is.na(as.numeric(chilltemp)))

# merge in labgroup (we could do this elsewhere someday)
bb.wlab <- merge(bb, taxon, by=c("genus","species"), all.x=TRUE)
tt <- table(bb.wlab$complex)
bb.wlab <- subset(bb.wlab, complex %in% names(tt[tt > 200])) ### testing 
  # [1] "Betula_pendula"       "Betula_pubescens"     "Fagus_sylvatica"      "Malus_domestica"     
  # [5] "Picea_abies"          "Prunus_complex"       "Pyrus_complex"        "Rhododendron_complex"
  # [9] "Ribes_nigrum"         "Sorbus_complex" 
myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica", "Picea_abies")
bb.wlab<-dplyr::filter(bb.wlab, complex%in%myspp)

columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
                   "photoperiod_day", "response", "response.time", "Total_Chill_portions",
                   "complex", "provenance.lat")

bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)


## make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$chill <- as.numeric(bb.wlab.sm$Total_Chill_portions)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)
bb.wlab.sm$lat<-as.numeric(bb.wlab.sm$provenance.lat)


## subsetting data, preparing genus variable, removing NAs
ospr.prepdata <- subset(bb.wlab.sm, select=c("resp", "chill", "photo", "force", "complex", "lat"))
dim(subset(bb.wlab.sm, is.na(chill)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE 
           & is.na(lat)==FALSE))
ospr.stan <- ospr.prepdata[complete.cases(ospr.prepdata),]
ospr.stan$sp <- as.numeric(as.factor(ospr.stan$complex))

#ospr.stan$resp<-ifelse(ospr.stan$resp==0, 0.01, ospr.stan$resp) ## if Gamma is necessary!
lat.stan<-stan_glmer(resp~chill+photo+force+lat+(1|sp) + (force-1|sp) + (photo-1|sp)
                     + (chill-1|sp) + (lat-1|sp), data=ospr.stan)
lat.stan.inter<-stan_glmer(resp~chill+photo+force+lat+(1|sp) + chill:photo +
                             chill:force + chill:lat + photo:force + photo:lat +
                             force:lat + (force-1|sp) + (photo-1|sp)
                           + (chill-1|sp) + (lat-1|sp) + (force:photo-1|sp) +
                             (force:chill-1|sp) + (photo:chill-1|sp) + 
                             (force:lat-1|sp) + (photo:lat-1|sp) + (chill:lat-1|sp), data=ospr.stan)

m<-lat.stan
# This is an Rstanarm object, and so behaves differently than th previous brms object. Thus the coeffs have to be extracted 
#differently.  There's probably a better way to do this, but I already had this code written up for something else, so used it again.
sum.m <-
  summary(
    m,
    pars = c(
      "(Intercept)",
      "chill",
      "force",
      "photo",
      "lat"
      ,
      "b[chill sp:1]",
      "b[force sp:1]",
      "b[photo sp:1]",
      "b[lat sp:1]"
      ,
      "b[chill sp:2]",
      "b[force sp:2]",
      "b[photo sp:2]",
      "b[lat sp:2]"
      ,
      "b[chill sp:3]",
      "b[force sp:3]",
      "b[photo sp:3]",
      "b[lat sp:3]"
      ,
      "b[chill sp:4]",
      "b[force sp:4]",
      "b[photo sp:4]",
      "b[lat sp:4]"
    )
  )

sum.m <-
  summary(
    m,
    pars = c(
      "(Intercept)",
      "chill",
      "force",
      "photo",
      "lat",
      "chill:force",
      "chill:photo",
      "chill:lat",
      "force:photo",
      "force:lat",
      "photo:lat"
      ,
      "b[chill sp:1]",
      "b[force sp:1]",
      "b[photo sp:1]",
      "b[lat sp:1]",
      "b[chill:force sp:1]",
      "b[chill:photo sp:1]",
      "b[chill:lat sp:1]",
      "b[force:photo sp:1]",
      "b[force:lat sp:1]",
      "b[photo:lat sp:1]"
      ,
      "b[chill sp:2]",
      "b[force sp:2]",
      "b[photo sp:2]",
      "b[lat sp:2]",
      "b[chill:force sp:2]",
      "b[chill:photo sp:2]",
      "b[chill:lat sp:2]",
      "b[force:photo sp:2]",
      "b[force:lat sp:2]",
      "b[photo:lat sp:2]"
      ,
      "b[chill sp:3]",
      "b[force sp:3]",
      "b[photo sp:3]",
      "b[lat sp:3]",
      "b[chill:force sp:3]",
      "b[chill:photo sp:3]",
      "b[chill:lat sp:3]",
      "b[force:photo sp:3]",
      "b[force:lat sp:3]",
      "b[photo:lat sp:3]"
      ,
      "b[chill sp:4]",
      "b[force sp:4]",
      "b[photo sp:4]",
      "b[lat sp:4]",
      "b[chill:force sp:4]",
      "b[chill:photo sp:4]",
      "b[chill:lat sp:4]",
      "b[force:photo sp:4]",
      "b[force:lat sp:4]",
      "b[photo:lat sp:4]"
    )
  )
cri.f<-sum.m[c(2:21),c(1,4,8)] #just selecting the mean and 95% CI. Removing the intercept 
fdf<-data.frame(cri.f)
#binding 
fdf2<-as.data.frame(
  cbind(
    (c(rownames(fdf)[1:20], rep(rev(rownames(fdf)[1:20]), each=4))), #stdarzing the parameter  names 
    as.numeric(as.character(fdf$mean)),  # the estimate 
    as.numeric(as.character(fdf$X2.5.)), #lower bound, 95% CI
    as.numeric(as.character(fdf$X97.5.)),  #upper bound, 95% CI
    as.numeric(c(rep(1, 20), rep(2, 5))),  # A variable to signify if the corresponding row is a fixed  or random effect. 1=global, 2=rndm
    as.numeric( c(rep(0,20), rep(seq(1:4),20 ))))) #sp variable. Zero when a factor 
names(fdf2)<-c("var", "Estimate", colnames(cri.f)[c(2,3)], "rndm", "sp") #renaming. 
fdf2$Estimate<-as.numeric(fdf2$Estimate)      
fdf2$`2.5%`<-as.numeric(fdf2$`2.5%`)
fdf2$`97.5%`<-as.numeric(fdf2$`97.5%`)      
#Fixed effect estimates:
fixed<-c(rep(0, 100), rep(as.numeric(rev(fdf2[c(1:100),2])), each=2))
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


## Woohoo!!!
#stan_glmer
#family:       gaussian [identity]
#formula:      resp ~ chill + photo + force + lat + (1 | sp)
#observations: 1889
#------
#  Median MAD_SD
#(Intercept) 98.1   15.3  
#chill       -0.2    0.0  
#photo       -1.7    0.2  
#force       -0.3    0.2  
#lat         -0.3    0.1  
#sigma       38.7    0.6  

#Error terms:
#  Groups   Name        Std.Dev.
#sp       (Intercept) 35      
#Residual             39      
#Num. levels: sp 5 

#Sample avg. posterior predictive distribution of y:
#  Median MAD_SD
#mean_PPD 43.6    1.2  


lat.brm<-brm(resp~ force + photo + chill + lat + force:photo + force:chill + photo:chill + force:lat + photo:lat + chill:lat + (1|sp) + (force-1|sp) + (photo-1|sp)
             + (chill-1|sp) + (lat-1|sp) + (force:photo-1|sp) +
               (force:chill-1|sp) + (photo:chill-1|sp) + (force:lat-1|sp) + (photo:lat-1|sp) + (chill:lat-1|sp), data=ospr.stan)

#lat.brm<-brm(resp~ force + photo + chill + lat + force:photo + force:chill + photo:chill + force:lat + photo:lat + 
#               chill:lat + (1|sp), data=ospr.stan)


#lat.brm<-brm(resp~ force + photo + chill + lat + (1|sp) + (force-1|sp) + (photo-1|sp)
#             + (chill-1|sp) + (lat-1|sp), data=ospr.stan)

m<-lat.brm
m.int<-posterior_interval(m)
sum.m<-summary(m)
cri.f<-as.data.frame(sum.m$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f<-cri.f[-1,] #removing the intercept 
fdf1<-as.data.frame(rbind(as.vector(cri.f[,1]), as.vector(cri.f[,2]), as.vector(cri.f[,3])))
fdf2<-cbind(fdf1, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2)<-c(rownames(cri.f), "sp", "perc")

cri.r<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$sp
cri.r2<-cri.r[, ,-1]
cri.r2<-cri.r2[,-2,]
dims<-dim(cri.r2)
twoDimMat <- matrix(cri.r2, prod(dims[1:2]), dims[3])
mat2<-cbind(twoDimMat, c(rep(1:4, length.out=12)), rep(c("Estimate", "2.5%", "95%"), each=4))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "sp", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, force:lat, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=18)) {
  for (j in seq(from=3, to=17, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$sp>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$sp<-as.factor(dfwide$sp)
## plotting

pd <- position_dodgev(height = -0.5)

estimates<-c("Forcing", "Photoperiod", "Chill Portions", "Forcing x Photoperiod", 
             "Forcing x Chill Portions", "Photoperiod x Chill Portions")
estimates<-c("Forcing", "Photoperiod", "Chill Portions", "Latitude")
dfwide$legend<-factor(dfwide$sp,
                      labels=c("Overall Effects","1","2","3","4","5"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, height =0, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue", "firebrick3", "orangered1","orange3","sienna2", "green4", "green3", "purple2"),
                      breaks=c("Overall Effects"))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) + 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.position = "none", legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.05, "cm")) +
  xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
fig1



# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
#photoperiod, and where we have a response in days and total chilling. There are NA in each 
#of these columns, including labgroup!

## remove NAs individually .... (not needed currently)
#ospr.stan$resp<-ospr.stan[which(is.na(ospr.stan$resp)==FALSE),]
#ospr.stan$chill<-ospr.stan[which(is.na(ospr.stan$chill)==FALSE),]
#ospr.stan$force<-ospr.stan[which(is.na(ospr.stan$force)==FALSE),]
#ospr.stan$photo<-ospr.stan[which(is.na(ospr.stan$photo)==FALSE),]
#ospr.stan$lat<-ospr.stan[which(is.na(ospr.stan$lat)==FALSE),]
#ospr.stan$complex<-ospr.stan[which(is.na(ospr.stan$complex)==FALSE),]



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
osp.td4 = stan('stan/lat/LAT_daysBBnointer_2level.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.99)) 

betas <- as.matrix(osp.td4, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","mu_b_lat_sp",
"b_force", "b_photo", "b_chill"))
mcmc_intervals(betas[,1:4])

launch_shinystan(osp.td4)
load("/Users/CatherineChamberlain/Downloads/shinystan-multiparam-gg.RData")
shinystan_multiparam_gg


#td4 <- summary(osp.td4)$summary
#preds.4<-td4[grep("yhat", rownames(td4)),]

save(td4, file="stan/lat/output/LAT_daysBBnointer_2level.Rda")



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

