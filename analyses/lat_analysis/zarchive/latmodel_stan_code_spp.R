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
#library(bayesplot)
library(rstanarm)
library(brms)
library(ggstance)
#library(forcats)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

setwd("~/Documents/git/ospree/analyses")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
source("lat_analysis/source/bbdataplease.R")
## (2) Deal with species
dim(bb.noNA)
d <- bb.noNA
source("lat_analysis/source/speciescomplex.R")
bb.noNA.wtaxa <- d
dim(bb.noNA.wtaxa)
unique(bb.noNA.wtaxa$complex)


# merge in labgroup (we could do this elsewhere someday)
bb.wlab <- merge(bb.resp, taxon, by=c("genus","species"), all.x=TRUE)
bb.wlab <- within(bb.wlab, { prov.lat <- ave(provenance.lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
bb.wlab <- subset(bb.wlab, bb.wlab$prov.lat>1) 
bb.wlab.photo<- within(bb.wlab, { photo <- ave(photoperiod_day, complex, FUN=function(x) length(unique(x)))}) # multiple photoperiods
bb.wlab.photo <- subset(bb.wlab.photo, bb.wlab.photo$photo>1) 
tt <- table(bb.wlab.photo$complex)### testing 
bb.wlab<-bb.wlab.photo
    # [1] "Betula_complex"        "Betula_pendula"        "Betula_pubescens"      "Fagus_sylvatica"      
    # [5] "Malus_domestica"       "Picea_abies"           "Picea_glauca"          "Pseudotsuga_menziesii"
    # [9] "Ribes_nigrum"          "Ulmus_complex"  
myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica", "Picea_abies", "Pseudotsuga_menziesii", "Ribes_nigrum", "Ulmus_complex")
bb.wlab<-dplyr::filter(bb.wlab, complex%in%myspp)

studies<-dplyr::select(bb.wlab, datasetID, complex)
studies<-studies[!duplicated(studies),]
studies<-within(studies, { studies <- ave(datasetID, complex, FUN=function(x) length(unique(x)))})
studies<-dplyr::select(studies, -datasetID)
studies<-studies[!duplicated(studies),]

columnstokeep <- c("datasetID", "genus", "species", "varetc", "woody", "forcetemp",
                   "photoperiod_day", "response", "response.time", "Total_Utah_Model",
                   "complex", "provenance.lat")

bb.wlab.sm <- subset(bb.wlab, select=columnstokeep)


## make a bunch of things numeric (eek!)
bb.wlab.sm$force <- as.numeric(bb.wlab.sm$forcetemp)
bb.wlab.sm$photo <- as.numeric(bb.wlab.sm$photoperiod_day)
bb.wlab.sm$chill <- as.numeric(bb.wlab.sm$Total_Utah_Model)
bb.wlab.sm$resp <- as.numeric(bb.wlab.sm$response.time)
bb.wlab.sm$lat<-as.numeric(bb.wlab.sm$provenance.lat)


## subsetting data, preparing genus variable, removing NAs
ospr.prepdata <- subset(bb.wlab.sm, select=c("resp", "chill", "photo", "force", "complex", "lat", "datasetID"))
dim(subset(bb.wlab.sm, is.na(chill)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE 
           & is.na(lat)==FALSE & is.na(datasetID)==FALSE))
ospr.stan <- ospr.prepdata[complete.cases(ospr.prepdata),]
ospr.stan$sp <- as.numeric(as.factor(ospr.stan$complex))

## Center? or Z-score?
ospr.stan$sm.chill<-ospr.stan$chill/240
## center the predictors:
#ospr.stan$force.cen <- ospr.stan$force-mean(ospr.stan$force,na.rm=TRUE)
#ospr.stan$photo.cen <- ospr.stan$photo-mean(ospr.stan$photo,na.rm=TRUE)
#ospr.stan$chill.cen <- ospr.stan$chill-mean(ospr.stan$chill,na.rm=TRUE)
#ospr.stan$lat.cen <- ospr.stan$lat-mean(ospr.stan$lat,na.rm=TRUE)

## z-score the predictors:
ospr.stan$force.z <- (ospr.stan$force-mean(ospr.stan$force,na.rm=TRUE))/sd(ospr.stan$force,na.rm=TRUE)
ospr.stan$photo.z <- (ospr.stan$photo-mean(ospr.stan$photo,na.rm=TRUE))/sd(ospr.stan$photo,na.rm=TRUE)
ospr.stan$chill.z <- (ospr.stan$chill-mean(ospr.stan$chill,na.rm=TRUE))/sd(ospr.stan$chill,na.rm=TRUE)
ospr.stan$lat.z <- (ospr.stan$lat-mean(ospr.stan$lat,na.rm=TRUE))/sd(ospr.stan$lat,na.rm=TRUE)

ospr.stan <- subset(ospr.stan, resp<600)

cp<-ggplot(ospr.stan, aes(x=chill, y=photo)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex) + theme(legend.position = "none")
fp<-ggplot(ospr.stan, aes(x=force, y=photo)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex) + theme(legend.position = "none")
cf<-ggplot(ospr.stan, aes(x=chill, y=force)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex) + theme(legend.position = "none")
lf<-ggplot(ospr.stan, aes(x=lat, y=force)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex) + theme(legend.position = "none")
pl<-ggplot(ospr.stan, aes(x=lat, y=photo)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex) + theme(legend.position = "none")
cl<-ggplot(ospr.stan, aes(x=lat, y=chill)) + geom_point(aes(col=as.factor(complex))) + 
  facet_wrap(~complex) + theme(legend.position = "none")
quartz()

#write.csv(ospr.stan, file="~/Documents/git/ospree/analyses/lat_analysis/lat_output/lat_wRibesandUlmus.csv", row.names = FALSE)
### Species random slope effect for main effects only
lat.allinter<-brm(resp~ force.z + photo.z + chill.z + lat.z + photo.z:lat.z + force.z:lat.z + force.z:photo.z + force.z:chill.z +
                    chill.z:photo.z + chill.z:lat.z +
                    (force.z + photo.z + chill.z + lat.z + photo.z:lat.z + force.z:lat.z + force.z:photo.z + force.z:chill.z +
                       chill.z:photo.z + chill.z:lat.z|sp), 
                  data=ospr.stan, warmup=2500,iter=4000, chains = 2, cores = 1,
                  control = list(max_treedepth = 12,adapt_delta = 0.99))

lat.stan<-stan_glmer(resp~ force + photo + sm.chill + lat + photo:lat +
                    (force + photo + sm.chill + lat|sp), data=ospr.stan, warmup=2500,iter=4000,
                    chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))

lat.brm<-brm(resp~ force + photo + sm.chill + lat + photo:lat +
                       (force + photo + sm.chill + lat + photo:lat|sp), data=ospr.stan, warmup=2500,iter=4000,
                     chains = 2, cores = 4,control = list(max_treedepth = 12,adapt_delta = 0.99))

lat.inter_brm<-brm(resp~ force + photo + sm.chill + lat + force:photo + force:sm.chill + photo:chill + photo:lat + force:lat +
                     (force + photo + sm.chill + lat + force:photo + force:sm.chill + photo:chill + photo:lat + force:lat|sp), 
                   data=ospr.stan, warmup=2500,iter=4000, chains = 2, cores = 4,
                   control = list(max_treedepth = 12,adapt_delta = 0.99))

### Rstanarm output:
# a: 121.1, f:-1.4, p: -3.7, c: -3.8, l: -0.5, pl: 0.0, sigma: 20.2

summary(lat.inter_arm) # 75 divergent transitions and bad... 


## Next, (1) compare to brms output, (2) add all interactions to make sure doesn't skew results, (3) add datasetID as random intercept?
lat.stan_brm<-brm(resp~ force + photo + sm.chill + lat + photo:lat +
                       (force + photo + sm.chill + lat|sp), data=ospr.stan, warmup=2500,iter=4000,
                     chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))

summary(lat.stan_brm) ## quite different from rstanrarm!
# a: 99.34, f: -1.37, p: -2.99, c: -3.8, l: -0.17, pl: 0.04, sigma: 19.95

lat.inter_brm<-brm(resp~ force + photo + sm.chill + lat + photo:lat + force:photo + force:sm.chill +
                     photo:sm.chill + force:lat + sm.chill:lat +
                    (force + photo + sm.chill + lat + photo:lat + force:photo + force:sm.chill +
                       photo:sm.chill + force:lat + sm.chill:lat|sp), data=ospr.stan, warmup=2500,iter=4000,
                  chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))

# hmmm... quite different... 
summary(lat.inter_brm)
# a: -13.41, f: 6.99, p: -4.98, c: -8.81, l: 2.49, pl: 0.07, fp: 0.01, fc: 0.21, pc: 0, fl: -.19, cl: 0.02, sigma: 19.38


lat.study_brm<-brm(resp~ force + photo + sm.chill + lat + photo:lat + (1|datasetID) +
                    (force + photo + sm.chill + lat|sp), data=ospr.stan, warmup=2500,iter=4000,
                  chains = 2, cores = 2,control = list(max_treedepth = 12,adapt_delta = 0.99))

summary(lat.study_brm)
# a: 105.36, f: -1.81, p: -1.16, c: -3.68, l: -0.21, pl: 0.01, sigma: 14.84 


stanplot(lat.stan_brm, pars = "^b_")
launch_shinystan(lat.stan_brm)



m<-lat.allinter
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
mat2<-cbind(twoDimMat, c(rep(1:7, length.out=21)), rep(c("Estimate", "2.5%", "95%"), each=7))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "sp", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, force.z:`chill.z:lat.z`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=240)) {
  for (j in seq(from=3, to=239, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$sp>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$sp<-as.factor(dfwide$sp)
## plotting

pd <- position_dodgev(height = -0.5)

estimates<-c("Forcing", "Photoperiod", "Chill Portions", "Latitude", "Photoperiod x Latitude",
               "Forcing x Latitude", "Forcing x Photoperiod", "Forcing x Chill", "Chill x Photo", "Chill x Lat")
dfwide$legend<-factor(dfwide$sp,
                   labels=c("Overall Effects","B. pendula","B. pubescens","F. sylvatica",
                            "P. abies", "P. menziesii", "R. nigrum", "U. complex"))
cols <- colorRampPalette(brewer.pal(8,"Accent"))(8)
estimates<-rev(estimates)
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, height =0, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=cols,
                      labels=c("Overall Effects", 
                               "B. pendula" = expression(paste(italic("Betula pendula"))),
                               "B. pubescens"= expression(paste(italic("Betula pubescens"))),
                               "F. sylvatica" = expression(paste(italic("Fagus sylvatica"))), 
                               "P. abies" = expression(paste(italic("Picea abies"))),
                               "P. menziesii" = expression(paste(italic("Pseudotsuga menziesii"))),
                               "R. nigrum" = expression(paste(italic("Ribes nigrum"))),
                               "U. complex" = expression(paste(italic("Ulmus complex")))))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) + 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.05, "cm")) +
  xlab(expression(atop("Model Estimate of Days to Budburst")))
quartz()
fig1


m<-lat.stan_final 
sum.m <-
  summary(
    m,
    pars = c(
      "(Intercept)",
      "force",
      "photo",
      "sm.chill",
      "lat",
      "photo:lat"
      ,
      "b[force sp:1]",
      "b[photo sp:1]",
      "b[sm.chill sp:1]",
      "b[lat sp:1]",
      "b[photo:lat sp:1]"
      ,
      "b[force sp:2]",
      "b[photo sp:2]",
      "b[sm.chill sp:2]",
      "b[lat sp:2]",
      "b[photo:lat sp:2]"
      ,
      "b[force sp:3]",
      "b[photo sp:3]",
      "b[sm.chill sp:3]",
      "b[lat sp:3]",
      "b[photo:lat sp:3]"
      ,
      "b[force sp:4]",
      "b[photo sp:4]",
      "b[sm.chill sp:4]",
      "b[lat sp:4]",
      "b[photo:lat sp:4]"
      ,
      "b[force sp:5]",
      "b[photo sp:5]",
      "b[sm.chill sp:5]",
      "b[lat sp:5]",
      "b[photo:lat sp:5]"
      ,
      "b[force sp:6]",
      "b[photo sp:6]",
      "b[sm.chill sp:6]",
      "b[lat sp:6]",
      "b[photo:lat sp:6]"
    )
  )

cri.f<-sum.m[c(2:36),c(1,4,8)] #just selecting the mean and 95% CI. Removing the intercept 
fdf<-data.frame(cri.f)
#binding 
fdf2<-as.data.frame(
  cbind(
    (c(rownames(fdf)[1:5], rep(rev(rownames(fdf)[1:5]), each=6))), #stdarzing the parameter  names 
    as.numeric(as.character(fdf$mean)),  # the estimate 
    as.numeric(as.character(fdf$X2.5.)), #lower bound, 95% CI
    as.numeric(as.character(fdf$X97.5.)),  #upper bound, 95% CI
    as.numeric(c(rep(1, 5), rep(2, 30))),  # A variable to signify if the corresponding row is a fixed  or random effect. 1=global, 2=rndm
    as.numeric( c(rep(0,5), rep(seq(1:6),5 ))))) #sp variable. Zero when a factor 

names(fdf2)<-c("var", "Estimate", colnames(cri.f)[c(2,3)], "rndm", "sp") #renaming. 

fdf2$Estimate<-as.numeric(fdf2$Estimate)      
fdf2$`2.5%`<-as.numeric(fdf2$`2.5%`)
fdf2$`97.5%`<-as.numeric(fdf2$`97.5%`)      


#Fixed effect estimates:
fixed<-c(rep(0, 5), rep(as.numeric(rev(fdf2[c(1:5),2])), each=6))
dff<-fdf2

#adding the fixef estiamtes to the random effect values:
dff$Estimate<-fdf2$Estimate+fixed
dff$`2.5%`<-fdf2$`2.5%`+fixed
dff$`97.5%`<-fdf2$`97.5%`+fixed

dff$var <- fct_inorder(dff$var) #so that the categorical variables plot in the right order 

## plotting

pd <- position_dodgev(height = -0.5)

estimates<-c("Forcing", "Photoperiod", "Chill Portions", "Latitude", "Photoperiod x Latitude")
dff$legend<-factor(dff$sp,
                      labels=c("Overall Effects","B. pendula","B. pubescens","F. sylvatica",
                               "P. abies","M. Domestica", "P. menziesii", "R. nigrum"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
quartz()
fig1 <-ggplot(dff, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`97.5%`)), position=pd, size=.5, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue", "firebrick3", "orangered1","orange3","sienna2", "green4", "purple2"),
                      labels=c("Overall Effects", 
                               "B. pendula" = expression(paste(italic("Betula pendula"))),
                               "B. pubescens"= expression(paste(italic("Betula pubescens"))),
                               "F. sylvatica" = expression(paste(italic("Fagus sylvatica"))), 
                               "P. abies" = expression(paste(italic("Picea abies"))),
                               "P. glauca" = expression(paste(italic("Picea glauca"))),
                               "P. menziesii" = expression(paste(italic("Pseudotsuga menziesii")))))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) + 
  scale_y_discrete(limits = rev(unique(sort(dff$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.2, "cm"), 
                              legend.text = element_text(size=8)) +
  xlab(expression(atop("Model Estimate of Days to Budburst")))
fig1

### Compare Gymnosperms to Angiosperms...
ospr.stan$type<-ifelse(ospr.stan$sp>3, 0, 1)
gyms<-ospr.stan[(ospr.stan$type==0),]
lat.gym<-stan_glm(resp~force+photo+sm.chill+lat+photo:lat, data=gyms) ## amazing pp_checks!

angios<-ospr.stan[(ospr.stan$type==1),]
lat.angio<-stan_glm(resp~force+photo+sm.chill+lat+photo:lat, data=angios) ## less amazing but okay

gym<-plot(lat.gym, pars="beta")
ang<-plot(lat.angio, pars="beta")
library(egg)
ggarrange(gym, ang, ncol=2)


####################################################################################################
####################################################################################################
########################################   EXTRA CODE BELOW... CUT LATER!! #########################
####################################################################################################
lat.oneinter.rnd<-stan_glmer(resp~ force + photo + sm.chill + lat + photo:lat + (1|sp), data=ospr.stan)

#family:       gaussian [identity]
#formula:      resp ~ force + photo + sm.chill + lat + photo:lat + (1 | sp)
#observations: 1669
#------
#  Median MAD_SD
#(Intercept) 248.0   20.5 
#force         0.5    0.2 
#photo       -12.7    1.3 
#sm.chill     -2.7    0.2 
#lat          -3.2    0.3 
#photo:lat     0.2    0.0 
#sigma        32.6    0.6 

#Error terms:
#  Groups   Name        Std.Dev.
#sp       (Intercept) 20      
#Residual             33      
#Num. levels: sp 6 

#Sample avg. posterior predictive distribution of y:
#  Median MAD_SD
#mean_PPD 47.6    1.1  

lat.oneinter<-stan_glm(resp~ force + photo + sm.chill + lat + photo:lat, data=ospr.stan)
#stan_glm
#family:       gaussian [identity]
#formula:      resp ~ force + photo + sm.chill + lat + photo:lat
#observations: 1781
#predictors:   6
#------
#  Median MAD_SD
#(Intercept) 337.6   16.9 
#force         0.8    0.2 
#photo       -14.9    1.2 
#sm.chill     -2.7    0.2 
#lat          -4.9    0.3 
#photo:lat     0.2    0.0 
#sigma        33.6    0.6 

#Sample avg. posterior predictive distribution of y:
#  Median MAD_SD
#mean_PPD 47.4    1.1 

lat.stan<-stan_glm(resp~ force + photo + sm.chill + lat + force:photo + force:sm.chill + photo:sm.chill + force:lat + photo:lat + 
                            sm.chill:lat, data=ospr.stan) ## exact same as with random

#stan_glm
#family:       gaussian [identity]
#formula:      resp ~ force + photo + sm.chill + lat + force:photo + force:sm.chill + 
#  photo:sm.chill + force:lat + photo:lat + sm.chill:lat
#observations: 1781
#predictors:   11
#------
#  Median MAD_SD
#(Intercept)     20.5   26.6 
#force           16.3    1.0 
#photo          -10.3    1.4 
#sm.chill         4.4    1.4 
#lat              0.3    0.5 
#force:photo     -0.1    0.0 
#force:sm.chill  -0.3    0.0 
#photo:sm.chill  -0.1    0.0 
#force:lat       -0.3    0.0 
#photo:lat        0.2    0.0 
#sm.chill:lat     0.0    0.0 
#sigma           31.5    0.5 

#Sample avg. posterior predictive distribution of y:
#  Median MAD_SD
#mean_PPD 47.4    1.1 

lat.stan.rnd<-stan_glmer(resp~ force + photo + sm.chill + lat + force:photo + force:sm.chill + photo:sm.chill + force:lat + photo:lat + 
                     sm.chill:lat + (1|sp), data=ospr.stan) ## pp checks are good! Tmin is off  -predicted to be ~-50

#stan_glmer
#family:       gaussian [identity]
#formula:      resp ~ force + photo + sm.chill + lat + force:photo + force:sm.chill + 
#  photo:sm.chill + force:lat + photo:lat + sm.chill:lat + (1 | sp)
#observations: 1781
#------
#  Median MAD_SD
#(Intercept)    -74.2   27.5 
#force           15.9    1.0 
#photo           -5.9    1.4 
#sm.chill         1.8    1.3 
#lat              2.0    0.5 
#force:photo     -0.1    0.0 
#force:sm.chill  -0.3    0.0 
#photo:sm.chill  -0.1    0.0 
#force:lat       -0.3    0.0 
#photo:lat        0.1    0.0 
#sm.chill:lat     0.0    0.0 
#sigma           29.8    0.5 

ospr.stan$presp<-as.integer(ospr.stan$resp+1)

lat.pois<-stan_glm(presp~ force + photo + sm.chill + lat + force:photo + force:sm.chill + photo:sm.chill + force:lat + photo:lat + 
                       sm.chill:lat, data=ospr.stan, family=poisson) ## slight improvement on pp checks

#family:       poisson [log]
#formula:      presp ~ force + photo + sm.chill + lat + force:photo + force:sm.chill + 
#  photo:sm.chill + force:lat + photo:lat + sm.chill:lat
#observations: 1781
#predictors:   11
#------
#  Median MAD_SD
#(Intercept)     1.5    0.1  
#force           0.3    0.0  
#photo          -0.1    0.0  
#sm.chill        0.3    0.0  
#lat             0.0    0.0  
#force:photo     0.0    0.0  
#force:sm.chill  0.0    0.0  
#photo:sm.chill  0.0    0.0  
#force:lat       0.0    0.0  
#photo:lat       0.0    0.0  
#sm.chill:lat    0.0    0.0 

lat.pois.rnd<-stan_glmer(presp~ force + photo + sm.chill + lat + force:photo + force:sm.chill + photo:sm.chill + force:lat + photo:lat + 
                       sm.chill:lat + (1|sp), data=ospr.stan, family=poisson) ## quite good pp_checks

#family:       poisson [log]
#formula:      presp ~ force + photo + sm.chill + lat + force:photo + force:sm.chill + 
#  photo:sm.chill + force:lat + photo:lat + sm.chill:lat + (1 | sp)
#observations: 1781
#------
#  Median MAD_SD
#(Intercept)    0.0    0.2   
#force          0.3    0.0   
#photo          0.0    0.0   
#sm.chill       0.2    0.0   
#lat            0.1    0.0   
#force:photo    0.0    0.0   
#force:sm.chill 0.0    0.0   
#photo:sm.chill 0.0    0.0   
#force:lat      0.0    0.0   
#photo:lat      0.0    0.0   
#sm.chill:lat   0.0    0.0  

lat.brm<-brm(presp~ cforce + cphoto + cchill + clat + cforce:cphoto + cforce:cchill + cphoto:cchill + cforce:clat + cphoto:clat + 
                            cchill:clat + (1|sp) + (cforce-1|sp) + (cphoto-1|sp)
                          + (cchill-1|sp) + (clat-1|sp) + (cphoto:clat-1|sp) +
                            (cforce:cphoto-1|sp) + (cforce:cchill-1|sp) + (cforce:clat-1|sp) +
                            (cphoto:cchill-1|sp) + (cchill:clat-1|sp), data=ospr.stan, family=poisson, chains=2)
lat.brm.uncent<-brm(resp~ force + photo + sm.chill + lat + photo:lat + (1|sp) + (force-1|sp) + (photo-1|sp)
             + (sm.chill-1|sp) + (lat-1|sp) + (photo:lat-1|sp), data=ospr.stan)


lat.brm.uncent<-brm(presp~ force + photo + chill + lat + force:photo + force:chill + photo:chill + force:lat + photo:lat + 
                      chill:lat + (1|sp), data=ospr.stan, family=poisson(), chains=2)

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
mat2<-cbind(twoDimMat, c(rep(1:6, length.out=18)), rep(c("Estimate", "2.5%", "95%"), each=6))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "sp", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, cforce:`cchill:clat`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=21)) {
  for (j in seq(from=3, to=20, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$sp>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$sp<-as.factor(dfwide$sp)
## plotting

pd <- position_dodgev(height = -0.5)

# [1] "Betula_pendula"       "Betula_pubescens"     "Fagus_sylvatica"      "Malus_domestica"     
# [5] "Picea_abies"          "Prunus_complex"       "Pyrus_complex"        "Rhododendron_complex"
# [9] "Ribes_nigrum"         "Sorbus_complex" 

estimates<-c("Forcing", "Photoperiod", "Chill Portions", "Latitude", "Forcing x Photoperiod", 
             "Forcing x Chill Portions", "Photoperiod x Chill Portions","Forcing x Latitude", 
             "Photoperiod x Latitude", "Chill Portions x Latitude")
estimates<-c("Forcing", "Photoperiod", "Chill Portions", "Latitude", "Photoperiod x Latitude")
dfwide$legend<-factor(dfwide$sp,
                      labels=c("Overall Effects","B. pendula","B. pubescens","F. sylvatica",
                               "P. abies","Prunus", "Sorbus"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
quartz()
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, height =0, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue", "firebrick3", "orangered1","orange3","sienna2", "green4", "purple2"),
                      breaks=c("Overall Effects", "B. pendula","B. pubescens","F. sylvatica",
                               "P. abies", "Prunus","Sorbus"))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) + 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.05, "cm")) +
  xlab(expression(atop("Model Estimate of Days to Budburst")))
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
chill = ospr.stan$sm.chill
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
#ospr.td3 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td,
 #              iter = 2000,warmup=1500,control=list(adapt_delta=0.95)) 


##############################
###### real data all chilling
ospr.td4 = stan('lat_analysis/lat_trunc_inter.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.99)) 

betas <- as.matrix(ospr.td4, pars = c("mu_b_force_sp","mu_b_photo_sp","mu_b_chill_sp","mu_b_lat_sp",
"b_force", "b_photo", "b_chill"))
mcmc_intervals(betas[,1:4])

launch_shinystan(ospr.td4)
load("/Users/CatherineChamberlain/Downloads/shinystan-multiparam-gg.RData")
shinystan_multiparam_gg


#td4 <- summary(ospr.td4)$summary
#preds.4<-td4[grep("yhat", rownames(td4)),]

save(td4, file="stan/lat/output/LAT_daysBBnointer_2level.Rda")



######################################
###### real data all chilling sigmoid
ospr.td5 = stan('stan/bb/M1_daysBBnointer_2level_interceptonly_sigmoid.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.95)) 

betas.td5 <- as.matrix(ospr.td5, pars = c("b_force", "b_photo","a_chill", "b_chill"))
mcmc_intervals(betas[,1:5])

#td5 <- summary(ospr.td5)$summary
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
ospr.td2 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td, 
             iter = 2000,warmup=1500,control=list(adapt_delta=0.90)) 

