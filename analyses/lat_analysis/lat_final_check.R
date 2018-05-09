## Started 8 May 2018 ##
## By Cat and Dan ##

## Try to run REAL Ospree data ##
## With Stan! ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

#library(rstan)
library(ggplot2)
#library(shinystan)
#library(bayesplot)
library(rstanarm)
library(brms)
library(ggstance)
library(forcats)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("danflynn", getwd())>0)) { 
  setwd("~/Documents/git/ospree") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses")

setwd("~/Documents/git/ospree/analyses")

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())


########################
#### get the data
lat<-read.csv("lat_analysis/lat_output/lat_studies.csv", header=TRUE)
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

bb.wlab<-bb.wlab[which(bb.wlab$datasetID%in%lat$datasetID),]
#sort(unique(bb.wlab$datasetID))
#sort(unique(lat$datasetID))
#tt <- table(bb.wlab$complex)
#bb.wlab <- subset(bb.wlab, complex %in% names(tt[tt > 45])) ### testing 
#unique(bb.wlab$complex)
myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica",
         "Picea_abies", "Picea_glauca","Pseudotsuga_menziesii")

bb.wlab<-dplyr::filter(bb.wlab, complex%in%myspp)

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
ospr.prepdata <- subset(bb.wlab.sm, select=c("resp", "chill", "photo", "force", "complex", "lat"))
dim(subset(bb.wlab.sm, is.na(chill)==FALSE & is.na(photo)==FALSE & is.na(force)==FALSE 
           & is.na(lat)==FALSE))
ospr.stan <- ospr.prepdata[complete.cases(ospr.prepdata),]
ospr.stan$sp <- as.numeric(as.factor(ospr.stan$complex))

## Center?
ospr.stan$sm.chill<-ospr.stan$chill/240

ospr.stan<-ospr.stan[which(ospr.stan$resp!=999),]

lat.stan_final<-stan_glmer(resp~ force + photo + sm.chill + lat + photo:lat + (1|sp) +
                             (force-1|sp) + (photo-1|sp) + (sm.chill-1|sp) +
                             (lat-1|sp) + (photo:lat-1|sp), data=ospr.stan)
lat.brm_final<-brm(resp~ force + photo + sm.chill + lat + photo:lat + (1|sp) +
                             (force-1|sp) + (photo-1|sp) + (sm.chill-1|sp) +
                             (lat-1|sp) + (photo:lat-1|sp), data=ospr.stan)


m<-lat.brm_final
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
dflong<- tidyr::gather(dftot, var, value, force:`photo:lat`, factor_key=TRUE)

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

estimates<-c("Forcing", "Photoperiod", "Chill Portions", "Latitude", 
             "Photoperiod x Latitude")
estimates<-c("Forcing", "Photoperiod", "Chill Portions", "Latitude", "Photoperiod x Latitude")
dfwide$legend<-factor(dfwide$sp,
                      labels=c("Overall Effects", "B. pendula","B. pubescens","F. sylvatica",
                               "P. abies", "P. glauca", "P. menziesii"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
quartz()
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, height =0, width=0)+
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
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.05, "cm")) +
  xlab(expression(atop("Model Estimate of Days to Budburst")))
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
                   labels=c("Overall Effects", "B. pendula","B. pubescens","F. sylvatica",
                            "P. abies", "P. glauca", "P. menziesii"))
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

