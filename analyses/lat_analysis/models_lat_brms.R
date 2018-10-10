## Started 6 July 2017 ##
## By Cat and Dan and others ##
# Updated a tiny bit by Dan 19 June 2018
# Updated 8 Oct 2018
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/lat_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/lat_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/lat_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/lat_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/lat_analysis")

library(rstanarm)
library(brms)
library(rstan)
library(sjPlot)
library(sjmisc)
library(RColorBrewer)
library(ggplot2)
library(egg)
library(broom)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("source/bbstanleadin.R")

bb.wlab <- bb
bb.wlab <- within(bb.wlab, { prov.lat <- ave(lat, complex, FUN=function(x) length(unique(x)))}) # multiple provenance.lats
bb.wlab <- subset(bb.wlab, bb.wlab$prov.lat>1) 
#bb.wlab.photo<- within(bb.wlab, { photo <- ave(photoperiod_day, complex, FUN=function(x) length(unique(x)))}) # multiple photoperiods
#bb.wlab.photo <- subset(bb.wlab.photo, bb.wlab.photo$photo>1) 
tt <- table(bb.wlab$complex)### testing 
#bb.wlab<-bb.wlab.photo

myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica", "Picea_abies", "Malus_domestica", "Ribes_nigrum", "Ulmus_complex")
bb.wlab<-subset(bb.wlab, complex%in%myspp)

lat.stan <- subset(bb.wlab, select=c(columnstokeep, "chill.cen", "photo.cen", "force.cen", "lat.cen",
                                     "force.z","chill.z", "photo.z", "lat.z"))

#write.csv(lat.stan, "lat_output/lat_arm.csv", row.names = FALSE)
lat.stan<-subset(lat.stan, lat.stan$resp<600)

lat.cen<- brm(resp ~ (force.z + photo.z + chill.z + lat.z + 
                                force.z:photo.z + force.z:chill.z + photo.z:chill.z + force.z:lat.z + 
                                  photo.z:lat.z + chill.z:lat.z)+ 
                        ((force.z + photo.z + chill.z + lat.z + 
                            force.z:photo.z + force.z:chill.z + photo.z:chill.z + force.z:lat.z + 
                            photo.z:lat.z + chill.z:lat.z)|complex), data = lat.stan, chains = 2)


#### Interaction Plots ######
cols <- colorRampPalette(brewer.pal(9,"Set1"))(6)
##### Interaction Plots code

fp<- plot_model(lat.cen, type = "pred", terms = c("force.z", "photo.z")) + xlab("Force.z") + 
  ylab("Days to Budburst") + ggtitle("") #+ theme(legend.position = "none") #+ 
  
lf<- plot_model(lat.cen, type = "pred", terms = c("force.z", "lat.z")) + xlab("Force.z") + 
  ylab("Days to Budburst") + ggtitle("")

lp<- plot_model(lat.cen, type = "pred", terms = c("photo.z", "lat.z")) + xlab("Photo.z") + 
  ylab("Days to Budburst") + ggtitle("")
lc<- plot_model(lat.cen, type = "pred", terms = c("chill.z", "lat.z")) + xlab("Chill.z") + 
  ylab("Days to Budburst") + ggtitle("")

quartz()
ggarrange(lf, lp, lc)


### Now Plot the effects
simple<-as.data.frame(tidy(lat.cen,robust = TRUE))
simple$term<-gsub(".*b_","",simple$term)
simple$term<-gsub(".*r_complex","",simple$term)
simple<-simple[!(simple$term=="sd_complex__force.z" | simple$term=="sd_complex__photo.z" | simple$term=="sd_complex__chill.z"
             | simple$term=="sd_complex__lat.z" | simple$term=="sd_complex__force.z:photo.z" | 
               simple$term=="sd_complex__force.z:chill.z" |simple$term=="sd_complex__photo.z:chill.z" |
               simple$term=="sd_complex__force.z:lat.z" |
               simple$term=="sd_complex__photo.z:lat.z" | simple$term=="sd_complex__chill.z:lat.z" |simple$term=="sigma" |
               simple$term=="lp__" | simple$term=="Intercept"),]
simple<-simple[-c(10:63),]

#myspp<-c("Betula_pendula", "Betula_pubescens", "Fagus_sylvatica", "Picea_abies", "Malus_domestica", "Ribes_nigrum", "Ulmus_complex")

simple$Jvar<-NA
simple$Jvar<-ifelse(simple$term=="force.z", 11, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,force.z]", 10.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,force.z]", 10.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,force.z]", 10.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,force.z]", 10.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,force.z]", 10.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,force.z]", 10.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,force.z]", 10.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="photo.z", 10, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,photo.z]", 9.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,photo.z]", 9.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,photo.z]", 9.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,photo.z]", 9.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,photo.z]", 9.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,photo.z]", 9.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,photo.z]", 9.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="chill.z", 9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,chill.z]", 8.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,chill.z]", 8.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,chill.z]", 8.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,chill.z]", 8.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,chill.z]", 8.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,chill.z]", 8.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,chill.z]", 8.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="lat.z", 8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,lat.z]", 7.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,lat.z]", 7.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,lat.z]", 7.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,lat.z]", 7.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,lat.z]", 7.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,lat.z]", 7.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,lat.z]", 7.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="force.z:photo.z", 7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,force.z:photo.z]", 6.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,force.z:photo.z]", 6.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,force.z:photo.z]", 6.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,force.z:photo.z]", 6.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,force.z:photo.z]", 6.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,force.z:photo.z]", 6.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,force.z:photo.z]", 6.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="force.z:chill.z", 6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,force.z:chill.z]", 5.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,force.z:chill.z]", 5.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,force.z:chill.z]", 5.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,force.z:chill.z]", 5.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,force.z:chill.z]", 5.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,force.z:chill.z]", 5.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,force.z:chill.z]", 5.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="photo.z:chill.z", 5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,photo.z:chill.z]", 4.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,photo.z:chill.z]", 4.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,photo.z:chill.z]", 4.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,photo.z:chill.z]", 4.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,photo.z:chill.z]", 4.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,photo.z:chill.z]", 4.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,photo.z:chill.z]", 4.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="force.z:lat.z", 4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,force.z:lat.z]", 3.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,force.z:lat.z]", 3.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,force.z:lat.z]", 3.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,force.z:lat.z]", 3.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,force.z:lat.z]", 3.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,force.z:lat.z]", 3.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,force.z:lat.z]", 3.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="photo.z:lat.z", 3, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,photo.z:lat.z]", 2.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,photo.z:lat.z]", 2.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,photo.z:lat.z]", 2.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,photo.z:lat.z]", 2.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,photo.z:lat.z]", 2.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,photo.z:lat.z]", 2.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,photo.z:lat.z]", 2.3, simple$Jvar)

simple$Jvar<-ifelse(simple$term=="chill.z:lat.z", 2, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pendula,chill.z:lat.z]", 1.9, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Betula_pubescens,chill.z:lat.z]", 1.8, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Fagus_sylvatica,chill.z:lat.z]", 1.7, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Malus_domestica,chill.z:lat.z]", 1.6, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Picea_abies,chill.z:lat.z]", 1.5, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ribes_nigrum,chill.z:lat.z]", 1.4, simple$Jvar)
simple$Jvar<-ifelse(simple$term=="[Ulmus_complex,chill.z:lat.z]", 1.3, simple$Jvar)


simple$species<-c(0,0,0,0,0,0,0,0,0, 1,2,3,4,5,6,7, 1,2,3,4,5,6,7, 1,2,3,4,5,6,7, 1,2,3,4,5,6,7, 1,2,3,4,5,6,7, 
                  1,2,3,4,5,6,7, 1,2,3,4,5,6,7, 1,2,3,4,5,6,7, 1,2,3,4,5,6,7)

cols <- colorRampPalette(brewer.pal(9,"Set1"))(8)
estimates<-c("Forcing", "Photoperiod", "Chilling", "Latitude", "Forcing x Photoperiod",
             "Forcing x Chilling", "Photoperiod x Chilling",
             "Forcing x Latitude", "Photoperiod x Latitude", "Chilling x Latitude")
estimates<-rev(estimates)
latmod<-ggplot(simple, aes(x=lower, xend=upper, y=Jvar, yend=Jvar, col=as.factor(species))) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=Jvar, col=as.factor(species), size=as.factor(species))) +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("1"=expression(paste(italic("Betula pendula"))),
                               "2"=expression(paste(italic("Betula pubescens"))),
                               "3"=expression(paste(italic("Fagus sylvatica"))),
                               "4"=expression(paste(italic("Malus domestica"))),
                               "5"=expression(paste(italic("Picea abies"))),
                               "6"=expression(paste(italic("Ribes nigrum"))),
                               "7"=expression(paste(italic("Ulmus complex"))),
                               "0"="Overall Effects"))+
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  scale_y_discrete(limits = sort(unique(simple$term)), labels=estimates) +
  xlab("Model Estimate of Change \nin Days to Budburst") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=5), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = c(0.85,0.25),
        legend.text.align = 0) + #+ coord_cartesian(ylim=c(1,5), xlim=c(-20, 10))
  scale_size_manual(values=c(3, 1, 1, 1, 1, 1, 1, 1, 1, 1), name="Species",
                    labels=c("1"=expression(paste(italic("Betula pendula"))),
                             "2"=expression(paste(italic("Betula pubescens"))),
                             "3"=expression(paste(italic("Fagus sylvatica"))),
                             "4"=expression(paste(italic("Malus domestica"))),
                             "5"=expression(paste(italic("Picea abies"))),
                             "6"=expression(paste(italic("Ribes nigrum"))),
                             "7"=expression(paste(italic("Ulmus complex"))),
                             "0"="Overall Effects"))
quartz()
latmod





