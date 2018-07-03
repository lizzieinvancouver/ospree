#Checking how much data we have/lose with different chilling units
#By Ailene
#Started 2 July 2018
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

# Load libraries
library(dplyr)
library(tidyr)
library(brms)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# 1. Get the data
d <- read.csv("../output/ospree_clean_withchill_BB.csv")
dim(d)#7370 rows
#We have chilling in 3 different units. Let's see how much data we
length(which(is.na(d$Total_Utah_Model)))#2662
length(which(is.na(d$Total_Chilling_Hours)))#2780
length(which(is.na(d$Total_Chill_portions)))#2780

#for each unit
utah<-d[-which(is.na(d$Total_Utah_Model)),]
hrs<-d[-which(is.na(d$Total_Chilling_Hours)),]
portions<-d[-which(is.na(d$Total_Chill_portions)),]

#how many studies with each unit?
unique(utah$datasetID)#63 studies
unique(hrs$datasetID)#61 studies
unique(portions$datasetID)#61 studies

#how many species with each unit?
unique(utah$genus)#85 genera
unique(hrs$genus)#85 genera
unique(portions$genus)#85 genera

#do model estimates change with different units?
source("source/bbstanleadin_units.R")

#Models with no interactions:
#Utah
m2l.ni.brms <- brm(y ~ chill+force+photo+#fixed effects
((chill+force+photo)|sp), #random effects
data = datalist.bb, 
chains = 2) 
#save coefs
coefs.utah.m2l.ni.brms<-fixef(m2l.ni.brms)[,1]
#brms model says a: 71.19, f=-22.10, p=--4.87 , c=-12.93; species sigma: 15.81 

#Chill portions
m2l.ni.brms.cp <- brm(y ~ chill+force+photo+#fixed effects
                     ((chill+force+photo)|sp), #random effects
                   data = datalist.bb.cports, 
                   chains = 2) 
#save coefs
coefs.cp.m2l.ni.brms<-fixef(m2l.ni.brms.cp)[,1]
#brms model says a: 71.19, f=-22.10, p=--4.87 , c=-12.93; species sigma: 15.81 

#zscored:
#Utah
m2l.nin.brms.z <- brm(resp ~ force.z + photo.z + chill.z +#main effects
                        ((force.z + photo.z + chill.z)|complex.wname), data = bb.stan,
                      chains = 2, cores = 2)
coefs.utah.m2l.ni.brms.z<-fixef(m2l.nin.brms.z)[,1]

#chill portions
m2l.nin.brms.z.cp <- brm(resp ~ force.z + photo.z + chill.ports.z +#main effects
                        ((force.z + photo.z + chill.ports.z)|complex.wname), data = bb.stan,
                      chains = 2, cores = 2)
coefs.cp.m2l.ni.brms.z<-fixef(m2l.nin.brms.z.cp)[,1]

coefs<-as.data.frame(cbind(coefs.utah.m2l.ni.brms,coefs.cp.m2l.ni.brms,coefs.utah.m2l.ni.brms.z,coefs.cp.m2l.ni.brms.z))
colnames(coefs)<-c("utah","chillport","utah.z","chillport.z")

#Models with interactions
#Utah
m2l.wistudy.brms <- brm(y ~ chill+force +photo+
                          chill:force+chill:photo+force:photo + 
                          ((chill+force+photo)|sp)+(1|study), 
                        data = datalist.bb,
                        chains = 2, cores = 2,control = list(max_treedepth = 12))
#save coefs and R2
coefs.utah.m2l.wistudy.brms<-c(fixef(m2l.wistudy.brms)[,1],
                            bayes_R2(coefs.utah.m2l.wistudy.brms))
#brms model says a: 71.19, f=-22.10, p=--4.87 , c=-12.93; species sigma: 15.81 

#Chill portions
m2l.wistudy.brms.cp <- brm(y ~ chill+force +photo+
                          chill:force+chill:photo+force:photo + 
                          ((chill+force+photo)|sp)+(1|study), 
                        data = datalist.bb.cports,
                        chains = 2, cores = 2,control = list(max_treedepth = 12))
#save coefs and R2
coefs.cp.m2l.wistudy.brms<-c(fixef(m2l.wistudy.brms.cp)[,1],
                          bayes_R2(m2l.wistudy.brms.cp))

#zscored:
#Utah
m2l.wistudy.brms.z <- brm(y ~ chill+force+photo+
                          chill:force+chill:photo+force:photo + 
                          ((chill+force+photo)|sp)+(1|study), 
                        data = datalist.bb.z,
                        chains = 2, cores = 2,control = list(max_treedepth = 12))
#save coefs and R2
coefs.utah.m2l.wistudy.brms.z<-c(fixef(m2l.wistudy.brms.z)[,1],
                                 bayes_R2(m2l.wistudy.brms.z)[1])
                                 
#brms model says a: 71.19, f=-22.10, p=--4.87 , c=-12.93; species sigma: 15.81 

#Chill portions
m2l.wistudy.brms.cp.z <- brm(y ~ chill+force+photo+
                             chill:force+chill:photo+force:photo + 
                             ((chill+force+photo)|sp)+(1|study), 
                           data = datalist.bb.cports.z,
                           chains = 2, cores = 2,control = list(max_treedepth = 12))
#save coefs
coefs.cp.m2l.wistudy.brms.z<-c(fixef(m2l.wistudy.brms.cp.z)[,1],
                          bayes_R2(m2l.wistudy.brms.cp.z)[1])

coefs2<-as.data.frame(cbind(coefs.utah.m2l.wistudy.brms,coefs.cp.m2l.wistudy.brms,coefs.utah.m2l.wistudy.brms.z,coefs.cp.m2l.wistudy.brms.z))
colnames(coefs2)<-c("utah","chillport","utah.z","chillport.z")

coefs3<-as.data.frame(cbind(coefs.utah.m2l.wistudy.brms.z,coefs.cp.m2l.wistudy.brms.z))
colnames(coefs3)<-c("utah.z","chillport.z")
rownames(coefs3)[8]<-"R2"
coefs3<-round(coefs3, digits=3)
#check min/max/mean for different chilling units:
min(datalist.bb.cports$chill, na.rm=TRUE)#0
max(datalist.bb.cports$chill, na.rm=TRUE)#180.0626
mean(datalist.bb.cports$chill, na.rm=TRUE)# 71.53816
hist(datalist.bb.cports$chill)
min(datalist.bb$chill, na.rm=TRUE)#-5.239583
max(datalist.bb$chill, na.rm=TRUE)# 19.68333
mean(datalist.bb$chill, na.rm=TRUE)#5.008387
hist(datalist.bb$chill)
min(datalist.bb.cports.z$chill, na.rm=TRUE)#-1.89686
max(datalist.bb.cports.z$chill, na.rm=TRUE)# 2.845298
mean(datalist.bb.cports.z$chill, na.rm=TRUE)# -0.0128191
hist(datalist.bb.cports.z$chill)
min(datalist.bb.z$chill, na.rm=TRUE)#-3.124661
max(datalist.bb.z$chill, na.rm=TRUE)# 4.238766
mean(datalist.bb.z$chill, na.rm=TRUE)#-0.02884226
hist(datalist.bb.z$chill)

#utah units are more normally distributed
hist(d$Total_Utah_Model)
hist(d$Total_Chill_portions)
min(d$Total_Chill_portions, na.rm=TRUE)#0
max(d$Total_Chill_portions, na.rm=TRUE)#180.0626
mean(d$Total_Chill_portions, na.rm=TRUE)# 71.37067
