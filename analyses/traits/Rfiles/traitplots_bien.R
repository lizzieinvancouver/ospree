rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {setwd("~/Documents/github/ospree/analyses")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

trt<-read.csv('~/Desktop/ospree_trait_analysis/bienwide.csv', header=TRUE)


source("traits/source/bbdataplease.R")

names(trt)
unique(trt$whole.plant.growth.form)
head(bb.noNA)

names(bb.noNA)

temp<-strsplit(as.character(trt$speciesname), " ", fixed=TRUE)
trt$genus<-unlist(lapply(temp, function(x) x[1]))
trt$species<-unlist(lapply(temp, function(x) x[2]))

unique(trt$genus)
unique(trt$speciesname)
unique(bb.noNA$respvar)

qual<-subset(trt, speciesname == "Viburnum lantanoides" & whole.plant.height>0)

head(qual)
colnames(qual)
unique(qual$ldmc)

qual2<-qual[, c("genus","species","unitname","whole.plant.height")]

head(qual2)

plot(density(na.omit(qual2$whole.plant.height)))


sp_list<-c("Quercus alba","Kalmia angustifolia", "Fagus sylvatica","Betula papyrifera", "Viburnum cassinoides", "Spirea alba","Corylus cornuta","Ribes nigrum","Acer pseudoplatanus","Prunus padus")

sp_list<-unique(trt$speciesname)
names(trt)
taylor<-data.frame(genus=c(),species=c(), mean_whole_ht=c())
for (i in 1:length(sp_list)){
  temp<-subset(trt, speciesname == sp_list[i] & whole.plant.height>0)
  taylor.temp<-data.frame(genus=temp$genus[1],species=temp$species[1],mean_whole_ht=mean(temp$whole.plant.height))
  taylor<-rbind(taylor, taylor.temp)
}
head(taylor)
colnames(qual)
unique(taylor$genus)

plot(na.omit(taylor$mean))


qual<-subset(trt, whole.plant.height>0)
unique(qual$unitname)

# Merge with the ospree data for each species, but only for respvar== budburst day
names(bb.noNA)
unique(bb.noNA$chilltemp)


beyonce<-subset(bb.noNA, respvar == "daystobudburst" & response.time<999 & chilltemp=="")
names(beyonce)
beyonce<-beyonce[,c(5,6,26,27,28)];
#head(beyonce)
mean.resp<-beyonce %>%
  group_by(genus,species) %>%
  summarise(mean.resp=mean(response.time))

bey<-merge(taylor, mean.resp, by=c("genus","species"))
head(bey)

unique(beyonce$response)
plot(mean.resp~mean_whole_ht,data=bey)
abline(lm(mean.resp~mean_whole_ht,data=bey), col="red")
summary(lm(mean.resp~mean_whole_ht,data=bey), col="red")
# To do: 
#create an average trait value for each species
# subset ospree for the subset of species we are using here and combine it with the trait data

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    21.0946     3.6834   5.727 1.14e-06 ***
#   mean_whole_ht   0.6100     0.2358   2.587   0.0134 *  