### combine the NA and EU extent data and calculate a few parameters to be ready for analysis come the mini-retrean
##Started  1 June 2020 by Dan



# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/analyses/ranges/") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/ranges/")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges/") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges/") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/ranges/") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges")

library(rgdal)
library(maps)
library(broom)
library(ggplot2)
library("raster")

EUextent<-read.csv(file = "output/range_extent.eusps.corr.csv")
NAextent<-read.csv(file="output/range_extent.nasps_corr.csv")
EUextent$continent<-"Europe"
NAextent$continent<-"N. American"

extent.data<-rbind(EUextent,NAextent)
extent.data$lat.extent<-extent.data$max.y-extent.data$min.y ### degree distance N to S
extent.data$lon.extent<-extent.data$max.x-extent.data$min.x ### degree distance E to W

extent.data$cent.lat<-.5*(extent.data$max.y+extent.data$min.y) ## mean latitiude
extent.data$cent.lon<-.5*(extent.data$max.x+extent.data$min.x) ##mean longtiude

write.csv(extent.data,"output/full_extent_data.csv", row.names = FALSE)
