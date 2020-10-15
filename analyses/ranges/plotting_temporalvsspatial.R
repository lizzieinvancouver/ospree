## Started 9 September 2020 ##
## By Lizzie so far ##

## Plotting spatial versus temporal variability ##

## housekeeping
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

# library
library(ggplot2)

# get the data 
climeur <- read.csv("output/Synthesis_climate_EUsps_corr.csv")
climnam <- read.csv("output/Synthesis_climate_NAMsps.csv")

climeur$X <- NULL
climeur$cont <- "Eur"
climnam$cont <- "NAM"

clim <- rbind(climeur, climnam)

# spatialtemporal_means.pdf
ggplot(clim, aes(x=Geo.Mean, y=Temp.Mean, color=species, group=cont, shape=cont)) +
    geom_point() +
    geom_smooth(aes(color=cont, group=cont), method = "lm", linetype = 2, lwd=0.25, se = TRUE) +
    facet_wrap(.~variable, scales="free") +
    theme_minimal()  +
    theme(
     strip.background = element_rect(
     color="white", fill="gray", size=0.75
     )
   )

# spatialtemporal_sds.pdf
ggplot(clim, aes(x=Geo.SD, y=Temp.SD, color=species, group=cont, shape=cont)) +
    geom_point() +
    geom_smooth(aes(color=cont, group=cont), method = "lm", linetype = 2, lwd=0.25, se = TRUE) +
    facet_wrap(.~variable, scales="free") +
    theme_minimal() +
    theme(
     strip.background = element_rect(
     color="white", fill="gray", size=0.75
     )
   )

# geo_meansd.pdf
ggplot(clim, aes(x=Geo.Mean, y=Geo.SD, color=species, group=cont, shape=cont)) +
    geom_point() +
    geom_smooth(aes(color=cont, group=cont), method = "lm", linetype = 2, lwd=0.25, se = TRUE) +
    facet_wrap(.~variable, scales="free") +
    theme_minimal() +
    theme(
     strip.background = element_rect(
     color="white", fill="gray", size=0.75
     )
   )

# temporal_meansd.pdf
ggplot(clim, aes(x=Temp.Mean, y=Temp.SD, color=species, group=cont, shape=cont)) +
    geom_point() +
    geom_smooth(aes(color=cont, group=cont), method = "lm", linetype = 2, lwd=0.25, se = TRUE) +
    facet_wrap(.~variable, scales="free") +
    theme_minimal() +
    theme(
     strip.background = element_rect(
     color="white", fill="gray", size=0.75
     )
   )
