#started Dec 15, 2021 by Deirdrerm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/github/ospree/analyses/traits")
} else if
(length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits")
}

# aim of this code is to adapt the mu plot code from the bb ms for the 26 spp. we are using in the traitors ms. 
figpath <- "figures"
figpathmore <- "traitors26spp"
xlim=c(-32,10)

files <- list.files(path = "output", pattern =".RDS" )
files
  
Model <- readRDS(paste("output/", files[1], sep = ""))

traitors.sp <- c("Acer_pensylvanicum", "Acer_pseudoplatanus", "Acer_saccharum", "Aesculus_hippocastanum", "Alnus_glutinosa", "Alnus_incana", "Betula_pendula", "Betula_populifolia", "Corylus_avellana", "Fagus_grandifolia","Fagus_sylvatica", "Fraxinus_excelsior", "Juglans_regia", "Populus_tremula", "Prunus_padus", "Prunus_serotina", "Quercus_alba", "Quercus_coccifera", "Quercus_ilex", "Quercus_petraea", "Quercus_robur", "Quercus_rubra", "Quercus_velutina", "Rhamnus_cathartica", "Sorbus_aucuparia", "Ulmus_pumila")

## functions for plotting 

source("Rfiles/source/bb_muplot.R")
source("Rfiles/source/plotletfx.R")

ModelFit <- rstan::extract(Model)

sumer.ni[grep("mu_", rownames(sumer.ni)),]
modelhere <- ModelFit

cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=12)
alphahere = 0.4

#function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2)
muplotfx(modelhere, "", 7, 8, c(0,3), xlim , 12, 3)

