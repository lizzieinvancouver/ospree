## Started mid November 2022 ##
## From files started September 2021 (that copied Nacho's Phylo_ospree_reanalyses.R code)
## By Nacho, with some edits by Lizzie ##

## Runs (or reads) the phylogeny models, extracts some output
## Does some basic plotting

rm(list=ls())
options(stringsAsFactors = FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/phylogeny")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/phylogeny") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny")


# Loading packages
library(caper)
library(pez)
library(phytools)
library(rstan)
library(shinystan)
library(plyr)
library(dplyr)

options(mc.cores = parallel::detectCores())

#'###############################
# Flags for how to run the code #
#'###############################
runmodels <- T
runbbstanleadin <- T # leave as false to speed up Supp and ms. compilation

#'######################################
#### get data through bbstanleadin ####
#'######################################

# Flags to choose for bbstanleadin.R #
setwd("..//bb_analysis") 

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- FALSE
use.flags.for.allsppmodel <- TRUE
use.yourown.flagdesign <- FALSE
nocrops <- TRUE
agiosponly <- TRUE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
   use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
   & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp =FALSE # for the main model this is false
  use.multcuespp = FALSE
  use.cropspp = FALSE
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.allsppmodel){
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = F # change to false for using utah instead of chill portions (most models use chill portions z)
  use.zscore = TRUE # change to false to use raw predictors
  
  # Default is species complex and no crops
  use.allspp = F
  use.multcuespp = FALSE
  use.cropspp = FALSE
  
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  
  #Default is all chilling data
  use.expchillonly = FALSE # change to true for only experimental chilling 
  #note: with only exp chilling, there is only exp photo and force too.
  #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
}

source("..//bb_analysis/source/bbstanleadin.R")

namesdat <- unique(paste(bb.stan$genus,bb.stan$species,sep="_"))
bb.stan$spps <- paste(bb.stan$genus,bb.stan$species,sep="_")
bb.stan$phylo <- paste(bb.stan$genus,bb.stan$species,sep="_")


#'###################################
#### get phylogeny              ####
#'###################################

setwd("..//phylogeny") 
source("source/get_phylo_models.R")

## read and pre-process phylogeny
#phylo <- read.tree("../../data/phylogeny/SBphylo_62complex.tre")
#phylo <- read.tree("../../data/phylogeny/SBphylo_101sps.tre")
phylo <- phy.plants.ospree


namesphy <- phylo$tip.label
phylo <- force.ultrametric(phylo, method="extend")
phylo$node.label <- seq(1,length(phylo$node.label),1)
is.ultrametric(phylo)
#plot(phylo, cex=0.7)
VCVPHY <- vcv.phylo(phylo,corr=TRUE)



## deal with subgrouping

if(nocrops & agiosponly){
  gymno <- c("Metasequoia_glyptostroboides",  "Pseudotsuga_menziesii","Larix_laricina",
             "Larix_gmelinii", "Larix_decidua" ,"Larix_kaempferi",   
             "Pinus_nigra","Pinus_sylvestris","Pinus_banksiana",  
             "Pinus_contorta","Pinus_wallichiana","Pinus_strobus", 
             "Picea_abies"   ,"Picea_mariana" ,"Picea_glauca" ,
             "Cedrus_libani" ,"Abies_alba"    ,"Abies_homolepis","Ginkgo_biloba")
  croplist <- read.csv("../../data/croplist/agricultural_species.csv")
  cropgymno <- c(croplist$Species_name,gymno)
  bb.stan$crops <- ifelse(bb.stan$spps %in% cropgymno, "cropgymno","nocrop")
  cropspps <- unique(bb.stan$spps[which(bb.stan$crops=="cropgymno")])
  bb.stan <- subset(bb.stan, crops == "nocrop")
  phylo <- drop.tip(phylo, cropspps)
  VCVPHY<-vcv.phylo(phylo,corr=T)
} 

if(nocrops & !agiosponly){
  croplist <- read.csv("../../data/croplist/agricultural_species.csv")
  bb.stan$crops <- ifelse(bb.stan$spps %in% croplist$Species_name, "crop","nocrop")
  cropspps <- unique(bb.stan$spps[which(bb.stan$crops=="crop")])
  bb.stan <- subset(bb.stan, crops == "nocrop")
  phylo <- drop.tip(phylo, cropspps)
  VCVPHY<-vcv.phylo(phylo,corr=T)
} 


if(!nocrops & agiosponly){
  gymno <- c("Metasequoia_glyptostroboides",  "Pseudotsuga_menziesii","Larix_laricina",
             "Larix_gmelinii", "Larix_decidua" ,"Larix_kaempferi",   
             "Pinus_nigra","Pinus_sylvestris","Pinus_banksiana",  
             "Pinus_contorta","Pinus_wallichiana","Pinus_strobus", 
             "Picea_abies"   ,"Picea_mariana" ,"Picea_glauca" ,
             "Cedrus_libani" ,"Abies_alba"    ,"Abies_homolepis","Ginkgo_biloba")
  cropgymno <- c(gymno)
  bb.stan$crops <- ifelse(bb.stan$spps %in% cropgymno, "cropgymno","nocrop")
  cropspps <- unique(bb.stan$spps[which(bb.stan$crops=="cropgymno")])
  bb.stan <- subset(bb.stan, crops == "nocrop")
  phylo <- drop.tip(phylo, cropspps)
  VCVPHY<-vcv.phylo(phylo,corr=T)
} 

# Get spps and VCVPHY in same order
# bb.stan$spps[phylo$tip.label]
phylo$tip.label
d <- bb.stan[match(phylo$tip.label, bb.stan$spps),] # hmmm, only gives ONE match

phymatch <- data.frame(tip=phylo$tip.label, sppnum=c(1:length(phylo$tip.label)))
d <- merge(bb.stan, phymatch, by.x="spps", by.y="tip")
d <- d[order(d$sppnum),]
# Tilia_cordata versus Tilia_Cordata in phylo
nspecies <- max(d$sppnum)

## remove outliers
# d$resp
head(d)
ff = subset(d,latbi %in% c("Populus_balsamifera","Populus_tremuloides"))
d = subset(d,!latbi %in% c("Populus_balsamifera","Populus_tremuloides"))
nspecies = 192
phylo <- drop.tip(phylo, c("Populus_balsamifera","Populus_tremuloides"))
d$sppnum <- as.numeric(as.factor(d$sppnum))


## remove names of species that are wrong (e.g. Acer pseudolatanus) Malyshev2018
idswrong = which(d$spps == "Acer_pseudolatauns")
d$spps[idswrong] = "Acer_pseudoplatanus"
d$species[idswrong] = "pseudoplatanus"
d$latbi[idswrong] = "Acer_pseudoplatanus"
d$phylo[idswrong] = "Acer_pseudoplatanus"

#d$sppnum[which(d$latbi=="Acer_pseudoplatanus")]
d$sppnum[idswrong] = 127
d$sppnum[which(d$sppnum>137)] = d$sppnum[which(d$sppnum>137)]-1

nspecies = 191
phylo <- drop.tip(phylo, "Acer_pseudolatauns")


## remove names of species that are wrong (e.g. Juglans spp) 
idswrong = which(d$spps == "Juglans_spp")
d <- d[-idswrong,]
phylo <- drop.tip(phylo, "Juglans_spp")

nspecies <- length(phylo$tip.label)
phymatch2 <- data.frame(tip=phylo$tip.label, sppnum=c(1:length(phylo$tip.label)))
d2 <- merge(d, phymatch2, by.x="spps", by.y="tip")
d2 <- d2[order(d2$sppnum.y),]
d2$sppnum <- d2$sppnum.y
d <- d2
#d$chill.z = as.numeric(scale(d$chill.ports))




d$forcePlot<-round(d$force)
d$chillPlot<-round(d$chill)
d$photoPlot<-round(d$photo)

rawz<-ggpubr::ggarrange(ggplot(d,aes(forcePlot,resp))+geom_hex(aes())+ylab("days to budburst")+xlab("forcing")+ggthemes::theme_few(),
ggplot(d,aes(chillPlot,resp))+geom_hex(aes())+ylab("")+xlab("chilling")+ggthemes::theme_few(),
ggplot(d,aes(photoPlot,resp))+geom_hex(aes())+ylab("")+xlab("photoperiod")+ggthemes::theme_few(),common.legend=TRUE,nrow=1,legend = "right")

tree<-ggtree::ggtree(tr = phylo)


a<-ggplot(d,aes(forcePlot,reorder(spps,sppnum)))+geom_tile(aes(fill=resp))+
  scale_fill_binned(type = "viridis",
                    breaks = c(10,20,30,40,50,60,70,80,90,100),
                    limits = c(0, 100),
                    name="days to budburst",
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE))+
ggthemes::theme_few()+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+ylab("")+xlab("forcing")

b<-ggplot(d,aes(chillPlot,reorder(spps,sppnum)))+geom_tile(aes(fill=resp))+
  scale_fill_binned(type = "viridis",
                    breaks = c(10,20,30,40,50,60,70,80,90,100),
                    limits = c(0, 100),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE))+ggthemes::theme_few()+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+ylab("")+xlab("chilling")


c<-ggplot(d,aes(photoPlot,reorder(spps,sppnum)))+geom_tile(aes(fill=resp))+
  scale_fill_binned(type = "viridis",
    breaks = c(10,20,30,40,50,60,70,80,90,100),
                    limits = c(0, 100),
                    guide = guide_coloursteps(even.steps = FALSE,
                                              show.limits = TRUE))+
  ggthemes::theme_few()+theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+ylab("")+xlab("photoperiod")


treeplot<-ggpubr::ggarrange(tree,a,b,c,nrow=1,common.legend=TRUE,legend="right")

pdf("~/Documents/git/ospree/analyses/phylogeny/figures/rawplots_2Ds.pdf")
ggpubr::ggarrange(treeplot,rawz,nrow=2,heights=c(.8,.5),labels = c("a)","b)"))
dev.off()

library(plotly)
library(gg3D)

install.packages("scatterplot3d", dependencies = TRUE)
library(scatterplot3d)
scatterplot3d(x = d$chill, y = d$force, z = d$photo,color=d$resp)





d2<-filter(d,resp<100)
fig <- plot_ly(x = ~d2$chill, y = ~d2$force, z = ~d2$photo,color=d2$resp,size=5,legend.title="Photoperiod")

d$photobin<-NA
d$photobin[d$photo<=8]<-".<8"
d$photobin[d$photo>8&d$photo<=12]<-".8-12"
d$photobin[d$photo>12&d$photo<=16]<-"12-16"
d$photobin[d$photo>=16]<-"16+"

fig<-plot_ly(type = "scatter3d",x = ~d$chill, y = ~d$force, z = ~d$resp,color=d$photobin,colors = "YlOrBr",alpha=0.6,size=10)

fig<- fig %>% layout(scene = list(xaxis = list(title = 'chilling'),
                                   yaxis = list(title = 'forcing'),
                                   zaxis = list(title = 'days to budburst')))


pdf("~/Documents/git/ospree/analyses/phylogeny/figures/rawplots_3D.pdf")
threed
dev.off()



