#####################################################
## Taken from models_stan.R in early December 2018 ##
#####################################################
## With a few tweaks the PEP 725 code should still run ##
## Right now, each chunk is set up to run separately (model, then plotting) ##

# Run models_stan.R until before first Stan model runs ... then run the below. 

#########################
## For PEP 725 species ##
#########################
if(use.pep){
getspp <- subset(bb.stan, select=c("complex", "complex.wname"))
allspp <- getspp[!duplicated(getspp), ]
allspp <- allspp[order(allspp$complex),]
pepspp <- c("Acer_pseudoplatanus", "Aesculus_hippocastanum", "Betula_pendula", "Corylus_avellana",
    "Fagus_sylvatica", "Larix_decidua", "Picea_abies", "Populus_tremula",
    "Prunus_padus","Quercus_robur", "Syringa_vulgaris")
# gymnastics to renumber species
somespp <-  allspp[which(allspp$complex.wname %in% pepspp),]
somespp$complex <- NULL
somespp$complex <- seq(1:nrow(somespp))

bb.stan <- bb.stan[which(bb.stan$complex.wname %in% pepspp),] 
bb.stan$complex <- NULL
dim(bb.stan)
bb.stan <- merge(bb.stan, somespp, by="complex.wname")
dim(bb.stan)

datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
)

}

##################################
## PLOTTING For PEP 725 species ##
##################################
# As of 11 July 2019 this runs if you first run models_stan.R through
# to after this: source("source/bbstanleadin.R") ##
# This code could easily be added to another file for the budburst ms if needed! ##

if(use.pep){
getspp <- subset(bb.stan, select=c("complex", "complex.wname"))
allspp <- getspp[!duplicated(getspp), ]
allspp <- allspp[order(allspp$complex),]
pepspp <- c("Acer_pseudoplatanus", "Aesculus_hippocastanum", "Betula_pendula", "Corylus_avellana",
    "Fagus_sylvatica", "Larix_decidua", "Picea_abies", "Populus_tremula",
    "Prunus_padus","Quercus_robur", "Syringa_vulgaris")
# gymnastics to renumber species
somespp <-  allspp[which(allspp$complex.wname %in% pepspp),]
somespp$complex <- NULL
somespp$complex <- seq(1:nrow(somespp))

bb.stan <- bb.stan[which(bb.stan$complex.wname %in% pepspp),] 
bb.stan$complex <- NULL
dim(bb.stan)
bb.stan <- merge(bb.stan, somespp, by="complex.wname")
dim(bb.stan)
   
bb.stan$quickgdd <- bb.stan$force*bb.stan$resp
bb.stan$utah <- bb.stan$chill*240
bb.stan$latbi <- paste(bb.stan$genus, bb.stan$species)

## GDD by chill unit plots, a few options
pdf(file.path("figures/gddbyutah_pepspp.pdf"), width = 7, height = 5)
ggplot(bb.stan, aes(utah, quickgdd, color=latbi)) +
    geom_point() +
    xlim(-10, 3300) +
    ylim(-55, 4500) +
    labs(x="Chilling (Utah units)", y="Growing degree days (GDD)", colour="Species") +
    geom_segment(y=-50, x=6.7*240, yend=-50, xend=12.5*240, col="black") + 
    theme_classic() +
    theme(legend.text = element_text(face = "italic"))
dev.off()

ggplot(bb.stan, aes(chill, quickgdd, color=complex.wname)) +
    geom_point() +
    xlim(-1, 15) +
    # geom_segment(y=-15, x=4.95, yend=-15, xend=14.70, col="black") + # these numbers come from comparetopepsims.R: range(bp$chillutah)/240
    geom_segment(y=-15, x=6.7, yend=-15, xend=12.5, col="black") + # these numbers come from comparetopepsims.R: quantile(bp$chillutah, c(0.1, 0.9))/240 (accurate as of 11 Jul 2019)
    theme_classic()

ggplot(bb.stan, aes(chill, quickgdd, color=complex.wname)) +
    geom_point() +
    facet_wrap(~complex.wname, ncol=3)

ggplot(subset(bb.stan, complex.wname=="Betula_pendula"), aes(chill, quickgdd, color=complex.wname)) +
    geom_point() +
    xlim(-1, 15) +
    geom_segment(y=-15, x=4.95, yend=-15, xend=14.70, col="black") + # these numbers come from comparetopepsims.R: range(bp$chillutah)/240
    theme_classic()

bb.stan.2spp <- bb.stan[which(bb.stan$latbi %in% c("Betula pendula", "Fagus sylvatica")),]

ggplot(bb.stan.2spp, aes(utah, quickgdd, colour=photo)) +
    geom_point() +
    facet_wrap(~latbi, scales="free") +
    ylab("Forcing units until budburst") +
    xlab(expression(paste("Chilling units until budburst (Utah)"), sep="")) +
    theme_classic()

pdf(file.path("figures/gddbyutah_betpenfagsyl.pdf"), width = 10, height = 4.5)
ggplot(bb.stan.2spp, aes(utah, quickgdd, colour=datasetID)) +
    geom_point() +
    facet_wrap(~latbi, scales="free") +
    ylab("Forcing units until budburst") +
    xlab(expression(paste("Chilling units until budburst (Utah)"), sep="")) +
    theme_classic()
dev.off()

ggplot(bb.stan.2spp, aes(utah, quickgdd, color=latbi)) +
    geom_point() +
    ylab("Forcing units until budburst") +
    xlab(expression(paste("Chilling units until budburst (Utah)"), sep="")) +
    theme_classic()

ggplot(bb.stan.2spp, aes(utah, quickgdd, color=datasetID, group=latbi)) +
    geom_point(aes(shape=latbi, color=datasetID)) +
    ylab("Forcing units until budburst") +
    xlab(expression(paste("Chilling units until budburst (Utah)"), sep="")) +
    theme_classic()

# Or do by studyID? (See also in decsens repo betpenexp.R)
ggplot(subset(bb.stan.2spp, latbi=="Fagus sylvatica"), aes(utah, quickgdd, colour=photo)) +
    geom_point() +
    facet_wrap(~datasetID) +
    ylab("Forcing units until budburst") +
    xlab(expression(paste("Chilling units until budburst (Utah)"), sep="")) +
    theme_classic()

ggplot(subset(bb.stan.2spp, latbi=="Betula pendula"), aes(utah, quickgdd, colour=photo)) +
    geom_point() +
    facet_wrap(~datasetID) +
    ylab("Forcing units until budburst") +
    xlab(expression(paste("Chilling units until budburst (Utah)"), sep="")) +
    theme_classic()



## Model plot!
load("stan/output/M1_daysBBnointer_2levelpepspp.Rda")
m1.bb <- m2l.ni

pdf(file.path(figpath, "M1nointer_wpepspp.pdf"), width = 7, height = 6)
source("source/bb_muplot.R")
dev.off()
}
