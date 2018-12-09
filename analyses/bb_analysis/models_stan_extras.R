#####################################################
## Taken from models_stan.R in early December 2018 ##
#####################################################
## With a few tweaks the PEP 725 code should still run ##

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
# Currently we use bb.stan.expphoto
bb.stan <- bb.stan.expphoto 
bb.stan <- bb.stan[which(bb.stan$complex.wname %in% pepspp),] 
bb.stan$complex <- NULL
dim(bb.stan)
bb.stan <- merge(bb.stan, somespp, by="complex.wname")
dim(bb.stan)
## END below copied from models_stan.R
   
bb.stan$quickgdd <- bb.stan$force*bb.stan$resp
ggplot(bb.stan, aes(chill, quickgdd, color=complex.wname)) +
    geom_point()
ggplot(bb.stan, aes(chill, quickgdd, color=complex.wname)) +
    geom_point() +
    facet_wrap(~complex.wname, ncol=3)

load("stan/output/M1_daysBBnointer_2levelpepspp.Rda")
m1.bb <- m2l.ni

pdf(file.path(figpath, "M1nointer_wpepspp.pdf"), width = 7, height = 6)
source("source/bb_muplot.R")
dev.off()
}
