## Started 20 January 2016 ##
## By EJ Forrestel, updates by Lizzie in Feb 2017 ##

## Cleaning up species names in OSPREE ##

library('Taxonstand')

# You need to run cleanmerge_all through step 6 before running this #
# Well, I guess you can run it before step 6, but we're not working with non-woody species #

d$taxa <- paste(d$genus,d$species,sep=" ")

##checking number of study reps and experiment reps for each species
d$sp_study <- paste(d$genus,d$species,d$datasetID,d$study,sep="_")
sp_study <- table(d$sp_study)
sp_study <- cbind(names(sp_study),sp_study)
taxa <- paste(d$genus,d$species,sep=" ")
taxa <- unique(taxa)

##matching to TPL 1.1
clean_names <- TPL(taxa) # patience, patience ...
# Updated 17 Nov 2022: warnings traced back to TPLck with W Pearse and J Davies and they seemed to think it was okay to ignore if names look good. 
# Also, they mentioned that TPL (the Plant List) is superseded is by World Flora Online ... where some of the unresolved are resolved ... and maybe this is not the IDEAL command for future use (for newer work in the lab, check for newer commands -- actually ... World Flora has a new R package!)
# subset(clean_names, New.Taxonomic.status=="Unresolved")
# subset(clean_names, New.Taxonomic.status=="")

# Manual cleanup needed for these
mancheck <- clean_names[clean_names$Plant.Name.Index == FALSE,]
manchecksp <- paste(mancheck$Genus, mancheck$Species)

## Notes on names ##

# Notes from January 2016
# Actinidia deliciosa = valid on IPNI, Actinidiaceae
# Pieris japonica = valid on IPNI, Ericaceae
# Quercus faginea = valid, possibly hybridparent of Quercus x subpyrenaica, Fagaceae
# Betula pendula = valid, synonym includes Betula alba var. pendula, Betulaceae
# Populus deltoides = valid,also basionym of Aigeiros deltoides, Salicaceae.

# Notes from February 2017
# Hamamelis japonica = seems okay according to IPNI
# Liquidambar orientalis = seems okay according to IPNI

# Notes from April 2017
# If you run this twice in a row (from start of cleanmerge_all.R through to end of this script), you get slightly different results. I checked a few names though and think we're good. I think the issue is something about not connecting to the theplantlist server consistently. 

# Notes from September 2019 -- Nov 2022 -- Lizzie moved the below fixes to clean_woody_sps.R
# Acer seudolatauns should be Acer pseudoplatanus
# Cornus cornuta should be Corylus cornuta
# Spirea alba is okay

