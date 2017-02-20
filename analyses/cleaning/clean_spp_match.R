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
clean_names <- TPL(taxa) # patience, patience

# Manual cleanup needed for these
mancheck <- clean_names[clean_names$Plant.Name.Index == FALSE,]
manchecksp <- paste(mancheck$Genus, mancheck$Species)

## Checks on names, need to update! ##

# Actinidia deliciosa = valid on IPNI, Actinidiaceae
# Pieris japonica = valid on IPNI, Ericaceae
# Quercus faginea = valid, possibly hybridparent of Quercus x subpyrenaica, Fagaceae
# Betula pendula = valid, synonym includes Betula alba var. pendula, Betulaceae
# Populus deltoides = valid,also basionym of Aigeiros deltoides, Salicaceae. 

# "Acer psuedoplatanus" misspelling of "Acer pseudoplatanus"
d[d$taxa=="Acer psuedoplatanus","species"] = "pseudoplatanus"

# decudia to decidua
d[d$taxa=="Larix decudia","species"] = "decidua"

# Pseduotsuga to Pseudotsuga - Douglas fir
d[d$taxa=="Pseduotsuga menziesii","genus"] = "Pseudotsuga"


 
