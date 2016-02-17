##cleaning up species names for the budburst review
##EJ Forrestel, 20 January 2016

require('Taxonstand')

##read in csv file with spp names
setwd("~/Documents/git/budreview")

dat <- read.csv("growthchambers_litreview.csv") 
dat$taxa <- paste(dat$genus,dat$species,sep=" ")

##checking number of study reps and experiment reps for each species
dat$sp_study <- paste(dat$genus,dat$species,dat$datasetID,dat$study,sep="_")
sp_study <- table(dat$sp_study)
sp_study <- cbind(names(sp_study),sp_study)
taxa <- paste(dat$genus,dat$species,sep=" ")
taxa <- unique(taxa)

##matching to TPL 1.1
clean_names <- TPL(taxa)


# Manual cleanup needed for these
mancheck <- clean_names[clean_names$Plant.Name.Index == FALSE,]
manchecksp <- paste(mancheck$Genus, mancheck$Species)

# Actinidia deliciosa = valid on IPNI, Actinidiaceae
# Pieris japonica = valid on IPNI, Ericaceae
# Quercus faginea = valid, possibly hybridparent of Quercus x subpyrenaica, Fagaceae
# Betula pendula = valid, synonym includes Betula alba var. pendula, Betulaceae
# Populus deltoides = valid,also basionym of Aigeiros deltoides, Salicaceae. 

# "Acer psuedoplatanus" misspelling of "Acer pseudoplatanus"
dat[dat$taxa=="Acer psuedoplatanus","species"] = "pseudoplatanus"

# decudia to decidua
dat[dat$taxa=="Larix decudia","species"] = "decidua"

# Pseduotsuga to Pseudotsuga - Douglas fir
dat[dat$taxa=="Pseduotsuga menziesii","genus"] = "Pseudotsuga"

# Write out taxa for next step


##use geosphere to pull daily latitudes and photoperiods

 