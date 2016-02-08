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
clean_names[clean_names$Plant.Name.Index == FALSE,]





##use geosphere to pull daily latitudes and photoperiods

 