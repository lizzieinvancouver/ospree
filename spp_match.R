##cleaning up species names for the budburst review
##EJ Forrestel, 20 January 2016

require('Taxonstand')

##read in csv file with spp names
dat <- read.csv("~/Documents/budreview/growthchambers_litreview_2016-01-15.csv")
dat$taxa <- paste(dat$genus,dat$species,sep=" ")

##checking number of study reps and experiment reps for each species
dat$sp_study <- paste(dat$genus,dat$species,dat$datasetID,dat$study,sep="_")
sp_study <- table(dat$sp_study)
sp_study <- cbind(names(sp_study),sp_study)
taxa <- paste(dat$genus,dat$species,sep=" ")
taxa <- unique(taxa)

##matching to TPL 1.1
clean_names <- TPL(taxa)









##use geosphere to pull daily latitudes and photoperiods

 