## Started 30 January 2021 ##
## By Lizzie so far ... ## 


#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
# Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Documents/ospree_trait_analysis/")
} else if
(length(grep("Lizzie", getwd())>0)) {   setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/traits") 
} 

# Get the data
dat <- read.csv("input/try_bien_nodups.csv") 
dat <- dat[,c("SpeciesName", "TraitName", "TraitValue", "UnitName", "Latitude",
    "Longitude", "project_pi", "database", "DatasetID")]
names(dat) <- c("latbi", "traitname", "traitvalue", "unit", "lat", "long", "project_pi",
    "database", "DatasetID")

heighter <- subset(dat, traitname=="Plant_height_vegetative")

# Percentage of data that is height?
nrow(heighter)/nrow(dat)

# Eeek! Definitely some name cleaning still needed
sort(unique(heighter$latbi)) 

# Just do one example species for now... 
testsp <- subset(heighter, latbi=="Quercus alba")
nrow(testsp)

howmanydf <- data.frame(resample.n=numeric(), mean=numeric(), sd=numeric())
for(i in 1:15000){
    mysample <- sample(testsp$traitvalue, i)
    dfadd <- data.frame(resample.n=i, mean=mean(mysample), sd=sd(mysample))
    howmanydf <- rbind(howmanydf, dfadd)
}

# the central limit theorem!
plot(mean~resample.n, data=howmanydf)
plot(sd~resample.n, data=howmanydf)

# need to repeat with other species and across studyID? ... and,
# come up with methods to write in paper that we like
