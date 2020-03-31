#### Working on making some climate plots for ranges
# Started 31 March 2020 by Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Set working directory
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else setwd("~/Documents/git/ospree/analyses/ranges") 


### Let's open up all climate files first...
# Name the objects first
mycsv = as.vector(dir("output", pattern=".csv"))

# Then create a list of dataframes
n <- length(mycsv)
mylist <- vector("list", n)

for(i in 1:n) mylist[[i]] <- read.csv(paste0("output/",mycsv[i]))
names(mylist) <- substr(mycsv, 18,30)

# Now, let's make sure all of the dataframes have the same column names
mylist <- lapply(mylist, function(x) 
{names(x) <- c("year", "gdd", "gdd.sd", "utah", "utah.sd", "ports", "ports.sd") ; 
return(x)})

# Okay, now let's merge them all together into one major dataframe 
# (not sure how to do this other than using dplyr or purrr)
mylist <- dplyr::bind_rows(mylist, .id = "source")


### Source names are a nightmare, so let's fix below
df <- mylist ## just to save mylist as is
df$gen <- substr(df$source, 0, 3) # easy to get first three letters of genus code
df$spp <- substr(gsub('.*_(.*)','\\1', df$source), 0, 3) ## now for the species code
df$spp <- ifelse(df$gen==df$spp, substr(df$source, 5, 7), df$spp)

df$species <- paste0(df$gen, df$spp)

# Need to make all species names the same, which means I need to make the first letter of genus capital 
df$gen <- paste0(toupper(substr(df$gen, 0, 1)), substr(df$gen, 2, 3))


### Alright, now I would like to clean up the species names and also add a column to mark NAM vs EUR
species.list <- read.csv("~/Documents/git/ospree/analyses/output/masterspecieslist.csv")
species.list <- as.vector(species.list$x)
spplist <- as.data.frame(species.list)
spplist <- rbind(spplist, "Alnus_rubra")

## The NAM species are listed by 6-digit code, need to fix...
zipped_names <- grep('\\.shp', unzip("~/Documents/git/ospree/analyses/ranges/NA_range_files/NA_ranges.zip", list=TRUE)$Name,ignore.case=TRUE, value=TRUE)

# generate a list of species with maps in the .zip  
species.list.maps <- unlist(zipped_names)
species.list.maps <- gsub(pattern = "(.*/)(.*)(.shp.*)", replacement = "\\2", x = species.list.maps)
species.list.clean <- species.list.maps

namspp <- data.frame(simpspp = species.list.clean,
                    compspp = c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra", 
                                "Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_excelsior", "Alnus_rubra",
                                "Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis",
                                "Acer_saccharum", "Alnus_incana", "Acer_rubrum", "Cornus_cornuta", "Picea_glauca"))

for(i in 1:c(nrow(spplist))){ # i=1
  for(j in 1:c(nrow(namspp))) #j=1
  spplist$species.list[i] <- ifelse(spplist$species.list[i]==namspp$simpspp[j], 
                                    namspp$compspp[j], spplist$species.list[i])
}
## Okay, all species names are complete now need to fix the main dataframe with climate data
df$complex_name <- NA
for(i in 1:c(nrow(df))){ # i=1
  for(j in 1:c(nrow(spplist))) #j=1
    df$complex_name[i] <- ifelse(df$gen[i]==substr(spplist$species.list[j], 0,3) & df$spp[i]==substr(gsub('.*_(.*)','\\1', spplist$species.list[j]), 0, 3),
                              spplist$species.list[j], df$complex_name[i])
  
}


#### Alright, now just need to add if NAM or EU
nam <- c("Betula_lenta", "Populus_grandidentata", "Fagus_grandifolia", "Quercus_rubra", 
  "Acer_pensylvanicum", "Betula_papyrifera", "Fraxinus_excelsior", "Alnus_rubra",
  "Pseudotsuga_menziesii", "Prunus_pensylvanica", "Betula_alleghaniensis",
  "Acer_saccharum", "Alnus_incana", "Acer_rubrum", "Cornus_cornuta", "Picea_glauca", "Alnus_rubra")

df$continent <- ifelse(df$complex_name%in%nam, "north america", "europe")
df$source <- NULL

# Let's quickly save our work:
write.csv(df, "ranges_climclean.csv", row.names=FALSE)

