#### Working on making some climate plots for ranges
# Started 31 March 2020 by Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Set working directory
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/ranges") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/ranges")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/ranges") 
} else
setwd("~/Documents/git/ospree/analyses/ranges") 


### From Lines 17-96 are just cleaning files so masked for now to make plots
if(FALSE){
  ### Let's open up all climate files first...
  # Name the objects first
  mycsv = as.vector(dir("climoutput", pattern=".csv"))
  
  # Then create a list of dataframes
  n <- length(mycsv)
  mylist <- vector("list", n)
  
  for(i in 1:n) mylist[[i]] <- read.csv(paste0("climoutput/",mycsv[i]))
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
}

####################################################
############# Now for the plots!! ##################
####################################################
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# Set up colors
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
my.pch <- rep(15:18, each=12)

allgdd <- ggplot(df, aes(x=year, y=gdd, col=complex_name, shape=complex_name)) + geom_point() + geom_line(aes(linetype=continent)) +
  theme_classic() + coord_cartesian(ylim=c(50, 850)) +
  scale_color_manual(name="Species", labels=sort(unique(df$complex_name)), values=my.pal) +
  scale_shape_manual(name="Species", labels=sort(unique(df$complex_name)), values=my.pch) # + guides(linetype=FALSE)

allutah <- ggplot(df, aes(x=year, y=utah, col=complex_name, shape=complex_name)) + geom_point() + geom_line(aes(linetype=continent)) +
  theme_classic() + coord_cartesian(ylim=c(450, 2500)) +
  scale_color_manual(name="Species", labels=sort(unique(df$complex_name)), values=my.pal) +
  scale_shape_manual(name="Species", labels=sort(unique(df$complex_name)), values=my.pch)


if(FALSE){
  eurdat <- df[(df$continent=="europe"),]
  eurgdd <- ggplot(eurdat, aes(x=year, y=gdd, col=complex_name, shape=complex_name)) + geom_point() + geom_line() +
    theme_classic() + coord_cartesian(ylim=c(50, 850)) +
    scale_color_manual(name="Species", labels=sort(unique(eurdat$complex_name)), values=my.pal) +
    scale_shape_manual(name="Species", labels=sort(unique(eurdat$complex_name)), values=my.pch) + guides(linetype=FALSE)
  
  namdat <- df[(df$continent=="north america"),]
  namgdd <- ggplot(namdat, aes(x=year, y=gdd, col=complex_name, shape=complex_name)) + geom_point() + geom_line(aes(linetype=continent)) +
    theme_classic() + coord_cartesian(ylim=c(50, 850)) +
    scale_color_manual(name="Species", labels=sort(unique(namdat$complex_name)), values=my.pal) +
    scale_shape_manual(name="Species", labels=sort(unique(namdat$complex_name)), values=my.pch) + guides(linetype=FALSE)
}


quartz()
allgdd
allutah
#eurgdd
#namgdd


#### Thinking about some barplots now...
eurdat$gddmean <- ave(eurdat$gdd, eurdat$year)
eurdat$gddmean.sd <- ave(eurdat$gdd, eurdat$year, FUN=sd)
namdat$gddmean <- ave(namdat$gdd, namdat$year)
namdat$gddmean.sd <- ave(namdat$gdd, namdat$year, FUN=sd)

eurdat.bar <- subset(eurdat, select=c(year, continent, gddmean, gddmean.sd))
namdat.bar <- subset(namdat, select=c(year, continent, gddmean, gddmean.sd))

all.bar <- dplyr::full_join(eurdat.bar, namdat.bar)
all.bar <- all.bar[!duplicated(all.bar),]

all.bar$ymin <- all.bar$gddmean-all.bar$gddmean.sd
all.bar$ymax <- all.bar$gddmean+all.bar$gddmean.sd

cont.pal <- brewer.pal(n = 8, name = "Dark2")

gddbar<- ggplot(all.bar, aes(x=as.factor(year), y=gddmean, fill=continent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = c(0.1, .95),
        legend.title = element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("GDD mean") + 
  scale_fill_manual(name="Continent", values=cont.pal,
                    labels=sort(unique(all.bar$continent))) 

quartz()
gddbar

#### Now for utah chill
eurdat$utahmean <- ave(eurdat$utah, eurdat$year)
eurdat$utahmean.sd <- ave(eurdat$utah, eurdat$year, FUN=sd)
namdat$utahmean <- ave(namdat$utah, namdat$year)
namdat$utahmean.sd <- ave(namdat$utah, namdat$year, FUN=sd)

eurdat.utahbar <- subset(eurdat, select=c(year, continent, utahmean, utahmean.sd))
namdat.utahbar <- subset(namdat, select=c(year, continent, utahmean, utahmean.sd))

all.barutah <- dplyr::full_join(eurdat.utahbar, namdat.utahbar)
all.barutah <- all.barutah[!duplicated(all.barutah),]

all.barutah$ymin <- all.barutah$utahmean-all.barutah$utahmean.sd
all.barutah$ymax <- all.barutah$utahmean+all.barutah$utahmean.sd

cont.pal <- brewer.pal(n = 8, name = "Dark2")

utahbar<- ggplot(all.barutah, aes(x=as.factor(year), y=utahmean, fill=continent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text.align = 0,
        legend.position = c(0.1, .95),
        legend.title = element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  xlab("") + 
  ylab("GDD mean") + 
  scale_fill_manual(name="Continent", values=cont.pal,
                    labels=sort(unique(all.bar$continent))) 

quartz()
utahbar


####### now let's try something wild... I want to somehow show all of the data we have
eur.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 3)

eurdat$gddymax <- eurdat$gdd + eurdat$gdd.sd
eurdat$gddymin <- eurdat$gdd - eurdat$gdd.sd

quartz()
ggplot(eurdat, aes(x=year, y=gdd, col=complex_name)) + geom_point() +
  theme_classic() + geom_errorbar(aes(ymin=gddymin, ymax=gddymax),width = 0.2, position=position_dodge(0.9)) +
  facet_wrap(~complex_name) + theme(legend.position = "none") +
  scale_color_manual(name="Species", labels=sort(unique(eurdat$complex_name)), 
                     values=eur.pal)


eurdat$utahymax <- eurdat$utah + eurdat$utah.sd
eurdat$utahymin <- eurdat$utah - eurdat$utah.sd

quartz()
ggplot(eurdat, aes(x=year, y=utah, col=complex_name)) + geom_point() +
  theme_classic() + geom_errorbar(aes(ymin=utahymin, ymax=utahymax),width = 0.2, position=position_dodge(0.9)) +
  facet_wrap(~complex_name) + theme(legend.position = "none") +
  scale_color_manual(name="Species", labels=sort(unique(eurdat$complex_name)), 
                     values=eur.pal)


nam.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 2)

namdat$gddymax <- namdat$gdd + namdat$gdd.sd
namdat$gddymin <- namdat$gdd - namdat$gdd.sd

quartz()
ggplot(namdat, aes(x=year, y=gdd, col=complex_name)) + geom_point() +
  theme_classic() + geom_errorbar(aes(ymin=gddymin, ymax=gddymax),width = 0.2, position=position_dodge(0.9)) +
  facet_wrap(~complex_name) + theme(legend.position = "none") +
  scale_color_manual(name="Species", labels=sort(unique(namdat$complex_name)), 
                     values=nam.pal)


namdat$utahymax <- namdat$utah + namdat$utah.sd
namdat$utahymin <- namdat$utah - namdat$utah.sd

quartz()
ggplot(namdat, aes(x=year, y=utah, col=complex_name)) + geom_point() +
  theme_classic() + geom_errorbar(aes(ymin=utahymin, ymax=utahymax),width = 0.2, position=position_dodge(0.9)) +
  facet_wrap(~complex_name) + theme(legend.position = "none") +
  scale_color_manual(name="Species", labels=sort(unique(namdat$complex_name)), 
                     values=nam.pal)



