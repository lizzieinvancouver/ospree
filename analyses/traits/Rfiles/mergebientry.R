# The aim of this code is to clean up the try and bien data and combine it into one useable file! There are many steps that need to be completed for this to happen:

#1. Fix typos try species names
#2. Remove columns in try that we don't need right now, there is tons of weather data and factors regarding site conditions
#3. Remove data from experiments where they manipulated factors that might change trails: fert additions, temp manip, chamber studies
#4. Check that the BIEN data is standardized in its trait units, lat/long
#5. Remove any dup data
#6. Subset to only deciduous species
#7. Subset to spp. with sufficient data, focus on leaf economic traits and SSD (one of the only wood economic traits)

rm(list=ls()) 
options(stringsAsFactors = FALSE)

require(stringr)
require(dplyr)

# Set working directory: 

#Anyone else working with this code should add their info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {  setwd("~/Desktop/ospree_trait_analysis")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

#Since the try data is still being cleaned, we are moving forward with the cleaning and plotting/preliminary testing of our hypotheses with the BIEN data
#biendat<-read.csv("input/newspp_BIEN_traitdata_Nov11.csv", header=TRUE)

# head(biendat)
# length(unique(biendat$scrubbed_species_binomial))
#94 species represented in some form

trydat<-read.csv("input/try_subsptraits.csv",head=TRUE)
#trydat<-read.csv("input/TryDataCleaned22012020.csv", header=TRUE)

biendat<-read.csv("input/bien_cleaned_Nov2020.csv")
ospree<-read.csv("input/traitors_bb_results_nocomplex.csv", header=TRUE)
ospree<-ospree[,c("Coefficient","Species","mean")]

length(unique(biendat$new.SpeciesName)) #85
length(unique(trydat$new.SpeciesName)) #62
length(unique(ospree$Species)) #234

##################################################################################
# Removing the gymnosperm from the ospree data 

breakname <- strsplit(as.character(ospree$Species), "_", fixed=TRUE)
ospree$genus <- unlist(lapply(breakname, function(x) x[1]))
ospree$species <- unlist(lapply(breakname, function(x) x[2]))

gymno<-c("Abies","Pinus","Picea","Pseudotsuga")

ospree.deci <- ospree[!ospree$genus %in% gymno,] # only want the deciduous species
ospree.deci$new.SpeciesName<-paste(ospree.deci$genus,ospree.deci$species, sep="_")
sort(unique(ospree.deci$new.SpeciesName))
length(unique(ospree.deci$new.SpeciesName)) # left with 222 species

###########################################################
#Changing names in BIEN to better match those in try
colnames(biendat)[colnames(biendat)=="trait_name"] <-"TraitName"
colnames(biendat)[colnames(biendat)=="trait_value"] <-"TraitValue" # Are the bien trait values standardized, something to check!! 
colnames(biendat)[colnames(biendat)=="unit"] <- "UnitName"
colnames(biendat)[colnames(biendat)=="longitude"] <- "Longitude"
colnames(biendat)[colnames(biendat)=="latitude"] <- "Latitude"

colnames(trydat)[colnames(trydat)=="StdValue"] <-"TraitValue"


# adding a new column witht the database the data is from
biendat$database<-"bien"
trydat$database<-"try"

#issue with the biend longtitude
trydat$Longitdue<-as.numeric(as.character(trydat$Longitude))

#bien has sla as and ldmc as
unique(biendat$TraitName)
unique(trydat$TraitName)

biendat$TraitName[which(biendat$TraitName == "leaf area per leaf dry mass")] <- "Specific_leaf_area"
biendat$TraitName[which(biendat$TraitName == "leaf dry mass per leaf fresh mass")] <- "Leaf_dry_matter_content"
biendat$TraitName[which(biendat$TraitName == "maximum whole plant height")] <- "Plant_height_vegetative"
biendat$TraitName[which(biendat$TraitName == "whole plant height")] <- "Plant_height_vegetative"
biendat$TraitName[which(biendat$TraitName == "stem wood density")] <- "Stem_specific_density"
##########################################################################################
# Merge the beind and try trait data

trybien<-rbind.fill(trydat, biendat)
nrow(biendat)+nrow(trydat) #looks good
##########################################################################################
# Calculating the average trait value for the try dat
unique(trybien$UnitName)
unique(trybien$TraitName)
unique(trybien$new.SpeciesName)


trtmean<-trybien %>% 
  group_by(new.SpeciesName,TraitName) %>% 
  summarize(trait.mean=mean(TraitValue,na.rm=TRUE),)
unique(trtmean$new.SpeciesName)
unique(ospree.deci$new.SpeciesName)
##################################################################################
fin<-merge(trtmean,ospree.deci, by="new.SpeciesName")
unique(fin$new.SpeciesName) #70
unique(fin$TraitName) #11 traits

head(fin)

#write.csv(fin, "input/try_bien_ospree_Nov2020.csv")
#write.csv(trybien,"input/try_bien_Nov2020.csv", row.names=FALSE)

##################################################################################

##################################################################################
# I think there might be duplicates in the data, but these will need to be looked into carefully
names(trybien)

trybien<-trybien[order(trybien$new.SpeciesName,trybien$TraitValue),]
trybien$label<-paste(trybien$new.SpeciesName,trybien$TraitValue,trybien$UnitName, sep="_")
trybien$dup<-duplicated(trybien[,"label"])

head(trybien);nrow(trybien)

dat.dup<-subset(trybien, dup=="TRUE")
head(dat.dup);nrow(dat.dup)

nrow(trybien)-nrow(dat.dup) # this suggests that there are only 27195 rows of unique data

t.dup<- aggregate(dat.dup["database"], dat.dup[c("new.SpeciesName","TraitName", "project_pi")], FUN=length) 

t.dup<-t.dup[order(t.dup$new.SpeciesName,t.dup$TraitName),]
# This look worked on a subset, but I ran it for over 40 min...too slow
# dupdat <- vector()
# for(i in 1:length(trybien$dup)){
#   if(trybien$dup[i] == "TRUE"){ 
#    dupdat<-rbind(dupdat, trybien[i,])
#   }
# }  

# try<-subset(trybien, database== "try"); sort(unique(try$project_pi))
# bien<-subset(trybien, database== "bien"); sort(unique(bien$project_pi))
# bien$project_pi[which(bien$project_pi == " Zanne, A.E., Lopez-Gonzalez, G., Coomes, D.A., Ilic, J., Jansen, S., Lewis, S.L., Miller, R.B., Swenson, N.G., Wiemann, M.C., and Chave, J")] <- "Amy Zanne"
# 
# # Lopez-gonzalez 
# lopezgonz <-  grep( "onzalez", unique(trybien$project_pi),  value = TRUE)
# lg<-subset(trybien, project_pi==" Lopez-Gonzalez" | project_pi=="Lopez-Gonzalez G")
# lg$dup<-duplicated(lg[,"label"])
# 
# dupdat <- vector()
# for(i in 1:length(lg$dup)){
#   if(lg$dup[i] == "TRUE"){
#    dupdat<-rbind(dupdat, lg[i,])
#   }
# }
# 
# # Zanne A
# zanne <-  grep( "anne", unique(trybien$project_pi),  value = TRUE)
# zan<-subset(trybien, project_pi=="Amy Zanne" | project_pi==" Zanne, A.E., Lopez-Gonzalez, G., Coomes, D.A., Ilic, J., Jansen, S., Lewis, S.L., Miller, R.B., Swenson, N.G., Wiemann, M.C., and Chave, J"| project_pi=="Zanne AE")
# zan$dup<-duplicated(zan[,"label"])
# 
# ##bien project_pi " Zanne, A.E., Lopez-Gonzalez, G., Coomes, D.A., Ilic, J., Jansen, S., Lewis, S.L., Miller, R.B., Swenson, N.G., Wiemann, M.C., and Chave, J" is the 
# #same as try referces: 
# #"Chave, J., D. Coomes, S. Jansen, S. L. Lewis, N. G. Swenson, and A. E. Zanne. 2009. Towards a world wide wood economics spectrum. Ecology Letters 12:351-366."
# 
# #Angela moles
# moles<-  grep( "ole", unique(trybien$project_pi),  value = TRUE)
# moles<-subset(trybien, project_pi=="Angela Moles" )
# moles<-moles[order(moles$TraitValue),]
# # I think these are different but it is hard to tell since there is no reference for the bien dataset
# 
# # C.E.T Paine
# paine<-  grep( "ain", unique(trybien$project_pi),  value = TRUE)
# paine<-subset(trybien, project_pi=="C. E. Timothy Paine" |  project_pi=="Paine CET" )
# paine<-paine[order(paine$TraitValue,paine$new.SpeciesName),]
# 
# # yes the try: "Paine CET, Amissah L, Auge H, Baraloto C, Baruffol M, Bourland N, Bruelheide H, Dainou K, de Gouvenain RC, Doucet J-L, Doust SJ, Fine PV a, Fortunel C, Haase J, Holl KD, Jactel H, Li X, Kitajima K, Koricheva J, Martinez-Garza C, Messier C, Paquette A, Philipson CD, Piotto D, Poorter L, Posada JM, Potvin C, Rainio K, Russo SE, Ruiz-Jaen M, Scherer-Lorenzen M, Webb CO, Zahawi RA & Hector A (2015) Globally, functional traits are weak predictors of juvenile tree growth, and we do not know why. Journal of Ecology, 103, 978\u0096989. DOI: 10.1111/1365-2745.12401" 
# #is the same as the bien:  "http://datadryad.org/resource/doi:10.5061/dryad.h9083"
# 
# #MJ Spasojevic
# spas<-  grep( "pasoj", unique(trybien$project_pi),  value = TRUE)
# spas<-subset(trybien, project_pi=="Marko Spasojevic" |  project_pi=="Spasojevic MJ" )
# spas<-spas[order(spas$TraitValue),]
# 
# # yes try's "Spasojevic, M. J., Turner, B. L., and Myers, J. A. (2016) When does intraspecific trait variation contribute to functional beta?diversity? J Ecol, 104: 487-496. doi:10.1111/1365-2745.12518" 
# # is the same as biens: http://datadryad.org/resource/doi:10.5061/dryad.rr4pm
# 
# dupdat
# unique(lg$Reference...source)
# dupdat
# nrow(dup) 
# b.studies<- aggregate(biendat["TraitName"], biendat[c("url_source", "project_pi")], FUN=length) 
# 
