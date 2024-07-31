

rangiesEu<-read.csv("output/Synthesis_climate_EUsps_STVfinalchill.csv") ### updated STV
rangiesNa<-read.csv("output/Synthesis_climate_NAMsps_STVfinal_nacho_chill.csv") ## updated stv

NAnames<-read.csv("output/Synthesis_climate_NAMsps_STVfinal_nacho.csv")

species<-rep(unique(NAnames$species),each=7)
rangiesNa$species<-species
colnames(rangiesNa)
colnames(rangiesEu)
rangiesNa<-rangiesNa[,c(1,2,3,4,5,7,6)]
rangiesEu$continent<-"Europe"
rangiesEu<-dplyr::select(rangiesEu,-X)
rangiesNa$continent<-"N. America"
rangiesNa<-dplyr::select(rangiesNa,-X)

ggdlf<-rbind(rangiesEu,rangiesNa)

ggdlf$species[which(ggdlf$species=="betulent")]<- "Betula_lenta"
ggdlf$species[which(ggdlf$species=="popugran")]<- "Populus_grandidentata"
ggdlf$species[which(ggdlf$species=="querrubr")]<- "Quercus_rubra"
ggdlf$species[which(ggdlf$species=="acerpens")]<- "Acer_pensylvanicum"
ggdlf$species[which(ggdlf$species=="betupapy")]<- "Betula_papyrifera"
ggdlf$species[which(ggdlf$species=="fraxnigr")]<- "Fraxinus_nigra"
ggdlf$species[which(ggdlf$species=="alnurubr")]<- "Alnus_rubra"
ggdlf$species[which(ggdlf$species=="pseumenz")]<- "Pseudotsuga_menziesii"
ggdlf$species[which(ggdlf$species=="prunpens")]<- "Prunus_pensylvanica"
ggdlf$species[which(ggdlf$species=="betualle")]<- "Betula_alleghaniensis"
ggdlf$species[which(ggdlf$species=="acersacr")]<- "Acer_saccharum"
ggdlf$species[which(ggdlf$species=="acerrubr")]<- "Acer_rubrum"
ggdlf$species[which(ggdlf$species=="corycorn")]<- "Corylus_cornuta"
ggdlf$species[which(ggdlf$species=="piceglau")]<- "Picea_glauca"
ggdlf$species[which(ggdlf$species=="fagugran")]<- "Fagus_grandifolia"
ggdlf$species[which(ggdlf$species=="robipseu")]<- "Robinia_pseudoacacia"
ggdlf$species[which(ggdlf$species=="poputrem")]<- "Populus_tremuloides"
ggdlf$species[which(ggdlf$species=="alnurugo")]<- "Alnus_incana"

ggdlf<-dplyr::filter(ggdlf,(species!="Alnus_incana") | (continent!="Europe"))

rangies<-ggdlf
rm(ggdlf)

lf<-filter(rangies,variable=="GDD.lastfrost")
lf<-dplyr::select(lf,species,Temp.SD,continent)
colnames(lf)[c(1,2)]<-c("complex.wname","SD.lastfrost")

#STV as per zoner paper
STV<-filter(rangies,variable=="MeanTmins")
STV<-dplyr::select(STV,species,Temp.SD,continent)
colnames(STV)[c(1,2)]<-c("complex.wname","STV")

##MeanForcing
GDD<-filter(rangies,variable=="GDD")
colnames(GDD)[c(1,2,3,4)]<-c("Temp.Mean.GDD","Temp.SD.GDD","Geo.Mean.GDD","Geo.SD.GDD")
colnames(GDD)[5]<-"complex.wname"

CP<-filter(rangies,variable=="Mean.Chill.Portions")
colnames(CP)[c(1,2,3,4)]<-c("Temp.Mean.CP","Temp.SD.CP","Geo.Mean.CP","Geo.SD.CP")
colnames(CP)[5]<-"complex.wname"

##get rid of "variable column
GDD<-dplyr::select(GDD,-variable)
CP<-dplyr::select(CP,-variable)

##remake the dataset
fulldat<-dplyr::left_join(lf,STV)
fulldat<-dplyr::left_join(fulldat,CP)
fulldat<-dplyr::left_join(fulldat,GDD)
