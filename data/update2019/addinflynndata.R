## 19 July 2019 - Cat
# Sorting through data to get raw means and standard errors for each tx and species for OSPREE

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set Working directory
setwd("~/Documents/git/buds/analyses") 

bblo <- read.csv("input/Budburst By Day.csv", header=TRUE)
bblo.sm <- subset(bblo, select=c("id", "sp", "site", "treatcode", "lday", "bday", "fday"))

bblo.sm$meanbb <- ave(bblo.sm$bday, bblo.sm$sp, bblo.sm$site, bblo.sm$treatcode, FUN=function(x) mean(x, na.rm=T))
bblo.sm$sdbb <- ave(bblo.sm$bday, bblo.sm$sp, bblo.sm$site, bblo.sm$treatcode, FUN=function(x) sd(x, na.rm=T))

bblo.sm$meanlo <- ave(bblo.sm$lday, bblo.sm$sp, bblo.sm$site, bblo.sm$treatcode, FUN=function(x) mean(x, na.rm=T))
bblo.sm$sdlo <- ave(bblo.sm$lday, bblo.sm$sp, bblo.sm$site, bblo.sm$treatcode, FUN=function(x) sd(x, na.rm=T))

bblo.sm$meanflo <- ave(bblo.sm$fday, bblo.sm$sp, bblo.sm$site, bblo.sm$treatcode, FUN=function(x) mean(x, na.rm=T))
bblo.sm$sdflo <- ave(bblo.sm$fday, bblo.sm$sp, bblo.sm$site, bblo.sm$treatcode, FUN=function(x) sd(x, na.rm=T))

meansbysp <- subset(bblo.sm, select=c("sp", "site", "treatcode", "meanbb", "sdbb", "meanlo", "sdlo",
                                      "meanflo", "sdflo"))
meansbysp <- meansbysp[!duplicated(meansbysp),]


### Can we make an excel sheet that matches ospree?...
osp.df <- data.frame(datasetID="flynn18", study="exp1", Entered.By="CJC", 
                  genus=rep(substr(meansbysp$sp,0,3), each=3), species=rep(substr(meansbysp$sp, 4, 6), each=3),
                  varetc="", woody="yes", population="", population.detail="", provenance.lat=rep(ifelse(meansbysp$site=="HF", 42.531705, 45.932675), each=3),
                  provenance.long=rep(ifelse(meansbysp$site=="HF", -72.189920, -74.025070), each=3),
                  population.altitude.m="", 
                  continent="North America", year=2015, material="cuttings", fieldchill="yes", 
                  fieldsample.date="01-Jan-2015",
                  chilltemp=rep(substr(meansbysp$treatcode,3,3), each=3), chillphotoperiod=0, 
                  chilldays=rep(substr(meansbysp$treatcode,3,3), each=3), forcetemp=rep(substr(meansbysp$treatcode,1,1), each=3),
                  forcetemp_night=rep(substr(meansbysp$treatcode,1,1), each=3), photoperiod=rep(substr(meansbysp$treatcode,2,2), each=3),
                  photoperiod_night=rep(substr(meansbysp$treatcode,2,2), each=3), 
                  respvar=rep(c("daystobudburst", "daystoleafout", "daystoflowerbudburst")),
                  response=1, response.time="", n=6, error.type="standard error", resp.error="",
                  figure.table..if.applicable.="", growing.lat=42.296074, growing.long=-71.133575, 
                  field.chill.units=rep(ifelse(meansbysp$site=="HF", 56.62, 44.63), each=3), 
                  cu.model="Dynamic")

sort(unique(osp.df$genus))
#[1] "ACE" "ALN" "ARO" "BET" "COR" "FAG" "FRA" "HAM" "ILE" "KAL" "LON" "LYO" "NYS" "POP" "PRU" "QUE"
#[17] "RHA" "RHO" "SPI" "VAC" "VIB"

osp.df$genus <- ifelse(osp.df$genus=="ACE", "Acer", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="ALN", "Alnus", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="ARO", "Aronia", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="BET", "Betula", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="COR", "Cornus", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="FAG", "Fagus", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="FRA", "Fraxinus", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="HAM", "Hamamelis", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="ILE", "Ilex", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="KAL", "Kalmia", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="LON", "Lonicera", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="LYO", "Lyonia", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="NYS", "Nyssa", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="POP", "Populus", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="PRU", "Prunus", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="QUE", "Quercus", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="RHA", "Rhamnus", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="RHO", "Rhododendron", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="SPI", "Spirea", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="VAC", "Vaccinium", osp.df$genus)
osp.df$genus <- ifelse(osp.df$genus=="VIB", "Viburnum", osp.df$genus)


sort(unique(osp.df$species))
#[1] "ALB" "ALL" "ANG" "CAN" "CAS" "COR" "FRA" "GRA" "INC" "LAN" "LEN" "LIG" "MEL" "MUC" "MYR" "NIG"
#[17] "PAP" "PEN" "PRI" "RUB" "SAC" "SYL" "VEL" "VIR"

osp.df$species <- ifelse(osp.df$species=="ALB", "alba", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="ALL", "alleghaniensis", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="ANG", "angustifolia", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="CAN", "canadensis", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="CAS", "cassinoides", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="COR", "cornuta", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="FRA", "frangula", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="GRA" & osp.df$genus=="Fagus", "grandifolia", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="GRA" & osp.df$genus=="Populus", "grandidentata", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="INC", "incana", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="LAN", "lantanoides", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="LEN", "lenta", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="LIG", "ligustrina", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="MEL", "melanocarpa", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="MUC", "mucronata", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="MYR", "myrtilloides", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="NIG", "nigra", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="PAP", "papyrifera", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="PEN" & osp.df$genus=="Acer", "pensylvanicum", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="PEN" & osp.df$genus=="Prunus", "pensylvanica", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="PRI", "prinophyllum", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="RUB" & osp.df$genus=="Acer", "rubrum", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="RUB" & osp.df$genus=="Quercus", "rubra", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="SAC", "saccharum", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="SYL", "sylvatica", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="VEL", "velutina", osp.df$species)
osp.df$species <- ifelse(osp.df$species=="VIR", "virginiana", osp.df$species)

osp.df$treatcode <- paste(osp.df$forcetemp, osp.df$photoperiod, osp.df$chilltemp, sep="")

osp.df$chilltemp <- ifelse(osp.df$chilltemp=="1", 1.5, osp.df$chilltemp)                 
osp.df$chilltemp <- ifelse(osp.df$chilltemp=="2", 4, osp.df$chilltemp) 
days <- c("1", "2")
osp.df$chilldays <- ifelse(osp.df$chilldays%in%days, 30, osp.df$chilldays) 
                  
osp.df$forcetemp <-ifelse(osp.df$forcetemp=="W", 20, 15)
osp.df$forcetemp_night <-ifelse(osp.df$forcetemp_night=="W", 10, 5)

osp.df$photoperiod <- ifelse(osp.df$photoperiod=="L", 12, 8)
osp.df$photoperiod_night <- ifelse(osp.df$photoperiod_night=="L", 12, 16)

osp.df$sp <- paste(substr(osp.df$gen, 0,3), substr(osp.df$species,0,3), sep="")
osp.df$site<- ifelse(osp.df$provenance.long==-72.18992, "HF", "SH")

osp.df$fieldsample.date <- ifelse(osp.df$site=="HF", "21-Jan-2015", "26-Jan-2018")

osp.df$sp <- lapply(osp.df$sp, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)})


for(i in c(1:nrow(meansbysp))){ ## Budburst
  for(j in c(1:nrow(osp.df)))
    
    osp.df$response.time[j] <- ifelse(osp.df$respvar[j]=="daystobudburst" & osp.df$sp[j]==meansbysp$sp[i] &
                                     osp.df$treatcode[j]==meansbysp$treatcode[i] &
                                     osp.df$site[j]==meansbysp$site[i], meansbysp$meanbb[i], osp.df$response.time[j])
}
for(i in c(1:nrow(meansbysp))){ ## Budburst resp.error
  for(j in c(1:nrow(osp.df)))
    osp.df$resp.error[j] <- ifelse(osp.df$respvar[j]=="daystobudburst" & osp.df$sp[j]==meansbysp$sp[i] &
                                   osp.df$treatcode[j]==meansbysp$treatcode[i] &
                                   osp.df$site[j]==meansbysp$site[i], meansbysp$sdbb[i], osp.df$resp.error[j])
}
  
for(i in c(1:nrow(meansbysp))){ ## Leafout
  for(j in c(1:nrow(osp.df)))
    osp.df$response.time[j] <- ifelse(osp.df$respvar[j]=="daystoleafout" & osp.df$sp[j]==meansbysp$sp[i] &
                                        osp.df$treatcode[j]==meansbysp$treatcode[i] &
                                        osp.df$site[j]==meansbysp$site[i], meansbysp$meanlo[i], osp.df$response.time[j])
}

for(i in c(1:nrow(meansbysp))){ ## Leafout resp.error
  for(j in c(1:nrow(osp.df)))
    osp.df$resp.error[j] <- ifelse(osp.df$respvar[j]=="daystoleafout" & osp.df$sp[j]==meansbysp$sp[i] &
                                     osp.df$treatcode[j]==meansbysp$treatcode[i] &
                                     osp.df$site[j]==meansbysp$site[i], meansbysp$sdlo[i], osp.df$resp.error[j])
}
    
for(i in c(1:nrow(meansbysp))){ ## Flower budburst
  for(j in c(1:nrow(osp.df)))
    osp.df$response.time[j] <- ifelse(osp.df$respvar[j]=="daystoflowerbudburst" & osp.df$sp[j]==meansbysp$sp[i] &
                                        osp.df$treatcode[j]==meansbysp$treatcode[i] &
                                        osp.df$site[j]==meansbysp$site[i], meansbysp$meanflo[i], osp.df$response.time[j])
}

for(i in c(1:nrow(meansbysp))){ ## Flower Budburst resp.error
  for(j in c(1:nrow(osp.df)))
    osp.df$resp.error[j] <- ifelse(osp.df$respvar[j]=="daystoflowerbudburst" & osp.df$sp[j]==meansbysp$sp[i] &
                                     osp.df$treatcode[j]==meansbysp$treatcode[i] &
                                     osp.df$site[j]==meansbysp$site[i], meansbysp$sdflo[i], osp.df$resp.error[j])
    
}
                  
osp.df <- osp.df[!(osp.df$response.time=="NaN"),]
osp.df$sp <- NULL
osp.df$site <- NULL
osp.df$treatcode <- NULL
osp.df$n <- ifelse(is.na(osp.df$resp.error), 1, osp.df$n)

write.csv(osp.df, "~/Documents/git/ospree/data/update2019/flynn2018_data.csv", row.names=FALSE)
