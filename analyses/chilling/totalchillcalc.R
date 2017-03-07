#Calculates Total Chilling by merging in field chilling from "fieldchillcalc_latlong.R", 
#This code adds the field chilling to experimental chilling to calculate total chilling

###I think the next 10 lines of code (commented out) are old and unnecessary)
#First, calculate field chilling data
#d$continent <- tolower(d$continent)
#d$datasetID <- as.character(d$datasetID)
#d$provenance.lat <- as.numeric(as.character(d$provenance.lat))
#d$provenance.long <- as.numeric(as.character(d$provenance.long))
#d$year <- as.numeric(as.character(d$year))
#d$fieldsample.date<-as.character(as.Date(d$fieldsample.date,"%m/%d/%y")) #needed for new version
#d<- as_data_frame(d)
#Make a column that indexes the study, provenance latitude,provenance longitude, and field sample date, in order to calculate field chilling
#d$ID_fieldsample.date2<-paste(d$datasetID,d$chill.lat,d$chill.long,d$fieldsample.date2, sep="_")

#Make a column that indexes the experimental chilling treatment (including chilltemp, chillphotoperiod & chilldays), in order to calculate field chilling
d$ID_chilltreat<-paste(d$datasetID,d$chilltemp,d$chilldays,sep=".")
           
##### Calculate experimental chilling, using chillday and chilltemp.
##there are many non-numeric values in the chilltemp and chilldays columns- these are unusable currently so remove:
#want table with datasetID chilling days, chilling temperature,  treat for each study. 
chilldat <- d %>% # start with the data frame
distinct(ID_chilltreat,.keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(datasetID, chilltemp, chilldays, year,ID_chilltreat)
chilldat$chilltemp<-as.numeric(chilldat$chilltemp)
chilldat$chilldays<-as.numeric(chilldat$chilldays)
chilldat<- chilldat[apply(chilldat, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
expchillcalcs <- vector()
###First, need file with hrly temperature data for each row in dataframe
           
for(i in 1:nrow(chilldat)) {
 # Skip if NA for chilltemp or chilldays data
  if(!is.na(chilldat$chilltemp[i]) & chilldat$chilldays[i] !=0 & !is.na(chilldat$chilldays[i])) {
  yr <- as.numeric(chilldat$year[i]) 
  if(is.na(yr)){ yr <- 2016} #temporary fix for when years are not currently listed!
   hrly =
   data.frame(
    Temp = rep(as.numeric(chilldat$chilltemp[i]), 
    times = 24 * round(as.numeric(chilldat$chilldays[i], digits=0))),
     Year = rep(yr, 
     times = 24 * round(as.numeric(chilldat$chilldays[i],digits=0))),
      JDay = sort(rep(as.numeric(seq(1:round(as.numeric(chilldat$chilldays[i], digits=0)))), times = 24))
      )
           
    expchillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) 
           
    } else { expchillcalc <- data.frame("Chilling_Hours"=NA, "Utah_Model"=NA, "Chill_portions"=NA) }
           
           expchillcalcs <- rbind(expchillcalcs, 
           data.frame(datasetID = chilldat$datasetID[i], 
           ID_chilltreat = chilldat$ID_chilltreat[i],
           expchillcalc[c("Chilling_Hours","Utah_Model","Chill_portions")]))
           
           }
           
           colnames(expchillcalcs)[3:5] <- c("Exp_Chilling_Hours","Exp_Utah_Model","Exp_Chill_portions")
           
#Merge field and experimental chilling data with the rest of the data
#Add experimental chilling. Right number of rows = 12898 rows, 61 columns
           dat3 <- merge(d, expchillcalcs, 
           by.x = c("datasetID","ID_chilltreat"),
           by.y=c("datasetID","ID_chilltreat"),
           all.x=T)
           #Add field chilling calculations to datafile. #still 12898 rows, now 64 columns (3 ways of estimating experimental chilling)
           ###First, read in chillcalc file, so that you don't have to run the above code with the external hard drive of climate data
           chillcalcs <- read.csv("output/fieldchillcalcslatlong.csv", header=T)
           chillcalcs <- chillcalcs[apply(chillcalcs, 1, function(x) all(!is.na(x))),] # only keep rows of all not NA. 256 rows now.
           
           colnames(chillcalcs) <- c("ID_fieldsample.date2","Season","End_year","Field_Chilling_Hours","Field_Utah_Model","Field_Chill_portions")
           #fieldchillcalcs has 256 rows and 6 columns
           # Some will be missing because they are not North America or Europe (eg biasi12, cook00, gansert02, nishimoto95). Others should have it: viheraaarni06 for example. Those without dates do not have field chilling, because do not have a field sample date.
           (nochillcalcs <- unique(dat3$ID_fieldsample.date2[!dat3$ID_fieldsample.date2 %in% chillcalcs$ID_fieldsample.date2]))
           
           chillcalcs <- chillcalcs[chillcalcs$ID_fieldsample.date2 %in% dat3$ID_fieldsample.date2,]
           
           dat4<-join(dat3, chillcalcs,by="ID_fieldsample.date2",match="all")
           #dat4<-full_join(dat3, chillcalcs, by="ID_fieldsample.date", match="all") #Added by Cat
           
           ### Now add column for total chilling (field plus experimental)
           ### First, total chilling = exp and field
           dat4$Total_Chilling_Hours <- dat4$Exp_Chilling_Hours+dat4$Field_Chilling_Hours
           dat4$Total_Utah_Model <- dat4$Exp_Utah_Model+dat4$Field_Utah_Model
           dat4$Total_Chill_portions <- dat4$Exp_Chill_portions+dat4$Field_Chill_portions
           
           #For sites with no experimental chilling, just use field chilling:
           dat4[which(is.na(dat4$Exp_Chilling_Hours)),]$Total_Chilling_Hours<-dat4[which(is.na(dat4$Exp_Chilling_Hours)),]$Field_Chilling_Hours
           dat4[which(is.na(dat4$Exp_Utah_Model)),]$Total_Utah_Model<-dat4[which(is.na(dat4$Exp_Utah_Model)),]$Field_Utah_Model
           dat4[which(is.na(dat4$Exp_Chill_portions)),]$Total_Chill_portions<-dat4[which(is.na(dat4$Exp_Chill_portions)),]$Field_Chill_portions
           #For sites with no field chilling, should we use only experimental chilling? not sure if this is ok...
           dat4[which(is.na(dat4$Field_Chilling_Hours)),]$Total_Chilling_Hours<-dat4[which(is.na(dat4$Field_Chilling_Hours)),]$Exp_Chilling_Hours
           dat4[which(is.na(dat4$Field_Utah_Model)),]$Total_Utah_Model<-dat4[which(is.na(dat4$Field_Utah_Model)),]$Exp_Utah_Model
           dat4[which(is.na(dat4$Field_Chill_portions)),]$Total_Chill_portions<-dat4[which(is.na(dat4$Field_Chill_portions)),]$Exp_Chill_portions
          