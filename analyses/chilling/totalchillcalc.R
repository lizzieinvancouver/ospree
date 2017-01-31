#Calculates Total Chilling by merging in field chilling from "fieldchillcalc_latlong.R", 
#This code adds this field chilling to experimental chilling to calculate total chilling

############################################################################################
# Start here if field chill calcs from the climate data have already been done
# Merge field and experimental chilling calculations with the rest of the data
############################################################################################
#First, calculate field chilling data
options(stringsAsFactors=FALSE)
setwd("~/git/ospree")

#d <- read.csv("analyses/output/ospree_master_clean.csv") ##this file should use the cleaned data file created from Lizzie's "cleanmerge_all.R" code
dat <- read.csv("analyses/output/ospree_master_clean.csv") #this file should use the cleaned data file created from Lizzie's "cleanmerge_all.R" code, after the chilltemps and chilldays have been cleaning with cleanin_chilltemp.R

colnames(dat)[17]<-"fsdate_tofix"#the date format in this new file needs to be changed, for this code to work
dat$fieldsample.date<-strptime(strptime(dat$fsdate_tofix, format = "%m/%d/%Y"),format = "%Y-%m-%d")

colnames(dat)[17]<-"fsdate_tofix"#the date format in this new file needs to be changed, for this code to work
dat$fieldsample.date<-strptime(strptime(dat$fsdate_tofix, format = "%m/%d/%Y"),format = "%Y-%m-%d")
#use only woody species
dat2 <- subset(dat, woody=="yes")
# make two data frames. North America and Europe, the lat longs and years.
           
dat2$continent <- tolower(dat2$continent)
dat2$datasetID <- as.character(dat2$datasetID)
dat2$provenance.lat <- as.numeric(as.character(dat2$provenance.lat))
dat2$provenance.long <- as.numeric(as.character(dat2$provenance.long))
dat2$year <- as.numeric(as.character(dat2$year))
dat2$fieldsample.date<-as.character(as.Date(dat2$fieldsample.date,"%m/%d/%y")) #needed for new version
dat2 <- as_data_frame(dat2)
           
#Make a column that indexes the study, provenance latitude,provenance longitude, and field sample date, in order to calculate field chilling
dat2$ID_fieldsample.date<-paste(dat2$datasetID,dat2$provenance.lat,dat2$provenance.long,dat2$fieldsample.date, sep="_")
#Make a column that indexes the experimental chilling treatment (including chilltemp, chillphotoperiod & chilldays), in order to calculate field chilling
dat2$ID_chilltreat<-paste(dat2$datasetID,dat2$chilltemp,dat2$chilldays,sep=".")
           
##### Calculate experimental chilling, using chillday and chilltemp.
##there are many non-numeric values in the chilltemp and chilldays columns- these are unusable currently so remove:
#want table with datasetID chilling days, chilling temperature,  treat for each study. 
chilldat <- dat2 %>% # start with the data frame
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
           
           
###Merge field and experimental chilling data with the rest of the data
# Add experimental chilling. Right number of rows = 12916 rows, 60 columns
           dat3 <- merge(dat2, expchillcalcs, 
           by.x = c("datasetID","ID_chilltreat"),
           by.y=c("datasetID","ID_chilltreat"),
           all.x=T)
           #Add field chilling calculations to datafile. #still 12916 rows, now 63 columns (3 ways of estimating experimental chilling)
           ###First, read in chillcalc file, so that you don't have to run the above code with the external hard drive of climate data
           chillcalcs <- read.csv("analyses/output/fieldchillcalcslatlong.csv", header=T)
           chillcalcs <- chillcalcs[apply(chillcalcs, 1, function(x) all(!is.na(x))),] # only keep rows of all not NA. 354 rows now.
           
           colnames(chillcalcs) <- c("ID_fieldsample.date","Field_Chilling_Hours","Field_Utah_Model","Field_Chill_portions")
           #fieldchillcalcs has 267 rows and 4 columns
           #check this dataframe for duplicates
           #chilldups <- chillcalcs[duplicated(chillcalcs$ID_fieldsample.date),]#for some reason, there are 14 study-field sample combinations that have duplicate chilling estimates. not sure why....and not sure which estimate to use.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="fu13_51.317_4.35_2010-12-01",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="fu13_51.317_4.35_2011-12-01",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="heide15_56.5_-3.062_2014-10-29",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="partanen98_64.85_29.583_1994-11-02",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="partanen98_60.35_24.983_1994-11-02",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="partanen98_64.85_29.583_1994-12-02",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="partanen98_60.35_24.983_1994-12-02",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="skuterud94_61.5_24.33_1993-11-25",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="skuterud94_59.666_11.33_1993-11-25",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="skuterud94_60.5_10.166_1993-11-25",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="sonsteby14_56.5_-3.062_2012-10-17",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="sonsteby14_59.666_10.792_2012-10-17",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="sonsteby14_56.5_-3.062_2013-10-22",]#2 rows with this field sample date- different chilling estimates.
           #chillcalcs[chillcalcs$ID_fieldsample.date=="sonsteby14_59.666_10.792_2013-10-22",]#2 rows with this field sample date- different chilling estimates.
           
           
           #(todrop <- chillcalcs$ID_fieldsample.date[!chillcalcs$ID_fieldsample.date %in% dat3$ID_fieldsample.date])#we lose 50 from heide07, heide07, heide77, kinet93,kronenberg76,lieten97,smeets80, smeets82,sonsteby06, sonsteby09a,sonsteby09b,verheul07,voipio01,bradford10,durner84
           
           # Some will be missing because they are not North America or Europe (eg biasi12, cook00, gansert02, nishimoto95). Others should have it: viheraaarni06 for example. Those without dates do not have field chilling, because do not have a field sample date.
           (nochillcalcs <- unique(dat3$ID_fieldsample.date[!dat3$ID_fieldsample.date %in% chillcalcs$ID_fieldsample.date]))
           
           chillcalcs <- chillcalcs[chillcalcs$ID_fieldsample.date %in% dat3$ID_fieldsample.date,]
           
           # now 280 rows
           
           #When doing either of the merges below, we get 549 extra rows. still not sure why!
           #dat4 <- merge(dat3, chillcalcs, 
           #               by = "ID_fieldsample.date",
           #               all.x = TRUE
           #               ) 
           dat4<-join(dat3, chillcalcs,by="ID_fieldsample.date",match="all")
           dat4<-full_join(dat3, chillcalcs, by="ID_fieldsample.date", match="all") #Added by Cat
           
           # Merge manually
           
           dat4a <- data.frame(dat3, chillcalcs[match(dat3$ID_fieldsample.date, chillcalcs$ID_fieldsample.date),])
           #dat4<-dat4a
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
           
           #write.csv(dat4,"analyses/output/ospree_clean_withchill.csv",row.names=FALSE, eol="\r\n")
           
           


