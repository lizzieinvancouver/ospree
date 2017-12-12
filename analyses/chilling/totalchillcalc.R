#Calculates Total Chilling by merging in field chilling from "fieldchillcalc_latlong.R", 
#This code adds the field chilling to experimental chilling to calculate total chilling
#by Ailene

#Make a column that indexes the experimental chilling treatment (including chilltemp, chillphotoperiod & chilldays), in order to calculate field chilling
d$ID_chilltreat<-paste(d$datasetID,d$chilltemp,d$chilldays,sep=".")
           
##### Calculate experimental chilling, using chillday and chilltemp.
#want table with datasetID chilling days, chilling temperature,  treat for each study. 
chilldat <- d %>% # start with the data frame
distinct(ID_chilltreat,.keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(datasetID, chilltemp, chilldays, year,ID_chilltreat,chillbyhand)
byhand<-chilldat[which(chilldat$chillbyhand==1),]#38 rows of chilldat that need to be calculated by hand
chilldat$chilltemp<-as.numeric(chilldat$chilltemp)#this replaces any non-numeric chilltemp values with NA
chilldat$chilldays<-as.numeric(chilldat$chilldays)
chilldat[which(is.na(chilldat$year)),]$year<-2016 #temporary fix for when years are not currently listed!
chilldat<- chilldat[apply(chilldat, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
chilldat<-rbind(chilldat,byhand)
expchillcalcs <- vector()
###First, need file with hrly temperature data for each row in dataframe
           
for(i in 1:nrow(chilldat)) {
 # First, calculate chilling when chilltemp and chilldays do not ==NA (i.e. )
  if(!is.na(chilldat$chilltemp[i]) & chilldat$chilldays[i] !=0 & !is.na(chilldat$chilldays[i]) & chilldat$chillbyhand[i]==0) {
  yr <- as.numeric(chilldat$year[i]) 
   hrly =
   data.frame(
    Temp = rep(as.numeric(chilldat$chilltemp[i]), 
    times = 24 * round(as.numeric(chilldat$chilldays[i], digits=0))),
     Year = rep(yr, 
     times = 24 * round(as.numeric(chilldat$chilldays[i],digits=0))),
      JDay = sort(rep(as.numeric(seq(1:round(as.numeric(chilldat$chilldays[i], digits=0)))), times = 24))
      )
           
    expchillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) 
           
  } else if (chilldat$chillbyhand[i]==1) {
    yr <- as.numeric(chilldat$year[i]) 
    if(chilldat$datasetID[i]=="jones12"){#jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
        temptreats<-strsplit(chilldat$chilltemp[i],",")
        temp1<-as.numeric(temptreats[[1]][1])
        temp2<-as.numeric(temptreats[[1]][2])
        temp3<-as.numeric(temptreats[[1]][3])
    hrly =
        data.frame(
          Temp = c(rep(temp1, times = 24 * as.numeric(chilldat$chilldays[i])/3),rep(temp2, times = 24 * as.numeric(chilldat$chilldays[i])/3),rep(temp3, times = 24 * as.numeric(chilldat$chilldays[i])/3)),
          Year = rep(yr, times = 24 * round(as.numeric(chilldat$chilldays[i],digits=0))),
          JDay = sort(rep(as.numeric(seq(1:round(as.numeric(chilldat$chilldays[i], digits=0)))), times = 24))
        )
    expchillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) 
    }
    if(chilldat$datasetID[i]=="lamb37"){#lamb37 - 26.6F for 8 hr and 37.4 for 16 hr
      temptreats<-strsplit(chilldat$chilltemp[i],",")
      temp1<-as.numeric(temptreats[[1]][1])
      temp2<-as.numeric(temptreats[[1]][2])
      hrly =
        data.frame(
          Temp = rep(c(rep(temp1, times = 8),rep(temp2, times = 16)),times=as.numeric(chilldat$chilldays[i])),
          Year = rep(yr, times = 24 * round(as.numeric(chilldat$chilldays[i],digits=0))),
          JDay = sort(rep(as.numeric(seq(1:round(as.numeric(chilldat$chilldays[i], digits=0)))), times = 24))
        )
      expchillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) 
    }
    if(chilldat$datasetID[i]=="li05"){##li05 - seedlings were chilled for 3 weeks at 6, and 3 weeks at 0.5
      temptreats<-strsplit(chilldat$chilltemp[i],",")
      temp1<-as.numeric(temptreats[[1]][1])
      temp2<-as.numeric(temptreats[[1]][2])
      hrly =
        data.frame(
          Temp = c(rep(temp1, times = 24 * as.numeric(chilldat$chilldays[i])/2),rep(temp2, times = 24 * as.numeric(chilldat$chilldays[i])/2)),
          Year = rep(yr, times = 24 * round(as.numeric(chilldat$chilldays[i],digits=0))),
          JDay = sort(rep(as.numeric(seq(1:round(as.numeric(chilldat$chilldays[i], digits=0)))), times = 24))
        )
      expchillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) 
    }
    if(chilldat$datasetID[i]=="man10"){#man10- seedlings were chilled for one month at -3, and one month at 2
      temptreats<-strsplit(chilldat$chilltemp[i],",")
      temp1<-as.numeric(temptreats[[1]][1])
      temp2<-as.numeric(temptreats[[1]][2])
      hrly =
        data.frame(
          Temp = c(rep(temp1, times = 24 * as.numeric(chilldat$chilldays[i])/2),rep(temp2, times = 24 * as.numeric(chilldat$chilldays[i])/2)),
          Year = rep(yr, times = 24 * round(as.numeric(chilldat$chilldays[i],digits=0))),
          JDay = sort(rep(as.numeric(seq(1:round(as.numeric(chilldat$chilldays[i], digits=0)))), times = 24))
        )
      expchillcalc <- chilling(hrly, hrly$JDay[1], hrly$JDay[nrow(hrly)]) 
    }
    
  } else {expchillcalc <- data.frame("Chilling_Hours"=NA, "Utah_Model"=NA, "Chill_portions"=NA) }
           
           expchillcalcs <- rbind(expchillcalcs, 
           data.frame(datasetID = chilldat$datasetID[i], 
           ID_chilltreat = chilldat$ID_chilltreat[i],
           expchillcalc[c("Chilling_Hours","Utah_Model","Chill_portions")]))
           }
           
           colnames(expchillcalcs)[3:5] <- c("Exp_Chilling_Hours","Exp_Utah_Model","Exp_Chill_portions")

#Merge field and experimental chilling data with the rest of the data
#Add experimental chilling. Right number of rows = 12862 rows, 63 columns
           dat3 <- merge(d, expchillcalcs, 
           by.x = c("datasetID","ID_chilltreat"),
           by.y=c("datasetID","ID_chilltreat"),
           all.x=T)
        
           dat3$ID_fieldsample.date2<-paste(dat3$datasetID,dat3$chill.lat,dat3$chill.long,dat3$fieldsample.date2,d2$addexpwarm, sep="_")
           #Add field chilling calculations to datafile. #still 12862 rows, now 64 columns (3 ways of estimating experimental chilling)
           ###First, read in chillcalc file, so that you don't have to run the above code with the external hard drive of climate data
           chillcalcs <- read.csv("output/fieldchillcalcslatlong.csv", header=T)
           chillcalcs <- chillcalcs[apply(chillcalcs, 1, function(x) all(!is.na(x))),] # only keep rows of all not NA.dim: 235   6
           
           colnames(chillcalcs) <- c("ID_fieldsample.date2","Season","End_year","Field_Chilling_Hours","Field_Utah_Model","Field_Chill_portions")
           #Check the sites that are missing chilling calculations because they are not North America or Europe (eg biasi12, cook00, gansert02, nishimoto95) or because they are too recent (Zohner16).
           #Also, any site without field sample dates do not have field chilling, because do not have a field sample date.
           #nochillcalcs <- unique(dat3$ID_fieldsample.date2[!dat3$ID_fieldsample.date2 %in% chillcalcs$ID_fieldsample.date2]))
           
           chillcalcs <- chillcalcs[chillcalcs$ID_fieldsample.date2 %in% dat3$ID_fieldsample.date2,]
           #found 2 duplicate chilling calculation for laube14a: laube14a_48.403008_11.711811_2012-01-30_0 and laube14a_48.403008_11.711811_2012-03-14_0. For some reason, there are 2 years listed for this one- 2010/2011 and 2011/2012. since field sample date was january 2012, it should just be te 2011-2012 season. Remove
           chillcalcs<-chillcalcs[-which(chillcalcs$ID_fieldsample.date2=="laube14a_48.403008_11.711811_2012-01-30_0" & chillcalcs$End_year==2011),]
           chillcalcs<-chillcalcs[-which(chillcalcs$ID_fieldsample.date2=="laube14a_48.403008_11.711811_2012-03-14_0" & chillcalcs$End_year==2011),]
           
           dat4<-join(dat3, chillcalcs,by="ID_fieldsample.date2",match="all")
           #dat4<-full_join(dat3, chillcalcs, by="ID_fieldsample.date2", match="all") #Added by Cat
           #We realized that some sites have included chilling estimates (rather than chiltemp and chillhours). Here we add those studies in:
           #unique(dat4$cu.model)
           #Four studies have field chilling reported with the utah model: "biasi12"    "cook00b"    "heide93"    "skuterud94"
          dat4$Field_Utah_Model[dat4$cu.model=="Utah"|dat4$cu.model=="Utah model"]<-dat4$field.chill.units[dat4$cu.model=="Utah"|dat4$cu.model=="Utah model"]#149 rows
           
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
           #check which sites are missing chilling data:
           #unique(dat4$datasetID[which(is.na(dat4$Total_Utah_Model))])
           
           stop("Not an error, just stopping here to say we're now done totalling up field and experimental chilling. Yay!")
           