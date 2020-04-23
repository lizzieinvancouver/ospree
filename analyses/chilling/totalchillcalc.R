#Calculates Total Chilling by merging in field chilling from "fieldchillcalc_latlong.R", 
#This code adds the field chilling to experimental chilling to calculate total chilling
# and adds column that identifies chilling type (field vs exp)
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
rownames(chilldat)<-NULL
expchillcalcs <- vector()
###First, need file with hrly temperature data for each row in dataframe

chilldat$chilldays <- round(as.numeric(chilldat$chilldays), digits=0) ##  need to fix anzanello
           
for(i in 1:nrow(chilldat)) { #i=331
  print(i)
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
  } else if(!is.na(chilldat$chilltemp[i]) & chilldat$chilldays[i] ==0 & !is.na(chilldat$chilldays[i]) & chilldat$chillbyhand[i]==0) {
    expchillcalc <- data.frame("Chilling_Hours"=0, "Utah_Model"=0, "Chill_portions"=0)
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
           #Add field chilling calculations 
           ###if you don't have climate data, read in chillcalc file, 
           chillcalcs <- read.csv("output/fieldchillcalcslatlong.csv", header=T)
           chillcalcs <- chillcalcs[apply(chillcalcs, 1, function(x) all(!is.na(x))),] # only keep rows of all not NA.dim: 235   6
           
           colnames(chillcalcs) <- c("ID_fieldsample.date2","Season","End_year","Field_Chilling_Hours","Field_Utah_Model","Field_Chill_portions")
           #Check the sites that are missing chilling calculations because they are not North America or Europe (eg biasi12, cook00, gansert02, nishimoto95) or because they are too recent (Zohner16).
           #Also, any site without field sample dates do not have field chilling, because do not have a field sample date.
           #nochillcalcs <- unique(dat3$ID_fieldsample.date2[!dat3$ID_fieldsample.date2 %in% chillcalcs$ID_fieldsample.date2])
           
           chillcalcs <- chillcalcs[chillcalcs$ID_fieldsample.date2 %in% dat3$ID_fieldsample.date2,]
           #found 2 duplicate chilling calculation for laube14a: laube14a_48.403008_11.711811_2012-01-30_0 and laube14a_48.403008_11.711811_2012-03-14_0. For some reason, there are 2 years listed for this one- 2010/2011 and 2011/2012. since field sample date was january 2012, it should just be te 2011-2012 season. Remove
           #no longer a problem so commenting the below out
           #chillcalcs<-chillcalcs[-which(chillcalcs$ID_fieldsample.date2=="laube14a_48.403008_11.711811_2012-01-30_0" & chillcalcs$End_year==2011),]
           #chillcalcs<-chillcalcs[-which(chillcalcs$ID_fieldsample.date2=="laube14a_48.403008_11.711811_2012-03-14_0" & chillcalcs$End_year==2011),]
           
           dat4<-join(dat3, chillcalcs,by="ID_fieldsample.date2",match="all")
           #We realized that some sites have included chilling estimates (rather than chiltemp and chillhours). Here we add those studies in:
           #unique(dat4$cu.model)
           #Four studies have field chilling reported with the utah model: "biasi12"    "cook00b"    "heide93"    "skuterud94"
           dat4$cu.model <- ifelse(is.na(dat4$cu.model), "", dat4$cu.model)
           dat4$Field_Utah_Model[dat4$cu.model=="Utah"|dat4$cu.model=="Utah model"]<-dat4$field.chill.units[dat4$cu.model=="Utah" | dat4$cu.model=="Utah model"]
           
          # chilling is calculated by multiplying chilldays and chilltemp together. 
          #however, if chilldays=0, sometimes chilltemp is listed as NA, yielding experimental chilling of NA when it should be 0. 
          dat4$chilldays[which(dat4$datasetID=="falusi90" & dat4$chilldays=="")] <-0 #no chilling for these
          dat4$chilldays[which(dat4$datasetID=="falusi96" & dat4$study=="exp2"& dat4$fieldchill=="no")] <-0  ## No chilling. would fix 44 rows  
          dat4$chilldays[which(d$datasetID=="falusi96" & dat4$study=="exp3"& dat4$fieldchill=="no")] <-0    ##would fix 52 rows
          ###li05 short day controls got no chilling
          dat4$chilldays[which(dat4$datasetID=="li05" & dat4$other.treatment=="short day controls")] <- 0  
          
          
          dat4$Exp_Chilling_Hours[which(dat4$chilldays=="0")]<-0
          dat4$Exp_Utah_Model[which(dat4$chilldays=="0")]<-0
          dat4$Exp_Chill_portions[which(dat4$chilldays=="0")]<-0
          #for caffarra11b, the field sample date with 0 chill days should be adjusted for utah and chilling hours
          dat4$Field_Chilling_Hours[which(dat4$datasetID=="caffarra11b" & dat4$chilldays=="0")]<-0
          dat4$Field_Utah_Model[which(dat4$datasetID=="caffarra11b" & dat4$chilldays=="0")]<-0
          
          ######Dan B's assement of biasi12:#######
          # I dont think this study actually did freeze treatments.
          #The column d$freeze.treatment.temp_day is actually 'chilling hours accumulated" based on table 1. this is not reflected in Chill_Hours columns in ospree_clean_withchill_BB.csv
          #Proposed solution: migrate this column to Field_Chilling_Hours somewhere in chilling code, but ask Ailene?
          dat4$Field_Chilling_Hours[dat4$datasetID=="biasi12"]<-as.numeric(dat4$freeze.treatment.temp_day[dat4$datasetID=="biasi12"])#this study actually did freeze treatments! but the columns are somehow shifted the values do not make sense for temperatures, mistake?
          dat4$freeze.treatment.temp_day[dat4$datasetID=="biasi12"]<-""
          
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
           
           ############### Updated 22 Oct 2018 by Ailene ####################
           ##### Chilling type : #####
           ### 6 types: 
           ### NA (no info on chilling); 
           ### exp (experimental chilling only)
           ### fldest (our estimate of field chilling, based on climate data from the provenance lat/long)
           ### fldrep (field chilling that the study reported)
           ### bothrep (both experimental and field (our estimate) chilling)
           ### bothest (both experimental and field (reported by study)
           dat4$chill_type<-NA
           dat4$chill_type[is.na(as.numeric(dat4$Exp_Chilling_Hours))==FALSE|is.na(as.numeric(dat4$Exp_Utah_Model))==FALSE|is.na(as.numeric(dat4$Exp_Chill_portions))==FALSE]<-"exp"#experimental chilling
           dat4$chill_type[is.na(as.numeric(dat4$Field_Chilling_Hours))==FALSE|is.na(as.numeric(dat4$Field_Utah_Model))==FALSE|is.na(as.numeric(dat4$Field_Chill_portions))==FALSE]<-"fldest"#field chilling that we estimate
           dat4$chill_type[is.na(as.numeric(dat4$Exp_Chilling_Hours))==FALSE & is.na(as.numeric(dat4$Field_Chilling_Hours))==FALSE]<-"bothest"#field chilling that we estimate
           dat4$chill_type[is.na(as.numeric(dat4$Exp_Utah_Model))==FALSE & is.na(as.numeric(dat4$Field_Utah_Model))==FALSE]<-"bothest"#field chilling that we estimate
           dat4$chill_type[is.na(as.numeric(dat4$Exp_Chill_portions))==FALSE & is.na(as.numeric(dat4$Field_Chill_portions))==FALSE]<-"bothest"#field chilling that we estimate
           dat4$chill_type[dat4$cu.model=="Utah"|dat4$cu.model=="Utah model"]<-"bothrep"#checked all these rows, and they have both reported field chilling and experimental chilling
           #there are other studies that have reported chilling but we are not currently using their data because they are in strange units
           #unique(dat4$cu.model)
           #Byron, "hoursbelow7deg","hours below 5.5","days-10","North Carolina"
           
           stop("Not an error, just stopping here to say we're now done totalling up field and experimental chilling. Yay!")
           
           table(dat4$chill_type,dat4$Exp_Chill_portions)
           