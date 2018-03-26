#Chilling imputation script for bb_analysis written by Dan B December 2017

#Update feb 12:
#missing 850 rows 

###falusi 90 87-88
d$fieldsample.date2[which(d$datasetID=="falusi90" & d$chilldays=="winter")] <- ###not fixable we dont know year
  d$chilldays[which(d$datasetID=="falusi90" & is.na(d$chilldays))] <-0 #fixed in total.chilling.R
  
  
  ######Falusi96 ### for all exp1 and 2 and 3 where field chill==yes added field sample date of default April 30 in cleaning_chilltemp.R
#for chilldays=0 fixed in total.chilling.R 
#exp1
  d$[which(d$datasetID=="falusi96" & d$study=="exp1")]<-  ambient #would fix 37 row
 #exp2   
    d$[which(d$datasetID=="falusi96" & d$study=="exp2"& d$fieldchill=="yes")]<-  ###would fix 80 entries
    d$chilldays[which(d$datasetID=="falusi96" & d$study=="exp2"& d$fieldchill=="no")] <-0  ## No chilling. would fix 44 rows  
  #exp3 
  d$fieldsample.date.2[which(d$datasetID=="falusi96" & d$study=="exp3"& d$fieldchill=="yes")]<- ##would fix 84 rows
  d$chilldays[which(d$datasetID=="falusi96" & d$study=="exp3"& d$fieldchill=="no")] <-0    ##would fix 52 rows DOne in totall.chilling.R

  
###ghelardini12 exp 1 and 2. Field sampling date is in the figures but never entered into the data.
  ##could get and additional 109 rows of data would 
  
###li05 short day controls got no chilling- fixed  in totalchilling.R
  d$chillday[which(d$datasetID=="li05" & d$other.treatment=="short day controls")] <- 0  
  
  ####caffara11a
  #just 9 rows. not sure how to fix
  
  #charrier 11 --look at again or ask lizzie
  ###missing field longitude for exp2 2
  #exp1 has lat and long 1 has it, but not filed sample date. Let's look at this together. NOT SURE
char<-subset(bb.all, datasetID=="charrier11")  
 #Table 1 results. I think everything got experimental chilling "4°C with light for 8 h/day for a between 0 and 1600
#hours. But the table doesnt indicate which got which chilling. It does reprot "chilling hours til endodormancy release" but I am not sure if that is so helpful.
##For fig 2 I think same problem as above. They did recieve some chilling.
#FUrther, we also don't know which 

  ###cook05 would need to reextract from figure-- not fixed
cook<-subset(bb.all, datasetID=="cook05") 
##A few problems, 1 we don't have south africa climate data.
##but Utah Chilling units are reported in the figure. BUt really impossibele to use because chill units and acutal data are on same figures but there is no correspondence..


#gansert02: only 4 rows not fixed no japanese climate data
  
##granhus09 not easy to fix. 0.7 chilling would way over estimate
  d$chilltemp[which(d$datasetID=="granhus09")] <- 0.7 # "Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C" ###change to just 0.7

  
###heide12 12 row. no climate data fromRussia 
  
###heide93 didnt calculate field chilling for corylus only#Ailene is investigating.
  
  ###laube 14b fixed in cleaning_chilltemp.R.
  d$fieldsample.date.2[which(d$datasetID=="laube14b")]<-03-03-2012
 
##Howe95 #THis is measuring days to BUDSET. I don't think it should BB_analysis
  
##Morin10 1 row is missing field sample date, fixed in cleaning_chilltemp.R.
  d$fieldsample.date.2[which(d$datasetID=="morin10")]<-01-01-2004 
  
  ###nishmoto95. #not fixed
  #includes own chilling calculation ###also we have field sample date but probably not climate data for japan
  
  ###sogaard08 chill temp fixed in cleaning_chilltemp.R.
  d$chilltemp[which(d$datasetID=="sogaard08" & d$study=="exp3b")] <- 4 
  
  ###worrall65 no field sampling date for the NAs (30 rows) (many come from figure 9 which doesnt exist
  )