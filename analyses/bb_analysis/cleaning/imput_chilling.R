#Chilling imputation script for bb_analysis written by Dan B December 2017

#Update feb 12:
#missing 850 rows 

######Falusi 90  chilling from fall- first week of february
###but ahhh we dont not what year? can we impute/. Received Dec 1989. Trial lasted 235 days. 
###I would guess it took time analyze results so I'd imagine the year of the trial is 1987-88 but no idea
d$fieldsample.date2[which(d$datasetID=="falusi90" & d$chilldays=="winter")] <- 02-01-88? ##would fix 94 rows
  d$chilldays[which(d$datasetID=="falusi90" & is.na(d$chilldays))] <-0 ##would fix 69 rows
  
  
  ######Falusi96
  #exp1 grown upder natural conditions in florence in 1987-chilling should just be ambient for 1987-88
  d$[which(d$datasetID=="falusi96" & d$study=="exp1")]<-  #would fix 37 row
    #exp2  ambient for 89-90
    d$[which(d$datasetID=="falusi96" & d$study=="exp2"& d$fieldchill=="yes")]<-  ###would fix 80 entries
    d$chilldays[which(d$datasetID=="falusi96" & d$study=="exp2"& d$fieldchill=="no")] <-0  ## No chilling. would fix 44 rows  
  
  #exp3  ambient for 90-91 brought in first week in march
  d$fieldsample.date.2[which(d$datasetID=="falusi96" & d$study=="exp3"& d$fieldchill=="yes")]<-03-01-91 ##would fix 84 rows
  d$chilldays[which(d$datasetID=="falusi96" & d$study=="exp3"& d$fieldchill=="no")] <-0    ##would fix 52 rows
  ####dealing with falusi alone restores 460 out of the 850 missing rows!
  
  ###ghelardini12 exp 1 and 2. Field sampling date is in the figures but never entered into the data.
  ##could get and additional 109 rows of data
  
  ###li05 short day controls got no chilling
  d$chillday[which(d$datasetID=="li05" & d$other.treatment=="short day controls")] <- 0  
  
  ####caffara11a
  #just 9 rows. not sure how to fix
  
  #charrier 11
  ###missing field longitude for exp2 2
  #exp1 has lat and long 1 has it, but not filed sample date. Let's look at this together. NOT SURE
  
  ###cook05 would need to reextract from figure
  
  #gansert02: only 4 rows
  #Mt Fuji so I don't think we have clomate data
  
  ###granhus09 easy logical imputation for all 21 rows
  d$chilltemp[which(d$datasetID=="granhus09")] <- 0.7 # "Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C" ###change to just 0.7
  
  ###heide12 just 12 rows from russia missing...maybe we dont have this cliamte data?
  
  ###heide93 only didnt calculate field chilling for corylus despite having sample date and lat long just 8 rows
  
  ###laube 14b
  d$fieldsample.date.2[which(d$datasetID=="laube14b")]<-03-03-2012 ###gets 17 more rows
  
  ##Howe95
  #THis is measuring days to BUDSET. I don't think it should BB_analysis
  
  ##Morin10 1 row is missing field sample date, but it should be the same as the rest
  d$fieldsample.date.2[which(d$datasetID=="morin10")]<-01-01-2004
  
  ###nishmoto95.
  #includes own chilling calculation
  ###also we have field sample date but probably not climate data for japan
  
  ###sogaard08 chill temp was not entered properly for this experiement
  d$chilltemp[which(d$datasetID=="sogaard08" & d$study=="exp3b")] <- 4
  
  ###worrall65 no field samliing date for the NAs (30 rows) (many come from figure 9 which doesnt exist
  )