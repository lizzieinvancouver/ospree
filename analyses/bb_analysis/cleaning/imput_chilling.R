#Chilling imputation script for bb_analysis written by Dan B December 2017
## I am not sure at what point in the cleaning code this should be called, so we might need to update the path.
##For now, run bb_cleanmergeall.R to line 59

##the following should be probably be fixed before chilling is calculated.
##########################things with NA experiment chill that should be 0 ##chilldays column is already 0 maybe this should be corrected in chilling calculations##########################################

##sogaard08
d$[which(d$datasetID=="sogaard08" & d$study=="exp1a")] <- 0
d$??[which(d$datasetID=="sogaard08" & d$study=="exp2")] <- 0
d$??[which(d$datasetID=="sogaard08" & d$study=="exp3a")] <- 0
d$??[which(d$datasetID=="sogaard08" & d$study=="exp1b")] <- 0
##one study has a chill temp
d$chilltemp[which(d$datasetID=="sogaard08" & d$study=="exp3b")] <- 4 ### I imagine this would allow for exp chill calculations

#thielges75 <- 0 chilldays become NA in chillin calcs
d$??[which(d$datasetID=="thielges75")] <- 0 ### just 1 row

  
#falusi96 exp2 field.chill==no
d$??[which(d$datasetID=="falusi96" & d$field.chill=="no")] <- 0 ### field.chill== "yes" but not calculated but maybe could be (esp exp 3) where chill occuted until 1st week in march 1991


###same as above but need 0 chill days and 0 chill temp imputed
d$chillday[which(d$datasetID=="li05" & d$other.treatment=="short day controls")] <- 0
d$chillday[which(d$datasetID=="junttila12" & d$figure.table..if.applicable.=="table2")] <- 0


###we have lat and long and field sample data but chilling wasnt calculated
  #gheraldini10 exp1- need to imput field sample day from figure 2
  #gheraldini10 exp2-need to imput field sample day from figure 4


###re-clean chilltemp column
d$chilltemp[which(d$datasetID=="granhus09")] <- 0.7 # "Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C" ###change to just 0.7

#Chilling hours until endodormancy release in table-not sre about this one
  #charrier11<-exp 1
  #charrier11<-exp 2 could be the same chilling requirement as above. 

#############Harder to fix##################

## multistep: 1) Lat long and field sample date 2) freeze treatment 3) exp chilling: figure 1
  #cook05: exp1 fig1: <- would need to re-extract from figure
##multistep: 1)Freeze treatment and 2)exp chilling
  #cook05: exp1 fig2,-would need to reextract chilling from figure 2

#Possible Impuation of field chilling dates
  #falusi96 exp1 fall 1987-?
  #falusi96 exp2 field.chill==yes begining october 1989
  #falusi96 exp3 field.chill==yes begining october 1990
  #falusi90 exp1 chilldays=winter  ? to first week of Febrary

###cant/wont fix
  #gansert02: only 4 rows would require climate data from multiple Mt Fugi Weather stations
 #caffarra11a exp1
#  #heide12 exp3 no climate data for Russia perhaps?
##############ask Lizze about###############:
#Charier11

