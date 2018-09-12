## Started in early 2017 by Ailene ##
## Moved to cleaning (from chilling) folder in August 2017 ##

## Checked that the provenance.lat and provenance.long are  correct for those sampling locations 
# that have "NA" for field chilling (in all 3 estimates) 
# in fieldchillcalcslatlong.csv file.

## (Checked between ospree and paper and/or ospree and map online to make sure that they are 
# in the correct location. the fact that they got NA for field chilling means that no climate
# data exist for that location). If some of these are corrected, we need to re-pull the climate
# data and estimate chilling.

# lls<-read.csv("output/fieldchillcalcslatlong.csv", header=TRUE)
# colnames(lls)
# lls[which(is.na(lls$Chilling_Hours)),]$datasetID#these are the provlat/longs that need to be checked
# unique(ospree[ospree$datasetID=="heide08",]$provenance.lat) # these seem to be correct- mapped to norway
# unique(ospree[ospree$datasetID=="heide08",]$provenance.long) # these seem to be correct- mapped to norway
# unique(d[d$datasetID=="pagter15",]$provenance.lat) # these seem to be correct- mapped to norway
# unique(ospree[ospree$datasetID=="pagter15",]$provenance.long) # these seem to be correct- mapped to norway
# however, I think think the growing lat/long for all Pagter sites, for the purposes of
# calculating chilling, should be latitude 55.306595, longitude=10.478950, since the paper says
# "before start of the experiment plants were grown outside. Hence, they initiated cold acclimation under natural conditions."
# I think the plants were grown outside at the field station where the experiment was conducted: 
# Department of Food Science, Aarhus University in Aarslev, Denmark (latitude 55°18′N).
d[d$datasetID=="pagter15",]$growing.lat <- 55.306595
d[d$datasetID=="pagter15",]$growing.long <- 10.478950

# unique(ospree[ospree$datasetID=="skre08",]$provenance.lat) # The "Iceland" population
    # maps to the middle of the ocean (with lat longs provided in paper)
#Iceland pop should be: 64.517928, 21.892008 (the ones which were 64.38333,21.66667)
d[d$datasetID=="skre08" & d$population=="Iceland",]$provenance.lat <- 64.517928
d[d$datasetID=="skre08" & d$population=="Iceland",]$provenance.long <- 21.892008

# Similarly, Melbu population (formerly 68.61667,14.61667) should be 68.501210, 14.800709
d[d$datasetID=="skre08" & d$population=="Melbu",]$provenance.lat <- 68.501210
d[d$datasetID=="skre08" & d$population=="Melbu",]$provenance.long <- 14.800709
# Also, it seems that all provenances were grown in Bergen:
# in reality, the seedlings were kept in Tromsø (69◦N, which is ~2000km north of Bergen) "for three months, 
# but around 1 November they were transferred to Norwegian Forest Research Institute, Bergen.
# I (Ailene) used the google maps lat/long for bergen to get these lat/longs
d[d$datasetID=="skre08",]$growing.lat <- 60.3927286
d[d$datasetID=="skre08",]$growing.long <- 5.2868622
# There are climate data available for skre08 but these would have to be scraped...

# Lizzie says "I just found hawkins12, long of 427.78 for Prince Rupert should be -127.78 and since they are WEST, shouldn't all of the hawkins12 longitudes be negative?!"
d[d$datasetID=="hawkins12" & d$population=="Prince Rupert 33" & d$provenance.long=="427.78",]$provenance.long <- -127.78

# head(ospree[ospree$datasetID=="boyer",1:15]) # for boyer, the "southeast georgia" population maps to water. should be 32, -82
d[d$datasetID=="boyer" & d$population=="Southeast Georgia",]$provenance.lat <-32 #lat ok
d[d$datasetID=="boyer" & d$population=="Southeast Georgia",]$provenance.long <- -82
# 34,-94 (Southeast Arkansas) should be: 34,-92
d[d$datasetID=="boyer" & d$population=="Southeast Arkansas",]$provenance.long<- -92#lat ok
# also, southeast virgina population is put in incorrectly- should be more like 37, -76, according to paper
d[d$datasetID=="boyer" & d$population=="Southeast Virginia",]$provenance.lat <- 37 # lat ok
d[d$datasetID=="boyer" & d$population=="Southeast Virginia",]$provenance.long <- -76
#boyer 34,-84 (North Georgia) lat/longs are fine
#also, after re-reading the paper, it seems that the plants in exp2 were all grown outside the research station where the study was conducted
# (I think Alabama Agricultural Experiment Station, Auburn Unviersity, AL: )
d[d$datasetID=="boyer" & d$study=="exp2",]$growing.lat<-32.5172108
d[d$datasetID=="boyer" & d$study=="exp2",]$growing.long<- -85.9572927

# Campbell75 Cloverdale population (45.37,-124.5) should be 46,-122.81 (was in the ocean)
# Port Townsend population (48.2, -123.25 ) should be 48.117039,-122.760447 (was in the ocean)
d[d$datasetID=="campbell75" & d$population=="Cloverdale",]$provenance.lat<-46
d[d$datasetID=="campbell75" & d$population=="Cloverdale",]$provenance.long<- -122.81
d[d$datasetID=="campbell75" & d$population=="Port Townsend",]$provenance.lat<-48.117039
d[d$datasetID=="campbell75" & d$population=="Port Townsend",]$provenance.long<- -122.760447

#sansperez10 has provenance lat and long columns wrong in excel file. 
#also, it is not possible to distinguish between the two provenances given in the study
d$provenance.lat[d$datasetID=="sanzperez10"]<-""
d$provenance.long[d$datasetID=="sanzperez10"]<-""
# Add continent to those that do not have it, and use consistent capitalization
#unique(d[d$continent=="",]$datasetID)
d[d$datasetID=="caffarra11a",]$continent <- "europe"
d[d$datasetID=="caffarra11b",]$continent <- "europe"
d[d$datasetID=="cook05",]$continent <- "africa"
d[d$datasetID=="devries82",]$continent <- "europe"
d[d$datasetID=="falusi03",]$continent <- "europe"
d[d$datasetID=="falusi90",]$continent <- "europe"
d[d$datasetID=="falusi96",]$continent <- "europe"
d[d$datasetID=="Heide03",]$continent <- "europe"
d[d$datasetID=="heide05",]$continent <-"europe"
d[d$datasetID=="heide05" & d$population.detail=="USA",]$continent <- "north america"
d[d$datasetID=="heide11",]$continent <-"europe"
d[d$datasetID=="karlsson03",]$continent <- "europe"
d[d$datasetID=="laube14a",]$continent <- "europe"
d[d$datasetID=="myking95",]$continent <-"europe"
d[d$datasetID=="pettersen71",]$continent <- "europe"
d[d$datasetID=="viheraaarnio06",]$continent <- "europe"
d[d$datasetID=="worrall67",]$continent <- "north america"
d[d$datasetID=="zohner16",]$continent <- "europe"

d[d$continent=="Europe",]$continent <- "europe"
d[d$continent=="North America",]$continent <- "north america"
d[d$continent=="South America",]$continent <- "south america"
d[d$continent=="Africa",]$continent <- "africa"
d[d$continent=="Oceania",]$continent <- "oceania"
d[d$continent=="Asia",]$continent <- "asia"

#  longitude of any north american sites that are positive (and should be negative)
d$provenance.long[which(d$continent=="north america" & as.numeric(d$provenance.long)>0)] <-
    as.numeric(d$provenance.long[which(d$continent=="north america" & as.numeric(d$provenance.long)>0)])*(-1)
#dim(d[which(d$continent=="north america" & as.numeric(d$growing.long)>0),])#==0 so no need to fix growing long for any studies

#unique(d$growing.long[which(d$continent=="europe" & as.numeric(d$growing.long)<0)])#-3.3700 -119.7670  -82.3250  -81.9372  -89.4120; -3.3700 is reasonable but the other 4 are not in europe so need to check these
#unique(d$provenance.long[which(d$continent=="europe" & as.numeric(d$provenance.long)<0)])#these all seem reasonable
#Ailene fixed the following in August 2017, which are all from #heide93,this was misentered
#i have no idea where the misentered growing lat/longs came from. the paper says "The experiments were conducted at As (59°30'N)...Young trees of local origin..." 
#so, i think the growing lat-long should be the same as the provenance lat-long for all heide93 studies
d$growing.long[d$datasetID=="heide93"] <- d$provenance.long[d$datasetID=="heide93"]
d$growing.lat[d$datasetID=="heide93"] <- d$provenance.lat[d$datasetID=="heide93"]

d$continent[which(d$continent=="europe" & as.numeric(d$growing.long)==-89.4120)]<-"north america" # this seems to be the correct longitude, given that the author is a professor at university of wisconsin. change the continent to North America?
d$provenance.lat[which(d$datasetID=="chavarria09" & as.numeric(d$provenance.lat)>0)] <-#lat should be negative for brazil
  as.numeric(d$provenance.lat[which(d$datasetID=="chavarria09" & as.numeric(d$provenance.lat)>0)])*(-1)
d$growing.lat[which(d$datasetID=="chavarria09" & as.numeric(d$growing.lat)>0)] <-#lat should be negative for brazil
  as.numeric(d$growing.lat[which(d$datasetID=="chavarria09" & as.numeric(d$growing.lat)>0)])*(-1)

stop("Not an error, just stopping here to say we're now done cleaning the lat/longs. The d item in your workspace is now all cleaned up for its lat/longs. Yay!")
