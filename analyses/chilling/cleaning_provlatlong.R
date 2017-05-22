#Check that the provenance.lat and provenance.long is correct for those sampling locations 
#that have "NA" for field chilling (in all 3 estimates) 
#in fieldchillcalcslatlong.csv file. 
#(Check between ospree and paper and/or ospree and map online to make sure that they are 
#in the correct location. the fact that they got NA for field chilling means that no climate data exist for that location). If some of these are corrected, we need to re-pull the climate data and estimate chilling.
#lls<-read.csv("output/fieldchillcalcslatlong.csv", header=TRUE)
#colnames(lls)
#lls[which(is.na(lls$Chilling_Hours)),]$datasetID#these are the provlat/longs that need to be checked
#unique(ospree[ospree$datasetID=="heide08",]$provenance.lat)#these seem to be correct- mapped to norway
#unique(ospree[ospree$datasetID=="heide08",]$provenance.long)#these seem to be correct- mapped to norway
#unique(d[d$datasetID=="pagter15",]$provenance.lat)#these seem to be correct- mapped to norway
#unique(ospree[ospree$datasetID=="pagter15",]$provenance.long)#these seem to be correct- mapped to norway
#however, I think think the growing lat/long for all PAgter sites, for the purposes of calculating chilling, should be latitude 55.306595, longitude=10.478950, 
#since the paper says"before start of the experiment plants were grown outside. Hence, they initiated cold acclimation under natural conditions."
#I think the plants were grown outside at the field station where the experiment was conducted: 
#Department of Food Science, Aarhus University in Aarslev, Denmark (latitude 55°18′N).
d[d$datasetID=="pagter15",]$growing.lat<-55.306595
d[d$datasetID=="pagter15",]$growing.long<-10.478950
#unique(ospree[ospree$datasetID=="skre08",]$provenance.lat)#The "Iceland" population maps to the middle of the ocean (with lat longs provided in paper)
#Iceland pop should be: 64.517928, 21.892008 (the ones which were 64.38333,21.66667)
d[d$datasetID=="skre08" & d$population=="Iceland",]$provenance.lat<-64.517928
d[d$datasetID=="skre08" & d$population=="Iceland",]$provenance.long<-21.892008
#Similarly, Melbu population (formerly 68.61667,14.61667) should be 68.501210, 14.800709
d[d$datasetID=="skre08" & d$population=="Melbu",]$provenance.lat<-68.501210
d[d$datasetID=="skre08" & d$population=="Melbu",]$provenance.long<-14.800709
#also, it seems that all provenances were grown in Bergen:
#in reality, the seedlings were kept in Tromsø (69◦N, which is ~2000km north of Bergen) "for three months, 
#but around 1November they were transferred to Norwegian Forest Research Institute, Bergen.
#I use the google maps lat/long for bergen to get these lat/longs
d[d$datasetID=="skre08",]$growing.lat<-60.3927286
d[d$datasetID=="skre08",]$growing.long<-5.2868622
#there are climate data available for skre08 but these would have to be scraped...

#head(ospree[ospree$datasetID=="boyer",1:15])#for boyer, the "southeast georgia" population maps to water. should be 32, -82
d[d$datasetID=="boyer" & d$population=="Southeast Georgia",]$provenance.lat<-32#lat ok
d[d$datasetID=="boyer" & d$population=="Southeast Georgia",]$provenance.long<--82

#34,-94 (Southeast Arkansas) should be: 34,-92
d[d$datasetID=="boyer" & d$population=="Southeast Arkansas",]$provenance.long<--92#lat ok

#also, southeast virgina population is put in incorrectly- should be more like 37, -76, according to paper
d[d$datasetID=="boyer" & d$population=="Southeast Virginia",]$provenance.lat<-37#lat ok
d[d$datasetID=="boyer" & d$population=="Southeast Virginia",]$provenance.long<--76
#boyer 34,-84 (North Georgia) lat/longs are fine
#also, after re-reading the paper, it seems that the plants in exp2 were all grown outside the research station where the study was conducted
#(i think Alabama Agricultural Experiment STation, Auburn Unviersity, AL: )
d[d$datasetID=="boyer" & d$study=="exp2",]$growing.lat<-32.5172108
d[d$datasetID=="boyer" & d$study=="exp2",]$growing.long<--85.9572927
#Campbell75 Cloverdale population (45.37,-124.5) should be 46,-122.81 (was in the ocean)
#Port Townsend population (48.2, -123.25 ) should be 48.117039,-122.760447 (was in the ocean)
d[d$datasetID=="campbell75" & d$population=="Cloverdale",]$provenance.lat<-46
d[d$datasetID=="campbell75" & d$population=="Cloverdale",]$provenance.long<--122.81
d[d$datasetID=="campbell75" & d$population=="Port Townsend",]$provenance.lat<-48.117039
d[d$datasetID=="campbell75" & d$population=="Port Townsend",]$provenance.long<--122.760447

