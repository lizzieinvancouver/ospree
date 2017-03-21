#My ospree to do tasks
# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {    setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")

#9:Look at ospree.csv to see if following fixes were made
#Exp 1 (which is the only exp from this paper missing a year): No year given (maybe it's 1988? -- just guessing based on year progression of next two experiments -- but we should check)
#Exp 2: Study starts in 1989
#Exp 3: Study starts in fall 1990
#Temporary fix: exp 1 1988, exp 2 1990 (if started in 89, measured in 90) and exp 3 1991
ospree<-read.csv("input/ospree.csv", header=T)
falusi<-ospree[ospree$datasetID=="falusi96",]
table(falusi$study,falusi$year)
falusi2<-ospree[ospree$datasetID=="falusi90",]
table(falusi2$study,falusi2$year)
dim(falusi2)
#Check that the provenance.lat and provenance.long is correct for those sampling locations 
#that have "NA" for field chilling (in all 3 estimates) 
#in fieldchillcalcslatlong.csv file. 
#(Check between ospree and paper and/or ospree and map online to make sure that they are 
#in the correct location. the fact that they got NA for field chilling means that no climate data exist for that location). If some of these are corrected, we need to re-pull the climate data and estimate chilling.
lls<-read.csv("output/fieldchillcalcslatlong.csv", header=TRUE)
colnames(lls)
lls[which(is.na(lls$Chilling_Hours)),]$datasetID
unique(ospree[ospree$datasetID=="heide08",]$provenance.lat)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="heide08",]$provenance.long)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="pagter15",]$provenance.lat)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="pagter15",]$provenance.long)#these seem to be correct- mapped to norway
#I think th#i think the lat/long for all PAgter sites, for the purposes of calculating chilling, should be latitude 55.306595, longitude=10.478950, 
#since the paper says"before start of the experiment plants were grown outside. Hence, they initiated cold acclimation under natural conditions."
#I think the plants were grown outside at the field station where the experiment was conducted: 
#Department of Food Science, Aarhus University in Aarslev, Denmark (latitude 55°18′N).
unique(ospree[ospree$datasetID=="skre08",]$provenance.lat)#The "Iceland" population maps to the middle of the ocean (with lat longs provided in paper)
#Iceland pop should be: 64.517928, 21.892008 (the ones which were 64.38333,21.66667)
#Similarly, Melbu population (formerly 68.61667,14.61667) should be 68.501210, 14.800709
head(ospree[ospree$datasetID=="boyer",1:15])#for boyer, the "southeast georgia" population maps to water. should be 32, -82
#also, southeast virgina population is not located in southeast virginia (currenty 32, 81;its in southeast georgia)- should be more like 37, -77
#34,-94 (Southeast Arkansas) should be: 34,-92
#34,-84 (North Georgia) is fine
head(ospree[ospree$datasetID=="campbell75",1:15])#for boyer, the "southeast georgia" population maps to water. should be 32, -82
#Cloverdale population (45.37,-124.5) should be 46,-122.81 (was in the ocean)
#Port Townsend population (48.2, -123.25 ) should be 48.117039,-122.760447 (was in the ocean)
#Questions
#use "growing lat long instead to calculate chilling? problem is, we didn't really use that field (10075 NAs, versus only 288 NAs in provanance lat)
##YES!
#Checking changes to chilling calculation fiel (re Lizzie's quesitons 3/21/2017)
#I think several of the changes may be for studies that now use growing lat/long instead of provenance lat/long to calculate chilling. 
#Check partanen01, calme94, heide93,for this
head(ospree[ospree$datasetID=="partanen01",]$provenance.lat)
head(ospree[ospree$datasetID=="calme94",]$growing.lat)
head(ospree[ospree$datasetID=="heide93",]$provenance.lat)
