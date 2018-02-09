Intro to the bb_dailyclimate folder

Everything in this folder is related to pulling and summarizing daily temperature data (tmin, tmax) for bud burst events

Ailene did much of this code, so ask her for clarification if you need it!
aettinger@fas.harvard.edu, ailene.ettinger@gmail.com

Want info on what climate data we used (and why)? See ospree/notes/dailyclimatdata.txt

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

The files here do the following:
1. cleaning_chilltemp.R: cleans the non-numeric data from the chilltemp and chill days columns in ospree_clean.csv; also adds year data and fieldsampledate data when missing. 

2. cleaning_provlatlong.R: Cleans the provenance.lat, provenance.long, growing.lat, and growing.long columns, to get appropriate location, since in some cases the lat/longs listed in papers were in the middle of the ocean!

3.fieldchillcalc_latlong.R: summarizes the lat/longs needed to pull climate data from europe and north america. These are the “growing” lat/longs if listed in the database, or, if no growing lat/long is given, then they are the provenance lat/long

4. pullclimate_eur.R & pullclimate_nam.R: pulls in climate data for europe and north america, respectively, as netcdf files, from an external hard drive, using the provenance lat/long columns and calculates field chilling experienced before the material was collected for the experiment. If you do not have the external hard drive, or do not want to take the time to pull the data (it can take ~5-10 minutes) then you can get around this by loading the RData file listed in chillmerge_all

5. interpolclimate.R: Interpolate hourly temperatures from the daily values & chilling using three different metrics. Generates a new csv file with the just the chilling estimates for those studies with field sample dates: fieldchillcalcslatlong.csv

6. totalchillcalc.R code merges the fieldchillcalcslatlong.csv with the ospree_clean.csv file and sums up the field and experimental chilling 

Last step creates a new version of ospree called “ospree_clean_withchill.csv”
that includes these chilling estimates



<><><><><><><><><><><><><><><><><><><><><><><><><><><><>
Some extra notes on what was cleaned, double-checked etc.
<><><><><><><><><><><><><><><><><><><><><><><><><><><><>


(1) Ailene checked that the provenance.lat and provenance.long is correct for those sampling locations that have "NA" for field chilling (in all 3 estimates) in fieldchillcalcslatlong.csv file.R code (with comments):
lls<-read.csv("output/fieldchillcalcslatlong.csv", header=TRUE)
colnames(lls)
lls[which(is.na(lls$Chilling_Hours)),]$datasetID
unique(ospree[ospree$datasetID=="heide08",]$provenance.lat)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="heide08",]$provenance.long)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="pagter15",]$provenance.lat)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="pagter15",]$provenance.long)#these seem to be correct- mapped to norway

(2) Ailene updated the lat/long for all Pagter sites, for the purposes of calculating chilling. They are now latitude=55.306595, longitude=10.478950, since the paper says"before start of the experiment plants were grown outside. Hence, they initiated cold acclimation under natural conditions." This was updated in the cleaning file
(plants were grown outside at the field station where the experiment was conducted: Department of Food Science, Aarhus University in Aarslev, Denmark (latitude 55°18′N).)


(3) Ailene also updated the following lat/longs in the cleaning code, because they mapped to the middle of the ocean or other inaccurate locations. 
R code (with comments):
unique(ospree[ospree$datasetID=="skre08",]$provenance.lat)#The "Iceland" population maps to the middle of the ocean (with lat longs provided in paper)
#Iceland pop should be: 64.517928, 21.892008 (the ones which were 64.38333,21.66667)
#Similarly, Melbu population (formerly 68.61667,14.61667) should be 68.501210, 14.800709
head(ospree[ospree$datasetID=="boyer",1:15])#for boyer, the "southeast georgia" population maps to water. should be 32, -82
#also, southeast virgina population is not located in southeast virginia (currently 32, 81;its in southeast georgia)- should be more like 37, -77
#34,-94 (Southeast Arkansas) should be: 34,-92
#34,-84 (North Georgia) is fine
head(ospree[ospree$datasetID=="campbell75",1:15])#for boyer, the "southeast georgia" population maps to water. should be 32, -82
#Cloverdale population (45.37,-124.5) should be 46,-122.81 (was in the ocean)
#Port Townsend population (48.2, -123.25 ) should be 48.117039,-122.760447 (was in the ocean)

(4) Ailene updated chilling calculation to use  "growing lat long” when present (instead of provenance late/long”) to calculate chilling? The only problem with this is that when we first input data we didn't seem to use this field consistently (10075 NAs, versus only 288 NAs in provanance lat). Nonetheless, we now we use growing lat/long when given (see analyses/chilling/fieldchillcalc_latlong.R).


(5) Ailene checked the chilling calculations for several studies that seemed to be questionable (changes to chilling calculation field, re Lizzie's question 3/21/2017, for partanen01, calme94, heide93). These are all fine now.
