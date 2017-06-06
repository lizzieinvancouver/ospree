Trying to understand the chilling folder?
I, your happy README file am here to help.

Everything in this folder is related to calculating total chilling (field chilling plus experimental chilling) in our dataset.
chillmerge_all.R the file that puts it all together, using the below files that are sourced.

Ailene did much of this code (July 2016-June 2017), so ask her for clarification if you need it!
aettinger@fas.harvard.edu, ailene.ettinger@gmail.com

Want info on what data we used (and why)? See ospree/notes/dailyclimatdata.txt

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


(1) Checked that the provenance.lat and provenance.long is correct for those sampling locations that have "NA" for field chilling (in all 3 estimates) in fieldchillcalcslatlong.csv file.(Check between ospree and paper and/or ospree and map online to make sure that they are in the correct location. the fact that they got NA for field chilling means that no climate data exist for that location). If some of these are corrected, we need to re-pull the climate data and estimate chilling.

R code (with comments):
lls<-read.csv("output/fieldchillcalcslatlong.csv", header=TRUE)
colnames(lls)
lls[which(is.na(lls$Chilling_Hours)),]$datasetID
unique(ospree[ospree$datasetID=="heide08",]$provenance.lat)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="heide08",]$provenance.long)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="pagter15",]$provenance.lat)#these seem to be correct- mapped to norway
unique(ospree[ospree$datasetID=="pagter15",]$provenance.long)#these seem to be correct- mapped to norway

(2) I think the lat/long for all Pagter sites, for the purposes of calculating chilling, should be latitude 55.306595, longitude=10.478950, since the paper says"before start of the experiment plants were grown outside. Hence, they initiated cold acclimation under natural conditions."

I think the plants were grown outside at the field station where the experiment was conducted: 
Department of Food Science, Aarhus University in Aarslev, Denmark (latitude 55°18′N).


(3) Some more issue checking …

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

(4) Question: use "growing lat long instead to calculate chilling? problem is, we didn't really use that field (10075 NAs, versus only 288 NAs in provanance lat)
We changed this! So now we use growing lat/long when given (see lines 17-8 in analyses/chilling/fieldchillcalc_latlong.R).


(5) Checking changes to chilling calculation field (re Lizzie's question 3/21/2017)
I think several of the changes may be for studies that now use growing lat/long instead of provenance lat/long to calculate chilling. 
Check partanen01, calme94, heide93,for this

R code:
head(ospree[ospree$datasetID=="partanen01",]$provenance.lat)
head(ospree[ospree$datasetID=="calme94",]$growing.lat)
head(ospree[ospree$datasetID=="heide93",]$provenance.lat)