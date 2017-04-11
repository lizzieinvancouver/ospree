Trying to understand the cleaning folder?
I, your happy REAME file am here to help.

So the goal is to centralize as much cleaning as possible in cleanmerge_all.R
To repeat, clean merge_all.R is your go to cleaning file (I hope).

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

It:
1. Reads in ospree.csv, which is the data_detailed tab from data/ospree.xlsx

* Note: Zohner data already merged into ospree.xlsx. See zohner_cleaning/zohnercleaning_README.txt for more about how that happened. 

2. There are some weird columns that get read in. We delete these. Poof… and they are gone!

3. Next, we source the clean_respvar.R code.

4. Next, we source the clean_photo.R code (because that is just how we roll).

5. The we source clean_forcetemp.R

6. Next! Source clean_woody_sps.R to remove non-woody species (bye bye Fragaria, we will miss you). And we clean a few species and genus names. 

Also, here if you want, you can run:
clean_spp_match.R — code that checks species against The Plant List (using R package Taxonstand), it returns species with possible issues via object manchecksp. We should check this off and on, but CLEANING SHOULD occur in clean_woody_sps.R (because Taxonstand is a little slow and thus we don’t need to run it each time).

7. The we source clean_responsetime.R which cleans response and response time columns.

8. Remove duplicate lines (with clean_duplicates.final.R)

9. Write it all out!


<><><><>
Wait! What are these other files?
<><><><>

cleanup_checksmaps.R — Dan Flynn’s old cleaning code that also makes some maps. The checking and the maps are nice, so we keep it here. 




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
#also, southeast virgina population is not located in southeast virginia (currenty 32, 81;its in southeast georgia)- should be more like 37, -77
#34,-94 (Southeast Arkansas) should be: 34,-92
#34,-84 (North Georgia) is fine
head(ospree[ospree$datasetID=="campbell75",1:15])#for boyer, the "southeast georgia" population maps to water. should be 32, -82
#Cloverdale population (45.37,-124.5) should be 46,-122.81 (was in the ocean)
#Port Townsend population (48.2, -123.25 ) should be 48.117039,-122.760447 (was in the ocean)

(4) Question: use "growing lat long instead to calculate chilling? problem is, we didn't really use that field (10075 NAs, versus only 288 NAs in provanance lat)
YES!


(5) Checking changes to chilling calculation field (re Lizzie's question 3/21/2017)
I think several of the changes may be for studies that now use growing lat/long instead of provenance lat/long to calculate chilling. 
Check partanen01, calme94, heide93,for this

R code:
head(ospree[ospree$datasetID=="partanen01",]$provenance.lat)
head(ospree[ospree$datasetID=="calme94",]$growing.lat)
head(ospree[ospree$datasetID=="heide93",]$provenance.lat)
