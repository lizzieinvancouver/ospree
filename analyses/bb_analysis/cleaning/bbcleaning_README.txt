Started 22 March 2017
by Cat 
Follow-up by Nacho (11 April 2017)
Follow-up by Lizzie (throughout June-July 2017 and April 2018)


The goal of this folder is to clean the response variable BB and centralize in 
bb_cleanmergeall.R

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

It:
1. Reads in output/ospree_clean_withchill.csv, which should have been cleaned previously

2. Sources clean_thermaltimetodays.R, which homogenizes thermaltime to days (but only for cases where we don’t have the same data as daystobudburst or percbudburst with response.time)

3. Sources clean_respvarmore.R (formerly called clean_phenstage.R, which relabels a little data that is budburst and deals with some variables that are not exactly days to bud burst since start of forcing conditions. Note: More phenstages are left — we only converted bud burst, so more data remains here that may be useful for other studies.) See also notes on recoding nonleafouts in bbcleaningresp_README.txt

4. source bb_analysis/cleaning/multiresp.R, flags any studies that have both percent budburst and days to budburst, and studies thermal time and days to budburst studies. It then removes studies with more than one response variable for the same treatments, keeping first daystobudburst, then percentbudburst, then thermaltime. It also deals with two studies where creation of respvar.simple collapsed two different response variables (see the file for more info). 

5a. sources clean_ambientforcing.R, which cleans the forcing predictor variable
5b. sources a check of the date of daily climate files used on 5c. (of interest if we update daily climate files after the date listed.) this step may not be necessary in the long term.
	<><><> This code also adds a new column `force_type' which specifies the method used in the experiment. Types: `amb' = ambient force temp; `exp' = experimentally manipulated forcing; `none' = there was no mention of forcing; `ramped' = force temp was experimentally increased or decreased during the experiment; `not enough info' = there was enough information to accurately calculate force temp

5c. sources clean_ambientforcingfromdailyclimate.R, which extracts ambient temperature data for experiments where we know provenance and time to budburst. This code adds ambient temperate data to the force temp column when force_type = amb

6. sources clean_bbperctodays.R, which transforms %bb to usable for days-only analyses, using a specified target budburst level (i.e. 90%) with surrounding acceptable range with an allowable buffer (i.e., 49.5%); it should delete all rows not within the target range and leave only row per treatment (the one closest to the target). NOTE: If you're doing analyses with % budburst as your response you would want to delete/alter this. 

7. sources clean_moreduplicates.R which removes some data that appears duplicated (very little!). As of 12 April 2018, deletes 2 rows (used to delete 4, and of those one was an obvious duplicate and are flagged under the more complex code).

8. sources clean_photoperiod.R which tries to eek out every last bit of photoperiod data we could get. This sort of code would be good for analyses beyond just BB. 
   <><><> This code also adds a new column `photo.type' which specifies the method used in     the experiment. Types: `amb' = ambient photoperiod; `exp' = experimentally manipulated photoperiod; `none' = there was no mention of photoperiod; `ramped' = photoperiod was experimentally increased or decreased during the experiment


9. saves a new file called “ospree_clean_withchill_BB.csv” for BB analyses


<><><><><><><><><><><><><><>
Consider: 
<><><><><><><><><><><><><><>
For our analyses, we should think about how to handle the following columns: 
	varetc
	population (etc.) 
	and related variables 


################ Clarification notes from Cat #################
Additional metadata for bb_analysis/cleaning

For converting thermal time to days to budburst, was able to convert
karlsson03 Figure 1. 

The degree days equation they used was > 0 degC
The equation for heide93 is simply…
daystobudburst = degree days / force temp 
However, this was not needed because percent bud burst was already converted to days to bud burst. Changing thermal time would result in duplicated results. See clean_thermaltimetodays.R for more information. 

The degree days equation they used was > 2 degC
The equation for karlsson03 is…
daystobudburst = degree days / (force temp - 2)


As of June 2017, ”daystoleafout" (these are from fu13, laube14b, and worrall67) and "leafunfoldingdate" (morin10) are also included in this  bud-burst cleaning code.

#################


<><><><><><><><><><><><><><>
Email from Julia Laube on 
<><><><><><><><><><><><><><>
On 26 July 2017

So well, the sums were calculated as day sums with no base temperature or let´s say a 0°C base temperature from January 1st onwards. The daily sums were not summed up as (daily min + daily max) / 2, but just as daily mean temperatures.

Hope that´s clear? If you need any further value/information, please let me know. 


<><><><><><><><><><><><><><>
Update to clean_phenstage.R
now called clean_respvarmore.R
<><><><><><><><><><><><><><>
The new script now addresses all respvar issues including respvars that are ‘phenstage’ or ‘dayofyeartobudburst’




################ Clarification notes from Nacho #################


Notes on "clean_ambientforcingfromdailyclimate.R" 
#################################################
Warnings are produced but no need to worry about them, informing of if statement with 2 elements 
out of which only one (in certain iterations) is utilized.


Notes on "get_meantemp_ambient.R" 
################################

This script extracts ambient temp from start of experiment (this is the date
 we already have built in the GDD code) through to budburst and calculates the mean temp.
Then it creates an output file.

IMPORTANT!!!
This file assumes users are working only with daystobudburst and percentbudburst and where response.time!=""

################ Notes on some papers by Lizzie #################
- vitra17 has thermaltimetobudburst -- does not include an equation and it is not clear to me how else it would have been calculated (issue #304)
- fu18 - ambient I think, waiting on Dan to check 
- fu19 - is forcing treatments are ambient+treatment temperatures, using outdoor climate-controlled chambers (issue #304)
- prevey18 - ambient, waiting on Deirdre to check ... 
- richardson18 - is ambient or ambient + x (issue #304)

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
<> Where to find notes on what  we changed from NA (or not) <>
<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

- See zarchive/imput_chilling.R for notes on remaining datasetIDs that cannot be fixed (and what we fixed) but in short, we cannot fix: cook05: from South Africa, they do report chilling but you cannot assign it to particular treatments conclusively; charrier11: again, they do report chilling but you cannot assign it to particular treatments conclusively; gansert02 - Japanese study (so no climate data); granhus09 -interrupted chilling with some unknown temperatures; ; heide12 - no climate data since it's Russia; nishimoto95 - Japanese study (so no climate data)

- See bbcleaningresp_README.txt for notes on NAs in response values

- See clean_photoperiod.R for notes on cleaning photoperiod NAs (there should be none left!) 

- See clean_ambientforcing.R and clean_ambientforcingfromdailyclimate.R for notes on cleaning force temp



