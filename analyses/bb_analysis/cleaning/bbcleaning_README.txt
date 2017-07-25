Started 22 March 2017
by Cat 
Follow-up by Nacho (11 April 2017)
Follow-up by Lizzie (throughout June-July 2017)


The goal of this folder is to clean the response variable BB and centralize in 
bb_cleanmergeall.R

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

It:
1. Reads in output/ospree_clean_withchill.csv, which should have been cleaned previously

2. Sources clean_thermaltimetodays.R, which homogenizes thermaltime to days (but only for cases where we don’t have the same data as daystobudburst or percbudburst with response.time)

3. Sources clean_phenstage.R, which relabels a little data that is budburst. Yay. (Note: More phenstages are left — we only converted bud burst, so more data remains here that may be useful for other studies.)

4. source bb_analysis/cleaning/multiresp.R, flags any studies that have both percent budburst and days to budburst, and studies thermal time and days to budburst studies. It then removes studies with more than one response variable for the same treatments, keeping first daystobudburst, then percentbudburst, then thermaltime. It also deals with two studies where creation of respvar.simple collapsed two different response variables (see the file for more info). 

5. sources clean_ambientforcing.R, which cleans the forcing predictor variable

6. sources clean_bbperctodays.R, which transforms %bb to days, using a specified target budburst level (i.e. 80%) with surrounding acceptable range with an allowable buffer (i.e., 40%).

7. sources clean_moreduplicates.R which removes some data that appears duplicated (very little!). As of 16 July 2017, deletes 4 rows -- one is an obvious duplicate and are flagged under the more complex code.

8. sources clean_photoperiod.R which tries to eek out every last bit of photoperiod data we could get. This sort of code would be good for analyses beyond just BB.

9. Next, remove any percentbudburst lower than 40% (clean_bbperctodays.R only cleans lines with more than entry per treatment)

10. saves a new file called “ospree_clean_withchill_BB.csv” for BB analyses


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