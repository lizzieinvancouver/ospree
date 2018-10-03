Started 28 September 2018
by Ailene

This folder (ospree/analyses/cleaning/checkphotoperiod/) was created after we found several instances where chilling/forcing/photoperiod treatments may potenially not be correct in ospree either because they were entered incorrectly in ospree.csv or because they were altered incorrectly in subsequent code that created ospree_clean_withchill_BB.csv

We also want to know how many of the studies manipulating photoperiod found significant effects of photoperiod (either main or interactive). Thus, we need to look at the original paper for this information as well. 

All in all we need to:
1) compare for consistency across 
	a) the paper
	b) ospree.csv 
	c) ospree_clean_withchill_BB.csv
2) note if significant effects were found for photoperiod

NOTE: we are not checking ALL studies for consistency. We are only checking studies that manipulate photoperiod, and studies that Lizzie identified as “in need of checking.”

<><><><><><><><><><><><><><>
What each file does (please update me as needed)
<><><><><><><><><><><><><><>

checkphoto.R - creates check_photo.csv, using code from following items 1 and 2 above

checkphoto.csv - built in checkphoto.R it lists the files to check
This includes the following columns:
	-idstudy: datasetID and experiment number
	-lat: provenance.lat
	-long: provenance.long
	-numtreats_photo: number of different photoperiod treatments
	-daylength_range: range (min-max) of photoperiod treatments, in daylight hrs

checkphoto_info.csv - created by hand by Ailene from checkphoto.csv, same as checkmeout.csv but adds several important columns:
	- who: who should check the study
	- treats.okay: a Y (yes) or N (no) answer. Y means ‘I checked chilling, 		forcing, and photoperiod columns for the study and all are fine’ while N 		means ‘I checked the study and we need to fix it.’ (If N is typed, there 		should be an issue made on github to go along with it.)
	-photo.effect: Y (yes), N (no), or NA answer. Y means ‘I looked at the study 	results and photoperiod or photoperiod x something (chilling/forcing) 		significantly affected the response.’ N means ‘I looked at the study results 	and photoperiod/photoperiod interactions were not significant.’ NA means the 	study did not manipulate photoperiod in any way.
	-photo.effect.desc: If it is easy to quickly mention what photoperiod does  	(e.g., 'longer photo advanced budburst' or 'longer photo reduces forcing’). 	Only do this if it is easy; it is ok to leave this column blank

** PLEASE ALWAYS SAVE THIS FILE (checkmeout_info.csv) AS A CSV **

