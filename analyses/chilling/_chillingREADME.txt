Trying to understand the chilling folder?
I, your happy README file am here to help.

Everything in this folder is related to calculating total chilling (field chilling plus experimental chilling) in our dataset.
chillmerge_all.R the file that puts it all together, using the below files that are sourced.

Ailene did much of this code, so ask her for clarification if you need it!

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

The files here do the following:
1. cleaning_chilltemp.R: cleans the non-numeric data from the chilltemp column in ospree_clean.csv, and creates a new version of ospree with a cleaned chill temp column called “ospree_clean_cc.csv”

2. fieldchillcalc_latlong.R: Starts with ospree_clean_XXX.csv, pulls in climate data (from an external hard drive) using the provenance lat/long columns and calculates field chilling experienced before the material was collected for the experiment. Generates a new csv file with the just the chilling estimates for those studies with field sample dates: fieldchillcalcslatlong.csv

3. totalchillcalc.R code merges the fieldchillcalcslatlong.csv with the ospree_clean_XXX.csv file and sums up the field and experimental chilling to create a new version of ospree called “ospree_clean_withchill.csv”
