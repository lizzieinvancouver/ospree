Trying to understand the chilling folder?
I, your happy README file am here to help.

Everything in this folder is related to calculating total chilling (field chilling plus experimental chilling) in our dataset.
chillmerge_all.R the file that puts it all together, using the below files that are sourced.

Ailene did much of this code, so ask her for clarification if you need it!
aettinger@fas.harvard.edu, ailene.ettinger@gmail.com

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

The files here do the following:
1. cleaning_chilltemp.R: cleans the non-numeric data from the chilltemp column in ospree_clean.csv.

2. cleaning_provlatlong.R: Cleans the provenance.latitude and provenance.longitude columns, to get appropriate location, since in some cases the lat/longs listed in papers were in the middle of the ocean!

3.fieldchillcalc_latlong.R: summarizes the lat/longs needed to pull climate data from europe and north america. These are the “growing” lat/longs if listed in the database, or, if no growing lat/long is given, then they are the provenance lat/long

4. pullclimate.R: pulls in climate data (netcdf files, from an external hard drive) using the provenance lat/long columns and calculates field chilling experienced before the material was collected for the experiment. 

5. interpolclimate.R: Interpolate hourly temperatures from the daily values & chilling using three different metrics. Generates a new csv file with the just the chilling estimates for those studies with field sample dates: fieldchillcalcslatlong.csv

6. totalchillcalc.R code merges the fieldchillcalcslatlong.csv with the ospree_clean.csv file and sums up the field and experimental chilling 

Last step creates a new version of ospree called “ospree_clean_withchill.csv”
that includes these chilling estimates



