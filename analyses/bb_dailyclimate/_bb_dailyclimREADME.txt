Intro to the bb_dailyclimate folder

Everything in this folder is related to pulling and summarizing daily temperature data (tmin, tmax) for bud burst events

Ailene did much of this code, so ask her for clarification if you need it!
aettinger@fas.harvard.edu, ailene.ettinger@gmail.com

Want info on what climate data we used (and why)? See ospree/notes/dailyclimatdata.txt

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

The files here do the following:
1. pulldailyclim.R: pulls climate data for europe and north america, as netcdf files, from an external hard drive, using the provenance lat/long columns. The code pulls 2 years of data for most studies (additional in some cases). It starts with ospree_clean.csv and sources some of the chilling code for cleaning, getting field lat/long, etc.  (see chilling folder for additional details). This file sources the following files that are also in the bb_dailyclimate folder:

1a. pulldailyclimate_eur.R 
1b. pulldailyclimate_nam.R 4. bb_daily_dataprep.R: For bud burst data, creates daily climate data for each bud burst event, using ambient climate data and experimental chilling data, as appropriate, to enable forcing to be calculated. The last step writes out

Last step creates a file called “dailytemp.csv” with daily climate data. 

2. bb_daily_dataprep.R: For bud burst data, creates daily climate data for each bud burst event, using ambient climate data and experimental chilling data, as appropriate, to enable forcing to be calculated. The last step creates a file that contains daily climate data for each bud burst event. This file is then broken into 4 files for sharing on git: percbb_dailyclimA.csv, percbb_dailyclimB.csv, percbb_dailyclimC.csv, and percbb_dailyclimD.csv located in ”output/dailyclim/" 
The 4 files can be rinded together to created the full daily climate file.
WARNING: THE LOOP AT THE END OF THIS CODE IS VERY SLOW!!! 
This file sources the following 3 files that are also in the bb_dailyclimate folder:
2a. bb_daily_dataprep_format_bbdat: formats the ospree phenology datafile so that it can be connected to the climate data
2b. bb_daily_dataprep_format_climdat: formats the ambient daily climate data (pulled in pulldailyclim.R) 
2c. bb_daily_dataprep_get_expclimdat: experimental chilling and photoperiod conditions.

