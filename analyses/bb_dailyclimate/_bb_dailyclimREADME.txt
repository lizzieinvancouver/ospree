Intro to the bb_dailyclimate folder

Everything in this folder is related to pulling and summarizing daily temperature data (tmin, tmax) for bud burst events

Ailene did much of this code, so ask her for clarification if you need it!
aettinger@fas.harvard.edu, ailene.ettinger@gmail.com

Want info on what climate data we used (and why)? See ospree/notes/dailyclimatdata.txt

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

The files here do the following:
1. bb_daily_dataprep.R: For bud burst data, creates daily climate data for each bud burst event, using ambient climate data and experimental chilling data, as appropriate, to enable forcing to be calculated

2. pulldailyclim.R: pulls daily climate data from North America and Europe. relies on chilling code for cleaning, getting field lat/long, etc. starts with ospree_clean.csv (but should this be ospree_cleanwithchill.csv?!? (see chilling folder for additional details).

3. pulldailyclimate_eur & pulldailyclimate_nam.R: pulls in climate data for europe and north america, respectively, as netcdf files, from an external hard drive, using the provenance lat/long columns. The code pulls 2 years of data for most studies (additional in some cases). If you do not have the external hard drive, or do not want to take the time to pull the data (it can take ~5-10 minutes) then you can get around this by loading the RData file listed in chillmerge_all

Last step creates a a file called “dailytemp.csv” with daily climate data. 
