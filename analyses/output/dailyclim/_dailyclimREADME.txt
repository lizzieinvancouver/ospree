Welcome to the dailyclim folder

This folder contains daily climate data for the bud burst analysis.

Ailene did much of this code, so ask her for clarification if you need it!
aettinger@fas.harvard.edu, ailene.ettinger@gmail.com

Want info on what climate data we used (and why)? See ospree/notes/dailyclimatdata.txt

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

This folder contains the following files:
1. dailytemp.csv: daily Tmin and Tmax for each provenance lat/long for European and North American sites that experienced field chilling. For most studies this file includes two years of daily Tmin and Tmax, including the year before the experiment was conducted and the year after the experiment started. (This allows for chilling and forcing to be calculated.) For some studies there are more than 2 years of data.

2. daily_expchill.csv: experimental chilling temperatures, in a daily format, and with columns for the experimental day length and the last date of experimental chilling. This is used to get the start date for forcing.

3.fieldclimate_daily.RData

4. The following  files contain daily climate data for each individual bud burst event, to allow forcing to  be estimated for each event. Too much data to be contained in a single csv on github so it is divided into 4 that can then be rbinded together:
	percbb_dailyclimA.csv
	percbb_dailyclimB.csv
	percbb_dailyclimC.csv
	percbb_dailyclimD.csv
	
5._archive: contains files related to pmp analysis, which we are no longer using/attempting.