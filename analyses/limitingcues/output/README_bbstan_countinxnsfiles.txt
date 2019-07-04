Started 3 July 2019
By Lizzie

Some info on:
- bbstan_allsppmodel_countinxns.csv
- bbstan_mainmodel_countinxns.csv
These are created in limitingcues/countinxns/countintxns.R

And are designed to count up treatment info for the budburst manuscript. 

If a different treat is given in treat1 and treat2 then the resulting number is the total number of studies (datasetID x study) that tested for an INTERACTION of those cues. 

Here's what the rest mean...

total-datasetIDstudies -- total studies considered
total-exp-chill --- manipulated chilling directly
total-fielddate-chill --- used multiple field sample dates 
NOTE: a single study could ONLY be assigned to exp chill or field chill.... not both, even though I think one had both (but it ended up in exp chill)

total-varied-forcing --- total studies that varied forcing
total-wsometreats-constant-forcing -- included at least one treatment with constant temperatures across 24 hours
total-variedwsometreats-varydaynight-forcing -- included at least one treatment with thermoperiodicity
NOTE: studies often had treatments with a constant daily temp and a temp with varying daynight temps so these two numbers can add up to greater than the studies that varied forcing

total-exp-chill-fpintxns --- is all studies that varied chill experimentally *and* also varied forcing or photoperiod
total-fielddate-chill-fpintxns--- is all studies that varied field sample dates *and* also varied forcing or photoperiod

