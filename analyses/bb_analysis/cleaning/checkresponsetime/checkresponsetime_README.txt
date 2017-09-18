Started 11 September 2017
by Lizzie 

This folder was created after we found several instances where response.time was actually days to bud burst *since start of year* not days to bud burst *since start of experiment.* So we decided to check:

All studies we planned to use for BB analysis that were 
(1) not done on cuttings OR
(2) >60 days response time. 

NOTE: This means we did not clean ALL response.time data, just stuff we planned to use for the BB analysis. You can check out bb_checkrespvar.R to see what that means. 

<><><><><><><><><><><><><><>
What each file does (please update me as needed)
<><><><><><><><><><><><><><>

bb_checkresptime.R - creates checkmeout.csv following items 1 and 2 above

checkmeout.csv - built in bb_checkresptime.R, it lists the files to check

checkmeout_info.csv - created by hand by Lizzie from checkmeout.csv, same as checkmeout.csv but adds two important columns:
- who: who should check the study
- responsetime.okay: a Y (yes) or N (no) answer. Y means ‘I checked the study and it’s fine’ while N means ‘I checked the study and we need to fix it.’
- ifno.fixed: If you answered N in responsetime.okay, have you or someone else pushed code that fixes the problem? Once you do fix it please add an quick explanation of how to this README (see below, we try to categorize the issues so add your dataset to one of the lettered problems if possible or add a new ‘problem’).
** PLEASE ALWAYS SAVE THIS FILE AS A CSV **


<><><><><><><><><><><><><><>
How we fixed things
<><><><><><><><><><><><><><>

Problem A: It was dayofyeartobudburst instead of daystobudburst then:
	(a1) In cleaning/clean.respvar.R: change respvar to dayofyeartobudburst
	(a2) Make the calculation changes in bb_analysis/cleaning/clean_respvarmore.R so that it is daystobudburst and relabel the respvar there to daystobudburst.

Note: If it is percentbudburst, then you just do a2 I think.

Datasets with this change:
gomory15
San-Perez09

Problem B: 