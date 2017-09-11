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
** PLEASE ALWAYS SAVE THIS FILE AS A CSV **