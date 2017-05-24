Started 22 March 2017
by Cat 
Follow-up by Nacho (11 April 2017)

################ Clarification notes from Cat
Additional metadata for bb_analysis/cleaning

For converting thermal time to days to budburst, was able to convert heide93 Figure 3 & 4 and
karlsson03 Figure 1. 

The degree days equation they used was > 0 degC
The equation for heide93 is simply…
daystobudburst = degree days / force temp 

The degree days equation they used was > 2 degC
The equation for karlsson03 is…
daystobudburst = degree days / (force temp - 2)
#################

The goal of this folder is to cleaning the response variable BB and centralize in 
bb_cleanmergeall.R

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

It:
1. Reads in output/ospree_clean_withchill.csv, which should have been cleaned previously

2. sources clean_thermaltimetodays.R, which homogenizes time to days 

3. sources clean_ambientforcing.R, which cleans the forcing predictor variable

4. sources clean_bbperctodays.R, which transforms %bb to days

5. sources clean_moreduplicates.R which removes some data with same response variable 

6. saves the file for BB analyses


<><><><><><><><><><><><><><>
Consider: 
<><><><><><><><><><><><><><>
For our analyses, we should think about how to handle the following columns: 
	varetc
	population (etc.) 
	and related variables 
