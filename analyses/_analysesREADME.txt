Info on the folders and files here!
Started by Lizzie on 23 March 2017

<><><><><><>
bb_analysis
<><><><><><>
All files related to bud burst analyses.
This includes CLEANING of the bud burst data and subsetting down to one value of bud burst for data where there were time-series data.

<><><><><><>
bbperc_analysis
<><><><><><>
All files related to percent bud burst analyses.


<><><><><><>
chilling
<><><><><><>
Go chilling! We awesomely used gridded climate data for North America and Europe to estimate chilling. All that work happens here!

<><><><><><>
cleaning
<><><><><><>
This is the MAIN CLEANING folder. It:
	- cleans errors in the data (mis-spellings, conversions done wrong etc.)
	- adds some useful columns (like getting all the detailed response variables into a smaller set)
	- removes non-woody species!
	- cleans taxa names	
	- and more, see the README in that folder. 


<><><><><><>
figures
<><><><><><>
General figures folder. 

<><><><><><>
input
<><><><><><>
Input folder. Everything in here should exist in the data folder also (perhaps in a different format though).

<><><><><><>
output
<><><><><><>
Output folder. Everything in here should be built by a script within the analyses folder somewhere.

<><><><><><>
scratch
<><><><><><>
Working on something or need something for a while but not forever? Put it here. But if you want to keep it, move it out within a few weeks. This folder is trashable. 

<><><><><><>
source
<><><><><><>
If you have code that is mainly just for sourcing (i.e., just a script of f(x)s or such). It goes here. But note that some source code could possibly go within source folders within special folders. 

<><><><><><>
stan
<><><><><><>
Stan models!


<><><><><><>
zarchive
<><><><><><>
Stuff we don’t quite want to part ways with but may never really need to look at again. 


<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
What order do I need to run files to get the data ready to analyze?
<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
If you are, say, analyzing the bud burst data you would run the following files to get a useful datafile ready to go:

(1) cleaning > cleanmerge_all.R

(2) chilling > chillmerge_all.R

(3) bb_analysis > cleaning > bb_cleanmergeall.R

(3) bb_analysis > bb_mergeall.R