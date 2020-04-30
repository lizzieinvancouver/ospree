Started 1 April 2020

** This file comes from checking up the 2019 data addition in depth over a month in April 2020; the R files in this folder are from various groups running code to check the data, make plots, run Stan models etc. It's not all the files we wrote, check for the git commit where Lizzie deleted some on 29 April 2020 *


Started a week before 1 Apr 2020 to cross-check our newly entered data (new papers we entered in summer 2019 through fall 2019) for whether it makes sense, things to watch out for (it's all crops or the experiment seems odd) and whether it was all entered correctly.

datacheck_assign.csv is a list of those new papers (by datasetID) with people assigned in groups to review them.

Updated
21 Apr 2020

New groups assigned (see datacheck_assign.csv, round 2), and each group has two SEPARATE groupings of data to review. Please review the set under round 2 for your team and separately analyze set 2.

Columns to fill in:
other.figs.toenter -- when you compare it to the paper, does all relevant data look to be entered from all figures?	
alldata.entered	-- when you compare it to the paper, does all relevant data look to be entered? That is, do the row numbers seem about right for what you see?
dataconverts.correctly	-- compare the data across cleaning files and see if things look about right (see sample datacheckup_emw.R)
other.notes -- any other notes

See also issues:
# 355 Check up for the new data
# 354 Fix malyshev
# 358 fix fu18
# 359-364 ... more paper issues!
# 373 fix fu18 further
# 375 checking malyshev18 dates


Updated
29 Apr 2020

Closing out the main issue #355 

I deleted all the image files and a couple R code files that did not look particularly unique, then I got sad and stopped deleting further. If you want these files back check commit b3aa7418291b317b58b4ae6427e3ac403715a22c on 29 April 2020 (5:40pm Vancouver time)