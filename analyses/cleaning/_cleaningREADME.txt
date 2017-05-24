Trying to understand the cleaning folder?
I, your happy REAME file am here to help.

So the goal is to centralize as much cleaning as possible in cleanmerge_all.R
To repeat, clean merge_all.R is your go to cleaning file (I hope).

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

It:
1. Reads in ospree.csv, which is the data_detailed tab from data/ospree.xlsx

* Note: Zohner data already merged into ospree.xlsx. See zohner_cleaning/zohnercleaning_README.txt for more about how that happened. 

2. There are some weird columns that get read in. We delete these. Poof… and they are gone!

3. Next, we source the clean_respvar.R code.

4. Next, we source the clean_photo.R code (because that is just how we roll).

5. The we source clean_forcetemp.R

6. Next! Source clean_woody_sps.R to remove non-woody species (bye bye Fragaria, we will miss you). And we clean a few species and genus names. 

Also, here if you want, you can run:
clean_spp_match.R — code that checks species against The Plant List (using R package Taxonstand), it returns species with possible issues via object manchecksp. We should check this off and on, but CLEANING SHOULD occur in clean_woody_sps.R (because Taxonstand is a little slow and thus we don’t need to run it each time).

7. The we source clean_responsetime.R which cleans response and response time columns.

8. Remove duplicate lines (with clean_duplicates.R) — this removes duplicate rows which could have been entered due to data being repeated across figures, two people accidentally entering the same paper or a copy/paste mistake. 

9. Write it all out!


<><><><>
What gets cleaned elsewhere?
<><><><>

**Chilling** Small errors in chilling are cleaned in chilling folder.


<><><><>
Wait! What are these other files?
<><><><>

cleanup_checksmaps.R — Dan Flynn’s old cleaning code that also makes some maps. The checking and the maps are nice, so we keep it here. 

