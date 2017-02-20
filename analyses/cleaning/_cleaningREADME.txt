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

6. Next! Source clean_woody_sps.R to remove non-woody species (bye bye Fragaria, we will miss you).

7. The we source clean_responsetime.R which cleans response and response time columns.

8. Fix minor mistakes in chilltemp column (with clean_chilltemp.R)

9. Remove duplicate lines (with clean_duplicates.final.R)

10. Write it all out!


<><><><>
Wait! What are these other files?
<><><><>

cleanup_checksmaps.R — Dan Flynn’s old cleaning code that also makes some maps. The checking and the maps are nice, so we keep it here. 

clean_spp_match — code that checks species against a taxa list (using R package Taxonstand), it returns 13 species with possible issues. We should check this (and clean some of them). We should discuss if want this in the script (Taxonstand is a little slow, maybe it should be optional and we should do cleaning in clean_woody_sps.R?)




