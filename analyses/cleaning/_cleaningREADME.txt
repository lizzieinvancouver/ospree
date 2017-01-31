Trying to understand the cleaning folder?
I, your happy REAME file am here to help.

So the goal is to centralize as much cleaning as possible in cleanmerge_all.R
To repeat, clean merge_all.R is your go to cleaning file (I hope).

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

It:
1. Reads in ospree.csv, which should be the data_detailed tab from data/ospree.xlsx

2. There are some weird issues we should deal with. Can we delete these columns? If so, delete them here. 

* Zohner data already merged in! We just need to decide what to do with remaining code.

3. Next, we source the clean_respvar code.

4. Then, with that cleaned, we source the clean_photo code code.

5-7. We should eventually then add in the next three cleaning reps.

8. Once 1 is functional we can write this out. Call it what you like but we must delete or archive the other random output files that we no longer need!


<><><><>
Look at before retreat if possible … 
<><><><>

cleanup_files — Dan Flynn’s old cleaning code, we should look at this again

clean_spp_match old code that checked species against some taxa list, starts to do some name cleaning … probably should update this.


