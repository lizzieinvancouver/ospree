Trying to understand the cleaning folder?
I, your happy REAME file am here to help.

So the goal is to centralize as much cleaning as possible in cleanmerge_all.R
To repeat, clean merge_all.R is your go to cleaning file (I hope).

<><><><><><><><><><><><><><>
Please update me as you go! 
<><><><><><><><><><><><><><>

It:
1. Reads in ospree.csv, which should be the data_detailed tab from data/ospree.xlsx

2. There are some weird columns that get read in. We delete these. 

* Zohner data already merged in! We just need to decide what to do with remaining code.

3. Next, we source the clean_respvar code.

4. Then, with that cleaned, we source the clean_photo code code.

5. The we source clean_forcetemp

6. source clean_woody_sps.R to remove non-woody species

7. The we source clean_responsetime.R which cleans response and response time coulumns.

8. Fix minor mistakes in chilltemp column

9. Remove duplicate lines

10. Write it all out!


<><><><>
Look at before retreat if possible … 
<><><><>

cleanup_files — Dan Flynn’s old cleaning code, we should look at this again

clean_spp_match old code that checked species against some taxa list, starts to do some name cleaning … probably should update this.


