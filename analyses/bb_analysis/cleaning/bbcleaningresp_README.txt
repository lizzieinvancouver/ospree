Started 14 March 2018
by Lizzie 

Notes on the response variable for BB analysis
This file focuses on which missing data are true NAs and which are non-budburst treatments. 
See also checkresponsetime/checkresponsetime_README.txt

If you ever want to use the non-budburst data, remember! We also set a minimum %budburst threshold for data (see clean_bbperctodays.R) and so we would need to go back and incorporate those tossed data too, in order to gather all non-budburst cases.

<><><><><><><><><><><><><><><><><><><><><><><><>
Response value issues: Recoded to 999
<><><><><><><><><><><><><><><><><><><><><><><><>
caffarra11b (4 NA): These are twigs that never burst bud (0% budburst).

gianfagna85 (11 NA): These are from fig 2 and are labelled there as >30 days

Heide03 (3 NA): These are also buds that never burst ("Buds that failed to burst after 60 days were recorded as flushing after 60 days for the purpose of the statistical analysis").

heide93a (6 NA): These are listed as >50 days in the original paper (I believe).

laube14a (22 NA): These look like twigs that never burst bud (0% budburst). 

nienstaedt66 (26 NA): I think these are mostly ones that did not reach 50% budburst (though the authors' wording is weird ... "graphical determination doubtful as 50% level was poorly fixed") and see above. 

spiers74 (10 NA): Twigs that never reach 10% burst bud (0% budburst). 

webb78 (2 NA): These look like twigs that never burst bud (0% budburst).

worrall67 (1/3 NA): From the Fig 4-5 caption: “Where there was no effect of photoperiod the 8 hour points were omitted for clarity, as is the point at 15 days (these plants did not leaf out).” Cat and Lizzie went through these together and decided that 14 d did not leafout  

zohner16 (5/130 NA): There are 135 Zohner NA's in our OSPREE. In the original data Zohner sent, there are 130 NA's and 5 entries he coded as NL. Here is what he says in the supplement:"NL indicates that no leaf-out occurred under 8-h (NL in black) or 16-h (NL in grey) day length. Some species leafed out before the last cutting date (C3), which is indicated by missing bars for the C3 treatment".So my (Dan B.) take away is that the NA's should be excluded and NL's should be included as didn't leafout in the time of the experiment (and see above). 
NA's that should be kept in Zohner:Amelanchier laevis x2 
Stachyurus sinensis 
Viburnum buddleifolium 
Viburnum plicatumall others can be removed.


<><><><><><><><><><><><><><><><><><><><><><><><>
Response value issues: true NA values
<><><><><><><><><><><><><><><><><><><><><><><><>
These data are naturally deleted in doing analyses 
so we do not delete them here
<><><><><><><><><><><><><><><><><><><><><><><><>
basler12 (16 NA): These are true NAs -- they did not sample the species at some elevations. We should delete these data.

boyer (1 NA): This is a --- in the original data. Again, I think we can toss it.

gianfagna85: I think we lose this study due to photoperiod issues, so I did not check it (as of 9 Apr 2018, after other cleaning, this study no longer shows up).

nienstaedt66 (26 NA): The data from the 0 chill treatments were NOT DONE for all but P. pungens (so there WAS a 0 chill for P. pungens but not for the other species).

skuterud94 (4 NA): RIGHT-censored data: I believe they are the opposite problem of no leafout. They leafed out too soon. The authors write "material from the storage at 6 C was excluded from March 4 because some of the shoots had started to leaf out."

sonsteby14 (18 NA): Cat and Lizzie looked at these and think they are true zeros, but the paper is hard to decipher on this point.

zohner16 (130/135 NA): There are 135 Zohner NA's in our OSPREE. In the original data Zohner sent, there are 130 NA's and see below.

worrall67 (2/3 NA): From the Fig 4-5 caption: “Where there was no effect of photoperiod the 8 hour points were omitted for clarity, as is the point at 15 days (these plants did not leaf out).” Cat and Lizzie went through these together and decided that the 0 chill ones are impossible to decipher. One more (see above) we recoded as a non-leafout.



