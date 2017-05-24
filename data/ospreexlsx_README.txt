Started 28 February 2017

Additional metadata on ospree.xlsx

These metadata only apply to ospree.xlsx … more cleaning happens in R. 
See ~/analyses/cleaning/_cleaningREADME.txt for starters.

<><><><><><><><><><><><><><>
Extra notes on some papers
<><><><><><><><><><><><><><>

<><><> Falusi03 and Falusi97 <><><>

Varies by material (apical, median, small or basal bud) — currently in clean_duplicates.R we delete down to one observation per treatment.

<><> Granhus et al. 2009 <><>

This paper is on potted seedlings and how warm spells during some dormancy-related period impact budburst. During chilling pots were exposed to ONE (1) 14 day warm periods (8C or 12C). Ailene and Lizzie decided 14 days were included in chill days 0-175 (if you look at figure 1 they are trying to show you a 14 day spell happening at different periods in each treatment).

Note that this MUST BE CONSIDERED if ever calculating *cumulative forcing.*


<><> Falusi 1990 (see also github issue #8) <><>

Year is not given (seriously).
Here's what we do know:

paper submitted in Dec 1989
experiment starts in autumn and lasts 235 days (almost 8 months)
this brings me to thinking the earliest that they started it was fall 1988.

I did email to try to find Falusi’s email address:

-------- Forwarded Message --------
Subject: 	Seeking current email address for Mauro Falusi
Date: 	Tue, 5 Jul 2016 17:58:47 -0400
From: 	Elizabeth Wolkovich <lizzie@oeb.harvard.edu>
To: 	Luisa.Ghelardini@vbsg.slu.se


Dear Dr. Ghelardini,

Apologies for the bother of this email. I am working on a meta-analysis 
of published studies on woody species phenology in growth chambers and 
to answer a few queries am trying to track down contact information for 
Mauro Falusi. I know you have published with Dr. Falusi before and was 
wondering if you might have an email address for him?

Many thanks for any information.

'Best,

Lizzie

-------- End Forwarded Message --------

But with no luck. 

**So we changed the year to 1988.**



<><> Falusi 1996 (see also github issue #9) <><>

Exp 1 (which is the only exp from this paper missing a year): No year given (maybe it's 1988? -- just guessing based on year progression of next two experiments -- but we should check)
Exp 2: Study starts in 1989
Exp 3: Study starts in fall 1990

**So we changed this as follows**


exp 1 1988, exp 2 1990 (if started in 89, measured in 90) and exp 3 1991

Some coding notes from Ailene if you want to check any of this:
ospree<-read.csv("input/ospree.csv", header=TRUE)
falusi<-ospree[ospree$datasetID=="falusi96",]
table(falusi$study,falusi$year)
falusi2<-ospree[ospree$datasetID=="falusi90",]
table(falusi2$study,falusi2$year)
dim(falusi2)


<><> Swartz1981 <><>

actual paper measure time to % bud burst in Growing Degree Hours. It seems this was converted to days to % budburst when input into our datasheet in ospree.xlsx.
**Actually, I think that it has been converted from growing degree hours to %budburst in the original figure, to growing degree days in our data sheet…it seems values from the figure have simply been divided by 24.***
***We checked this as a group by re-extracting these data. Problematic because highly variable based on scale of capture. We think, our data was converted from growing degree hours to growing degree days by dividing figure values by 24.

2 May 2017 - CC
*** Swartz81 - uses Utah model:
“Effective chilling hours (units) in refrigeration or field, were calculated by the method of Richardson et al (1974) [Utah chill unit model]"
* The figure x axis says base is 4.5 degC but caption clarifies they used 5 degC refrigerators. However, both 4.5 degC and 5 degC are considered 1 chill unit under the Utah model.
* Utah Chill Unit Model - 5 degC/hr = 1 chill unit
* Response.time = Growing Degree Hours/24
* No need to change!


<><> Charrier11 <><>
*** Charrier11 - uses MTB (Mean Time until Budburst) which is calculated as number of hours at 25 degC
* TS entered data and divided by 24 for Figure 2 entries - which is correct to get days to budburst
* Table 1 entries are just taken directly from table, and do not need to be changed
