Started 28 February 2017

Additional metadata on ospree.xlsx

These metadata only apply to ospree.xlsx … more cleaning happens in R. 
See ~/analyses/cleaning/_cleaningREADME.txt for starters.

<><><><><><><><><><><><><><>
Extra notes on some papers
<><><><><><><><><><><><><><>

Falusi 1990 (see also github issue #8)

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



Falusi 1996 (see also github issue #9)

Exp 1 (which is the only exp from this paper missing a year): No year given (maybe it's 1988? -- just guessing based on year progression of next two experiments -- but we should check)
Exp 2: Study starts in 1989
Exp 3: Study starts in fall 1990

**So we changed this as follows**

exp 1 1988, exp 2 1990 (if started in 89, measured in 90) and exp 3 1991


Swartz1981
actual paper measure time to % bud burst in Growing Degree Hours. It seems this was converted to days to % budburst when input into our datasheet in ospree.xlsx.
**Actually, I think that it has been converted from growing degree hours to %budburst in the original figure, to growing degree days in our data sheet…it seems values from the figure have simply been divided by 24.***
***We checked this as a group by re-extracting these data. Problematic because highly variable based on scale of capture. We think, our data was converted from growing degree hours to growing degree days by dividing figure values by 24.