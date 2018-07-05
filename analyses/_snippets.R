## Potentially useful code snippets ##

## Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

## some quick subsetting for Isabelle
goo <- read.csv("~/Documents/git/projects/treegarden/budreview/ospree/analyses/output/ospree_clean_withchill_BB.csv")
goosm <- subset(goo, forcetemp!="" & forcetemp_night!="")

goosm$ft <- as.numeric(goosm$forcetemp)-as.numeric(goosm$forcetemp_night)
subby <- subset(goosm, ft>0)
hist(subby$ft)
unique(subby$datasetID)

min(subby$ft)
max(subby$ft)

## check that the respvar and respvar.simple make sense
goober1 <- subset(d, select=c("respvar", "respvar.simple"))
goober2 <- goober1[!duplicated(goober1), ]
goober2[order(goober2$respvar.simple),]

## some quick subsetting for Frederik and Yann (WSL)
stud <- read.csv("~/Documents/git/projects/treegarden/budreview/ospree/analyses/output/studytype_table.csv")
stud.ch <- subset(stud, chill>2)
unique(stud.ch$datasetID)

brr <- subset(goo, is.na(chilltemp)==FALSE & chilltemp!="" & chilltemp<0)
unique(brr$datasetID)
