## Potentially useful code snippets ##

## Just for dealing with data issues in Oct 2018 (delete later)

## To look at models before we imputed so much photoperiod ...

# CHANGE bbdataplease
d<-read.csv("Oct2018compare/olderBBdata/ospree_clean_withchill_BB8Jun2018.csv", header=TRUE)
# CHANGE stan plotting (this is just before m1.bb <- m2l.winsp) 
load("stan/output/Oct2018_check/M1_daysBBwinter_2level_8Jun2018.Rda")


## Plotting cues against each other ...

# plotlet from D Flynn
plotlet <- function(x, y, xlab=NULL, ylab=NULL, data, xtimes, ytimes, groups = NULL, ...){
  if(is.null(xlab)) xlab = x; if(is.null(ylab)) ylab = y
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
    else {
      colz = c("brown", "blue3")
      ccolz = rep(colz[1], length(groups))
      ccolz[groups == 2] = colz[2]
      col.pch = ccolz
      col.lines = alpha(ccolz, 0.4)
    }
  
  plot(
  data[grep(paste(x,"\\[",sep=""), rownames(data)),1]*xtimes,
  data[grep(paste(y,"\\[",sep=""), rownames(data)),1]*ytimes,
  pch = "+",
  ylab = ylab,
  xlab = xlab,
  col = col.pch,
  ...
  )

  abline(h=0, lty = 3, col = "grey60")
  abline(v=0, lty = 3, col = "grey60")
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"]*xtimes,
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"25%"]*ytimes,
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"]*xtimes,
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"75%"]*ytimes,
    length = 0, col = col.lines)
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"25%"]*xtimes,
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"]*ytimes,
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"75%"]*xtimes,
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"]*ytimes,
    length = 0, col = col.lines)
  
  # match with species names
  text( data[grep(paste(x,"\\[",sep=""), rownames(data)),1]*xtimes,
        data[grep(paste(y,"\\[",sep=""), rownames(data)),1]*ytimes,
        sort(unique(bb.stan$complex.wname)), # ALERT! This is data species, we should change!!
        cex = 0.5, 
        pos = 3,
        col = col.pch)
}

plotlet( "b_photo", "b_force",
         ylab = "Advance due to 5° warming (OSPREE)", 
         xlab = "Advance due to 4 hr longer photoperiod (OSPREE)", 
         ylim = c(-27, 0.5),
         xlim = c(-16, 0.5),
         #  xaxt="n", 
         xtimes=4,
         ytimes=5,
         group = NULL,
         data = sumer.wi)

plotlet( "b_photo", "b_force",
         ylab = "Advance due to 5° warming (OSPREE)", 
         xlab = "Advance due to 4 hr longer photoperiod (OSPREE)", 
         ylim = c(-27, 5),
         xlim = c(-16, 5),
         #  xaxt="n", 
         xtimes=4,
         ytimes=5,
         group = NULL,
         data = sumer.wi)

## Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/")

## some quick subsetting for Isabelle
goo <- read.csv("output/ospree_clean_withchill_BB.csv")
goosm <- subset(goo, forcetemp!="" & forcetemp_night!="")

goosm$ft <- as.numeric(goosm$forcetemp)-as.numeric(goosm$forcetemp_night)
subby <- subset(goosm, ft>0)
hist(subby$ft)
unique(subby$datasetID)

min(subby$ft)
max(subby$ft)

## check that the respvar and respvar.simple make sense
goober1 <- subset(goosm, select=c("respvar", "respvar.simple"))
goober2 <- goober1[!duplicated(goober1), ]
goober2[order(goober2$respvar.simple),]

## some quick subsetting for Frederik and Yann (WSL)
stud <- read.csv("output/studytype_table.csv")
stud.ch <- subset(stud, chill>2)
unique(stud.ch$datasetID)

brr <- subset(goo, is.na(chilltemp)==FALSE & chilltemp!="" & chilltemp<0)
unique(brr$datasetID)

## some quick subsetting for Rob and Shannon
d <- read.csv("output/ospree_clean.csv")

# check what we're about to do:
unique(as.numeric(d$chilltemp))
d$chilltemp <- as.numeric(d$chilltemp)
morebrr <- subset(d, chilltemp<0)
unique(morebrr$datasetID)

# And for looking at performance!
table(d$respvar.simple)
growthdat <- subset(d, respvar.simple=="growth")
table(growthdat$respvar)
lookseegrowth <- subset(growthdat, select=c("datasetID", "study", "genus", "species", "respvar"))
lookseegrowth.nodup <- lookseegrowth[!duplicated(lookseegrowth), ]
table(lookseegrowth.nodup$datasetID) # 20 studies have some response data
