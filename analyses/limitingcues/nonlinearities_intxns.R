## Started 12 August 2019 ##
## By Lizzie, with help from Ailene ##

## Trying to flesh out how non-linearities via intxns work ##
## IMPT Note: the effects (fpeff etc.) are overwritten throughout the code, so check which you're using!

## TO DO ##
# Think on what set of cue shifts we want (all change at once, only one shifts, two shift at once ...) ##

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else setwd("~/Documents/git/ospree/analyses")

# Get some values from Flynn & Wolovich
# estimate chilling (utah) of exp
hfchill <- 2062.50-814.5 # 1248
shchill <- 1847.50-599.50 # double-check ...
feff <- -8.8/5
peff <- -4.5/4
ceff <- -15.8/1248
fpeff <- -0.6/20 # not sure how to convert this! 
fceff <- 9.1/6000 # not sure how to convert this! 
pceff <- -0.3/6000 # not sure how to convert this! (super small)

fpeff.alt <- 0.01 # smaller than above
pceff.alt <- 0.001 # bigger than above

## make up df where all 3 cues increase ... 
df <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=seq(from=6, to=24, length.out=100),
    chill=seq(from=100, to=2000, length.out=100))
# Now estimate bb with no intxns and with intxns
df$bb.simple <- feff*df$force + peff*df$photo + ceff*df$chill
df$bb <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff*(df$force*df$photo) +
    fceff*(df$force*df$chill) + pceff*(df$chill*df$photo)
    
df$bb.alt <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt*(df$force*df$photo) +
    fceff*(df$force*df$chill) + pceff.alt*(df$chill*df$photo)
    
# add some alternatives to above
df$bb.fpaltonly <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt*(df$force*df$photo)
chillhere <- 1000
photohere <- 14
df$bb.staticchillphoto <- feff*df$force + peff*photohere + ceff*chillhere + fpeff*(df$force*photohere) + 
    fceff*(df$force*chillhere) + pceff*(chillhere*photohere)
    
plot(bb~photo, data=df, type="l", ylim=c(-80, 40))
lines(bb.simple~photo, data=df, col="orange")
lines(bb.fpaltonly~photo, data=df, col="red")
lines(bb.alt~photo, data=df, col="deeppink")
lines(bb.staticchillphoto~photo, data=df, col="grey")

colz <- c("orange", "deeppink", "darkred")

pdf("limitingcues/figures/intxnsims.pdf", width=7.5, height=5)
plot(bb~force, data=df, type="l", col=colz[1])
lines(bb.fpaltonly~force, data=df, col=colz[2])
lines(bb.simple~force, data=df, col=colz[3])
legend(26, -16, c("all intxns", "F x P alt", "no intxn"), 
   lty=rep(1, 3), col=colz, cex=0.75, bty="n")
dev.off()

## another version of df
feff <- -8.8/5
peff <- -4.5/4
fpeff.alt <- abs(feff/16) # make it 1/16 the size of the F effect

# increase photoperiod at warming > 20
df1 <- data.frame("force"=seq(from=5, to=30, length.out=100),
    photo=c(seq(from=14, to=12, length.out=40), rep(12, 60)),
    chill=seq(from=2000, to=2000, length.out=100))
df1$bb <- feff*df1$force + peff*df1$photo + ceff*df1$chill + fpeff.alt*(df1$force*df1$photo)
df1$bblin <- feff*df1$force + peff*df1$photo + ceff*df1$chill 

plot(bb~force, data=df1, ylim=c(-100, -30))
points(bblin~force, data=df1, col="blue")


##
## Trying to make 8-panel figure....

if(FALSE){
# Round off for good measure 
feff <- -10/5
peff <- -2.5/4 # making this smaller, since we think it is
ceff <- -16/1250
fpeff <- -(feff/50) # estimates above put relative size at 59
fceff <- -(peff/50) # estimates above put relative size at 21
pceff <- 0 # for simplicity
fpeff.alt <- c(feff/10, feff/50, feff/500,  0, -(feff/500), -(feff/50) -(feff/10))
}

source("limitingcues/source/intxnplots.R")

if(FALSE){ # if you need to trouble shoot the intxnplotme f(x)s
df <- dfcpcon
intxnsvector <- fpeff.alt
ylim <- c(-120,70)
xlim <- c(0,40)
xcol <- "force"
xlab <- "Forcing"
ylab <- "Budburst day"
maintext <- "Changing FxP: F increases, C, P are constant"
cexmain <- 0.75
}

fpeff.alt <- c(-0.1,-0.01,  0,  0.01, 0.1)

## make up df
dfcpcon <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=rep(12, 100),
    chill=rep(2000,100))
dfccon <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=seq(from=14, to=6, length.out=100),
    chill=rep(2000,100))
dfpcon <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=rep(12, 100),
    chill=seq(from=2000, to=500, length.out=100))
dfallchange <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=seq(from=14, to=6, length.out=100),
    chill=seq(from=2000, to=500, length.out=100))

dfthreshpc <- data.frame("forcereal"=c(seq(from=5, to=20, length.out=40), rep(20, 60)), photo=rep(12, 100),
    chill=rep(2000,100), "force"=seq(from=5, to=30, length.out=100))
dfpthreshccon <- data.frame("forcereal"=seq(from=5, to=30, length.out=100), photo=c(seq(from=14, to=12, length.out=40), rep(12, 60)),
    chill=rep(2000,100), "force"=seq(from=5, to=30, length.out=100))
dfpconcthresh <- data.frame("forcereal"=seq(from=5, to=30, length.out=100), photo=rep(12, 100),
    chill=c(seq(from=2000, to=1000, length.out=40), rep(1000, 60)),
    "force"=seq(from=5, to=30, length.out=100))

pdf("limitingcues/figures/intxnsims_changingFP.pdf", width=9, height=6)
par(mfrow=c(2,3))
par(mar=c(3,5,2,2)) 
colz <- c("orange", "deeppink", "darkred")
intxnplotmefp(dfcpcon, fpeff.alt, c(-120,70), c(0,40), "force", "Forcing", "Budburst day",
      "Changing FxP: F increases, C, P are constant", 0.75)
intxnplotmefp(dfccon, fpeff.alt, c(-120,70), c(0,40),  "force","Forcing", "Budburst day",
      "Changing FxP: F increases, P decreases, C constant", 0.75)
intxnplotmefp(dfpcon, fpeff.alt, c(-120,70), c(0,40), "force", "Forcing","Budburst day",
      "Changing FxP: F increases, P constant, C decreases", 0.75)
intxnplotmefp(dfallchange, fpeff.alt, c(-120,70), c(0,40), "force", "Forcing","Budburst day",
     "Changing FxP: F increases, P and C both decrease", 0.75)
# intxnplotmefp(dfthreshpc, fpeff.alt, c(-120,70), c(0,40),"force", "Forcing","Budburst day",
 #   "Changing FxP: F threshold", 0.75)
intxnplotmefp(dfpthreshccon, fpeff.alt, c(-120,70), c(0, 40),  "force", "Forcing","Budburst day",
    "Changing FxP: F increases, P threshold", 0.75)
intxnplotmefp(dfpconcthresh, fpeff.alt, c(-120,70), c(0,40), "force",  "Forcing", "Budburst day",
    "Changing FxP: F increases, C threshold", 0.75)
dev.off()

fceff.alt <- c(-0.005, -0.01, -0.001, 0, 0.001, 0.01, 0.005)

pdf("limitingcues/figures/intxnsims_changingFC.pdf", width=7.5, height=6)
par(mfrow=c(2,2))
par(mar=c(3,5,2,2)) 
colz <- c("orange", "deeppink", "darkred")
intxnplotmefc(dfcpcon, fceff.alt, c(-300, 300), c(0,40),"force", "Forcing", "Budburst day",
    "Changing FxC: F increases, C, P are constant", 0.75)
intxnplotmefc(dfccon, fceff.alt, c(-300, 300), c(0,40),"force", "Forcing", "Budburst day",
    "Changing FxC: F increases, P decreases, C constant", 0.75)
intxnplotmefc(dfpcon, fceff.alt, c(-300, 300),  c(0,40),"force", "Forcing","Budburst day",
     "Changing FxC: F increases, P constant, C decreases", 0.75)
intxnplotmefc(dfallchange, fceff.alt, c(-300, 300), c(0,40),"force", "Forcing","Budburst day",
    "Changing FxC: F increases, P and C both decrease", 0.75)
dev.off()




###
## Work by Ailene to look at magnitude of interactive effect ...
## 
fpeff.alt <- c(-0.1,-0.01,  0,  0.01, 0.1)
pceff.alt <- pceff

## make up df
df <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=seq(from=6, to=24, length.out=100),
    chill=seq(from=100, to=2000, length.out=100))
df$bb.nointxn <- feff*df$force + peff*df$photo + ceff*df$chill
df$bb <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff*(df$chill*df$photo)
#Plot the model with interactions and the simple model
pdf("limitingcues/figures/intxnsims_allintsAE.pdf", width=7.5, height=5)
#quartz()
colz <- c("orange", "deeppink", "darkred")

plot(bb~force, data=df, type="l", col=colz[1], ylim=c(-120,70), xlim=c(0,40),
    main="changing f*p, with all interactions: C decreases, F and P increase", cex.main=0.7)
lines(bb.nointxn~force, data=df, col=colz[3])
text(df$force[length(df$force)]+1,df$bb[length(df$bb)], labels="bb",col=colz[1], cex=0.5)
text(df$force[length(df$force)]+1,df$bb.nointxn[length(df$bb.nointxn)], labels="bb.nointxn",col=colz[3], cex=0.5)

#look at effect of changing the sign and magnitude of fp interaction only (keep pceff constant)
ncols<-dim(df)[2]

for(i in 1:length(fpeff.alt)){
  
newcol<- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt[i]*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff.alt*(df$chill*df$photo)
df<-cbind(df,newcol)
colnames(df)[ncols+i]<-paste("bb.alt",fpeff.alt[i], sep="_")
lines(newcol~df$force, col=colz[2])
text(df$force[length(df$force)]+1,newcol[length(newcol)], labels=paste("f*p=",fpeff.alt[i]),col=colz[2], cex=0.5)

}
dev.off()

# now look at effect of changing fp with no other interactions
pdf("limitingcues/figures/intxnsims_onlyfpAE.pdf", width=7.5, height=5)
fpeff.alt <- c(-0.1,-.01,  0, 0.01,.05,.07, .1, .2)

#quartz()
plot(bb~force, data=df, type="l", col=colz[1], ylim=c(-170,60), xlim=c(0,40),
    main="changing f*p, with only f*p: C decreases, F and P increase", cex.main=0.7)
lines(bb.nointxn~force, data=df, col=colz[3])
text(df$force[length(df$force)]+1,df$bb[length(df$bb)], labels="bb",col=colz[1], cex=0.5)
text(df$force[length(df$force)]+1,df$bb.nointxn[length(df$bb.nointxn)], labels="bb.nointxn",col=colz[3], cex=0.5)
ncols<-dim(df)[2]

for(i in 1:length(fpeff.alt)){
  newcol<- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt[i]*(df$force*df$photo)
  df<-cbind(df,newcol)
  colnames(df)[ncols+i]<-paste("bb.altonly",fpeff.alt[i], sep="_")
  lines(newcol~df$force, col=colz[2])
  text(df$force[length(df$force)]+1,newcol[length(newcol)], labels=paste("fp=",fpeff.alt[i]),col=colz[2], cex=0.5)
}

dev.off()


