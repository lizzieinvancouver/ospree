## Started 12 August 2019 ##
## By Lizzie, with help from Ailene ##
## Trying to flesh out how non-linearities via intxns work ##

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
pceff <- -0.3/6000 # not sure how to convert this!

fpeff.alt <- 0.01
pceff.alt <- 0.001 

## make up df
df <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=seq(from=6, to=24, length.out=100),
    chill=seq(from=100, to=2000, length.out=100))
df$bb.simple <- feff*df$force + peff*df$photo + ceff*df$chill
df$bb <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff*(df$chill*df$photo)
df$bb.alt <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff.alt*(df$chill*df$photo)

# add some alternatives to above
df$bb.fpaltonly <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt*(df$force*df$photo)
chillhere <- 1000
photohere <- 14
df$bb.staticchillphoto <- feff*df$force + peff*photohere + ceff*chillhere + fpeff*(df$force*photohere) + fceff*(df$force*chillhere) +
    pceff*(chillhere*photohere)


plot(bb~photo, data=df, type="l", ylim=c(-50, 40))
lines(bb.simple~photo, data=df, col="orange")
lines(bb.fpaltonly~photo, data=df, col="red")
lines(bb.alt~photo, data=df, col="deeppink")
lines(bb.staticchillphoto~photo, data=df, col="grey")

plot(bb.staticchillphoto~photo, data=df)


colz <- c("orange", "deeppink", "darkred")

pdf("limitingcues/figures/intxnsims.pdf", width=7.5, height=5)
plot(bb~force, data=df, type="l", col=colz[1])
lines(bb.fpaltonly~force, data=df, col=colz[2])
lines(bb.simple~force, data=df, col=colz[3])
legend(26, -16, c("all intxns", "F x P alt", "no intxn"), 
   lty=rep(1, 3), col=colz, cex=0.75, bty="n")
dev.off()

## another try at df
feff <- -8.8/5
peff <- -4.5/4
fpeff.alt <- abs(feff/16) # make it 1/16 the size of the F effect

# increase photoperiod at warming > 20
df1 <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=c(seq(from=14, to=12, length.out=40), rep(12, 60)),
    chill=seq(from=2000, to=2000, length.out=100))
df1$bb <- feff*df1$force + peff*df1$photo + ceff*df1$chill + fpeff.alt*(df1$force*df1$photo)
df1$bblin <- feff*df1$force + peff*df1$photo + ceff*df1$chill 

plot(bb~force, data=df1, ylim=c(-100, -30))
points(bblin~force, data=df1, col="blue")

# small increases in photoperiod across whole time
df2 <- data.frame("force"=seq(from=5, to=35, length.out=100), photo=seq(from=14, to=12.5, length.out=100),
    chill=seq(from=2000, to=2000, length.out=100))
df2$bb <- feff*df2$force + peff*df2$photo + ceff*df2$chill + fpeff.alt*(df2$force*df2$photo)
df2$bblin <- feff*df2$force + peff*df2$photo + ceff*df2$chill 

plot(bb~force, data=df2, ylim=c(-60, -30))
points(bblin~force, data=df2, col="blue")


## Work by Ailene to look at magnitude of interactive effect ...
fpeff.alt <- c(-0.1,-.01,  0,  0.01, .1)
pceff.alt <- pceff

## make up df
df <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=seq(from=6, to=24, length.out=100),
    chill=seq(from=100, to=2000, length.out=100))
df$bb.simple <- feff*df$force + peff*df$photo + ceff*df$chill
df$bb <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff*(df$chill*df$photo)
#Plot the model with interactions and the simple model
pdf("limitingcues/figures/intxnsims_allintsAE.pdf", width=7.5, height=5)
#quartz()
colz <- c("orange", "deeppink", "darkred")

plot(bb~force, data=df, type="l", col=colz[1], ylim=c(-120,70), xlim=c(0,40), main="changing f*p, with all interactions")
lines(bb.simple~force, data=df, col=colz[3])
text(df$force[length(df$force)]+1,df$bb[length(df$bb)], labels="bb",col=colz[1], cex=.5)
text(df$force[length(df$force)]+1,df$bb.simple[length(df$bb.simple)], labels="bb.simple",col=colz[3], cex=.5)

#look at effect of changing the sign and magnitude of fp interaction only (keept pceff constant)
ncols<-dim(df)[2]

for(i in 1:length(fpeff.alt)){
  
newcol<- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt[i]*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff.alt*(df$chill*df$photo)
df<-cbind(df,newcol)
colnames(df)[ncols+i]<-paste("bb.alt",fpeff.alt[i], sep="_")
lines(newcol~df$force, col=colz[2])
text(df$force[length(df$force)]+1,newcol[length(newcol)], labels=paste("f*p=",fpeff.alt[i]),col=colz[2], cex=.5)

}
dev.off()
# now look at effect of changing fp with no other interactions
pdf("limitingcues/figures/intxnsims_onlyfpAE.pdf", width=7.5, height=5)
fpeff.alt <- c(-0.1,-.01,  0, 0.01,.05,.07, .1, .2)

#quartz()
plot(bb~force, data=df, type="l", col=colz[1], ylim=c(-170,60), xlim=c(0,40), main="changing f*p, with only f*p")
lines(bb.simple~force, data=df, col=colz[3])
text(df$force[length(df$force)]+1,df$bb[length(df$bb)], labels="bb",col=colz[1], cex=.5)
text(df$force[length(df$force)]+1,df$bb.simple[length(df$bb.simple)], labels="bb.simple",col=colz[3], cex=.5)
ncols<-dim(df)[2]

for(i in 1:length(fpeff.alt)){
  newcol<- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt[i]*(df$force*df$photo)
  df<-cbind(df,newcol)
  colnames(df)[ncols+i]<-paste("bb.altonly",fpeff.alt[i], sep="_")
  lines(newcol~df$force, col=colz[2])
  text(df$force[length(df$force)]+1,newcol[length(newcol)], labels=paste("fp=",fpeff.alt[i]),col=colz[2], cex=.5)
}

dev.off()


