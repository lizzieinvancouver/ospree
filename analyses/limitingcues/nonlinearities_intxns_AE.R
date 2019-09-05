## Started 12 August 2019 ##
## By Lizzie ##
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
plot(bb~force, data=df, type="l", col=colz[1], ylim=c(-120,70), xlim=c(0,40), main="changing f*p, with all interactions")
colz <- c("orange", "deeppink", "darkred")
lines(bb.simple~force, data=df, col=colz[3])
text(df$force[length(df$force)]+1,df$bb[length(df$bb)], labels="bb",col=colz[1], cex=.5)
text(df$force[length(df$force)]+1,df$bb.simple[length(df$bb.simple)], labels="bb.simple",col=colz[3], cex=.5)

#look at effect of changing the sign and magnitude of fp interaction only (keept pceff constant)

for(i in 1:length(fpeff.alt)){
  
newcol<- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt[i]*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff.alt*(df$chill*df$photo)
ncols<-dim(df)[2]
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
colz <- c("orange", "deeppink", "darkred")
lines(bb.simple~force, data=df, col=colz[3])
text(df$force[length(df$force)]+1,df$bb[length(df$bb)], labels="bb",col=colz[1], cex=.5)
text(df$force[length(df$force)]+1,df$bb.simple[length(df$bb.simple)], labels="bb.simple",col=colz[3], cex=.5)

for(i in 1:length(fpeff.alt)){
  newcol<- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt[i]*(df$force*df$photo)
  ncols<-dim(df)[2]
  
  df<-cbind(df,newcol)
  colnames(df)[12+i]<-paste("bb.altonly",fpeff.alt[i], sep="_")
  lines(newcol~df$force, col=colz[2])
  text(df$force[length(df$force)]+1,newcol[length(newcol)], labels=paste("fp=",fpeff.alt[i]),col=colz[2], cex=.5)
}

dev.off()

