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

fpeff.alt <- 0.01
pceff.alt <- 0.001 

# make up df
df <- data.frame("force"=seq(from=5, to=30, length.out=100), photo=seq(from=6, to=24, length.out=100),
    chill=seq(from=100, to=2000, length.out=100))
df$bb.simple <- feff*df$force + peff*df$photo + ceff*df$chill
df$bb <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff*(df$chill*df$photo)
df$bb.alt <- feff*df$force + peff*df$photo + ceff*df$chill + fpeff.alt*(df$force*df$photo) + fceff*(df$force*df$chill) +
    pceff.alt*(df$chill*df$photo)

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
