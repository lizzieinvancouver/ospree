## Started 11 Jan 2021 ##
## By Lizzie so far ##

## Plotting interactions for limiting cues paper ##
## Taken from nonlinearities_intxns.R and decsensSimsGrandma.R (utah chill) ##

# See also _dothis_limitingcues.txt #

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/limitingcues") 
} else setwd("~/Documents/git/ospree/analyses/limitingcues")

########################
## Photoperiod figure ##
########################

if(FALSE){
    # old values
    feff <- -8.8/5
    peff <- -4.5/4
    ceff <- -15.8/1248
    fpeff <- peff/20
    fceff <- 9.1/6000 
    pceff <- ceff/2
    fpeff2 <- peff/100
}

feff <- -8.8/5
peff <- -4.5/4
fpeff <- peff/20
fpeff2 <- peff/100

## make up df with budburst based on above 
howlong <- 100
pefflinear <- -1
df <- data.frame("temperature"=seq(from=5, to=20, length.out=howlong), "photo"=seq(from=6, to=24, length.out=howlong))
df$photolinear <- 14+df$photo*pefflinear
df$photohinge <- df$photolinear
df$photohinge[which(df$photo>14)] <- 0
df$force <- df$temperature


# Now estimate bb with no intxns and with intxns
intercept <- 120
df$bbforce <- intercept + feff*df$force
df$bbforcephoto <- intercept + feff*df$force + peff*df$photolinear
df$bbforcephotohinge <- intercept + feff*df$force + peff*df$photohinge
df$bbforcephotoint <- intercept + feff*df$force + peff*df$photolinear + fpeff*(df$force*df$photolinear)
df$bbforcephotohingeint <- intercept + feff*df$force + peff*df$photohinge + fpeff*(df$force*df$photohinge)

df$bbforcephotoint2 <- intercept + feff*df$force + peff*df$photolinear + fpeff2*(df$force*df$photolinear)
df$bbforcephotohingeint2 <- intercept + feff*df$force + peff*df$photohinge + fpeff2*(df$force*df$photohinge)



# ... and PLOT!

library("viridis")
colz <- viridis(4, alpha = 0.1)

usr <- par('usr') # was supposed to help with rectangles, but did not


pdf("figures/intxnsims2021photo.pdf", width=8, height=3)
par(mfrow=c(1,3))
plot(photolinear~photo, data=df, type="l", xlim=c(0,24), xlab="photoperiod", ylab="Photoperiod effect", col="blue")
lines(photohinge~photo, data=df, lty=2, col="red")
rect(0, -11, 6, 10, col=colz[1], border="NA") # xleft, ybottom, xright, ytop

plot(bbforce~temperature, data=df, type="l", ylab="Budburst day")
lines(bbforcephoto~temperature, data=df, col="blue")
lines(bbforcephotoint~temperature, data=df, col="blue", lty=2)
lines(bbforcephotoint2~temperature, data=df, col="blue", lty=2)

plot(bbforce~temperature, data=df, type="l", ylab="Budburst day")
lines(bbforcephotohinge~temperature, data=df, col="red")
lines(bbforcephotohingeint~temperature, data=df, col="red", lty=2)
lines(bbforcephotohingeint2~temperature, data=df, col="red", lty=2)
dev.off()



########################
## Chilling figures ##
########################

# utah chill

# full utah (taken from supp of Ettinger et al. 2020)
pdf("figures/utahcill.pdf", width=5, height=4)
degC <- c(1.39, 1.4, 2.39, 2.4, 9.09, 9.1, 12.39, 12.4, 15.89, 15.9, 17.89, 17.9)
utah <- c(0, 0.5,  0.5, 1, 1,  0.5,  0.5, 0, -0, -0.5, -0.5, -1)
plot(utah~degC, ylab="Chill accumulated", xlab="", col="darkslategray3",
     type="n", xlim=c(-5, 20), ylim=c(-1.5, 1.5)) # cex.lab=1.5, cex.axis=1.5, 
abline(0, 0, col="gray", lty=2, lwd=2)
lines(utah~degC, type="l", ylab="Chill accumulated", xlab="", col="darkslategray3", lwd=3)
dev.off()

feff <- -8.8/5
cefflinear <- (-15.8/1248)*0.8
chillhinge <- -15.8/1248
fceff <- 9.1/6000
fceff2 <- fceff/10

## make up chill-effects plot
# y = a + bx
# -bx = a
whenhinge <- 2000
intercepthere <- -(chillhinge*whenhinge)
whenlinear <-

howlong <- 100
df <- data.frame("temperature"=seq(from=5, to=20, length.out=howlong), "chill"=seq(from=100, to=3000, length.out=howlong))
df$chilllinear <- intercepthere + df$chill*cefflinear
df$chillhinge <- intercepthere + df$chill*chillhinge
df$chillhinge[which(df$chill>2000)] <- 0
df$force <- df$temperature


# RIGHT ... need to think on this!
par(mfrow=c(1,1))
plot(chilllinear~chill, data=df, type="l", xlab="Chilling", ylab="Chill effect", col="blue")
lines(chillhinge~chill, data=df, lty=2, col="red")
rect(0, -11, 6, 10, col=colz[1], border="NA") # xleft, ybottom, xright, ytop

## Have not worked on below!

# Now estimate bb with no intxns and with intxns
intercept <- 120
df$bbforce <- intercept + feff*df$force
df$bbforcechill <- intercept + feff*df$force + peff*df$chilllinear
df$bbforcechillhinge <- intercept + feff*df$force + peff*df$chillhinge
df$bbforcechillint <- intercept + feff*df$force + peff*df$chilllinear + fpeff*(df$force*df$chilllinear)
df$bbforcechillhingeint <- intercept + feff*df$force + peff*df$chillhinge + fpeff*(df$force*df$chillhinge)

df$bbforcechillint2 <- intercept + feff*df$force + peff*df$chilllinear + fpeff2*(df$force*df$chilllinear)
df$bbforcechillhingeint2 <- intercept + feff*df$force + peff*df$chillhinge + fpeff2*(df$force*df$chillhinge)

