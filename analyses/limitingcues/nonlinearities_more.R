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
## Some general stuff ##
########################

library("viridis")
colz <- viridis(4, alpha = 0.1)
interceptbb <- 120

if(FALSE){
    # old values from when I started this file
    feff <- -8.8/5
    peff <- -4.5/4
    ceff <- -15.8/1248
    fpeff <- peff/20
    fceff <- 9.1/6000 
    pceff <- ceff/2
    fpeff2 <- peff/100
}

########################
## Photoperiod figures ##
########################
feff <- -8.8/5
peff <- -4.5/4
fpeff <- peff/20
fpeff2 <- peff/100

##
## My first attempt 
## Here I made the 'effects' panel separate from the budburst panels
## Note that for photoperiod I also added a little 'no effect' rectangle, but that's a small issue
##

## make up df with budburst based on above 
howlong <- 100
pefflinear <- -1
df <- data.frame("temperature"=seq(from=5, to=20, length.out=howlong), "photo"=seq(from=6, to=24, length.out=howlong))
df$photolinear <- 14+df$photo*pefflinear
df$photohinge <- df$photolinear
df$photohinge[which(df$photo>14)] <- 0
df$force <- df$temperature

# Now estimate bb with no intxns and with intxns

df$bbforce <- interceptbb + feff*df$force
df$bbforcephoto <- interceptbb + feff*df$force + peff*df$photolinear
df$bbforcephotohinge <- interceptbb + feff*df$force + peff*df$photohinge
df$bbforcephotoint <- interceptbb + feff*df$force + peff*df$photolinear + fpeff*(df$force*df$photolinear)
df$bbforcephotohingeint <- interceptbb + feff*df$force + peff*df$photohinge + fpeff*(df$force*df$photohinge)
df$bbforcephotoint2 <- interceptbb + feff*df$force + peff*df$photolinear + fpeff2*(df$force*df$photolinear)
df$bbforcephotohingeint2 <- interceptbb + feff*df$force + peff*df$photohinge + fpeff2*(df$force*df$photohinge)


# ... and PLOT!
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


##
## My second attempt 
## Here I made the 'effects' and budburst panels from the same underlying data
##
howlong <- 100
plineff <- -4.5/4
phingeff <- (-4.5/4)*0.8
whenhingep <- 14
interceptphinge <- -(phingeff*whenhingep)
whenlinearp <- 24
interceptplinear <- -(plineff*whenlinearp)

photcoef <- -0.1

df <- data.frame("temperature"=seq(from=5, to=20, length.out=howlong), "photo"=seq(from=6, to=24, length.out=howlong))
df$photolinear <- interceptphinge + df$photo*plineff
df$photohinge <- interceptphinge + df$photo*phingeff
df$photohinge[which(df$photo>whenhingep)] <- 0
df$force <- df$temperature

# Now estimate bb with no intxns and with intxns

df$bbforce <- interceptbb + feff*df$force
df$bbforcephoto <- interceptbb + feff*df$force + photcoef*df$photolinear
df$bbforcephotohinge <- interceptbb + feff*df$force + photcoef*df$photohinge
df$bbforcephotoint <- interceptbb + feff*df$force + photcoef*df$photolinear + fpeff*(df$force*df$photolinear)
df$bbforcephotohingeint <- interceptbb + feff*df$force + photcoef*df$photohinge + fpeff*(df$force*df$photohinge)
df$bbforcephotoint2 <- interceptbb + feff*df$force + photcoef*df$photolinear + fpeff2*(df$force*df$photolinear)
df$bbforcephotohingeint2 <- interceptbb + feff*df$force + photcoef*df$photohinge + fpeff2*(df$force*df$photohinge)


# ... and PLOT!
pdf("figures/intxnsims2021photoalt.pdf", width=8, height=3)
par(mfrow=c(1,3))
plot(photolinear~photo, data=df, type="l", xlim=c(0,24), xlab="photoperiod", ylab="Photoperiod effect", col="blue")
lines(photohinge~photo, data=df, lty=2, col="red")

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

# utah chill figure

# full utah (values taken from supp of Ettinger et al. 2020)
pdf("figures/utahcill.pdf", width=5, height=4)
degC <- c(1.39, 1.4, 2.39, 2.4, 9.09, 9.1, 12.39, 12.4, 15.89, 15.9, 17.89, 17.9)
utah <- c(0, 0.5,  0.5, 1, 1,  0.5,  0.5, 0, -0, -0.5, -0.5, -1)
plot(utah~degC, ylab="Chill accumulated", xlab="", col="darkslategray3",
     type="n", xlim=c(-5, 20), ylim=c(-1.5, 1.5)) # cex.lab=1.5, cex.axis=1.5, 
abline(0, 0, col="gray", lty=2, lwd=2)
lines(utah~degC, type="l", ylab="Chill accumulated", xlab="", col="darkslategray3", lwd=3)
dev.off()

feff <- -8.8/5
clineff <- (-15.8/1248)*0.8 
chingeff <- -15.8/1248
fceff <- 9.1/200
fceff2 <- fceff/10
chillcoef <- -0.2

## make up chill-effects plot
# Some basic math I used to get the lines to cross zero at the right spot
# y = a + bx
# -bx = a
whenhingec <- 2000
intercepcthinge <- -(chingeff*whenhingec)
whenclinear <- 3000
interceptclinear <- -(clineff*whenclinear)

howlong <- 100
df <- data.frame("temperature"=seq(from=5, to=20, length.out=howlong), "chill"=seq(from=100, to=3000, length.out=howlong))
df$chilllinear <- intercepcthinge + clineff*df$chill # replace intercepcthinge with interceptclinear if you want it to end at 0 (but I liked this version better)
df$chillhinge <- intercepcthinge + chingeff*df$chill
df$chillhinge[which(df$chill>2000)] <- 0
df$force <- df$temperature

par(mfrow=c(1,1))
plot(chilllinear~chill, data=df, type="l", xlab="Chilling", ylab="Chill effect", col="blue")
lines(chillhinge~chill, data=df, lty=2, col="red")

# Now estimate bb with no intxns and with intxns
# Build ... temperature-only, then with chill, then with two different sized intxn terms
df$bbforce <- interceptbb + feff*df$force
df$bbforcechill <- interceptbb + feff*df$force + chillcoef*df$chilllinear
df$bbforcechillhinge <- interceptbb + feff*df$force + chillcoef*df$chillhinge
df$bbforcechillint <- interceptbb + feff*df$force + chillcoef*df$chilllinear + fceff*(df$force*df$chilllinear)
df$bbforcechillhingeint <- interceptbb + feff*df$force + chillcoef*df$chillhinge + fceff*(df$force*df$chillhinge)
df$bbforcechillint2 <- interceptbb + feff*df$force + chillcoef*df$chilllinear + fceff2*(df$force*df$chilllinear)
df$bbforcechillhingeint2 <- interceptbb + feff*df$force + chillcoef*df$chillhinge + fceff2*(df$force*df$chillhinge)

pdf("figures/intxnsims2021chill.pdf", width=8, height=3)
par(mfrow=c(1,3))
plot(chilllinear~chill, data=df, type="l", xlab="Chilling", ylab="Chilling effect", col="blue")
lines(chillhinge~chill, data=df, lty=2, col="red")
rect(0, -11, 6, 10, col=colz[1], border="NA") # xleft, ybottom, xright, ytop

plot(bbforce~temperature, data=df, type="l", ylab="Budburst day")
lines(bbforcechill~temperature, data=df, col="blue")
lines(bbforcechillint~temperature, data=df, col="blue", lty=2)
lines(bbforcechillint2~temperature, data=df, col="blue", lty=2)

plot(bbforce~temperature, data=df, type="l", ylab="Budburst day")
lines(bbforcechillhinge~temperature, data=df, col="red")
lines(bbforcechillhingeint~temperature, data=df, col="red", lty=2)
lines(bbforcechillhingeint2~temperature, data=df, col="red", lty=2)
dev.off()
