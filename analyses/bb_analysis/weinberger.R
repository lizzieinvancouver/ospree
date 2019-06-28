# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
# libraries
library(shinystan)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)
library("ggpubr")
library(ggstance)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

figpath <- "figures"

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else if (length(grep("ailene", getwd()))>0) {setwd("~/Documents/GitHub/ospree/analyses/bb_analysis")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/bb_analysis") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/bb_analysis") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

######################################
# Flags to choose for bbstanleadin.R #
######################################

# Master flags! Here you pick if you want the flags for the main model (figure in main text) versus the all spp model (supp)
use.flags.for.mainmodel <- TRUE
use.flags.for.allsppmodel <- FALSE
use.yourown.flagdesign <- FALSE

if(use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.yourown.flagdesign |
   use.yourown.flagdesign  & use.flags.for.allsppmodel | use.flags.for.mainmodel==TRUE & use.flags.for.allsppmodel
   & use.yourown.flagdesign) print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp =FALSE # for the main model this is false
  use.multcuespp = FALSE
  use.cropspp = FALSE
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.allsppmodel){
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
  use.zscore = TRUE # change to false to use raw predictors
  
  # Default is species complex and no crops
  use.allspp = FALSE
  use.multcuespp = FALSE
  use.cropspp = FALSE
  
  # Default is species complex use  alltypes of designs
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  
  #Default is all chilling data
  use.expchillonly = FALSE # change to true for only experimental chilling 
  #note: with only exp chilling, there is only exp photo and force too.
  #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
}
source("source/bbstanleadin.R")
source("..//misc/getfielddates.R") # f(x) counts up field sample dates separated by a number of days you specify
source("..//misc/getcuesbystudy_fxs.R")

#d is bb.noNA, use this to find the filed weiberger studies
dat<-d
dat$latbi <- paste(dat$genus, dat$species)
dat$fieldsample.date <- as.Date(dat$fieldsample.date, format="%d-%b-%Y")

# Turn things back to numeric so they can be counted
dat$forcetemp <- as.numeric(dat$forcetemp)
dat$forcetemp_night <- as.numeric(dat$forcetemp_night)
dat$photoperiod_night <- as.numeric(dat$photoperiod_night)
dat$photoperiod_day <- as.numeric(dat$photoperiod_day)
dat$chilltemp <- as.numeric(dat$chilltemp)
dat$datasetIDstudy <- paste(dat$datasetID, dat$study)




### These are the fuctions Lizzie made in getfielddates.R and getcuesbystudy_fxs.R
ddatefx.all <- subset(dat, select=c("datasetID", "study", "fieldsample.date"))
ddatefx <- ddatefx.all[!duplicated(ddatefx.all), ]
ddatefx$datasetIDstudy <- paste(ddatefx$datasetID, ddatefx$study)

dates2weeks <- countfieldsample(ddatefx, 14)

# ... and next for each treatment
ddatefxtreat.all <- subset(dat, select=c("datasetID", "study", "fieldsample.date", "force", "photoperiod_day", "chilltemp"))
ddatefxtreat <- ddatefxtreat.all[!duplicated(ddatefxtreat.all), ]
ddatefxtreat$datasetIDstudy <- paste(ddatefxtreat$datasetID, ddatefxtreat$study, ddatefxtreat$force,
                                     ddatefxtreat$photoperiod_day, ddatefxtreat$chilltemp)

dates2weekstreat <- countfieldsample(ddatefxtreat, 14)
names(dates2weekstreat)[names(dates2weekstreat)=="count"] <- "fs.date.count"

weinberger<-filter(dates2weekstreat,fs.date.count>=2)##this selects studies that have more than 2 sample dates 2 weeks apart
#clean it a little bit
weinberger$datasetID<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 1)
weinberger$study<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 2)
weinberger$force<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 3)
weinberger$photo<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 4)
weinberger$chill<-sapply(strsplit(weinberger$datasetIDstudy, " "), "[", 5)

#### This is a list of weinberger stduies
weinstuds<-as.vector(paste(weinberger$datasetID, weinberger$study))
weinstuds<-unique(weinstuds)




####make a new column in bb.stan
bb.stan$dataIDstudyID<-paste(bb.stan$datasetID, bb.stan$study)

##add the weinberger infrmation
bb.stan$weinberger<-ifelse(bb.stan$dataIDstudyID %in% weinstuds,1,0)

###check waht species are weinberger
wein.sp<-filter(bb.stan,weinberger==1)
unique(wein.sp$complex.wname) 
not.wein<-filter(bb.stan,weinberger==0)
unique(not.wein$complex.wname)

unique(wein.sp$dataIDstudyID) ## how many weinberger experiments
unique(not.wein$dataIDstudyID) ## howmany non weinberger
unique(bb.stan$dataIDstudyID) ##42 total experiments

sp.match<-intersect(unique(wein.sp$complex.wname), unique(not.wein$complex.wname))
sp.match ### species in both

######################
# now we exclude non-weinberger studies that do NOT manipulate chilling

nonwein.expchill <- subset(not.wein, chill_type!="fldest")
unique(nonwein.expchill$dataIDstudyID) ## this is number of experiments that manipulate chilling but not weinberger (16)

count(unique(not.wein$dataIDstudyID)) ##all non weinbergerstudes
count(unique(nonwein.expchill$dataIDstudyID)) ### studies that manipulate chilling with out weinberger
25-16
unique(wein.sp$chill_type) # 11 rows incl falusi97, caffarra11b, heide93
studiestoincl <- c(unique(nonwein.expchill$dataIDstudyID), unique(wein.sp$dataIDstudyID))


##### a data set
bb.stan.alt <- bb.stan[which(bb.stan$dataIDstudyID %in% unique(studiestoincl)),] ##data 1 without feild estiamtes

# or, or .. we include ONLY non-weinberger studies with exp chilling only
nonwein.expchillonly <- subset(not.wein, chill_type=="exp")
studiestoincl.exponly <- c(unique(nonwein.expchillonly$dataIDstudyID), unique(wein.sp$dataIDstudyID))

bb.stan.alt.exponly <- bb.stan[which(bb.stan$dataIDstudyID %in% unique(studiestoincl.exponly)),]
bb.stan.alt.exponly.wein<-filter(bb.stan.alt.exponly,weinberger==1)
unique(bb.stan.alt.exponly.wein$complex.wname)
bb.stan.alt.exponly.nowein<-filter(bb.stan.alt.exponly,weinberger==0)
unique(bb.stan.alt.exponly.nowein$complex.wname)

###match species with wein and non within the subset "bb.stan.alt"
unique(bb.stan.alt$complex.wname)
bb.stan.alt.wein<-filter(bb.stan.alt,weinberger==1)
unique(bb.stan.alt.wein$complex.wname)
bb.stan.alt.nowein<-filter(bb.stan.alt,weinberger==0)
unique(bb.stan.alt.nowein$complex.wname)
sp.match.bb.alt<-intersect(unique(bb.stan.alt.wein$complex.wname), unique(bb.stan.alt.nowein$complex.wname)) ###species that overlap wein and nonwein from bb.stan.alt
sp.match.bb.alt ### only 11 sp

sp.match.bb.alt.exponly<-intersect(unique(bb.stan.alt.exponly.wein$complex.wname), unique(bb.stan.alt.exponly.nowein$complex.wname)) ### species that overlap wein and non wein for bb.stan,alt.exp.only
sp.match.bb.alt.exponly##only 6 species

# now we exclude try to use matching species
# rm fldest
bb.stan.alt.matchsp <- bb.stan.alt[which(bb.stan.alt$complex.wname %in% sp.match.bb.alt),] ###if you want to exclude non-weinberger studesi that dont manipulate chilling
bb.stan.alt.exponly.matchsp<- bb.stan.alt.exponly[which(bb.stan.alt.exponly$complex.wname %in% sp.match.bb.alt.exponly),] ###if want only exp non weinbergers
#bb.stan.matchsp <- bb.stan[which(bb.stan$complex.wname %in% sp.match),] # if you want to incl fldest

#make complexes numeric for stan
bb.stan.alt.matchsp$complex <- as.numeric(as.factor(bb.stan.alt.matchsp$complex.wname))
bb.stan.alt.exponly.matchsp$complex <- as.numeric(as.factor(bb.stan.alt.exponly.matchsp$complex.wname))

##how lare are each stat set
nrow(bb.stan.alt.matchsp) ###889
nrow(bb.stan.alt.exponly.matchsp) ###639
nrow(bb.stan.alt) ##1529


## Set up the bb.stan to use
#bb.stan <- bb.stan.alt
#bb.stan <- bb.stan.alt.exponly
bb.stan <- bb.stan.alt.matchsp # this is target dataset weinberger and non weinberger but only species that appear in both
#bb.stan<-bb.stan.matchsp
#bb.stan<-bb.stan.alt.exponly.matchsp

source("source/bb_zscorepreds.R")### need to re zscore things on new data set

######################
####make datalist
#wein.data.chillports <- with(bb.stan, 
 #                            list(y=resp, 
  #                                chill = chill.ports.z, 
   #                               force = force.z, 
    #                              photo = photo.z,
     #                             weinberger= weinberger,
      #0                            sp = complex,
        #                          N = nrow(bb.stan),
         #                         n_sp = length(unique(bb.stan$complex))
          #                   )
#)

if (use.chillports = FALSE){
  wein.data.utah <- with(bb.stan, 
                       list(y=resp, 
                            chill = chill.z, 
                            force = force.z, 
                            photo = photo.z,
                            weinberger= weinberger,
                            sp = complex,
                            N = nrow(bb.stan),
                            n_sp = length(unique(bb.stan$complex))
                       )
)
}


if (use.chillports == TRUE){
  wein.data.utah <- with(bb.stan, 
                         list(y=resp, 
                              chill = chill.ports.z, 
                              force = force.z, 
                              photo = photo.z,
                              weinberger= weinberger,
                              sp = complex,
                              N = nrow(bb.stan),
                              n_sp = length(unique(bb.stan$complex))
                         )
  )
}

###model
#m2l.ni = stan('stan/weinbergerint.stan', data = wein.data,
# iter = 2500, warmup=1500)

#wein.mod.2 = stan('stan/weinberger_fewint.stan', data = wein.data.chillports,
#            iter = 2500, warmup=1500)
#summary

#wein.mod.3.cp = stan('stan/wein_intpoolonly.stan', data = wein.data.chillports,
 #                    iter = 2500, warmup=1500)

##this is the model for the manuscript
wein.mod.3.ut = stan('stan/wein_intpoolonly.stan', data = wein.data.utah,
iter = 2500, warmup=1500)





###some weinberger plotss of raw data
#chill.wein<-ggplot(bb.stan,aes(chill.z,resp, color=complex.wname,shape=as.factor(weinberger)))+geom_point()+geom_smooth(method='lm',se=FALSE,fullrange=TRUE, aes(linetype=as.factor(weinberger)))+theme_bw() 
#force.wein<-ggplot(bb.stan,aes(force.z,resp, color=complex.wname,shape=as.factor(weinberger)))+geom_point()+geom_smooth(method='lm',se=FALSE,fullrange=TRUE, aes(linetype=as.factor(weinberger)))+theme_bw() 
#photo.wein<-ggplot(bb.stan,aes(photo.z,resp, color=complex.wname,shape=as.factor(weinberger)))+geom_point()+geom_smooth(method='lm',se=FALSE,fullrange=TRUE, aes(linetype=as.factor(weinberger)))+theme_bw()
#chilly<-ggplot(bb.stan,aes(as.factor(weinberger),chill))+geom_boxplot()+theme_bw()
#forcey<-ggplot(bb.stan,aes(as.factor(weinberger),force))+geom_boxplot()+theme_bw()
#photoy<-ggplot(bb.stan,aes(as.factor(weinberger),photo))+geom_boxplot()+theme_bw()
#ggarrange(chill.wein,force.wein,photo.wein,chilly,forcey,photoy, ncol=3,nrow=2, common.legend = TRUE, legend="right")
#wein.force<-ggplot(bb.stan,aes(force,resp, color=as.factor(weinberger)))+geom_point()+geom_smooth(method='lm',fullrange=TRUE)+ggthemes::theme_base()
#wein.photo<-ggplot(bb.stan,aes(photo,resp, color=as.factor(weinberger)))+geom_point()+geom_smooth(method='lm',fullrange=TRUE)+ggthemes::theme_base()

##### r-square models
observed.here <- bb.stan$resp

#wein.sum <- summary(wein.mod.2)$summary
#wein.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
#          "b_weinberger", "b_cw","b_pw","b_fw"),]

wein.sum3.ut <- summary(wein.mod.3.ut)$summary 
matchysp.ut<-rownames_to_column(as.data.frame(wein.sum3.ut[c("mu_a_sp", "b_force", "b_photo", "b_chill",
                                                             "b_weinberger", "b_cw","b_pw","b_fw"),]),"predictor") ## this is out put for manuscript
wein.sum3.cp <- summary(wein.mod.3.cp)$summary
matchysp.cp<-rownames_to_column(as.data.frame(wein.sum3.cp[c("mu_a_sp", "b_force", "b_photo", "b_chill",
                                                             "b_weinberger", "b_cw","b_pw","b_fw"),]),"predictor")
matchysp.cp$chilltype<-"chillportios"
whichmodel <- wein.sum3.ut
###model plots
spp <- sort(unique(bb.stan$complex))
n <- length(spp)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colv = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

jpeg("../figures/weinberger4supp.jpeg")
par(mfrow=c(1,3))
par(mar=c(4,4, 1,1))
plot(resp~chill.z, data=bb.stan, type="n",ylab="days to budburst")
for (sp in c(1:length(spp))){
  subby <- subset(bb.stan, complex==spp[sp])
  points(resp~chill.z, data=subby, main=subby$complex.wname[1], col=colv[sp],pch=ifelse(subby$weinberger==1,2,3))
  #intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
  #slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
  #abline(intercepthere, slopehere, col=colv[sp])
}
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
       whichmodel[grep("b_chill", rownames(whichmodel)),1], col="black", lwd=2)
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1]-whichmodel[(grep("b_weinberger", rownames(whichmodel))),1],
       whichmodel[grep("b_chill", rownames(whichmodel)),1]+whichmodel[grep("b_cw", rownames(whichmodel)),1], col="blue", lwd=2)

plot(resp~force.z, data=bb.stan, type="n",ylab="")
for (sp in c(1:length(spp))){
  subby <- subset(bb.stan, complex==spp[sp])
  points(resp~force.z, data=subby, main=subby$complex.wname[1], col=colv[sp],pch=ifelse(subby$weinberger==1,2,3))
  #intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
  #slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
  #abline(intercepthere, slopehere, col=colv[sp])
}
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
       whichmodel[grep("b_force", rownames(whichmodel)),1], col="black", lwd=2)
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1]-whichmodel[(grep("b_weinberger", rownames(whichmodel))),1],
       whichmodel[grep("b_force", rownames(whichmodel)),1]+whichmodel[grep("b_fw", rownames(whichmodel)),1], col="blue", lwd=2)
plot(resp~photo.z, data=bb.stan, type="n",ylab="")
for (sp in c(1:length(spp))){
  subby <- subset(bb.stan, complex==spp[sp])
  points(resp~photo.z, data=subby, main=subby$complex.wname[1], col=colv[sp],pch=ifelse(subby$weinberger==1,2,3))
  #intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
  #slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
  #abline(intercepthere, slopehere, col=colv[sp])
}
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
       whichmodel[grep("b_photo", rownames(whichmodel)),1], col="black", lwd=2)
abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1]-whichmodel[(grep("b_weinberger", rownames(whichmodel))),1],
       whichmodel[grep("b_photo", rownames(whichmodel)),1]+whichmodel[grep("b_pw", rownames(whichmodel)),1], col="blue", lwd=2)
legend("topright", legend=c("not weinberger", "weinberger"),
       col=c("black", "blue"),pch=c(3,2), lty=1:1, cex=0.8,bty=1,
       box.lty=1,xpd = NA)
dev.off()



#####mu plot
whichy<-rownames_to_column(as.data.frame(whichmodel), var = "predictor")

whichy<-filter(whichy,between(row_number(),1,21))
whichy<-filter(whichy,!between(row_number(),5,6))

colnames(whichy)<-c("predictor", "mean"  ,    "se_mean" ,  "sd"  ,      "Q2.5"      ,"Q25"    ,   "Q50" ,      "Q75"  ,     "Q97.5",     "n_eff",     "Rhat" )
pd2=position_dodgev(height=0.4)
ggplot(whichy,aes(mean,predictor))+geom_point(aes(),position=pd2)+geom_errorbarh(aes(xmin=Q25,xmax=Q75),linetype="solid",position=pd2,width=0)+geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),linetype="dotted",position=pd2,width=0)+geom_vline(aes(xintercept=0),color="black")+theme_bw()
+scale_color_manual(values=c("orchid4", "springgreen4"))+ggtitle("HF-Continuous")

##or
jpeg("../figures/weinberger_MU_4supp.jpeg")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 12, name = "Paired"), 4)
# display.brewer.all()
my.pch <- rep(15:18, each=3)
alphahere = 0.4

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
  spnum <- length(unique(bb.stan$complex.wname))
  #pdf(file.path(figpath, paste("muplot", nameforfig, figpathmore, ".pdf", sep="")),
   #   width = width, height = height)
  par(xpd=FALSE)
  par(mar=c(5,7,3,5))
  plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
       xlab="Model estimate change in days to BB", ylab="", main=nameforfig)
  axis(2, at=1:8, labels=rev(c("mu_a_sp","b_force", "b_photo", "b_chill","b_weinberger","b_fw","b_pw","b_cw")), las=1)
  abline(v=0, lty=2, col="darkgrey")
  rownameshere <- c("mu_a_sp","b_force", "b_photo", "b_chill","b_weinberger","b_fw","b_pw","b_cw")
  ppeffects <- c("mu_a_sp","b_force", "b_photo", "b_chill","b_weinberger","b_fw","b_pw","b_cw") # or 1:4 here...
  for(i in 1:8){
    pos.y<-(8:1)[i]
    pos.x<-summary(modelhere)$summary[rownameshere[i],"mean"]
    lines(summary(modelhere)$summary[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
    points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
    for(spsi in 1:spnum){
      pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(modelhere)$summary),fixed=TRUE))[1:1]
      jitt<-runif(1,0.05,0.4)
      pos.y.sps.i<-pos.y-jitt
      pos.x.sps.i<-summary(modelhere)$summary[pos.sps.i[i],"mean"]
      lines(summary(modelhere)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[spsi], alphahere))
      points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
      
    }
  }
  par(xpd=TRUE) # so I can plot legend outside
  legend(leg1, leg2, sort(unique(bb.stan$complex.wname)), pch=my.pch[1:spnum],
         col=alpha(my.pal[1:spnum], alphahere),
         cex=.75, bty="n",y.intersp=0.75)
 # dev.off()
}

muplotfx(wein.mod.3.ut, "Weinberger", 7, 8, c(0,8), c(-14, 50) , 27, 2.5)
dev.off()



wein.sum3.ut <- summary(wein.mod.3.ut)$summary
matchysp.ut<-rownames_to_column(as.data.frame(wein.sum3.ut[c("mu_a_sp", "b_force", "b_photo", "b_chill",
                                                             "b_weinberger", "b_cw","b_pw","b_fw"),]),"predictor")
matchysp.ut$chilltype<-"Eutaw"
matchysps<-rbind(matchysp.cp,matchysp.ut)

#write.csv(matchysps,"poolintonly.csv")

# pooling on main effects
preds.wein.sum <- wein.sum[grep("yhat", rownames(wein.sum)),]
wein.sum.R2 <- 1- sum((observed.here-preds.wein.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
wein.mod.R2 <- 1- sum((observed.here-preds.wein.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
summary(lm(preds.wein.sum[,1]~observed.here)) # Multiple R-squared:  

##pooling on just intercept 
preds.wein.sum3 <- wein.sum3.ut[grep("yhat", rownames(wein.sum3.ut)),]
wein.sum.3.R2 <- 1- sum((observed.here-preds.wein.sum3[,1])^2)/sum((observed.here-mean(observed.here))^2)
wein.mod.3.R2 <- 1- sum((observed.here-preds.wein.sum3[,1])^2)/sum((observed.here-mean(observed.here))^2)
summary(lm(preds.wein.sum3[,1]~observed.here)) # Multiple R-squared: 

### the pooling on main effects model seems better, since its results run counter to our prediction
##### Code below mdified from models_stan_plotting_pp.R

#1 compare wein.sum (pooling) to wein.sum2 (pooling only on intercept)
spp <- sort(unique(bb.stan$complex))


with.pool <- data.frame(complex=rep(NA, length(spp)),
                        intercept=rep(NA, length(spp)), 
                        force=rep(NA, length(spp)), 
                        photo=rep(NA, length(spp)),
                        chill=rep(NA, length(spp)))

less.pool <- data.frame(complex=rep(NA, length(spp)),
                        intercept=rep(NA, length(spp)), 
                        force=rep(NA, length(spp)), 
                        photo=rep(NA, length(spp)),
                        chill=rep(NA, length(spp)))


modhere <-wein.sum
for (sp in c(1:length(spp))){
  with.pool$complex[sp] <- spp[sp]
  with.pool$intercept[sp] <- modhere[grep("a_sp", rownames(modhere)),1][spp[sp]+2]
  with.pool$force[sp] <- modhere[grep("b_force", rownames(modhere)),1][spp[sp]+2]
  with.pool$photo[sp] <- modhere[grep("b_photo", rownames(modhere)),1][spp[sp]+2]
  with.pool$chill[sp] <- modhere[grep("b_chill", rownames(modhere)),1][spp[sp]+2]
}

with.pool$model<-"slope and intercept pool"

modhere2 <-wein.sum2
for (sp in c(1:length(spp))){
  less.pool$complex[sp] <- spp[sp]
  less.pool$intercept[sp] <- modhere2[grep("a_sp", rownames(modhere2)),1][spp[sp]+2]
  less.pool$force[sp] <- modhere2[grep("b_force", rownames(modhere2)),1]
  less.pool$photo[sp] <- modhere2[grep("b_photo", rownames(modhere2)),1]
  less.pool$chill[sp] <- modhere2[grep("b_chill", rownames(modhere2)),1]
}
less.pool$model<-"pooling int. only"

###now you have a dataframe of the results from each model
less.pool$complex.wname <- sort(unique(bb.stan$complex.wname))
with.pool$complex.wname <- sort(unique(bb.stan$complex.wname))
df.pulled <- rbind(less.pool, with.pool)

###photo
pdf(file.path(figpath, "models_WEIN_compare_pp_photo.pdf"), width = 9, height = 6)
ggplot(df.pulled) + 
  aes(x = intercept, y = photo, color = model) + 
  geom_point(size = 2) + 
  geom_path(aes(group = as.character(complex), color = NULL), 
            arrow = arrow(length = unit(.02, "npc")))+
  ggrepel::geom_text_repel(
    aes(label = complex.wname, color = NULL), 
    data = less.pool, size=2) + 
  theme(legend.position = "bottom")+
  ggtitle("Pooling of weinberger regression parameters: Int and photo") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2")+
  theme_linedraw()
dev.off()  

pdf(file.path(figpath, "models_WEIN_compare_pp_force.pdf"), width = 9, height = 6)
ggplot(df.pulled) + 
  aes(x = intercept, y = force, color = model) + 
  geom_point(size = 2) + 
  geom_path(aes(group = as.character(complex), color = NULL), 
            arrow = arrow(length = unit(.02, "npc")))+
  ggrepel::geom_text_repel(
    aes(label = complex.wname, color = NULL), 
    data = less.pool, size=2) + 
  theme(legend.position = "bottom")+
  ggtitle("Pooling of weinberger regression parameters: Int and force") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Accent")+
  theme_linedraw()
dev.off() 

pdf(file.path(figpath, "models_WEIN_compare_pp_chill.pdf"), width = 9, height = 6)
ggplot(df.pulled) + 
  aes(x = intercept, y = chill, color = model) + 
  geom_point(size = 2) + 
  geom_path(aes(group = as.character(complex), color = NULL), 
            arrow = arrow(length = unit(.02, "npc")))+
  ggrepel::geom_text_repel(
    aes(label = complex.wname, color = NULL), 
    data = less.pool, size=2) + 
  theme(legend.position = "bottom")+
  ggtitle("Pooling of weinberger regression parameters: Int and chill") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Set1")+
  theme_linedraw()
dev.off()
