# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(shinystan)
library(RColorBrewer)


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

# dostan = TRUE
# Flags to choose for bbstanleadin.R

use.chillports = FALSE # change to true for using chillportions instead of utah units
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

weinberger<-filter(dates2weekstreat,fs.date.count>=2)
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
sp.match<-intersect(unique(wein.sp$complex.wname), unique(not.wein$complex.wname))

sp.match ### species in both

######################
# now we exclude non-weinberger studies that do NOT manipulate chilling
nonwein.expchill <- subset(not.wein, chill_type!="fldest")
unique(wein.sp$chill_type) # 11 rows incl falusi97, caffarra11b, heide93
studiestoincl <- c(unique(nonwein.expchill$dataIDstudyID), unique(wein.sp$dataIDstudyID))

bb.stan.alt <- bb.stan[which(bb.stan$dataIDstudyID %in% unique(studiestoincl)),]

# or, or .. we include ONLY non-weinberger studies with exp chilling only
nonwein.expchillonly <- subset(not.wein, chill_type=="exp")
studiestoincl.exponly <- c(unique(nonwein.expchillonly$dataIDstudyID), unique(wein.sp$dataIDstudyID))

bb.stan.alt <- bb.stan[which(bb.stan$dataIDstudyID %in% unique(studiestoincl)),]
bb.stan.alt.exponly <- bb.stan[which(bb.stan$dataIDstudyID %in% unique(studiestoincl.exponly)),]

# now we exclude try to use matching species
# rm fldest
#bb.stan.matchsp <- bb.stan.alt[which(bb.stan.alt$complex.wname %in% sp.match),]
bb.stan.matchsp <- bb.stan[which(bb.stan$complex.wname %in% sp.match),] # if you want to incl fldest
bb.stan.matchsp$complex <- as.numeric(as.factor(bb.stan.matchsp$complex.wname))



## Set up the bb.stan to use
bb.stan <- bb.stan.alt
#bb.stan <- bb.stan.alt.exponly
#bb.stan <- bb.stan.matchsp
######################
####make datalist
wein.data <- with(bb.stan, 
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

###model
#m2l.ni = stan('stan/weinbergerint.stan', data = wein.data,
             # iter = 2500, warmup=1500)

wein.mod.2 = stan('stan/weinberger_fewint.stan', data = wein.data,
              iter = 2500, warmup=1500)

wein.mod.3 = stan('stan/wein_intpoolonly.stan', data = wein.data,
                  iter = 2500, warmup=1500)


###some weinberger plotss
wein.chill<-ggplot(bb.stan,aes(chill,resp, color=as.factor(weinberger)))+geom_point()+geom_smooth(method='lm',fullrange=TRUE)+ggthemes::theme_base() 
wein.force<-ggplot(bb.stan,aes(force,resp, color=as.factor(weinberger)))+geom_point()+geom_smooth(method='lm',fullrange=TRUE)+ggthemes::theme_base()
wein.photo<-ggplot(bb.stan,aes(photo,resp, color=as.factor(weinberger)))+geom_point()+geom_smooth(method='lm',fullrange=TRUE)+ggthemes::theme_base()

##### r-square models
observed.here <- bb.stan$resp

wein.sum <- summary(wein.mod.2)$summary
wein.sum[c("mu_a_sp", "mu_b_force_sp", "mu_b_photo_sp", "mu_b_chill_sp",
           "b_weinberger", "b_cw","b_pw","b_fw"),]

wein.sum2 <- summary(wein.mod.3)$summary
wein.sum2[c("mu_a_sp", "b_force", "b_photo", "b_chill",
            "b_weinberger", "b_cw","b_pw","b_fw"),]

# pooling on main effects
preds.wein.sum <- wein.sum[grep("yhat", rownames(wein.sum)),]
wein.sum.R2 <- 1- sum((observed.here-preds.wein.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
wein.mod.R2 <- 1- sum((observed.here-preds.wein.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
summary(lm(preds.wein.sum[,1]~observed.here)) # Multiple R-squared:  

##pooling on just intercept 
preds.wein.sum2 <- wein.sum2[grep("yhat", rownames(wein.sum2)),]
wein.sum.2.R2 <- 1- sum((observed.here-preds.wein.sum2[,1])^2)/sum((observed.here-mean(observed.here))^2)
wein.mod.2.R2 <- 1- sum((observed.here-preds.wein.sum2[,1])^2)/sum((observed.here-mean(observed.here))^2)
summary(lm(preds.wein.sum2[,1]~observed.here)) # Multiple R-squared: 

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

