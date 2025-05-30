## Started 24 March 2020 ##
## Let's review the new data! ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rstan)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)#for arranging plots in a grid 
library(cowplot)#f
library(shinystan)
library(bayesplot)# nice posterior check plots

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



# Grab the data, all of it
dall <- read.csv("..//output/ospree_clean.csv", header=TRUE)
dbb <- read.csv("..//output/ospree_clean_withchill_BB.csv", header=TRUE)

# Get your subset ... 
sort(unique(dbb$datasetID))

medata <- c("malyshev18", "flynn18","ramos17")

mall <- dall[which(dall$datasetID %in% medata),]
mdbb <- dbb[which(dbb$datasetID %in% medata),]
unique(mall$datasetID)
unique(mdbb$datasetID) ### ramos17 dissapears in cleaing note: the paper is ramos 18 on google drive

# check if you lose data between all and bb (if you do, find out what is lost!)
dim(mall) #1332
dim(mdbb) #1006
table(mall$datasetID)
1006+326 ####see row differences between the two dataframes result from the loss of ramos.

#########################################################################################
############# task 1 Find out why ramos17 goes away.##################################
#1. Run chillmergeall.R after every step in cleaning type table(d$datasetID=="ramos17")
#hmmmm it still appear to be in the dataset at the end of chillmergeall.R
#2. run bb_cleanmergall.R
## We lose half of ramos17 (163 rows in cleaning/multirespr) (perceptbudburst reproductive)
### we lose the rest in clean_bbperctodays.R. We checked this before, and Lizzie was going to follow up
#see double check clean_bbperctodays.R  issue #310


## bb stuff... this code poached from:
# models_stan.R, models_stan_plotting.R, bbstanleadin.R, and bbdataplease.R 
## just the bb data ...
d <- mdbb
respvars.thatwewant <- c("daystobudburst", "percentbudburst")

table(d$respvar.simple)
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),] ### removes 110 rows of flowering
bb.resp <- subset(bb.resp, respvar != "thermaltime") # doesn't remove anything

## make a bunch of things numeric (eek!)

# values before check making everythig numberic
unique(bb.resp$forcetemp)
bb.resp$forceday <- as.numeric(bb.resp$forcetemp)
unique(bb.resp$forceday)

bb.resp$forcenight <- as.numeric(bb.resp$forcetemp_night)

bb.resp$photonight <- as.numeric(bb.resp$photoperiod_night)

# Correct forcing for day/night differences
bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$force <- bb.resp$forceday
bb.resp$force[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] <-
    (bb.resp$forceday[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photo[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] +
    bb.resp$forcenight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photonight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE])/24
####do to thermoperiodicity, this calculation makes it so there are 4 forcing treamtnets instead of 2 for

bb.resp$chill <- as.numeric(bb.resp$Total_Utah_Model) 
bb.resp$chill.hrs <- as.numeric(bb.resp$Total_Chilling_Hours)
bb.resp$chill.ports <- as.numeric(bb.resp$Total_Chill_portions) 

bb.resp$resp <- as.numeric(bb.resp$response.time)

bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(chill)==FALSE & is.na(resp)==FALSE)
dim(bb.noNA) ### nothing is lost by making things numeric. Yay!


# Fix species... so it plays well with rstan
bb.noNA$latbi <- paste(bb.noNA$genus, bb.noNA$species)
bb.noNA$complex <- as.numeric(as.factor(bb.noNA$latbi))


# Start looking at the data ...
ggplot(bb.noNA, aes(chill, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
ggplot(bb.noNA, aes(force, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
ggplot(bb.noNA, aes(photo, resp, colour=latbi)) + geom_point() + facet_grid(datasetID~.)
# Compare it to  the paper ....
comppaper <- subset(bb.noNA, select=c("datasetID", "latbi", "chill", "force",
    "photo", "resp", "figure.table..if.applicable."))
comppaperfig <- subset(bb.noNA, select=c("datasetID", "latbi", "figure.table..if.applicable."))
comppaperfig[!duplicated(comppaperfig), ]

# Hmm, chilling units seem to vary in malyshev18 figure 2 agreed

### st up z.score
bb.noNA$chill.z<-(bb.noNA$chill-mean(bb.noNA$chill))/sd(bb.noNA$chill)
bb.noNA$force.z<-(bb.noNA$force-mean(bb.noNA$force))/sd(bb.noNA$force)
bb.noNA$photo.z<-(bb.noNA$photo-mean(bb.noNA$photo))/sd(bb.noNA$photo)
# rstan
datalist.bb <- with(bb.noNA, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.noNA),
                         n_sp = length(unique(bb.noNA$complex))
                    ))

modhere = stan('stan/datacheckup_db_fj_intonly.stan', data = datalist.bb,
    iter = 9000, warmup=7000,control=list(adapt_delta=.99))

y<-bb.noNA$resp
library(shinystan)
launch_shinystan(modhere) # please go to: Explore -> Click on 'bivariate' on right and make sure y-axis is log-posterior
##signa force is the worst, but so is sigma chill, photo not great either
# At this point I would try a different model formulation! Which ones would you try?

# Some plotting (if you have a good model!)
bb.stan <- bb.noNA
post.mohere <- extract(modhere)
sumer.here <- summary(modhere)$summary
whichmodel <- sumer.here
spp <- sort(unique(bb.noNA$complex))
n <- length(spp)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colv = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

jpeg("..//misc/datacheck/modpreds_db_fj.jpeg", width = 800, height = 500)

par(mfrow=c(1,3))
plot(resp~chill, data=bb.stan, type="n")

for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~chill, data=subby, main=subby$complex.wname[1], col=colv[sp])}
abline(a=whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],b=whichmodel[grep("b_chill", rownames(whichmodel)),1], col="black", lwd=2)



plot(resp~force, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~force, data=subby, main=subby$complex.wname[1], col=colv[sp])}
abline(a=whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],b=whichmodel[grep("b_force", rownames(whichmodel)),1], col="black", lwd=2)



plot(resp~photo, data=bb.stan, type="n")
for (sp in c(1:length(spp))){
    subby <- subset(bb.stan, complex==spp[sp])
    points(resp~photo, data=subby, main=subby$complex.wname[1], col=colv[sp])
    }

abline(whichmodel[grep("mu_a_sp", rownames(whichmodel)),1],
           whichmodel[grep("b_photo", rownames(whichmodel)),1], col="black", lwd=3)
dev.off()
#Faith's model plots 
speciesEffects <- as.data.frame(post.mohere)

mainEffects<- c("mu_a_sp", "sigma_a_sp", "sigma_y",  "b_force", "b_photo", "b_chill") #important model parameters 
mainEffectsPlot <- mcmc_intervals(speciesEffects[,mainEffects] ) + yaxis_text(on = TRUE, size = 14)+
   xaxis_text(on = TRUE, size = 14) +
  xaxis_title(size = 14, family = "sans") +
  ggplot2::xlab("Esimated model values") 

spNames <- names(speciesEffects)[grep("a_sp.", names(speciesEffects))]
names(speciesEffects)[names(speciesEffects) %in% spNames] <- levels(as.factor(bb.noNA$latbi))
speciesAlphas <- c("mu_a_sp", levels(as.factor(bb.noNA$latbi))) # mian alpha and all the individual species alphas
alphaPlot <- mcmc_intervals(speciesEffects[,speciesAlphas] )
SpeciesPlot <- alphaPlot +
  xaxis_title(size = 14, family = "sans") +
  ggplot2::xlab("Estimated alpha budburst") + 
  vline_at(median(speciesEffects$mu_a_sp), color = "gray20", alpha = 0.2) + 
  yaxis_text(on = TRUE, size = 14)

png("~/Documents/github/ospree/analyses/bb_analysis/stan/ModelParameterEstimates_fj.png", width = 800, height = 500)
cowplot::plot_grid(mainEffectsPlot, SpeciesPlot, nrow = 1,  align = "h", rel_widths = c(1,1.6))
dev.off()
#Making some nice data plots 
#------------------------------------

#boxplots by species and study 
chillPlotF <- ggplot(data = bb.noNA[bb.noNA$datasetID == "flynn18",], aes(x = latbi, y = chill))
chillPlotF2  <- chillPlotF + 
  geom_boxplot() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Species")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("flynn18")

chillPlotM <- ggplot(data = bb.noNA[bb.noNA$datasetID == "malyshev18",], aes(x = latbi, y = chill))
chillPlotM2  <- chillPlotM + 
  geom_boxplot() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Species")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("malyshev18")+
  theme(axis.title.y=element_blank())

forcePlotF <- ggplot(data = bb.noNA[bb.noNA$datasetID == "flynn18",], aes(x = latbi, y = force))
forcePlotF2  <- forcePlotF + 
  geom_boxplot() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Species")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

forcePlotM <- ggplot(data = bb.noNA[bb.noNA$datasetID == "malyshev18",], aes(x = latbi, y = force))
forcePlotM2  <- forcePlotM + 
  geom_boxplot() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Species")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank())

photoPlotF <- ggplot(data = bb.noNA[bb.noNA$datasetID == "flynn18",], aes(x = latbi, y = photo))
photoPlotF2  <- photoPlotF + 
  geom_boxplot() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Species")+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold"))

photoPlotM <- ggplot(data = bb.noNA[bb.noNA$datasetID == "malyshev18",], aes(x = latbi, y = photo))
photoPlotM2  <- photoPlotM + 
  geom_boxplot() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Species")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(axis.title.y=element_blank())


png("~/Documents/github/ospree/analyses/bb_analysis/stan/SPecies_data_split_fj.png", width = 600, height = 750)
cowplot::plot_grid(chillPlotF2, chillPlotM2, forcePlotF2, forcePlotM2, photoPlotF2, photoPlotM2, ncol = 2,  align = "v", rel_heights = c(1,1,1.75))
dev.off()

#how much overlap of species?
speciesCount <- data.frame(table(bb.noNA$latbi, bb.noNA$datasetID))
names(speciesCount) <- c("Species", "datasetID", "n_spec")
speciesCountPlot <- ggplot(data = bb.noNA, aes(x=latbi, fill=datasetID)) 
speciesCountPlot2 <- speciesCountPlot + geom_histogram(alpha=0.2, position="identity", stat="count")+
  xlab("Species")+ 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

png("~/Documents/github/ospree/analyses/bb_analysis/stan/SpeciesCount_fj.png", width = 600, height = 400)
speciesCountPlot2
dev.off()

#chilling vs focrcing and photoperiod
chillForce <- ggplot(data = bb.noNA, aes(x = chill, y = force, colour = datasetID))
chillForcePlot <- chillForce + geom_point() + facet_wrap(~photo)+ 
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

png("~/Documents/github/ospree/analyses/bb_analysis/stan/chill_force_photo_fj,png", width = 600, height = 400)
chillForcePlot
dev.off()
