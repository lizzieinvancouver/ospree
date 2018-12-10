## Started 8 December 2018 ##
## By Lizzie ##
## With help from ... https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/ ##

## Visualize partial pooling of main budburst model ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(RColorBrewer)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("ailene", getwd())>0)) { 
  setwd("/Users/aileneettinger/git/ospree/analyses/bb_analysis")
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses") 
} else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")

figpath <- "figures"

## set up the flags
use.chillports = TRUE 
use.allspp = FALSE
use.multcuespp = FALSE
use.cropspp = FALSE
# Default is species complex use  alltypes of designs
use.expramptypes.fp = TRUE
use.exptypes.fp = FALSE
use.expchillonly = FALSE

## name your figures paths (based on flags above) ... this needs work
if(use.allspp==FALSE & use.expramptypes.fp==TRUE){
    figpathmore <- "spcom.expramp.fp"
    }
if(use.allspp==TRUE & use.expramptypes.fp==TRUE){
    figpathmore <- "allspp.expramp.fp"
    }


##
source("source/bbstanleadin.R")

##
# Load fitted stan model: no interactions
load("stan/output/m2lni_alltypes.Rda")

sumer.ni <- summary(m2l.ni)$summary
sumer.ni[grep("mu_", rownames(sumer.ni)),]


###########################
## And now we plot ... ##
###########################

com.pool.mod <- lm(resp ~ force.z+photo.z+chill.z, bb.stan)
spp <- sort(unique(bb.stan$complex))
no.pool <- data.frame(complex=rep(NA, length(spp)),
                 intercept=rep(NA, length(spp)), 
                 force=rep(NA, length(spp)), 
                 photo=rep(NA, length(spp)),
                 chill=rep(NA, length(spp)))
with.pool <- no.pool
df.gravity <- no.pool[1:2,2:5]
with.pool$model <- "partial pooling"
no.pool$model <- "no pooling"


for (sp in c(1:length(spp))){
    no.pool$complex[sp] <- spp[sp]
    subby <- subset(bb.stan,  complex==spp[sp])
    lmfit <- lm(resp ~ force+photo+chill, data=subby)
    no.pool$intercept[sp] <- coef(lmfit)["(Intercept)"]
    no.pool$force[sp] <- coef(lmfit)["force"]
    no.pool$photo[sp] <- coef(lmfit)["photo"]
    no.pool$chill[sp] <- coef(lmfit)["chill"]
    }

modhere <- sumer.ni
for (sp in c(1:length(spp))){
    with.pool$complex[sp] <- spp[sp]
    with.pool$intercept[sp] <- modhere[grep("a_sp", rownames(modhere)),1][spp[sp]+2]
    with.pool$force[sp] <- modhere[grep("b_force", rownames(modhere)),1][spp[sp]+2]
    with.pool$photo[sp] <- modhere[grep("b_photo", rownames(modhere)),1][spp[sp]+2]
    with.pool$chill[sp] <- modhere[grep("b_chill", rownames(modhere)),1][spp[sp]+2]
    }


no.pool$complex.wname <- sort(unique(bb.stan$complex.wname))
with.pool$complex.wname <- sort(unique(bb.stan$complex.wname))
df.pulled <- rbind(no.pool, with.pool)

df.gravity$model <- NA
df.gravity$intercept[1] <-coef(com.pool.mod)["(Intercept)"]
df.gravity$force[1] <-coef(com.pool.mod)["force.z"]
df.gravity$photo[1] <-coef(com.pool.mod)["photo.z"]
df.gravity$chill[1] <-coef(com.pool.mod)["chill.z"]
df.gravity$model[1] <- "complete pooling"
df.gravity$intercept[2] <- modhere[grep("mu_a_sp", rownames(modhere)),1]
df.gravity$force[2] <- modhere[grep("mu_b_force_sp", rownames(modhere)),1]
df.gravity$photo[2] <- modhere[grep("mu_b_photo_sp", rownames(modhere)),1]
df.gravity$chill[2] <- modhere[grep("mu_b_chill_sp", rownames(modhere)),1]
df.gravity$model[2] <- "partial pooling (mu)"

pdf(file.path(figpath, "modelscompare_pp_force.pdf"), width = 9, height = 6)
ggplot(df.pulled) + 
  aes(x = intercept, y = force, color = model) + 
  geom_point(size = 2) + 
  geom_point(data = df.gravity, size = 5) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = as.character(complex), color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = complex.wname, color = NULL), 
    data = no.pool, size=2) + 
  theme(legend.position = "bottom") + 
  ggtitle("Pooling of regression parameters: Int and forcing") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") 
dev.off()

pdf(file.path(figpath, "modelscompare_pp_photo.pdf"), width = 9, height = 6)
ggplot(df.pulled) + 
  aes(x = intercept, y = photo, color = model) + 
  geom_point(size = 2) + 
  geom_point(data = df.gravity, size = 5) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = as.character(complex), color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = complex.wname, color = NULL), 
    data = no.pool, size=2) + 
  theme(legend.position = "bottom") + 
  ggtitle("Pooling of regression parameters: Int and photo") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") 
dev.off()

pdf(file.path(figpath, "modelscompare_pp_chill.pdf"), width = 9, height = 6)
ggplot(df.pulled) + 
  aes(x = intercept, y = chill, color = model) + 
  geom_point(size = 2) + 
  geom_point(data = df.gravity, size = 5) + 
  # Draw an arrow connecting the observations between models
  geom_path(aes(group = as.character(complex), color = NULL), 
            arrow = arrow(length = unit(.02, "npc"))) + 
  # Use ggrepel to jitter the labels away from the points
  ggrepel::geom_text_repel(
    aes(label = complex.wname, color = NULL), 
    data = no.pool, size=2) + 
  theme(legend.position = "bottom") + 
  ggtitle("Pooling of regression parameters: Int and chilling") + 
  xlab("Intercept estimate") + 
  ylab("Slope estimate") + 
  scale_color_brewer(palette = "Dark2") 
dev.off()

