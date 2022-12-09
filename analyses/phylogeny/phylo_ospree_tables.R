# Started June 2018 #
# Source file for some of the climatefuture supp stuff #

# Clear workspace
#rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

library(xtable) # needed for Sweaving
modellambest <- read.csv("output/angio_noout_lambest191.csv")
modellamb0 <- read.csv("output/angio_noout_lamb0_191.csv")


make.mytable1 <- xtable(modellambest, 
                        caption="Full model parameters estimated for 191 tree species.",
                        label="tab:modelanglamb")


#align(make.mytable1) <- c( 'p{0.20}',  'p{0.20in}', 'p{0.25in}', 'r{0.25in}', 'p{0.35in}', 'p{0.35in}')


make.mytable2 <- xtable(modellamb0, 
                        caption="Model parameters for non-phylogenetic model (lambda = 0) estimated for 191 tree species.",
                        label="tab:modlamb0")


