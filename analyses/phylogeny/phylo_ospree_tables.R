# Started June 2018 #
# Source file for some of the climatefuture supp stuff #

# Clear workspace
#rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny") 
} else if (length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses/phylogeny")
}else if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/analyses/phylogeny") 
} else if(length(grep("catchamberlain", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
} else if(length(grep("danielbuonaiuto", getwd()))>0) { 
  setwd("~/Documents/git/ospree/analyses/phylogeny") 
}else setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/phylogeny")


library(xtable) # needed for Sweaving
modellambest <- read.csv("output/angio_noout_lambest191.csv")
modellamb0 <- read.csv("output/angio_noout_lamb0_191.csv")
modelspslevest <- read.csv("output/spslevestimates191.csv")
modelspslev0 <- read.csv("output/spslevestimateslamb0_191.csv")

colnames(modelspslevest) = colnames(modelspslev0) = c("Species","b.chill","low","up",
                          "b.force","low","up",
                          "b.photo","low","up")


make.mytable1 <- xtable(modellambest, 
                        caption="Full model parameters estimated for 191 tree species.",
                        label="tab:modelanglamb")


#align(make.mytable1) <- c( 'p{0.20}',  'p{0.20in}', 'p{0.25in}', 'r{0.25in}', 'p{0.35in}', 'p{0.35in}')



make.mytable2 <- xtable(modellamb0, 
                        caption="Model parameters for non-phylogenetic model (lambda = 0) estimated for 191 tree species.",
                        label="tab:modlamb0")


make.mytable3 <- xtable(modelspslevest, 
                        caption="Estimated sensitivities of 191 tree species to three environmental cues: chilling (b.chill), forcing (b.force) and photoperiod (b.photo), along with their corresponding 2.5\\% (low) and 97.5\\% (up) Credible Intervals. Values correspond to the full model accounting for phylogenetic relationships.",
                        label="tab:tablesupp3")

align(make.mytable3) <- c( 'p{0.10}',  'p{1.50in}', 'c{0.32in}', 'c{0.32in}', 
                           'c{0.32in}', 'c{0.32in}','c{0.32in}', 'c{0.32in}' , 
                           'c{0.2in}', 'c{0.3in}', 'c{0.1in}')

add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n",
                  "\\hline\n",
                  "\\multicolumn{", dim(modelspslevest)[2] + 0, "}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n",
                  "\\endlastfoot\n")
add.to.row$command <- command


make.mytable4 <- xtable(modelspslev0, 
                        caption="Estimated sensitivities of 191 tree species to three environmental cues: chilling (b.chill), forcing (b.force) and photoperiod (b.photo), along with their corresponding 2.5\\% (low) and 97.5\\% (up) Credible Intervals. Values correspond to the non phylogenetic model.",
                        label="tab:tablesupp2")
align(make.mytable4) <- c( 'p{0.10}',  'p{1.50in}', 'c{0.32in}', 'c{0.32in}', 
                           'c{0.32in}', 'c{0.32in}','c{0.32in}', 'c{0.32in}' , 
                           'c{0.2in}', 'c{0.3in}', 'c{0.1in}')
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n",
                  "\\hline\n",
                  "\\multicolumn{", dim(modelspslev0)[2] + 0, "}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n",
                  "\\endlastfoot\n")
add.to.row$command <- command


