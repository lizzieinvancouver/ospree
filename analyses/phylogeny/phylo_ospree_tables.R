# Started June 2018 #
# Source file for some of the climatefuture supp stuff #

# Clear workspace
#rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
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

colnames(modelspslevest) = colnames(modelspslev0) = c("Species","$\\beta_{chill,j}$","lowUI","upUI",
                          "$\\beta_{force,j}$","lowUI","upUI",
                          "$\\beta_{photo,j}$","lowUI","upUI")

names(modellambest)[names(modellambest)=="X"] <- "Parameter"
lookupmodelnames <- c("a_z"="$\\mu_{\\alpha}$",
                      "b_zf"="$\\mu_{\\beta_{force}}$",
                      "b_zc"="$\\mu_{\\beta_{chill}}$",
                      "b_zp"="$\\mu_{\\beta_{photo}}$",
                      "lam_interceptsa"="$\\lambda_{\\alpha}$",
                      "lam_interceptsbf"="$\\lambda_{\\beta_{force}}$",
                      "lam_interceptsbc"="$\\lambda_{\\beta_{chill}}$",
                      "lam_interceptsbp"="$\\lambda_{\\beta_{photo}}$",
                      "sigma_interceptsa"="$\\sigma_{\\alpha}$",
                      "sigma_interceptsbf"="$\\sigma_{\\beta_{force}}$",
                      "sigma_interceptsbc"="$\\sigma_{\\beta_{chill}}$",
                      "sigma_interceptsbp"="$\\sigma_{\\beta_{photo}}$",
                      "sigma_y"="$\\sigma_y$")
modellambest$Parameter <- unname(lookupmodelnames[modellambest$Parameter])
names(modellambest)[names(modellambest)=="X2.5."] <- "2.5\\%"
names(modellambest)[names(modellambest)=="X50."] <- "50\\%"
names(modellambest)[names(modellambest)=="X97.5."] <- "97.5\\%"
names(modellambest)[names(modellambest)=="n_eff"] <- "$n_{eff}$"

make.mytable1 <- xtable(modellambest, 
                        caption="Model parameters estimated for 191 tree species including mean, standard deviation (sd), 2.5\\%, 50\\%, and 97.5\\% uncertainty intervals (z-scored model, thus predictors are directly comparable to one another), alongside model diagnostics.",
                        label="tab:modelanglamb")

#align(make.mytable1) <- c( 'p{0.20}',  'p{0.20in}', 'p{0.25in}', 'r{0.25in}', 'p{0.35in}', 'p{0.35in}')


names(modellamb0)[names(modellamb0)=="X"] <- "Parameter"
lookupmodelnames0 <- c("a_z"="$\\mu_{\\alpha}$",
                      "b_zf"="$\\mu_{\\beta_{force}}$",
                      "b_zc"="$\\mu_{\\beta_{chill}}$",
                      "b_zp"="$\\mu_{\\beta_{photo}}$",
                      "sigma_interceptsa"="$\\sigma_{\\alpha}$",
                      "sigma_interceptsbf"="$\\sigma_{\\beta_{force}}$",
                      "sigma_interceptsbc"="$\\sigma_{\\beta_{chill}}$",
                      "sigma_interceptsbp"="$\\sigma_{\\beta_{photo}}$",
                      "sigma_y"="$\\sigma_y$")
modellamb0$Parameter <- unname(lookupmodelnames0[modellamb0$Parameter])
names(modellamb0)[names(modellamb0)=="X2.5."] <- "2.5\\%"
names(modellamb0)[names(modellamb0)=="X50."] <- "50\\%"
names(modellamb0)[names(modellamb0)=="X97.5."] <- "97.5\\%"
names(modellamb0)[names(modellamb0)=="n_eff"] <- "$n_{eff}$"

make.mytable2 <- xtable(modellamb0, 
                        caption="Model parameters for non-phylogenetic model ($\\lambda$ = 0) estimated for 191 tree species including mean, standard deviation (sd), 2.5\\%, 50\\%, and 97.5\\% uncertainty intervals (z-scored model, thus predictors are directly comparable to one another), alongside model diagnostics.",
                        label="tab:modlamb0")


make.mytable3 <- xtable(modelspslevest, 
                        caption="Estimated sensitivities of 191 tree species to three environmental cues: chilling ($\\beta_{chill,j}$), forcing ($\\beta_{force,j}$) and photoperiod ($\\beta_{photo,j}$), along with their corresponding 2.5\\% (low) and 97.5\\% (up) uncertainty intervals (UI). Values correspond to the full model accounting for phylogenetic relationships.",
                        label="tab:tablesupp3")

if(FALSE){
align(make.mytable3) <- c( 'p{0.10}',  'p{1.50in}', 'c{0.32in}', 'c{0.32in}', 
                           'c{0.32in}', 'c{0.32in}','c{0.32in}', 'c{0.32in}' , 
                          'c{0.2in}', 'c{0.3in}', 'c{0.1in}')
}

add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n",
                  "\\hline\n",
                  "\\multicolumn{", dim(modelspslevest)[2] + 0, "}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n",
                  "\\endlastfoot\n")
add.to.row$command <- command


make.mytable4 <- xtable(modelspslev0, 
                        caption="Estimated sensitivities of 191 tree species to three environmental cues: chilling ($\\beta_{chill,j}$), forcing ($\\beta_{force,j}$) and photoperiod ($\\beta_{photo,j}$), along with their corresponding 2.5\\% (low) and 97.5\\% (up) uncertainty intervals (UI). Values correspond to the non phylogenetic model.",
                        label="tab:tablesupp2")

if(FALSE){
align(make.mytable4) <- c( 'p{0.10}',  'p{1.50in}', 'c{0.32in}', 'c{0.32in}', 
                           'c{0.32in}', 'c{0.32in}','c{0.32in}', 'c{0.32in}' , 
                          'c{0.2in}', 'c{0.3in}', 'c{0.1in}')
}

add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n",
                  "\\hline\n",
                  "\\multicolumn{", dim(modelspslev0)[2] + 0, "}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n",
                  "\\endlastfoot\n")
add.to.row$command <- command


if(length(grep("Ignacio", getwd()))>0) { 
  setwd("~/GitHub/ospree/docs/phylogeny") 
}
