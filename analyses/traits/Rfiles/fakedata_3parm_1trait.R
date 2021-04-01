# Date started: March 31, 2021
# The purpose of this code is to generate test data for the traitors model with all three climate parameters and a single trait, here we start with height:

if(length(grep("deirdreloughnan", getwd()) > 0)) {
  setwd("~/Documents/github/ospree/analyses/traits")
} else{
  setwd("/home/faith/Documents/github/ospree/ospree/analyses/jointmodel/") 
}

rm(list=ls()) 
options(stringsAsFactors = FALSE)

Nstudy <- 10 # number of studies w/ traits
Nspp <- 5 # number of species with traits

# First making a data frame for the test trait data
ntrt <- 10 # rep per trait
N <- Nspp * ntrt * Nstudy

# Next, making a data frame for the pheno data

nphen <- 4 # rep per pheno event 
Nph.sp <- Nspp * nphen


traitstanpheno <- list(
  yTraiti = trait.observe, 
  N = length(trait.observe), # sample size for trait data is the same as phenology data 
  n_spec = nsp,  # number of species is the same for traits and phenology data
  species = dat2df$speciesID, 
  study = dat2df$studyID, 
  n_study = nstudy, 
  yPhenoi = dat2df$phen.response, 
  Nph = length(trait.observe), # sample size for trait data is the same as phenology data  
  forcingi = dat2df$forcing,
  photoi = data2df$photo,
  chillingi = dat2df$chilling)  


