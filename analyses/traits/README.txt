OSPREE traits
Relationships between spring phenology and functional traits

<><><><><><><><><><>
Where to find things
<><><><><><><><><><>

## Trait wiki
https://github.com/lizzieinvancouver/ospree/wiki/Traits! 

## Google Drive

Trait data files for both BIEN and TRY trait data: raw files and compiled/cleaned files

All PDFs included in the literature review & future publications 

## Midge/data/Ospree_traits

tryDataCleanedNew_Nov2020.csv - reorganized try data with extraneous rows and datasets (experiments) removed

try_subspeciestraits.csv - cleaned try data with corrected species names, etc

bien_cleaned_Nov2020.csv - reorganized bien data with corrected units and column names

try_bien_Nov2020.csv - merged try and bien trait data, subsetted to key traits and ospree species, not averaged 

## github/ospree/anaylses/traits/input
try_bien_nodups_1.csv - final, cleaned files of try and bien data saved on OSPREE repo
try_bien_nodups_2.csv

# R files
## github/ospree/anaylses/traits/Rfiles

cleaning_bien/get.BIEN.data.R - using BIEN package to get data for different species lists, including full OSPREE splits (which we are using) and phylogeny species list
cleaning_bien/BIEN_clenaing.R - fixing species names, renaming colns etc
cleaning_try/TRY_cleaning_master.R - reorganizing try data, cleaning species names etc
get_phylogeny_traitors.R - code to prune the Smith and Brown tree
mergebientry.R - merging the bien and try data, removing experiment data, renaming columns
heightresample.R - resampling height data, limiting the number of rows of data to 5000 for any given species * to be sourced for the height data used for modelling
traitors_clean_duplicates.R - removes duplicated data across the BIEN and Try databases
traitors_PCA.R - PCA used to visualize trait relationships and identify key traits and species for further analyses

### Test data and model checks
Rfiles/height_simPriorChecks.R - Faith's code to fix issues with the priors - including fitting positive values - this code focuses on height
Rfiles/SLA_simPriorChecks.R - Faith's code to fix issues with the priors - but for SLA
Rfiles/LNC_simPriorChecks.R - Faith's code to fix issues with the priors - but for LNC
simulation_traitsandmore.R - Geoff's simulation code for generating test data

joint_forcingchillphoto_model.R - DL's initial attempt at a joint model for a single trait and three cues * model has issues with low n_eff 

Phenology_meanTraitValues.R - model using mean trait values and phenology model
Phenology_meanTraitValues_simPriorCheck.R - simulation code for prior predictive checks for mean trait model

### Stan code

phenology_combined.stan - Main model code, priors specified in the rcode, developed by Faith and Geoff
phenology_combined_generatedquantities.stan - main model but with generated quantities block
trait_only.stan - DL's initial attempts at building the model, trait only
joint_3cue_newprior.stan DL's initial attempts at building the model, with cues

### Model code

SeedMass_log10_phenologycombined.R - code to run the main model, with trait specific priors
height_phenologycombined.R
LNC_phenologycombined.R
SLA_phenologycombined.R

###Manuscript files and plotting code

Plotting_mugrandspvsbetaSp.R - rcode to plot the alpha vs beta parameters and generate ms figure
concept_fig.R - code sourced to generate the conceptual figures in the ms
mdlConsVsAcq.R - code to plot examples of a conservative sp. response vs a acquisitive sp. response
traitFit.R - code to plot the model fit compared to the raw data and the means
results_Height_plot.R - sourced