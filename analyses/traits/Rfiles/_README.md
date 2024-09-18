# README for OSPREE Trait Rfiles folder

## Modelling
Each trait had a R file for running the model:
- SLA_phenologycombined.R
- LNC_phenologycombined.R
- height_phenologycombined.R
- SeedMass_log10_phenologycombined.R

## Plotting code
1. **slopesConsAcqu_sla_lnc_100.pdf & slopesConsAcqu_ht_sm_100.pdf**
- mdlConsVsAcq.R: sources the below 4 files to show difference in trait effects between acquisitive and conservative species 
- results_SLA_plot.R
- results_LNC_plot.R
- results_Height_plot.R
- results_SeedMass_plot.R
  
2. **cuetrait_wtrend_maintext_Qrubra.pdf**
- cue_slope_plot.R: plots cue responses for each trait with crosses showing the species level 50% UI

3.**studySpHist.pdf**
- studyVsSpecies.R: creates histogram comparing species and study level variation

4. **FourTraitFit_37spp_wp.png**
- traitFit.R: creates plot comparing raw data with model estimates and trait means
  
5. **PCA_geometricmean.pdf**
- traitors_PCA.R



