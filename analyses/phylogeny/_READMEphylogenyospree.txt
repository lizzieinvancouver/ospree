Started 10 November 2022

phylo_ospree_compact4.R right now has the current models *and* Lizzie's attempts at using Betancourt model, which she should move. 


comments by Nacho nov 2022

phylo_ospree_compact4_angiogymno_updateprior.R has the last version of the model, including removal of outlier species and correcting Acer_pseudolatauns for Acer_pseudoplatanus

To get the model results three potential stan files can be fed to the model:

stan/uber_threeslopeintercept_modified_cholesky_updatedpriors.stan
stan/uber_threeslopeintercept_modified_cholesky_updatedpriors_lamb0.stan
stan/uber_threeslopeintercept_modified_cholesky_updatedpriors_lamb1.stan


To plotting results and getting model variances and such use

phylo_ospree_plots.R


