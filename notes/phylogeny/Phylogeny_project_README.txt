OSPREE Phylogenetic analyses
############################

This file summarizes where we are at regarding phylogenetic analyses of OSPREE data.

## Folder: analyses/phylogeny/

The folder has a master code: 'models_phylo.R' that does the following:

	- sources source/bbstanleadin.phyla.R

	- applies BMRS phylogenetic models to data from Dan Flynn (it could be used for all ospree)

	- fits modelst to forcing, chilling and photoperiod, separately

	- plots relationships and PPCs to the analyses/phylogeny/figures/ folder

## Folder: ospree/data/phylogeny/

This folder contains code to subset a larger plant phylogeny (Zanne et al. 2014) to either the
ospree species or species in Flynn's paper. The folder contains three phylogenies:
- ospree.phylogeny.tre
- ospreeFlynn.phylogeny.tre
- Vascular_Plants_rooted.dated.tre

## Results so far
Looking at the figures it seems like the lambda parameter is somewhat large (~0.7) and 
thus, the response to each cue is phylogenetically structured but not according a Brownian Motion model (lambda=1)


Next steps:
############ 
(please add below the steps you think we still need)


* revisit the models (i.e. using all ospree data)

* fit different models of evolution for the response to each cue (e.g. looking at the slopes BB~cue?)

* try PWR and relate cues with traits in a phylogenetic context to check if we can identify
  suites of traits more strongly associated to a given cue at different parts of the phylogeny

* think about specific hypotheses-questions (e.g. do we expect responses to a given cue to be more associated
  to given traits in certain clades?; Do the BB ms. analyses change when accounting for phylogenetic non-independece?)

* ...




