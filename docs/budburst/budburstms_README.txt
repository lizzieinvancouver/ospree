Budburst Manuscript README 
OSPREE
Observed Spring Phenology Responses in Éxperimental Environments

<><><><><><><><><><><><><><><><<><><><><><><><><><><><><><><><><><><><>><><><><>
Files used to create the main budburst manuscript:
Ettinger, A.K., Chamberlain, C.J., Morales-Castilla, I. et al. 
Winter temperatures predominate in spring phenological responses to warming. 
Nat. Clim. Chang. 10, 1137–1142 (2020). https://doi.org/10.1038/s41558-020-00917-3

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#Figure 1: conceptual figure,created in Adobe Illustrator by Nacho primarily 
	All versons located in ospree/docs/buddburst/figures/concept; 
	filename in published version is Fig_bbconcept_dormant_V8.png

#Figure 2: filename: analyses/bb_analysis/figures/muplotspcompexprampfputah_z.pdf
	Built in analyses/bb_analysis/models_stan_plotting_justmuplot.R

#Figure 3: filename: analyses/bb_analysis/figures/bbmod_2d3dplot_utah_withPEP.pdf
	Built in analyses/bb_analysis/models_stan_plotting_mu_ests_chilling.R
	(includes both 2D and 3D panels)

#Figure 4: filename: analyses/bb_analysis/figures/forecasting/tempforecastbothspp_1_7_degwarm3D_utah.png
	Built in analyses/bb_analysis/forecasting/forecast_changebb.R 

In the text ....
# Sample size, min max treatments come from get_n.R
# Numbers of chilling treatments come from tables prepared in ospree/analyses/
limitingcues/countintxns/countintxns.R

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
Files used to create figures in the Extended Data
<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#Figure ED1: Map of days to budburst experiments in the OSPREE database. 
	file name: OSPREECOORDSwcoords.png
	Built by Nacho, using output/bbmapdata.csv 
	which is built in analyses/bb_analysis/studydesignplotsbb.R

#Figure ED2: Map of maximum and minimum chilling, forcing, and photoperiod treatments
	file name: minmaxtreatments_COORDS.png
	Built by Nacho, putting together map (using output/jointdatamsp.csv)
	and plots of min/max treatments (minforce_scatt.png, maxforce_scatt.png,
	minphoto_scatt.png, maxphoto_scatt.png,minchilling_scatt.png, maxchill_scatt.png)
	built with analyses/bb_analysis/Getmax_mintreat_ospree_chk.R

#Figure ED3: The diversity of study designs used in analyses.
	file names: studydesign_heat3panelallsppmodel.pdf, studydesign_heat3panelmainmodel.pdf
	built in analyses/bb_analysis/studydesignplotsbb.R

#Figure ED4: Chilling accumulates differently in experiments vs natural systems
	file name: exp_vs_field_chill_withwarmingcols.pdf
	built in analyses/bb_analysis/exp_vs_field_chill.R

#Figure ED5: Estimates for effects of chilling exceeded forcing, photoperiod, latitude
	file name: latanalysis_spcom_expramp_fp.pdf
	built in analyses/lat_analysis/plotting_lat_models.R

#Figure ED6: Forecasted changes in chilling and spring phenology with PEP725 database.
	file name: heatmapsbetpepfinalarrows.png
	built in analyses/bb_analysis/forecasting/forecast_heatmaps.R
	then improved dramatically by Nacho

#Figure ED7: Budburst is affected by climate-change induced shifts in photoperiod
	file name: fagsyl_3lats.pdf
	built in analyses/bb_analyses/forecast/forecast_changebb_lat.R

#Figure ED8: Declining sensitivities observed in long-term European data
	filename: peprealandsims.pdf
	Built in analyses/bb_analysis/PEP_climate/comparetopepsims.R

#Figure ED9: Day of leafout varies with chilling, growing degree-days, and MST.
	filename: betpen_multruns_utahgddmat.pdf
	Built in analyses/bb_analysis/PEP_climate/betpen_chillandgdd_tg_forsims.R

#Figure ED10: GDD versus chill units at the time of budburst from OSPREE
	filename: gddbyutah_pepspp.pdf}
	Built in analyses/bb_analysis/models_stan_pepspp.R

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
Files used to create tables and figures in the supplemental materials
<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#Table S1: Dataset names and references for papers in the OSPREE database
	Built in analyses/bb_analysis/maketables.forsupp/make_sp_table_forsupp.R

#Table S2: Species included in the OSPREE database
	Built in analyses/bb_analysis/maketables.forsupp/mod_table.R

#Table S3: Number of studies testing for interactions between chilling, forcing, photoperiod
	Built in analyses/bb_analysis/maketables.forsupp/mod_table.R

#Table S4: Utah chill units

#Table S5: Estimates from models fit with standardized predictors

#Table S6: Estimates from models fit with predictors on their natural scales	

#Table S7: Estimates from model fit to data to which within group centering was applied

#Table S8: Estimates from model fit to data from experiments with two interactions between cues 

#Table S9: Estimates from model fit to data excluding Zohner

#Table S10: Estimates from latitude model fit with standardized predictors
	Built in analyses/bb_analysis/maketables.forsupp/mod_table_lat.R

#Tabe S11: Estimates from chilling study design model fit with standardized predictors
	Built in analyses/bb_analysis/weinberger.R

#Table S12: Estimates from the life stage model fit with standardized predictors.
	life stage model fit in analyses/bb_analysis/models_stan_newbbmods.R

#Table S13: Locations and pre-warming winter and spring conditions
	Built in analyses/bb_analysis/maketables.forsupp/clim_table.R

#Table S14: Absolute sliding time window results
	Built by Cat from csv files generated in analyses/bb_analysis/pep_sims/simmonds_slidingwin/sw_simmonds.R

#Figure S1: Days over which chilling and forcing were accumulate
	filename:analyses/bb_analysis/figures/chilldaysforcedays.pdf
	Built in:analyses/bb_analysis/chill_force_plot_rev1.R

#Figure S2: Estimates of budburst forcing temperatures and estimated chilling 3D with constant chilltemp
	filename:analyses/bb_analysis/figures/bbmod_3dplot_utah.pdf
	Built in: analyses/bb_analysis/forecasting/forecast_changebb.R 

#Figure S3: Chilling study design affects estimates of major cues
	filename: analyses/figures/weinberger_MU_4supp.pdf
	Built in analyses/bb_analysis/weinberger.R

#Figure S4: Implications of warming on budburst timing varies across species and sites,
	filename:analyses/bb_analysis/figures/forecasting/tempforecastbothspp_1_7_degwarm.pdf}
	Built in analyses/bb_analysis/forecasting/forecast_changebb.R 
#Figure S5: Implications of global warming on chilling vary by site
	filename:analyses/bb_analysis/figures/forecasting/chillforecast_bothspp_PEP_utah.pdf
	Built in analyses/bb_analysis/forecasting/forecast_changebb.R 


In the text ....
# Numbers of chilling treatments, numbers of studies that tested interactions between cues and thermoperiodicity numbers come from tables prepared in ospree/analyses/limitingcues/countintxns/countintxns.R

# What is budburst definitions and numbers come from ospree/analyses/input/whatisbudburst.csv and ospree/data/whatis_budburst.xlsx

