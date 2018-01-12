
#check the date of when these daily climate summary files were created in case they are older than you'd like:
#these files are used by the "clean_ambientforcingfromdailyclimate.R" script in step 5c
print(file.info("output/pmp/percbb_clim_pmpA.csv")$ctime)
print(file.info("output/pmp/percbb_clim_pmpB.csv")$ctime)
print(file.info("output/pmp/percbb_clim_pmpC.csv")$ctime)
print(file.info("output/pmp/percbb_clim_pmpD.csv")$ctime)
stop("Warning: this code in the next step relies on files that were created on the dates listed 
      above. If those dates are deemed too old by you, then you should rerun the 
     'bb_daily_dataprep.R' script (this script is slow).")