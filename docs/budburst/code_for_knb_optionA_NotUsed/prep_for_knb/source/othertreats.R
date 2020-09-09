#################################################
############### Other Treatments ################
###### (not chilling, forcing, photoperiod) #####
#################################################
##### Reviews some rarely used columns with #####  
#### treatments not used in bb model; removes ###
## all rows that include non-ambient treatments #
#################################################

othercols <- c("dormancy_induction_temp_day", "dormancy_induction_temp_night",
    "dormancy_induction_days", "dormancy_induction_photoperiod_day",
    "dormancy_induction_photoperiod_night", "dormancy_induction_days.1",
    "freeze.treatment.time", "freeze.treatment.photoperiod_day",
    "freeze.treatment.photoperiod_night", "freeze.treatment.temp_day",
    "freeze.treatment.temp_night")

rows.othertreats <- which(bb.noNA$dormancy_induction_temp_day!=""| bb.noNA$dormancy_induction_temp_night!=""|
    bb.noNA$dormancy_induction_days!=""| bb.noNA$dormancy_induction_photoperiod_day!=""|
    bb.noNA$dormancy_induction_photoperiod_night!=""| bb.noNA$dormancy_induction_days.1!=""|
    bb.noNA$freeze.treatment.time!=""| bb.noNA$freeze.treatment.photoperiod_day!=""|
    bb.noNA$freeze.treatment.photoperiod_night!=""| bb.noNA$freeze.treatment.temp_day!=""|
    bb.noNA$freeze.treatment.temp_night!="")

# Three main columns appear to cover all the other columns
main.othertreats <- c(which(bb.noNA$dormancy_induction_temp_day!=""|
    bb.noNA$freeze.treatment.temp_day!=""| bb.noNA$dormancy_induction_days!=""))
setdiff(rows.othertreats, main.othertreats) # should be zero

# should also lead to zero rows ...
check <- bb.noNA[setdiff(rows.othertreats, main.othertreats),]
check[,othercols]

table(bb.noNA$dormancy_induction_temp_day) # include ambient?
table(bb.noNA$freeze.treatment.temp_day) # include ambient?
table(bb.noNA$dormancy_induction_days) # include ambient?

dat.wothertreats <- bb.noNA[main.othertreats,]

unique(dat.wothertreats$datasetID) 

amb.othertreats <- c(which(bb.noNA$dormancy_induction_temp_day=="ambient"|
    bb.noNA$freeze.treatment.temp_day=="ambient"  | bb.noNA$dormancy_induction_days=="ambient"))

# Delete rows with other treatments that are not set to ambient 
#length(main.othertreats)
#length(amb.othertreats)
#length(main.othertreats[!main.othertreats %in% amb.othertreats])
othertreats.delete <- main.othertreats[!main.othertreats %in% amb.othertreats]
