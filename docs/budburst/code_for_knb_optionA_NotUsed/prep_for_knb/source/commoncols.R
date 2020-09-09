#################################################
############### Remove unused columns ###########
#################################################
columnstokeep <- c("datasetID","study", "genus", "species", "varetc", "woody", "forcetemp", "forcetemp_night",
                   "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", "chilltemp",
                  "chilldays", "fieldsample.date",
                  "provenance.lat", "force", "photo", "chill", "resp", "force_type",  "photo_type", "chill_type")

columnschillunits <- c("chill.hrs","chill.ports")

columnstokeepnvar <- c("datasetID","study", "genus", "species", "varetc", "woody", "forcetemp", "forcetemp_night",
                   "photoperiod_day", "response", "response.time", "Total_Chilling_Hours", "chilltemp",
                   "chilldays", "fieldsample.date",
                   "provenance.lat", "force", "photo", "chill", "resp", "force_type",  "photo_type", "chill_type",
                   "n","resp_error","error.type")
