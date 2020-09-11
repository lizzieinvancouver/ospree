#################################################
############### Remove unused columns ###########
#################################################
columnstokeep <- c("datasetID","study", "genus", "species","provenance.lat", "forcetemp", "forcetemp_night",
                   "photoperiod_day", "response", "response.time", "chilltemp",
                  "chilldays", "fieldsample.date",
                   "force", "photo", "chill", "resp", "force_type",  "photo_type", "chill_type")

columnschillunits <- c("chill.hrs","chill.ports")
