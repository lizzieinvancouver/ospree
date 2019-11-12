# This is code that is SOURCED! #
# See countintxns.R
# I set up most of the code in countintxns.R to need numeric values for photo and force
# So, here I make them numeric to make the code run, but the absolute numbers are not correct
# But, there is tons of other code that does that (just not yet for the whole dataset).

unique(datsm$forcetemp)
# the "" seem to be true no forcetemp, will set to ambient
# for the rest I am setting ambient or mean daily to 10 ... THIS IS NOT ACCURATE!
# but, it works for here since we just need each treatment to be unique (when it should be) and numeric
datsm$force.org <- datsm$forcetemp
datsm$forcetemp[datsm$forcetemp==""] <- 10
datsm$forcetemp[datsm$forcetemp=="meandaily"] <- 10
datsm$forcetemp[datsm$forcetemp=="ambient"] <- 10
datsm$forcetemp[datsm$forcetemp=="ambient+1"] <- 11
datsm$forcetemp[datsm$forcetemp=="ambient+2"] <- 12
datsm$forcetemp[datsm$forcetemp=="ambient+3"] <- 13
datsm$forcetemp[datsm$forcetemp=="ambient+4"] <- 14
datsm$forcetemp[datsm$forcetemp=="ambient+5"] <- 15
datsm$forcetemp[datsm$forcetemp=="ambient+6"] <- 16
datsm$forcetemp[datsm$forcetemp=="ambient + 4"] <- 14
datsm$forcetemp[datsm$forcetemp=="ambient + .7"] <- 10.7
datsm$forcetemp[datsm$forcetemp=="ambient + 1.5"] <- 11.5
datsm$forcetemp[datsm$forcetemp=="ambient + 4.9"] <- 14.9
datsm$forcetemp[datsm$forcetemp=="ambient + 3"] <- 13
datsm$forcetemp[datsm$forcetemp=="18-27 (20 average)"] <- 20
datsm$forcetemp[datsm$forcetemp=="7-27.5"] <- 17.25
datsm$forcetemp[datsm$forcetemp=="22-27"] <- 24.5
datsm$forcetemp[datsm$forcetemp=="0 ramped up 3 degrees every 6 days"] <- 15
datsm$forcetemp[datsm$forcetemp=="mean of 9, 12, 15"] <- 12

unique(datsm$forcetemp_night)
datsm$forcetemp_night[datsm$forcetemp_night==""] <- 10
datsm$forcetemp_night[datsm$forcetemp_night=="meandaily"] <- 10
datsm$forcetemp_night[datsm$forcetemp_night=="ambient"] <- 10
datsm$forcetemp_night[datsm$forcetemp_night=="ambient+1"] <- 11
datsm$forcetemp_night[datsm$forcetemp_night=="ambient+2"] <- 12
datsm$forcetemp_night[datsm$forcetemp_night=="ambient+3"] <- 13
datsm$forcetemp_night[datsm$forcetemp_night=="ambient+4"] <- 14
datsm$forcetemp_night[datsm$forcetemp_night=="ambient+5"] <- 15
datsm$forcetemp_night[datsm$forcetemp_night=="ambient+6"] <- 16
datsm$forcetemp_night[datsm$forcetemp_night=="ambient + 4"] <- 14
datsm$forcetemp_night[datsm$forcetemp_night=="ambient + .7"] <- 10.7
datsm$forcetemp_night[datsm$forcetemp_night=="ambient + 1.5"] <- 11.5
datsm$forcetemp_night[datsm$forcetemp_night=="ambient + 4.9"] <- 14.9
datsm$forcetemp_night[datsm$forcetemp_night=="ambient + 3"] <- 13
datsm$forcetemp_night[datsm$forcetemp_night=="18-27 (20 average)"] <- 20
datsm$forcetemp_night[datsm$forcetemp_night=="19-27"] <- 23
datsm$forcetemp_night[datsm$forcetemp_night=="7-27.5"] <- 17.25
datsm$forcetemp_night[datsm$forcetemp_night=="22-27"] <- 24.5
datsm$forcetemp_night[datsm$forcetemp_night=="10 ramped up 3 degrees every 6 days"] <- 15
datsm$forcetemp_night[datsm$forcetemp_night=="mean of 9, 12, 15"] <- 12
datsm$forcetemp_night[datsm$forcetemp_night=="10 then decreased 1.1C every two days until -13°C"] <- -1.5
datsm$forcetemp_night[datsm$forcetemp_night=="2 for 2 weeks"] <- 2
unique(datsm$forcetemp_night)

# for the rest I am setting ambient 12 hrs and constant to 14 hrs ... THIS IS NOT ACCURATE!
# but, it works for here since we just need each treatment to be unique (when it should be) and numeric
unique(datsm$photoperiod_day)
datsm$photo.org <- datsm$photoperiod_day
datsm$photoperiod_day[datsm$photoperiod_day==""] <- 12
datsm$photoperiod_day[datsm$photoperiod_day=="ambient"] <- 12
datsm$photoperiod_day[datsm$photoperiod_day=="constant"] <- 14
datsm$photoperiod_day[datsm$photoperiod_day=="14-9.5"] <- 11.75
datsm$photoperiod_day[datsm$photoperiod_day=="13-9.5"] <- 11.225

