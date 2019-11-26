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
datsm$force.unchanged <- datsm$forcetemp

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

# Alternative cleaning -- not numeric, but shorter and consistent
datsm$force.org[datsm$force.org==""] <- "not given"
datsm$force.org[datsm$force.org=="ambient + 4"] <- "ambient+4"
datsm$force.org[datsm$force.org=="ambient + .7"] <- "ambient+0.7"
datsm$force.org[datsm$force.org=="ambient + 1.5"] <- "ambient+1.5"
datsm$force.org[datsm$force.org=="ambient + 4.9"] <- "ambient+5"
datsm$force.org[datsm$force.org=="ambient + 3"] <- "ambient+3"
datsm$force.org[datsm$force.org=="0 ramped up 3 degrees every 6 days"] <- "0 ramped up 2X/week"

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
datsm$photo.unchanged <- datsm$photoperiod_day

datsm$photoperiod_day[datsm$photoperiod_day==""] <- 12
datsm$photoperiod_day[datsm$photoperiod_day=="ambient"] <- 12
datsm$photoperiod_day[datsm$photoperiod_day=="constant"] <- 14
datsm$photoperiod_day[datsm$photoperiod_day=="14-9.5"] <- 11.75
datsm$photoperiod_day[datsm$photoperiod_day=="13-9.5"] <- 11.225

## chilltemp
unique(datsm$chilltemp)
datsm$chilltemp.org <- datsm$chilltemp
datsm$chilltemp.unchanged <- datsm$chilltemp
datsm$chilltemp.org[datsm$chilltemp.org=="Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C"] <- "0.7 with 8 or 12 C interruptions"
datsm$chilltemp.org[datsm$chilltemp.org=="negative 23 to 13 degrees Celsius"] <- "-23 to -13 C"
datsm$chilltemp.org[datsm$chilltemp.org=="neg 3,2"] <- "-3 followed by 2 C"
