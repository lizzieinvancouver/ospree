## 22 June 2017 - Cat
## Checking where lost photoperiod data is going

# Load from bb_cleanmergeall.R

amb<-d[which(d$photoperiod_day=="ambient"),]
unique(amb$datasetID)
# "charrier11"  "fu13"        "gomory15"    "gunderson12" "hawkins12"   "lamb37"      "linkosalo06"
# "morin10"     "partanen98"  "schnabel87"  "sonsteby14"  "chavarria09" "falusi96"    "guak98"     
# "jones12"     "rinne97"     "sanzperez10"

blank<-d[which(d$photoperiod_day==''),]
unique(blank$datasetID)
# "gianfagna85" "nishimoto95" "falusi96" 

## charrier11: Table 1, Exp 1 - under long day conditions at 25 degC forcing
d$photoperiod_day[which(d$datasetID=="charrier11" & d$figure.table..if.applicable.== "table 1")] <- 16
d$photoperiod_night[which(d$datasetID=="charrier11" & d$figure.table..if.applicable.== "table 1")] <- 8

# fu13: "The experimental climate-controlled chambers were sunlit, facing south with a 
# transparent polycarbonate plate (4 mm thick) at the top (light absorption = 15% (De Boeck et al., 2006)).
# "winter 2009 to spring 2010 and winter 2010 to spring 2011", too vague to calculate?

# gomory15: "In spring 2011, budburst phenology was scored on each plant at approx. 
# two-week intervals between March 1 and June 29"
### Should we use calculations? If we decide to make it binary maybe?
## Use two growing.lats
low.initial<-daylength(48.44820, "2011-03-01") # 10.9715
low.start<-daylength(48.44820, "2011-04-14") # 13.56398
low.end<-daylength(48.44820, "2011-04-26") # 14.23436
gom.photo<-mean(10.9715, 13.56398, 14.23436) # 10.9715 -> 11 hr photo

high.initial<-daylength(49.01791, "2011-03-01") # 10.9490
high.start<-daylength(49.01791, "2011-05-06") # 14.8131
high.end<-daylength(49.01791, "2011-05-21") # 15.5071
gom.high<-mean(10.9490, 14.8131, 15.5071) #10.949 -> 11 hr photo

# both growing latitudes have the same photoperiod...
d$photoperiod_day[which(d$datasetID=="gomory15")] <- 11
d$photoperiod_night[which(d$datasetID=="gomory15")] <- 13

# gunderson12: clear panels on growth chamber kept outside, use coordinates and day of year from figure 2 to calculate?
oak.start<-daylength(35.931428, "2002-03-01") # 11.3705
oak.end<-daylength(35.931428, "2002-05-05") # 13.7482

gund.photo<-mean(11.3705, 13.7482) #11.3705 -> 11 hr photo

d$photoperiod_day[which(d$datasetID=="gunderson12")] <- 11
d$photoperiod_night[which(d$datasetID=="gunderson12")] <- 13


#hawkins12: http://weatherimages.org/latlon-sun.html - daylength values
south.hawk<-daylength(48.43, "1998-04-07") # 13.1584
central.hawk<-daylength(50.68, "1998-04-27") # 14.4765
north.hawk<-daylength(53.90, "1998-05-03") # 15.1800

d$photoperiod_day[which(d$datasetID=="hawkins12" & d$growing.lat==48.43)] <- 13
d$photoperiod_night[which(d$datasetID=="hawkins12" & d$growing.lat==48.43)] <- 11
d$photoperiod_day[which(d$datasetID=="hawkins12" & d$growing.lat==50.68)] <- 15
d$photoperiod_night[which(d$datasetID=="hawkins12" & d$growing.lat==50.68)] <- 9
d$photoperiod_day[which(d$datasetID=="hawkins12" & d$growing.lat==53.90)] <- 15
d$photoperiod_night[which(d$datasetID=="hawkins12" & d$growing.lat==53.90)] <- 9

# lamb37: not enough information

# morin10: not enough information

# partanen98: can determine from Figure 2!
# before extracting data, photoperiod ranges from 8-11 hrs

# schnabel87: ambient entries are in field comparisons to experiment - do we want to keep?
sch.1<-daylength(46.206, "1984-10-11") # 11.1698
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="11-Oct-1984")] <- 11
sch.2<-daylength(46.206, "1984-10-25") # 10.4364
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="25-Oct-1984")] <- 10
sch.3<-daylength(46.206, "1984-11-8") # 9.7562
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="8-Nov-1984")] <- 10
sch.4<-daylength(46.206, "1984-11-22") # 9.1797
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="22-Nov-1984")] <- 9
sch.5<-daylength(46.206, "1984-12-06") # 8.7750
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="6-Dec-1984")] <- 9
sch.6<-daylength(46.206, "1984-12-20") # 8.6099
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="20-Dec-1984")] <- 9
sch.7<-daylength(46.206, "1985-1-3") # 8.7066
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="3-Jan-1985")] <- 9
sch.8<-daylength(46.206, "1985-1-17") # 9.0567
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="17-Jan-1985")] <- 9
sch.9<-daylength(46.206, "1985-1-31") # 9.6008
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="31-Jan-1985")] <- 10
sch.10<-daylength(46.206, "1985-2-14") # 10.2680
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="14-Feb-1985")] <- 10
sch.11<-daylength(46.206, "1985-2-28") # 11.0016
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="28-Feb-1985")] <- 11
sch.12<-daylength(46.206, "1985-3-14") # 11.7643
d$photoperiod_night[which(d$datasetID=="schnabel87" & d$photoperiod_day=="ambient" & d$fieldsample.date=="14-Mar-1985")] <- 12




# sonsteby14: Figure 5 has ambient, should be changed to 24 hour photoperiod
d$photoperiod_day[which(d$photoperiod_day=="ambient" & d$figure.table..if.applicable.=="fig 5" & d$datasetID=="sonsteby14")] <- 24
d$photoperiod_night[which(d$photoperiod_day==24 & d$figure.table..if.applicable.=="fig 5" & d$datasetID=="sonsteby14")] <- 0

# chavarria09: not enough information and cannot calculate using geosphere package

# falusi96: not enough information for experiment 1 & 2
# exp 1 is in ambient nursery - only manipulated provenance latitude
# exp 2 - not enough information

# guak98: Should be able to calculate
guak.start<-daylength(44.5659, "1996-2-17") # 10.5147
guak.end<-daylength(44.5659, "1996-4-8") # 13.1222

guak.photo<-mean(10.5147,13.1222) # 10.5147 -> 11 hr photo

d$photoperiod_day[which(d$datasetID=="guak98")] <- 11
d$photoperiod_night[which(d$datasetID=="guak98")] <- 13

# jones12: not enough information

# rinne97: does not specify, not even ambient. Can't fix.

# sanzperez10: uses ambient light but also uses shade cloth to manipulate percentage of sunlight... not sure if we can calculate

# gianfagna85: not enough information

# nishimoto85: not enough information














