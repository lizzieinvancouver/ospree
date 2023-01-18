# HARVARD FOREST CLIMATE DATA SINCE 1964

# Create a file of daily air temperature and precipitation from the Shaler
# and Fisher met stations.

# Use overlapping measurements (2001-07-01 to 2002-06-30) to adjust
# Shaler data to match Fisher data.

# For the overlap period, set estimated values in Shaler data to NA.
# Use Fisher hourly data to calculate daily values to match Shaler observation
# period (8 am to 8 am).  Also use Fisher hourly data to estimate difference 
# between 24-hour max & min air temps and max & min hourly averages over 
# 24-hour period.  Set Fisher values to NA if Shaler values are NA.

# Calculate adjustments to Shaler data using annual means of daily temperature
# and annual totals of daily precipitation.

# Combine Shaler data (1964-01-01 to 2001-02-10) and Fisher data
# (since 2001-02-11).

# Values in the resulting dataset are flagged as "E" if estimated from nearby
# stations (Shaler & Fisher) or from hourly or 15-minute data (Fisher).

# Input files:
#   hf-shaler-gap-filled.csv (Shaler, metric units)
#   hf001-06-daily-m.csv (Fisher, metric units)
#   hf001-08-hourly-m.csv (Fisher, metric units)

# Output files:
#   hf300-05-daily-m.csv (combined, metric units)
#   hf300-06-daily-e.csv (combined, English units)
#   hf-shaler-fisher-overlap.csv (combined, metric units)

# Emery R.Boose

# rev. 20-Oct-2019


# Overlap dates
start.overlap <- "2001-07-01"
end.overlap <- "2002-06-30"

# Read data files
ss <- read.csv("hf-shaler-gap-filled.csv", stringsAsFactors=FALSE)
ff <- read.csv("hf001-06-daily-m.csv", stringsAsFactors=FALSE)
hh <- read.csv("hf001-08-hourly-m.csv", stringsAsFactors=FALSE)

ss$date <- as.Date(ss$date)
ff$date <- as.Date(ff$date)

# Get Shaler overlap data
ss.over <- ss[(ss$date >= start.overlap & ss$date <= end.overlap), ]

# Convert Shaler estimated values to NA
index <- which(ss.over[ , "f.airt"] == "E")
ss.over[index, "airt"] <- NA

index <- which(ss.over[ , "f.airtmax"] == "E")
ss.over[index, "airtmax"] <- NA

index <- which(ss.over[ , "f.airtmin"] == "E")
ss.over[index, "airtmin"] <- NA

index <- which(ss.over[ , "f.prec"] == "E")
ss.over[index, "prec"] <- NA

# Remove unnecessary columns
ss.over <- ss.over[ , c("date", "airt", "airtmax", "airtmin", "prec")]

# Calculate daily Fisher values for 8 am to 8 am period
hh$date <- substr(hh$datetime, 1, 10)
hh$date <- as.Date(hh$date)

hh$date2 <- hh$date

hh$hour <- substr(hh$datetime, 12, 13)
hh$hour <- as.numeric(hh$hour)

# Remove unnecessary rows
hh <- hh[hh$date >= "2001-06-01" & hh$date <= "2002-08-01", ] 
  
# Set to next day if after 8 am
for (i in 1:nrow(hh)) {
  if (hh$hour[i] > 8) hh$date2[i] <- hh$date2[i] + 1  
}

# Aggregate by 8 am days
ff.airt <- aggregate(airt~date2, data=hh, FUN=mean, na.action=na.pass, na.rm=TRUE)
ff.airtmax <- aggregate(airt~date2, data=hh, FUN=max, na.action=na.pass, na.rm=TRUE)
ff.airtmin <- aggregate(airt~date2, data=hh, FUN=min, na.action=na.pass, na.rm=TRUE)
ff.prec <- aggregate(prec~date2, data=hh, FUN=sum, na.action=na.pass, na.rm=TRUE)

# Get Fisher overlap data
ff.over <- data.frame(ff.airt$date2, ff.airt$airt, ff.airtmax$airt, ff.airtmin$airt, 
  ff.prec$prec)

colnames(ff.over) <- c("date", "airt", "airtmax", "airtmin", "prec")
ff.over$date <- as.Date(ff.over$date)

ff.over <- ff.over[(ff.over$date >= start.overlap & ff.over$date <= end.overlap), ]

# Get hourly max & min air temperatures
dd.airtmax <- aggregate(airt~date, data=hh, FUN=max, na.action=na.pass, na.rm=TRUE)
dd.airtmin <- aggregate(airt~date, data=hh, FUN=min, na.action=na.pass, na.rm=TRUE)

dd <- data.frame(dd.airtmax$date, dd.airtmax$airt, dd.airtmin$airt)

colnames(dd) <- c("date", "airtmax", "airtmin")
dd$date <- as.Date(dd$date)

dd <- dd[(dd$date >= start.overlap & dd$date <= end.overlap), ]

# Get 24-hour max & min air temperatures
ee <- ff[ff$date >= start.overlap & ff$date <= end.overlap, ]
ee <- ee[, c("date", "airtmax", "airtmin")]

# Adjust max & min air temperatures
airtmax.adj <- mean(ee$airtmax, na.rm=TRUE) - mean(dd$airtmax, na.rm=TRUE)
airtmin.adj <- mean(ee$airtmin, na.rm=TRUE) - mean(dd$airtmin, na.rm=TRUE)

ff.over$airtmax <- ff.over$airtmax + airtmax.adj
ff.over$airtmin <- ff.over$airtmin + airtmin.adj

# Calculate mean air temp from min & max
ss.over$airt <- (ss.over$airtmin + ss.over$airtmax) / 2
ff.over$airt <- (ff.over$airtmin + ff.over$airtmax) / 2

# Set Fisher value to NA if Shaler value is NA
index <- which(is.na(ss.over[ , "airt"]))
ff.over[index, "airt"] <- NA

index <- which(is.na(ss.over[ , "airtmax"]))
ff.over[index, "airtmax"] <- NA

index <- which(is.na(ss.over[ , "airtmin"]))
ff.over[index, "airtmin"] <- NA

index <- which(is.na(ss.over[ , "prec"]))
ff.over[index, "prec"] <- NA

# Calculate differences & ratios
airt.diff <- mean(ff.over$airt, na.rm=TRUE) - mean(ss.over$airt, na.rm=TRUE)
airtmax.diff <- mean(ff.over$airtmax, na.rm=TRUE) - mean(ss.over$airtmax, na.rm=TRUE)
airtmin.diff <- mean(ff.over$airtmin, na.rm=TRUE) - mean(ss.over$airtmin, na.rm=TRUE)
prec.ratio <- sum(ff.over$prec, na.rm=TRUE ) / sum(ss.over$prec, na.rm=TRUE)

# Get Shaler data
end.shaler <- "2001-02-10"

ss.out  <- ss[ss$date <= end.shaler, ]
ss.out <- ss.out[ , c("date", "airt", "f.airt", "airtmax", "f.airtmax", "airtmin",
	"f.airtmin", "prec", "f.prec")]

# Apply adjustments to Shaler data
ss.out$airt    <- round(ss.out$airt + airt.diff, 1)
ss.out$airtmax <- round(ss.out$airtmax + airtmax.diff, 1)
ss.out$airtmin <- round(ss.out$airtmin + airtmin.diff, 1)
ss.out$prec    <- round(ss.out$prec * prec.ratio, 1)

# Get Fisher data
start.fisher <- "2001-02-11"

ff.out  <- ff[(ff$date >= start.fisher), ]
ff.out <- ff.out[ , c("date", "airt", "f.airt", "airtmax", "f.airtmax", "airtmin",
	"f.airtmin", "prec", "f.prec")]

# Combine datasets
xxm <- rbind(ss.out, ff.out)

# Convert to English units
xxe <- xxm

xxe[ , c("airt", "airtmax", "airtmin")] <- round(xxe[ , c("airt", "airtmax", "airtmin")] * 1.8 + 32, 1)
xxe[ , "prec"] <- round(xxe[ , "prec"] / 25.4, 2)

# Save data to file
write.csv(xxm, "hf300-05-daily-m.csv", row.names=FALSE)
write.csv(xxe, "hf300-06-daily-e.csv", row.names=FALSE)

# Create data frame for overlap data
zz <- as.data.frame(cbind(as.character(ss.over$date), ss.over$airt, ff.over$airt, 
  ss.over$airtmax, ff.over$airtmax, ss.over$airtmin, ff.over$airtmin, ss.over$prec, 
  ff.over$prec))

colnames(zz) <- c("date", "ss.airt", "ff.airt", "ss.airtmax", "ff.airtmax", 
  "ss.airtmin", "ff.airtmin", "ss.prec", "ff.prec")

zz$date <- as.Date(zz$date)

write.csv(zz, "hf-shaler-fisher-overlap.csv", row.names=FALSE)

