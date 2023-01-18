# HARVARD FOREST CLIMATE DATA SINCE 1964

# Fill gaps in Shaler daily air temperature & precipitation
# with daily data from Amherst MA & Bedford MA downloaded
# from the National Climatic Data Center.

# Input files (English units):
#   hf001-01-daily-e.csv (Shaler data from HF Data Archive)
#   amherst-ma-1964-2002.csv (Amherst MA data from NCDC)
#   bedford-ma-1964-2002.csv (Bedford MA data from NCDC)

# Output file (metric units):
#   hf-shaler-gap-filled.csv

# Missing or questionable values in Shaler data are replaced with
# estimated values from Amherst MA or Bedford MA and flagged as "E".

# Emery R.Boose

# revised 20-Oct-2019


# Read Shaler data
ss <- read.csv("hf000-02-daily-e.csv", stringsAsFactors=FALSE)
ss$date <- as.Date(ss$date)

# Read Amherst MA data
aa <- read.csv("amherst-ma-1964-2002.csv", stringsAsFactors=FALSE)
names(aa)[1] <- "date"
aa$date <- as.Date(aa$date)
aa$airt <- (aa$airtmax + aa$airtmin)/2

# Read Bedford MA data
bb <- read.csv("bedford-ma-1964-2002.csv", stringsAsFactors=FALSE)
names(bb)[1] <- "date"
bb$date <- as.Date(bb$date)
bb$airt <- (bb$airtmax + bb$airtmin)/2

# Set Shaler questionable values to NA
index <- which(ss[ , "f.airt"] == "Q")
ss[index, "airt"] <- NA
ss[index, "f.airt"] <- "M"

index <- which(ss[ , "f.airtmax"] == "Q")
ss[index, "airtmax"] <- NA
ss[index, "f.airtmax"] <- "M"

index <- which(ss[ , "f.airtmin"] == "Q")
ss[index, "airtmin"] <- NA
ss[index, "f.airtmin"] <- "M"

index <- which(ss[ , "f.prec"] == "Q")
ss[index, "prec"] <- NA
ss[index, "f.prec"] <- "M"

# Merge data frames by date. Merge is necessary because there are 
# missing rows in the Amherst MA and Bedford MA data.
xx <- merge(ss, aa, by="date", all=TRUE)
zz <- merge(xx, bb, by="date", all=TRUE)

colnames(zz) <- c("date", "airt", "f.airt", "airtmax", "f.airtmax", "airtmin",
  "f.airtmin", "prec", "f.prec", "a.airtmax", "a.airtmin", "a.prec", "a.airt",
  "b.airtmax", "b.airtmin", "b.prec", "b.airt")

# Calculate annual differences & ratios for Amherst MA
zz2 <- zz

for (i in 1:nrow(zz2)) {
  if (is.na(zz2$airt[i])) zz2$a.airt[i] <- NA
  if (is.na(zz2$airtmax[i])) zz2$a.airtmax[i] <- NA
  if (is.na(zz2$airtmin[i])) zz2$a.airtmin[i] <- NA
  if (is.na(zz2$prec[i])) zz2$a.prec[i] <- NA
  
  if (is.na(zz2$a.airt[i])) zz2$airt[i] <- NA
  if (is.na(zz2$a.airtmax[i])) zz2$airtmax[i] <- NA
  if (is.na(zz2$a.airtmin[i])) zz2$airtmin[i] <- NA
  if (is.na(zz2$a.prec[i])) zz2$prec[i] <- NA
}

a.airt.diff <- mean(zz2$airt, na.rm=TRUE) - mean(zz2$a.airt, na.rm=TRUE)
a.airtmax.diff <- mean(zz2$airtmax, na.rm=TRUE) - mean(zz2$a.airtmax, na.rm=TRUE)
a.airtmin.diff <- mean(zz2$airtmin, na.rm=TRUE) - mean(zz2$a.airtmin, na.rm=TRUE)
a.prec.ratio <- sum(zz2$prec, na.rm=TRUE ) / sum(zz2$a.prec, na.rm=TRUE)

# Calculate annual differences & ratio for Bedford MA
zz2 <- zz

for (i in 1:nrow(zz2)) {
  if (is.na(zz2$airt[i])) zz2$b.airt[i] <- NA
  if (is.na(zz2$airtmax[i])) zz2$b.airtmax[i] <- NA
  if (is.na(zz2$airtmin[i])) zz2$b.airtmin[i] <- NA
  if (is.na(zz2$prec[i])) zz2$b.prec[i] <- NA
  
  if (is.na(zz2$b.airt[i])) zz2$airt[i] <- NA
  if (is.na(zz2$b.airtmax[i])) zz2$airtmax[i] <- NA
  if (is.na(zz2$b.airtmin[i])) zz2$airtmin[i] <- NA
  if (is.na(zz2$b.prec[i])) zz2$prec[i] <- NA
}

b.airt.diff <- mean(zz2$airt, na.rm=TRUE) - mean(zz2$b.airt, na.rm=TRUE)
b.airtmax.diff <- mean(zz2$airtmax, na.rm=TRUE) - mean(zz2$b.airtmax, na.rm=TRUE)
b.airtmin.diff <- mean(zz2$airtmin, na.rm=TRUE) - mean(zz2$b.airtmin, na.rm=TRUE)
b.prec.ratio <- sum(zz2$prec, na.rm=TRUE ) / sum(zz2$b.prec, na.rm=TRUE)

# Replace missing values with estimated values. Use Amherst MA data
# if available; otherwise use Bedford MA data.
for (i in 1:nrow(zz)) {
	if (is.na(zz$airt[i])) {
		if (!is.na(zz$a.airt[i])) {
			zz$airt[i] <- zz$a.airt[i] + a.airt.diff
			zz$f.airt[i] <- "E"
		} else if (!is.na(zz$b.airt[i[]])) {
			zz$airt[i] <- zz$b.airt[i] + b.airt.diff
			zz$f.airt[i] <- "E"		
		}
	}
  
	if (is.na(zz$airtmax[i])) {
		if (!is.na(zz$a.airtmax[i])) {
			zz$airtmax[i] <- zz$a.airtmax[i] + a.airtmax.diff
			zz$f.airtmax[i] <- "E"
		} else if (!is.na(zz$b.airtmax[i[]])) {
			zz$airtmax[i] <- zz$b.airtmax[i] + b.airtmax.diff
			zz$f.airtmax[i] <- "E"		
		}
	}

	if (is.na(zz$airtmin[i])) {
		if (!is.na(zz$a.airtmin[i])) {
			zz$airtmin[i] <- zz$a.airtmin[i] + a.airtmin.diff
			zz$f.airtmin[i] <- "E"
		} else if (!is.na(zz$b.airtmin[i[]])) {
			zz$airtmin[i] <- zz$b.airtmin[i] + b.airtmin.diff
			zz$f.airtmin[i] <- "E"		
		}
	}

	if (is.na(zz$prec[i])) {
		if (!is.na(zz$a.prec[i])) {
			zz$prec[i] <- zz$a.prec[i] * a.prec.ratio
			zz$f.prec[i] <- "E"
		} else if (!is.na(zz$b.prec[i[]])) {
			zz$prec[i] <- zz$b.prec[i] * b.prec.ratio
			zz$f.prec[i] <- "E"		
		}
	}
}

# Remove unnecessary columns
zz[ , c("a.airtmax", "a.airtmin", "a.prec", "a.airt", "b.airtmax", "b.airtmin",
	"b.prec", "b.airt")] <- list(NULL)

# Convert to metric units
zz[ , c("airt", "airtmax", "airtmin")] <- round((zz[ , c("airt", "airtmax", "airtmin")] - 32) / 1.8, 1)
zz[ , "prec"] <- round(zz[ , "prec"] * 25.4, 3)

# Save to file
write.csv(zz, "hf-shaler-gap-filled.csv", row.names=FALSE)

