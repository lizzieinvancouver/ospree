##########################################################################
# 
# Author: Mirco Migliavacca (version 1 2012/5/26)
# SCRIPT FOR THE EXTRACTION OF SPRING PHENOPHASES FROM HF DATASET
#
# Adapted by Emery R. Boose 2015/5/4 to use current file formats
# Minor updates ERB 2019/5/7
#
# Input file:
#     hf003-03-spring.csv
#
# Output files:
#     hf003-05-spring-mean-ind.csv (dates by year and tree)
#     hf003-06-spring-mean-spp.csv (dates by year and species)
# 
# Variables:
#     bb = budbreak
#     l75 = 75% leaf development
#     fbb = flowering budbreak
#     fopn = flowers open
#     sd.bb,sd.l75,sd.fbb,sd.fopn: standard deviations of dates
#
##########################################################################

setwd("/Users/lizzie/Desktop/HF/phenology") # Lizzie added

spring <- function(dat, treecode) {	

  subdat <- subset(dat, tree.id==treecode)
  niterp <- 200000

  # start yearly loop	
  years <- unique(subdat$year)
  nyears <- length(years)
  i <- 1

  for (n in years) {
    #extract year		
    ydat <- subset(subdat, year==n)
		
    ifelse(i==1, tree.id <- treecode, tree.id <- c(tree.id, treecode))
		
    ifelse(i==1, species <- unlist(strsplit(treecode, "-"))[1], 
      species <- c(species, unlist(strsplit(treecode, "-"))[1]))
		
    if (length(ydat$bbrk[is.na(ydat$bbrk)==FALSE]) > 2) { 
      interp <- approx(ydat$doy, ydat$bbrk, n=niterp)
	
      ifelse(i==1, bb<-interp$x[interp$y >= 50][1], 
        bb <- c(bb, interp$x[interp$y >= 50][1]))	# 50% development
		}
		
    if (length(ydat$bbrk[is.na(ydat$bbrk)==FALSE]) <= 2) { 
      ifelse(i==1, bb <- NA, bb <- c(bb, NA))	# NA if values cannot be calculated
    }
		
    if (length(ydat$l75[is.na(ydat$l75)==FALSE]) > 2) { 
      interp <- approx(ydat$doy, ydat$l75, n=niterp)
      ifelse(i==1, l75 <- interp$x[interp$y >= 50][1],
        l75 <- c(l75, interp$x[interp$y >= 50][1]))	# 50% development (max 80)
    }
		
    if (length(ydat$l75[is.na(ydat$l75)==FALSE]) <= 2) { 
      ifelse(i==1, l75 <- NA, l75 <- c(l75, NA))	# NA if values cannot be calculated
    }
		
    if (length(ydat$fbud[is.na(ydat$fbud)==FALSE]) > 0) { 	
      day1 <- which(ydat$fbud=="BB")[1]	
      if (is.na(day1)==TRUE) { 
        ifelse(i==1, fbb <- NA, fbb <- c(fbb, NA))	# NA if values cannot be calculated
      }
			
      if (is.na(day1)==FALSE) { 
        ifelse(day1==1, tmpday <- ydat$doy[day1], tmpday <- (ydat$doy[day1]+ydat$doy[day1-1])/2)						
        ifelse(i==1, fbb <- tmpday, fbb <- c(fbb,fbb <- tmpday))	# flower budburst
      }
    }
		
    if (length(ydat$fbud[is.na(ydat$fbud)==FALSE]) == 0) { 
      ifelse(i==1, fbb <- NA, fbb <- c(fbb, NA))	# NA if values cannot be calculated
    }
		
    if (length(ydat$fopn[is.na(ydat$fopn)==FALSE]) > 2) { 
      interp <- approx(ydat$doy, ydat$fopn, n=niterp)
      ifelse(i==1, fopn <- interp$x[interp$y>=50][1],
        fopn <- c(fopn,interp$x[interp$y>=50][1]))	# 50% development (max 80)
    }
		
    if (length(ydat$fopn[is.na(ydat$fopn)==FALSE]) <= 2) { 
      ifelse(i==1, fopn <- NA, fopn <- c(fopn, NA))	# NA if values cannot be calculated
    }
		
    i = i + 1
  }	
	
  return(data.frame(year=years, tree.id=tree.id, species=species, bb.doy=bb, l75.doy=l75, fbb.doy=fbb, fopn.doy=fopn, stringsAsFactors = FALSE))	
}

file.in <- "hf003-03-spring.csv"

file.out.tree <- "hf003-05-spring-mean-ind.csv"
file.out.species <- "hf003-06-spring-mean-spp.csv"

dat <- read.csv(file(file.in), header=TRUE, stringsAsFactors=FALSE)

tmp <- as.POSIXct(dat$date)
dat$year <- format(tmp, format="%Y")

# start tree ID loop

treecodes <- unique(dat$tree)
treecodes <- sort(treecodes)

for (loop in treecodes) {
  #extract year	
  out <- spring(dat, loop)
  ifelse(loop==treecodes[1], out.frame <- out, out.frame <- rbind(out.frame, out))
  cat("..Analyzing ", loop, "\n")
}

# convert julian day values to integer
out.frame.int <- out.frame
for (i in 4:7) {
  out.frame.int[ , i] <- round(out.frame.int[ , i])
}

# write csv file by tree

write.csv(out.frame.int, file=file.out.tree, row.names=FALSE, quote=FALSE)

# aggregate data by tree species

tmp.frame <- out.frame

# mean dates
out.mean.frame <- aggregate(tmp.frame, by=list(out.frame$year, out.frame$species), FUN="mean", na.rm=TRUE)
out.mean.frame$year <- NULL
out.mean.frame$species <- NULL

# standard deviation
out.sd.frame <- aggregate(tmp.frame, by = list(out.frame$species, out.frame$year), FUN="sd", na.rm=TRUE)
out.sd.frame <- subset(out.sd.frame, select = c(bb.doy, l75.doy, fbb.doy, fopn.doy))

new.name <- paste('sd', names(out.sd.frame), sep='.')
names(out.sd.frame) <- new.name

out.mean.frame <- cbind(out.mean.frame, out.sd.frame)

names(out.mean.frame)[1] <- "year"
names(out.mean.frame)[2] <- "species"

out.mean.frame$tree.id <- NULL

# convert julian day values to integer
out.mean.frame.int <- out.mean.frame
for (i in 3:6) {
  out.mean.frame.int[ , i] <- round(out.mean.frame.int[ , i])
}

# write csv file by species

write.csv(out.mean.frame.int, file=file.out.species, row.names=FALSE, quote=FALSE)

print("END")

