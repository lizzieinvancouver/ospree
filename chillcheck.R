## Started 17 July 2016 ##
## By Ailene Ettinger, aettiger@fas.harvard.edu##
## check chilldays and chilltemp columns in ospree to get list of papers 
## and things that need to be fixed to remove non-numeric entries in these columns
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(dplyr)
setwd("~/git/ospree") # setwd("~/Documents/git/ospree")
ospree <- read.csv("ospree_clean.csv", header=TRUE)

ospree$chilltemp.num<-as.numeric(ospree$chilltemp)
ospree$chilldays.num<-as.numeric(ospree$chilldays)

##check which cells are NOT numeric:
x <- ospree[which(is.na(ospree$chilltemp.num)),]
x2 <- x[-which(x$chilltemp==""),] # looking for both NA in the chilltemp.num and are not blank in chilltemp

x2$ID.chilltemps<-paste(x2$datasetID, x2$chilltemp)

x3 <- x2 %>% # start with the data frame. Find just distinct conditions and studies. 48 rows
  distinct(ID.chilltemps,.keep_all = TRUE) %>% # establishing grouping variables
  select(datasetID, chilltemp)

# y is the same as "x2" above, but for days instead of temperatures
y <- ospree[which(is.na(ospree$chilldays.num)),]
y2 <- y[-which(y$chilldays==""),]

y2$ID.chilldays <- paste(y2$datasetID,y2$chilldays)

y3 <- y2 %>% # start with the data frame
  distinct(ID.chilldays,.keep_all = TRUE) %>% # establishing grouping variables
  select(datasetID, chilldays)

##merge the two
z <- merge(x3, y3, all=TRUE)
write.csv(z,"input/chilldaystempstocheck.csv",row.names=FALSE, eol="\r\n")
