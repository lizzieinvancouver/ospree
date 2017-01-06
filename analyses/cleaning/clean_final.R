# Started 6 January 2017 - Cat
## Attempt to make one master csv file that is totally cleaned and includes
## Zohner data and chilling hours

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(dplyr)
library(tidyr)

# Set working directory: 
setwd("~/Documents/git/ospree/analyses/input")

# Name data frame:
respvar <- read.csv("ospree_clean_respvar.csv")
zohner <- read.csv("zohner_formated.csv", fileEncoding="latin1")
photo <- read.csv("ospree_clean_photo.csv")

# Start combining the fixes...
photo<-dplyr::select(photo, -ID_chilltreat, -Total_Chilling_Hours, -Total_Chill_portions,
                       -Total_Utah_Model, -Field_Chilling_Hours, -Field_Utah_Model, -Field_Chill_portions,
                       -ID_fieldsample.date.1, -Exp_Chilling_Hours, -Exp_Utah_Model, -Exp_Chill_portions,
                       -ID_fieldsample.date)
respvar<-dplyr::select(respvar, -ID_chilltreat, -Total_Chilling_Hours, -Total_Chill_portions,
                       -Total_Utah_Model, -Field_Chilling_Hours, -Field_Utah_Model, -Field_Chill_portions,
                       -ID_fieldsample.date.1, -Exp_Chilling_Hours, -Exp_Utah_Model, -Exp_Chill_portions,
                       -ID_fieldsample.date)
clean <- cbind(photo, respvar[!names(respvar) %in% names(respvar)])

# Work on Zohner
zohner$respvar.simple[zohner$respvar == "daystobudburst"] <- "daystobudburst"
# Which studies have multiple respvar but only one respvar.simple?
zohner$datasetIDstudy <- paste(zohner$datasetID, zohner$study)

studyresp <- with(zohner, paste(datasetIDstudy, respvar))
studyresps <- with(zohner, paste(datasetIDstudy, respvar.simple))

xx <- tapply(studyresp, studyresps, function(x)
  length(unique(x)) > 1)

multiresp <- names(xx)[xx==T]

# make a flag for this 
zohner$multiresp <- !is.na(match(studyresps, multiresp))

# which studies have multiple original respvars and each one is a separate respvar.simple?
xx <- data.frame(datasetIDstudy = zohner$datasetIDstudy, studyresp, studyresps)

multibothresp <- vector()

for(i in unique(zohner$datasetIDstudy)){
  xz <- xx[xx$datasetIDstudy == i,]
  
  ta <- table(xz$studyresp, xz$studyresps)
  
  multibothresp <- 
    c(multibothresp, 
      identical(nrow(ta), ncol(ta)) & nrow(ta) > 1
    )
  
}
# these are the studies fit this criterion
(mb <- unique(zohner$datasetIDstudy)[multibothresp])

zohner$multibothresp <- !is.na(match(zohner$datasetIDstudy, mb))
zohner<-dplyr::select(zohner, -X) %>%
  rename("chillphotoperiod" = chillphoto)

write.csv(zohner, "~/Documents/git/ospree/analyses/output/zohner_clean.csv", row.names = FALSE)

# Combine Zohner data to clean
names(zohner) <- names(clean) 
final <- rbind(zohner, clean)

write.csv(final, "~/Documents/git/ospree/analyses/output/ospree_master_clean.csv", row.names=FALSE)
write.csv(final, "~/Documents/git/ospree/analyses/input/ospree_master_clean.csv", row.names=FALSE)

