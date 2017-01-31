## Started 27 Jan 2017 ##
## By Lizzie, Cat so far ##

## Code to add in Zohner data to master data ##
## Written as code to be sourced in cleanmerge_all.R ##

## Lots of code from clean_final.R ##

## Need to adjust below code so it runs off d in cleanmerge_all.R ##
## See more notes on this file in cleanmerge_all.R ##

zohner <- read.csv("input/zohner_formated.csv", fileEncoding="latin1")

# Note, the df clean is effectively d in new cleanmerge_all.R code #

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
