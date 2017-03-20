## Started 28 Jan 2017 ##
## By Lizzie with heroic efforts by Dan B. ##
## Oh no, I am working on a weekend! But will stop soon ##

## This code cleans responsetime ##
## In many cases we needed to fix each entry on a case-by-case basis sadly ... so this is a bit long. ##

# Here's the issue ...
# Some data were XY graphs with days on X and percent budburst or something else on the Y
# These data should have been entered with X in response.time columnn and Y in response.
# A `1' in response should signify that the only response variable is days to budburst and there is not corresponding Y axis of data #

# We decided to rename things that SHOULD BE 1 under this old system to "timeonly" #

#################################### 
## Let's start with the 1 entries ##
####################################
areone <- d[which(d$response==1),]
unique(areone$respvar.simple)

areone.rows <- which(d$response==1) # will use later

# Onward
respvar.time <- c("daystobudburst", "daystoflower", "thermaltime")
respvar.perc <- c("percentbudburst", "percentflower", "otherpercents")
respvar.other <- c("phenstage", "flowernumber", "growth", "othernums")
 ## checking respvar.time related issues
areone.time <- areone[which(areone$respvar.simple %in% respvar.time),]
# hist(as.numeric(areone.time$response.time), breaks=30) ## hmm, looks generally okay -- (TODO) need to check on some rogue entries and maybe a negative?!

negative.time <- subset(areone.time, response.time<0)
unique(negative.time$datasetID)
checkers1 <- c("caffarra11b","calme94","heide12","heide93a","howe95")  
negatives <- negative.time[which(negative.time$datasetID %in% checkers1),]
# negatives[,c(1,25:27,31)] 

areone.perc <- areone[which(areone$respvar.simple %in% respvar.perc),]
# areone.perc[,c(1,25:27,31)]
unique(areone.perc$datasetID)

areone.other <- areone[which(areone$respvar.simple %in% respvar.other),]
dim(areone.other)
unique(areone.other$datasetID) 
# need to go through each one ... for example sonsteby13 is looks to be an example of where this `1' idea fails us.
checkers2 <- c("cannell83", "gunderson12","heide11", "pettersen71","sonsteby13") 
areone.other[which(areone.other$datasetID %in% checkers2),]

####all issues from above:
#calme94- Betula and Quercus are switched!
##change the following from >[experiment duration] to no response in $reponse.time
#caffarra11b  #heide12. #heide93a #howe95. transform negative value to zero in calme 94 
d<- within(d, response.time[datasetID=="calme94" & response.time==-1.8]<-0)
d<- within(d, response.time[datasetID=="caffara11b" & response.time==""]<-"no response")
d<- within(d, response.time[datasetID=="heide12" & response.time==">60"]<-"no response")
d<- within(d, response.time[datasetID=="heide12" & response.time==">90"]<-"no response")
d<- within(d, response.time[datasetID=="heide93a" & response.time==">50"]<-"no response")
d<- within(d, response.time[datasetID=="howe95" & response.time==">60"]<-"no response")
filter(d,datasetID=="heide12")

#falusi96 in respvar: For data from table 2, change to mean days to reach phenostage 3
#canell83: would need to fix this is respvar by calculating thermal time from figure 3
#gunderson12: in clean_bbperctodays.R extract budstage 3. In paper: "at 3, buds were just open and leaf tips were beginning to emerge")
#heide11: This is fine
#petterson71 This is fine 
#sonsteby13 This is fine
#ruesink98" This is fine

##
## Now fix, what we should fix ##
##

## Rewrite entries where 1 means, "only response.time entry"
d$response[which(d$response==1 & d$respvar.simple %in% respvar.time)] <- "timeonly"


########################################################
## Entries that are time but are not in response.time ##
########################################################
getemptytime <- subset(d, response.time=="" | is.na(response.time)==TRUE)
#notwheretheyshouldbe <- getemptytime[which(getemptytime$respvar.simple %in% respvar.time),] Lizzie's original line

names(getemptytime) #there is no respvar.time
notwheretheyshouldbe <- getemptytime[which(getemptytime$respvar.simple %in% respvar.time),] 
unique(notwheretheyshouldbe$datasetID) # some of these seem to be in wrong column, some are just empty ... TODO -- go through each to figure out issue ...
#to check: "ashby62" "basler12 "basler14" "boyer" "caffarra11b" "cook05""laube14b" "skuterud94" "zohner16"
View(filter(notwheretheyshouldbe,datasetID=="basler14"))
#ashby62 in paper is weeks to bud burst
#basler12-ok empty time response.time correspond with NA's in orrginal paper

##
## Now fix, what we should fix ##
##

# for the ones that we decide were entered in the wrong column do this (and, IMPORTANT!) check you subsetted correctly by comparing, e.g.,
fixashby62 <- which(d$response.time=="" & d$datasetID=="ashby62" &
    d$respvar.simple %in% respvar.time)
length(fixashby62)
nrow(subset(notwheretheyshouldbe, datasetID=="ashby62"))

# fix two pieces:
# (1) move response to response time
d$response.time[fixashby62] <- d$response[fixashby62]
# (2) make response timeonly
d$response[fixashby62] <- "timeonly"



#################################################################
## Entries with nothing in response, but data in response.time ##
#################################################################
getempty <- subset(d, response=="" | is.na(response)==TRUE)
subset(getempty, is.na(response.time)==TRUE) # no NAs
whynoresponse <- getempty[which(!getempty$response.time==""),]

unique(whynoresponse$datasetID) # need to go through each one ... again, make notes about ones we're not changing and change the others (and check your subsetting!)

##
## Now fix, what we should fix ##
##

# campbell75, Lizzie entered this, it's a weird measure of time called DARD, it is timeonly
# check my subsetting
length(d$response[which(d$response=="" & d$datasetID=="campbell75" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="campbell75"))

# now overwrite the response cells here
d$response[which(d$response=="" & d$datasetID=="campbell75" &
    !d$response.time=="")] <- "timeonly"


#######################################################
## Things that are time but response column is not 1 ##
#######################################################
weirdresponsecolumn <- subset(d, response!=1 & response!="timeonly")
shouldbeone <- weirdresponsecolumn[which(weirdresponsecolumn$respvar.simple %in%
    respvar.time),]

# check it out
unique(shouldbeone$datasetID)
# shouldbeone[,c(1, 25:27)] # TODO: need to go through each one ... again, make notes about ones we're not changing and change the others (and check your subsetting!)

##
## Now fix, what we should fix ##
##



###############################################################
## Which rows have been looked at through all these queries? ##
###############################################################

checked.rows <- c(row.names(areone), row.names(notwheretheyshouldbe),
    row.names(whynoresponse), row.names(shouldbeone))
checked.rows.nodups <- checked.rows[!duplicated(checked.rows)]

checkedwhat <- d[checked.rows.nodups,]

nrow(checkedwhat)/nrow(d)
