## Started 28 Jan 2017 ##
## By Lizzie ##
## Oh no, I am working on a weekend! But will stop soon ##

## Eventually we should use this code to clean responsetime ##
## I (Lizzie) think we will need to fix each entry on a case-by-case basis sadly ...

# Added TODO notes throughout (but then forgot so some are missing) #

# SHOULD WE rename the `1' entries as "timeonly" or such?!

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

# TODO .. Okay, this NA needs someone to check the respvar (because there isn't one) ... 
subset(areone, is.na(respvar.simple)==TRUE)

# Onward
respvar.time <- c("daystobudburst", "daystoflower", "thermaltime")
respvar.perc <- c("percentbudburst", "percentflower", "otherpercents")
respvar.other <- c("phenstage", "flowernumber", "growth", "othernums")
 
areone.time <- areone[which(areone$respvar.simple %in% respvar.time),]
hist(as.numeric(areone.time$response.time), breaks=30) ## hmm, looks generally okay -- (TODO) need to check on some rogue entries and maybe a negative?!

areone.perc <- areone[which(areone$respvar.simple %in% respvar.perc),]
areone.perc # TODO, these look okay to me, could double check (20 rows total)

areone.other <- areone[which(areone$respvar.simple %in% respvar.other),]
dim(areone.other)
unique(areone.other$datasetID) # need to go through each one ... for example sonsteby13 is looks to be an example of where this `1' idea fails us.

# We should go through each entry somehow:
sonsteby13 <- subset(areone.other, datasetID=="sonsteby13")
unique(sonsteby13$figure.table..if.applicable)
# and make a note like ...
# checked sonsteby13 and 1 appears to be a true 1, leave alone

##
## Now fix, what we should fix ##
##

## Rewrite entries where 1 means, "only response.time entry"
d$response[which(d$response==1 & d$respvar.simple %in% respvar.time)] <- "timeonly"


########################################################
## Entries that are time but are not in response.time ##
########################################################
getemptytime <- subset(d, response.time=="" | is.na(response.time)==TRUE)
notwheretheyshouldbe <- getemptytime[which(getemptytime$respvar.simple %in% respvar.time),]

unique(notwheretheyshouldbe$datasetID) # some of these seem to be in wrong column, some are just empty ... TODO -- go through each to figure out issue ...

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
