## Started 28 Jan 2017 ##
## By Lizzie ##
## Oh no, I am working on a weekend! But will stop soon ##

## Eventually we should use this code to clean responsetime ##
## I (Lizzie) think we will need to fix each entry on a case-by-case basis sadly ...

# Added TODO notes throughout #

# SHOULD WE rename the `1' entries as "timeonly" or such?!

# Here's the issue ...
# Some data were XY graphs with days on X and percent budburst or something else on the Y
# These data should have been entered with X in response.time columnn and Y in response.
# A `1' in response should signify that the only response variable is days to budburst and there is not corresponding Y axis of data #

##
# Let's start with the 1 entries
##
areone <- d[which(d$response==1),]
unique(areone$respvar.simple)

# TODO .. Okay, this NA needs someone to check the respvar ... 
subset(areone, is.na(respvar.simple)==TRUE)

# Onward
respvar.time <- c("daystobudburst", "daystoflower")
respvar.perc <- c("percentbudburst", "percentflower", "otherpercents")
respvar.other <- c("thermaltime", "phenstage", "flowernumber", "growth", "othernums")
 
areone.time <- areone[which(areone$respvar.simple %in% respvar.time),]
hist(as.numeric(areone.time$response.time), breaks=30) ## hmm, looks generally okay -- (TODO) need to check on some rogue entries and maybe a negative?!

areone.perc <- areone[which(areone$respvar.simple %in% respvar.perc),]
areone.perc # TODO, these look okay to me, could double check (20 rows total)

areone.other <- areone[which(areone$respvar.simple %in% respvar.other),]
dim(areone.other)
unique(areone.other$datasetID) # need to go through each one ... for example basler looks okay to me, but I think sonsteby13 is either entered wrong or is an example of where this `1' idea fails us.


##
# Lizzie START HERE!
##
##
thinkonme <- d[which(d$response>1),] # column nums of possible interest: 25:27

# only when response==1 should response time have a value, but that's not true:
areconfused <- subset(d, response!=1 & response.time!="")

# neither one of these looks good
unique(thinkonme$respvar.simple)

d[(is.na(d$response.time)==TRUE),]

# what to worry about, basically everything where response.time is not empty
fixme <- subset(d, response.time != "" & is.na(response.time)==FALSE)
########################################
## Cleaning response and response.time #
####### on a case-by-case basis ########
########################################

# start a new column that we will fill up
d$resp <- NA

# If there is nothing in 

# Here's an easy one:
d[(d$datasetID=="cronje03"),]
# DF wrote 25per in response (even though respvar is daysto25%budburst)
# So response.time is what we want ...
d$resp[(d$datasetID=="cronje03")] <- d$response.time[(d$datasetID=="cronje03")]


# this was Lizzie's old fix, definitely not good enough
# but a nuce example of how to move column values around based on conditional statements!
d$responsedays <- d$response.time
d$responsedays[which(d$response>1 & d$response.time=="")] <-
    d$response[which(d$response>1 & d$response.time=="")]


