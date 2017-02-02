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



# Onward
respvar.time <- c("daystobudburst", "daystoflower", "thermaltime")
respvar.perc <- c("percentbudburst", "percentflower", "otherpercents")
respvar.other <- c("phenstage", "flowernumber", "growth", "othernums")
 ## checking respvar.time related issues
areone.time <- areone[which(areone$respvar.simple %in% respvar.time),]
hist(as.numeric(areone.time$response.time), breaks=30) ## hmm, looks generally okay -- (TODO) need to check on some rogue entries and maybe a negative?!
range(areone.time$response.time,na.rm=TRUE)
negative.time<-subset(areone.time, response.time<0)
unique(negative.time$datasetID)
checkers1<-c("caffarra11b","calme94","heide12","heide93a","howe95")  
negatives<-negative.time[which(negative.time$datasetID %in% checkers1),]
negatives[,c(1,25:27)]
nrow(negatives)
calme94<-subset(negative.time,datasetID=="calme94")
unique(calme94$figure.table..if.applicable)
caraffara11b<-subset(negative.time,datasetID=="carraffara11b")
unique(caraffara11b$figure.table..if.applicable)
heide12<-subset(negative.time,datasetID=="heide12")
unique(heide12$figure.table..if.applicable)
heide93a<-subset(negative.time,datasetID=="heide93a")
unique(heide93a$figure.table..if.applicable)
howe95<-subset(negative.time,datasetID=="howe95")
unique(howe95$figure.table..if.applicable)
###issues with:"caffarra11b" "calme94"     "heide12"     "heide93a"    "howe95" 
#calme94 seems to be wrong species (says Q. rub should be b. allegheniensis) and should be 0 based on figure?
#caraffara11b had no respose under these treatment
#heide12...it seems like no flowering in duration of experiement under these condition >60 ot 90 respectibrlu
#heide 1993a doesn't exist in the folders but I looked online and is same scenario as above
#howe95 should be days to budset or better days to percent budset, the ones in my list did not set buds 

areone.perc <- areone[which(areone$respvar.simple %in% respvar.perc),]
areone.perc # TODO, these look okay to me, could double check (20 rows total)
View(areone.perc[,c(1,25:27,31)])
unique(areone.perc$datasetID)
View(filter(areone.perc,datasetID=="smeets82"))

##here are the issues with
#"falusi96" should be mean days to reach phenostage 3 
#"lyndon77" this is non woody
#"nerd95"  non woody
#"ruesink98" correct, 1 refers to 1%
#"smeets82" non woody

areone.other <- areone[which(areone$respvar.simple %in% respvar.other),]
dim(areone.other)
unique(areone.other$datasetID) 
# need to go through each one ... for example sonsteby13 is looks to be an example of where this `1' idea fails us.

# We should go through each entry somehow:[1] "cannell83"    "darrow36"     "gunderson12"  "heide01"      "heide11"      "kronenberg76" "pettersen71" "sonsteby06"   "sonsteby13"  

checkers2<-c("cannell83" ,   "darrow36"  ,   "gunderson12" , "heide01"    ,  "heide11"    ,  "kronenberg76",
"pettersen71" , "sonsteby06"  , "sonsteby13") 
View(areone.other[which(areone.other$datasetID %in% checkers2),])
View(filter(areone.other,datasetID=="gunderson12"))
View(filter(areone.other,datasetID=="heide11"))
View(filter(areone.other,datasetID=="pettersen71"))
# and make a note like ...
#canell83: this indeed refers to budstage 1, but the variable is weird. see paper
#darrow36: not woody
#gunderson12: This one's weird. It sould be mean "score" for stage 4. higher score means more leave fully emerge. 6 mean 100% reached leaf out.
#heide01: not woody
#heide11: This describes reaching 1 cumultive growth increment not sure how it converts 
#kronenberg76-couldn't find paper...also not woody
#petterson71 this is days until 1mm of growth/14 days 
#sosteby06 not woody
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
