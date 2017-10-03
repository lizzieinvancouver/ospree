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
## We'll check if they should be 1 #
####################################
areone <- d[which(d$response==1),]
unique(areone$respvar.simple)

areone.rows <- which(d$response==1) # will use for indexing later

## Start work to break down into respvar categories, to help with sorting ##
respvar.time <- c("daystobudburst", "daystoflower", "thermaltime")
respvar.perc <- c("percentbudburst", "percentflower", "otherpercents")
respvar.other <- c("phenstage", "flowernumber", "growth", "othernums")
# Checking respvar.time related issues
areone.time <- areone[which(areone$respvar.simple %in% respvar.time),]
# hist(as.numeric(areone.time$response.time), breaks=30) # Whoops! Some negative entries?

# Dealing with negative entries
negative.time <- subset(areone.time, response.time<0)
unique(negative.time$datasetID)
checkers1 <- unique(negative.time$datasetID)
negatives <- negative.time[which(negative.time$datasetID %in% checkers1),]
# negatives[,c(1,25:27,31)] 

# Checking respvar.perc related issues
areone.perc <- areone[which(areone$respvar.simple %in% respvar.perc),]
# areone.perc[,c(1,25:27,31)]
unique(areone.perc$datasetID)

# Checking respvar.other related issues
areone.other <- areone[which(areone$respvar.simple %in% respvar.other),]
dim(areone.other)
unique(areone.other$datasetID)
checkers2 <- unique(areone.other$datasetID)
# areone.other[which(areone.other$datasetID %in% checkers2),]

##
## Now we go through each one ... and change it if we should, or make a note if not ##
##

#calme94- Betula and Quercus are switched!fixed below
##change the following from >[experiment duration] to no response in $reponse.time
#caffarra11b  #heide12. #heide93a #howe95. transform negative value to zero in calme 94 
d<- within(d, response.time[datasetID=="calme94" & response.time==-1.8]<-0)
# View(subset(d,datasetID=="calme94" ))
d<- within(d, genus[datasetID=="calme94" & species=="rubra"]<-"Betula")
d<- within(d, genus[datasetID=="calme94" & species=="alleghaniensis"]<-"Quercus")
d<- within(d, species[datasetID=="calme94" & genus=="Quercus"]<-"rubra")
d<- within(d, species[datasetID=="calme94" & genus=="Betula"]<-"alleghaniensis")

d<- within(d, response.time[datasetID=="caffara11b" & response.time==""]<-"no response")
d<- within(d, response.time[datasetID=="heide12" & response.time==">60"]<-"no response")
d<- within(d, response.time[datasetID=="heide12" & response.time==">90"]<-"no response")
d<- within(d, response.time[datasetID=="heide93a" & response.time==">50"]<-"no response")
d<- within(d, response.time[datasetID=="howe95" & response.time==">60"]<-"no response")

#falusi96 in respvar: For data from table 2, change to mean days to reach phenostage 3
d<-within(d, respvar[datasetID=="falusi96" & figure.table..if.applicable.=="table 2"]<-"meandaystostage3")

#canell83: would need to fix this is respvar by calculating thermal time from figure 3- No need to change here.
#see note to above in  ospreexlsx_README.txt
#gunderson12: This is fine, do not change
#heide11: This is fine, do not change
#petterson71: This is fine, do not change 
#sonsteby13: This is fine, do not change
#ruesink98: This is fine, do not change


## Rewrite entries where 1 means, "only response.time entry"
d$response[which(d$response==1 & d$respvar.simple %in% respvar.time)] <- "timeonly"


########################################################
## Entries that are time but are not in response.time ##
########################################################
getemptytime <- subset(d, response.time=="" | is.na(response.time)==TRUE)
notwheretheyshouldbe <- getemptytime[which(getemptytime$respvar.simple %in% respvar.time),] 
unique(notwheretheyshouldbe$datasetID) # some of these seem to be in wrong column, some are just empty

##
## Now we go through each one ... and change it if we should, or make a note if not ##
##

## Findings:
#ashby62 need transformation as below, but also...in paper is weeks to bud burst should*by 7 #### fixed below
#basler12-ok, do not chnage
#boyer-ok, do not change
#caffara11b-ok, do not change
#laube14b- should be response-time only and move values to response time ###fixed below
#skuterud94-figure 4 entries ok, figure 5 entries as above ###fixed below
#zohner16-ok, do not change

## Now fixing:
# for the ones that we decide were entered in the wrong column do this (and, IMPORTANT!) check you subsetted correctly by comparing, e.g.,
fixashby62 <- which(d$response.time=="" & d$datasetID=="ashby62" & d$respvar.simple %in% respvar.time)
length(fixashby62)
nrow(subset(notwheretheyshouldbe, datasetID=="ashby62"))

fixlaube14b<-which(d$response.time=="" & d$datasetID=="laube14b" &
    d$respvar.simple %in% respvar.time)
length(fixlaube14b)
nrow(subset(notwheretheyshouldbe, datasetID=="laube14b"))

fixskuterud94<-which(d$response.time=="" & d$datasetID=="skuterud94" & d$figure.table..if.applicable.== "fig5" &
    d$respvar.simple %in% respvar.time)
length(fixskuterud94)
nrow(subset(notwheretheyshouldbe, datasetID=="skuterud94" & figure.table..if.applicable.=="fig5"))

# fixes that happen in two steps:
# (1) move response to response time
# (2) make response timeonly
d$response.time[fixashby62] <- as.numeric(d$response[fixashby62])*7 
d$response[fixashby62] <- "timeonly"
d$response.time[fixlaube14b] <- d$response[fixlaube14b]
d$response[fixlaube14b] <- "timeonly"
d$response.time[fixskuterud94] <- d$response[fixskuterud94]
d$response[fixskuterud94] <- "timeonly"


#################################################################
## Entries with nothing in response, but data in response.time ##
#################################################################
getempty <- subset(d, response=="" | is.na(response)==TRUE)
subset(getempty, is.na(response.time)==TRUE) # no NAs
whynoresponse <- getempty[which(!getempty$response.time==""),]
unique(whynoresponse$datasetID) 

##
## Now we go through each one ... and change it if we should, or make a note if not ##
##

## Findings: 
#campell75-make time only
# "gianfagna85"-"make time only
#"laube14a" make time only
#"linkosalo06" make time only
#"nienstaedt66" make timeonly
#"nishimoto95" make time only
#"skre08" make time only      
# "spiers74" make time only
#"zohner16" make time only

## Now fix, what we should fix: 
# campbell75, Lizzie entered this, it's a weird measure of time called DARD, it is timeonly
# check subsetting
length(d$response[which(d$response=="" & d$datasetID=="campbell75" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="campbell75"))
length(d$response[which(d$response=="" & d$datasetID=="gianfagna85" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="gianfagna85"))
length(d$response[which(d$response=="" & d$datasetID=="laube14a" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="laube14a"))
length(d$response[which(d$response=="" & d$datasetID=="linkosalo06" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="linkosalo06"))
length(d$response[which(d$response=="" & d$datasetID=="nienstaedt66" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="nienstaedt66"))
length(d$response[which(d$response=="" & d$datasetID=="nishimoto95" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="nishimoto95"))
length(d$response[which(d$response=="" & d$datasetID=="skre08" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="skre08"))
length(d$response[which(d$response=="" & d$datasetID=="spiers74" &
     !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="spiers74"))
length(d$response[which(d$response=="" & d$datasetID=="zohner16" &
    !d$response.time=="")])
nrow(subset(whynoresponse, datasetID=="zohner16"))

# now overwrite the response cells here
d$response[which(d$response=="" & d$datasetID=="campbell75" &
    !d$response.time=="")] <- "timeonly"
d$response[which(d$response=="" & d$datasetID=="gianfagna85" &
    !d$response.time=="")] <- "timeonly"
d$response[which(d$response=="" & d$datasetID=="laube14a" &
    !d$response.time=="")] <- "timeonly"
d$response[which(d$response=="" & d$datasetID=="linkosalo06" &
    !d$response.time=="")] <- "timeonly"
d$response[which(d$response=="" & d$datasetID=="nienstaedt66" &
    !d$response.time=="")] <- "timeonly"
d$response[which(d$response=="" & d$datasetID=="nishimoto95" &
    !d$response.time=="")] <- "timeonly"
d$response[which(d$response=="" & d$datasetID=="skre08" &
    !d$response.time=="")] <- "timeonly"
d$response[which(d$response=="" & d$datasetID=="spiers74" &
     !d$response.time=="")] <- "timeonly"
d$response[which(d$response=="" & d$datasetID=="zohner16" &
     !d$response.time=="")] <- "timeonly"


#######################################################
## Things that are time but response column is not 1 ##
#######################################################
weirdresponsecolumn <- subset(d, response!=1 & response!="timeonly")
shouldbeone <- weirdresponsecolumn[which(weirdresponsecolumn$respvar.simple %in%
    respvar.time),]

# check it out
unique(shouldbeone$datasetID)
# shouldbeone[,c(1, 25:27)]

##
## Now we go through each one ... and change it if we should, or make a note if not ##
##

## Findings: 
## Dan has now fixed (Whoop!) 
#"boyer": ->time only, no response   fixed below   
#"cook05" response=50per-> "timeonly"     fixed below
#"cronje03" "response=25per-> "timeonly" fixed below
#"heide12"  response->response.time and response shouldd be timeonly done
#"sonsteby14"  same as heide12 fixed below
#"spiers74" ...time only, no respone  fixed below
#"webb78"   time only, no response   fixed below
#"zohner16" time only, no response fixed below
#"falusi90" respose is actually % budburst, fixed below 

## ask Lizzie about
  #"ghelardini10" handled in multiresp code
# "heide93" handled in thermaltime or multiresp code   

##
##check subsetting
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="boyer" & 
    d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="boyer"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="cook05" &
    d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="cook05"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="cronje03" &
    d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="cronje03"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="heide12" &
    d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="heide12"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="sonsteby14" &
    d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="sonsteby14"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="spiers74" &
    d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="spiers74"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="webb78" &
    d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="webb78"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="zohner16" &
    d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="zohner16"))

## Now fix, what we should fix:
##
# fix Boyer
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="boyer" &
    d$respvar.simple %in% respvar.time)]<-"timeonly"
d$response.time[which(d$response=="timeonly" & d$response.time=="" & d$datasetID=="boyer")]<-"no response"
# fix cook05
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="cook05" &
    d$respvar.simple %in% respvar.time)]<-"timeonly"
#fix cronje03
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="cronje03" &
    d$respvar.simple %in% respvar.time)]<-"timeonly"
#fixheide12
fixheide12<-which(d$response!=1 & d$response!="timeonly" & d$datasetID=="heide12" &
    d$respvar.simple %in% respvar.time)
d$response.time[fixheide12] <- as.numeric(d$response[fixheide12]) 
d$response[fixheide12] <- "timeonly"
#fix sonsteby14
fixsonsteby14<-which(d$response!=1 & d$response!="timeonly" & d$datasetID=="sontsteby14" &
    d$respvar.simple %in% respvar.time)
d$response.time[fixsonsteby14] <- as.numeric(d$response[fixsonsteby14]) 
d$response[fixsonsteby14] <- "timeonly"
##fix spiers74
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="timeonly" &
    d$respvar.simple %in% respvar.time)]<-"timeonly"
##fixwebb78
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="webb78" &
    d$respvar.simple %in% respvar.time)]<-"timeonly"
d$response.time[which(d$response=="timeonly" & d$response.time==">100" &
    d$datasetID=="webb78")]<-"no response"
#fixzohner16
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="zohner16" &
    d$respvar.simple %in% respvar.time)]<-"timeonly"
d$response.time[which(d$response=="timeonly" & d$response.time=="" &
    d$datasetID=="zohner16")]<-"no response"

#fix falusi90 make respvar and respvar.simple =percentbudburst
unique(d$respvar)
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="falusi90" &
                          d$respvar.simple %in% respvar.time)])
nrow(subset(shouldbeone, datasetID=="falusi90"))
d<- within(d, respvar[datasetID=="falusi90" & respvar=="daystobudburst"]<-"percentbudburst")
d<- within(d, respvar.simple[datasetID=="falusi90" & respvar=="percentbudburst"]<-"percentbudburst")


### Cat (edits on 15 Septmber 2017) - to fix ramos99
d$response.time[which(d$forcetemp==30 & d$datasetID=="ramos99")]<-14
d$response.time[which(d$forcetemp==20 & d$datasetID=="ramos99")]<-21
d$response.time[which(d$forcetemp==12.5 & d$datasetID=="ramos99")]<-NA


###############################################################
## Which rows have been looked at through all these queries? ##
###############################################################

checked.rows <- c(row.names(areone), row.names(notwheretheyshouldbe),
    row.names(whynoresponse), row.names(shouldbeone))
checked.rows.nodups <- checked.rows[!duplicated(checked.rows)]

checkedwhat <- d[checked.rows.nodups,]

nrow(checkedwhat)/nrow(d)
stop("Not an error, just stopping here to say we're now done cleaning responsetime. The d item in your workspace is now all cleaned up for its response.time column Zippitydoodah ...")

