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

## Now we go through each one ... for example sonsteby13 is looks to be an example of where this `1' idea fails us.
checkers2 <- unique(areone.other$datasetID)
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
#filter(d,datasetID=="heide12")
#falusi96 in respvar: For data from table 2, change to mean days to reach phenostage 3
#canell83: would need to fix this is respvar by calculating thermal time from figure 3
#gunderson12: in clean_bbperctodays.R extract budstage 3. In paper: "at 3, buds were just open and leaf tips were beginning to emerge")
#heide11: This is fine
#petterson71 This is fine 
#sonsteby13 This is fine
#ruesink98" This is fine


## Rewrite entries where 1 means, "only response.time entry"
d$response[which(d$response==1 & d$respvar.simple %in% respvar.time)] <- "timeonly"


########################################################
## Entries that are time but are not in response.time ##
########################################################
getemptytime <- subset(d, response.time=="" | is.na(response.time)==TRUE)
notwheretheyshouldbe <- getemptytime[which(getemptytime$respvar.simple %in% respvar.time),] 
unique(notwheretheyshouldbe$datasetID) # some of these seem to be in wrong column, some are just empty ... TODO -- go through each to figure out issue ...
#These lines used to look at subset of each dataserID in above list
#View(filter(notwheretheyshouldbe,datasetID=="cronje03"))

###findings
#ashby62 need transformation as below, but also...in paper is weeks to bud burst should*by 7###fixed below
#basler12-ok
#boyer-ok
#caffara11b-ok
#laube14b- should be response-time only and move values to response time ###fixed below
#skuterud94-figure 4 entries ok, figure 5 entries as above ###fixed below
#zohner16-ok
##
## Now fix, what we should fix ##
##

# for the ones that we decide were entered in the wrong column do this (and, IMPORTANT!) check you subsetted correctly by comparing, e.g.,
fixashby62 <- which(d$response.time=="" & d$datasetID=="ashby62" &
    d$respvar.simple %in% respvar.time)
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

# fix two pieces:
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
unique(whynoresponse$datasetID) # need to go through each one ... again, make notes about ones we're not changing and change the others (and check your subsetting!)
# "gianfagna85"-"make time only
#"laube14a" make time only
#"linkosalo06" make time only
#"nienstaedt66" maketimeonly
#"nishimoto95" make time only
#"skre08" maketime only      
# "spiers74" maketime only
#"zohner16" make time only
#View(filter(whynoresponse,datasetID=="zohner16"))

##
## Now fix, what we should fix ##
##

# campbell75, Lizzie entered this, it's a weird measure of time called DARD, it is timeonly
# check my subsetting
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
# shouldbeone[,c(1, 25:27)] # TODO: need to go through each one ... again, make notes about ones we're not changing and change the others (and check your subsetting!)
#View(filter(shouldbeone,datasetID=="boyer"))
###Dan has noew fixed
#"boyer": ->time only, no response    done   
#"cook05" response=50per-> "timeonly"     done
#"cronje03" "response=25per-> "timeonly" done
#"heide12"  response->response.time and response shouldd be timeonly done
#"sonsteby14"  same as heide12 done
#"spiers74" okay...time only, no respone  done
#"webb78"   time only, no response   done
#"zohner16" time only, no response done

###### ask Lizzie about
#"falusi90" respose is actually % budburst, respone time    
#"ghelardini10" cat says this is fine, see her for details ask lizzie about this
# "heide93" cats says same as ghelardin10   

##
##check subsetting
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="boyer"& d$respvar.simple %in%
                          respvar.time)])
nrow(subset(shouldbeone, datasetID=="boyer"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="cook05"& d$respvar.simple %in%
                          respvar.time)])
nrow(subset(shouldbeone, datasetID=="cook05"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="cronje03"& d$respvar.simple %in%
                          respvar.time)])
nrow(subset(shouldbeone, datasetID=="cronje03"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="heide12"& d$respvar.simple %in%
                          respvar.time)])
nrow(subset(shouldbeone, datasetID=="heide12"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="sonsteby14"& d$respvar.simple %in%
                          respvar.time)])
nrow(subset(shouldbeone, datasetID=="sonsteby14"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="spiers74"& d$respvar.simple %in%
                          respvar.time)])
nrow(subset(shouldbeone, datasetID=="spiers74"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="webb78"& d$respvar.simple %in%
                          respvar.time)])
nrow(subset(shouldbeone, datasetID=="webb78"))
length(d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="zohner16"& d$respvar.simple %in%
                          respvar.time)])
nrow(subset(shouldbeone, datasetID=="zohner16"))

## Now fix, what we should fix ##
##
###Fix Boyer
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="boyer"& d$respvar.simple %in%
                   respvar.time)]<-"timeonly"
d$response.time[which(d$response=="timeonly" & d$response.time==""& d$datasetID=="boyer")]<-"no response"
##Fix cook05
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="cook05"& d$respvar.simple %in%
                   respvar.time)]<-"timeonly"
#fix cronje03
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="cronje03"& d$respvar.simple %in%
                   respvar.time)]<-"timeonly"
#fixheide12
fixheide12<-which(d$response!=1 & d$response!="timeonly" & d$datasetID=="heide12"& d$respvar.simple %in%
                    respvar.time)
d$response.time[fixheide12] <- as.numeric(d$response[fixheide12]) 
d$response[fixheide12] <- "timeonly"
#View(subset(d,datasetID=="heide12"))  
#fix sonsteby14
fixsonsteby14<-which(d$response!=1 & d$response!="timeonly" & d$datasetID=="sontsteby14"& d$respvar.simple %in%
                    respvar.time)
d$response.time[fixsonsteby14] <- as.numeric(d$response[fixsonsteby14]) 
d$response[fixsonsteby14] <- "timeonly"
##fix spiers74
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="timeonly"& d$respvar.simple %in%
                   respvar.time)]<-"timeonly"
##fixwebb78
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="webb78"& d$respvar.simple %in%
                   respvar.time)]<-"timeonly"
d$response.time[which(d$response=="timeonly" & d$response.time==">100"& d$datasetID=="webb78")]<-"no response"
#fixzohner16
d$response[which(d$response!=1 & d$response!="timeonly" & d$datasetID=="zohner16"& d$respvar.simple %in%
                   respvar.time)]<-"timeonly"
d$response.time[which(d$response=="timeonly" & d$response.time==""& d$datasetID=="zohner16")]<-"no response"


###############################################################
## Which rows have been looked at through all these queries? ##
###############################################################

checked.rows <- c(row.names(areone), row.names(notwheretheyshouldbe),
    row.names(whynoresponse), row.names(shouldbeone))
checked.rows.nodups <- checked.rows[!duplicated(checked.rows)]

checkedwhat <- d[checked.rows.nodups,]

nrow(checkedwhat)/nrow(d)
