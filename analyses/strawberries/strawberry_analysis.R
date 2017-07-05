###run clean merge all up to line 6
berries<-filter(d, genus=="Fragaria")
table(berries$respvar.simple)


###filtering and cleaning
bud<-filter(berries, respvar.simple=="percentbudburst")
buddy<-filter(bud, response.time!="")
flo<-filter(berries, respvar=="inflorescences") ##what is this variable? there are 200 percent value and respvar simpl eis just flowers. Its actually "number not percent"


#filter(berries, respvar==c("inflorescences", "flowers"))

floy<-filter(flo, response.time!="")
unique(floy$datasetID)
unique(buddy$datasetID)


##make prediucotrs
buddy$responsedays <- as.numeric(buddy$response.time)
buddy$forcetemp <- as.numeric(buddy$forcetemp)
buddy$photoperiod_day <- as.numeric(buddy$photoperiod_day)

floy$responsedays<-as.numeric(floy$response.time)
floy$forcetemp <- as.numeric(floy$forcetemp)
floy$photoperiod_day <- as.numeric(floy$photoperiod_day)

### a few fun exploratory models
#####%budburst
mod <- lm(response~responsedays+responsedays:forcetemp+responsedays:photoperiod_day, data=buddy)
summary(mod)
###percent flower
mod2 <- lm(response~responsedays+responsedays:forcetemp+responsedays:photoperiod_day, data=floy)
summary(mod2)

#centering
buddy$responsedays_cent <- as.numeric(buddy$response.time)-
  mean(as.numeric(buddy$response.time),na.rm=TRUE)
buddy$forcetemp_cent <- as.numeric(buddy$forcetemp)-
  mean(as.numeric(buddy$forcetemp),na.rm=TRUE)
buddy$photoper_cent <- as.numeric(buddy$photoperiod_day)-
  mean(as.numeric(buddy$photoperiod_day),na.rm=TRUE)

###centered %bb
mod3 <- lm(response~responsedays_cent+responsedays_cent:forcetemp_cent+responsedays_cent:photoper_cent, data=buddy)
summary(mod3)

#centering flower
floy$responsedays_cent <- as.numeric(floy$response.time)-
  mean(as.numeric(floy$response.time),na.rm=TRUE)
floy$forcetemp_cent <- as.numeric(floy$forcetemp)-
  mean(as.numeric(floy$forcetemp),na.rm=TRUE)
floy$photoper_cent <- as.numeric(floy$photoperiod_day)-
  mean(as.numeric(floy$photoperiod_day),na.rm=TRUE)

#cented %flo
mod4 <- lm(response~responsedays_cent+responsedays_cent:forcetemp_cent+responsedays_cent:photoper_cent, data=floy)
summary(mod4)

###look at jsut phenology

flo.phen<-filter(berries, respvar.simple=="daystoflower")
flo.phen$forcetemp <- as.numeric(flo.phen$forcetemp)
flo.phen$photoperiod_day <- as.numeric(flo.phen$photoperiod_day)
flowers<-lm(response.time~forcetemp+photoperiod_day, data=flo.phen)
summary(flowers)
plot(flowers)

#####there was no photo period or temperature manipulation at all!!!!
bud.phen<-filter(berries, respvar.simple=="daystobudburst")
bud.phen<- within(bud.phen, response.time[response=="no response" & response.time==""]<-"no response")

fix <- which(bud.phen$figure.table..if.applicable.=="fig 3")
bud.phen$response.time[fix] <- as.numeric(bud.phen$response[fix])
bud.phen<-filter(bud.phen, figure.table..if.applicable.!= "fig 4")

bud.phen$forcetemp <- as.numeric(bud.phen$forcetemp)
bud.phen$photoperiod_day <- as.numeric(bud.phen$photoperiod_day)

leaves<-lm(response.time~forcetemp+photoperiod_day, data=bud.phen)
summary(leaves)
