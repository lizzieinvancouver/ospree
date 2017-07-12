###run clean merge all up to line 6
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)
library(dplyr)

berries<-read.csv("output/strawberries.csv")
table(berries$respvar.simple)
berries$forcetemp <- as.numeric(berries$forcetemp)
berries$photoperiod_day <- as.numeric(berries$photoperiod_day)
berries$chilldays<-as.numeric(berries$chilldays)
berries$responsedays <- as.numeric(berries$response.time)
berries$response <- as.numeric(berries$response)

condition1<-c("percentbudburst","percentflower")
straw <- filter(berries, respvar.simple %in% condition1)




###filtering and cleaning

bud<-filter(berries, respvar.simple=="percentbudburst")
buddy<-filter(bud, response.time!="")

flo<-filter(berries, respvar=="inflorescences") ##what is this variable? there are 200 percent value and respvar simpl eis just flowers. Its actually "number not percent"


floy<-filter(flo, response.time!="")
unique(floy$datasetID)
unique(buddy$datasetID)


##make prediucotrs
buddy$responsedays <- as.numeric(buddy$response.time)
buddy$forcetemp <- as.numeric(buddy$forcetemp)
buddy$photoperiod_day <- as.numeric(buddy$photoperiod_day)
buddy$chilldays<-as.numeric(buddy$chilldays)
buddy$response <- as.numeric(buddy$response)

floy$responsedays<-as.numeric(floy$response.time)
floy$forcetemp <- as.numeric(floy$forcetemp)
floy$photoperiod_day <- as.numeric(floy$photoperiod_day)
floy$chilldays<-as.numeric(floy$chilldays)

###are there enought
#someplots
plot(buddy$photoperiod_day,buddy$response.time)
plot(floy$photoperiod_day,floy$response.time)
plot(buddy$forcetemp,buddy$response.time)
plot(floy$forcetemp,floy$response.time)

### a few fun exploratory models with just forcing and photoperoid
#####%budburst
mod <- lm(response~responsedays+responsedays:forcetemp+responsedays:photoperiod_day, data=buddy)
summary(mod)
table(buddy$photoperiod_day)
table(buddy$chilldays)
###percent flower
mod2 <- lm(response~responsedays+responsedays:forcetemp+responsedays:photoperiod_day, data=floy)
summary(mod2)
table(floy$forcetemp) #Why is floy rank deficient in force temp, they dont work as a mixed model either


###look at jsut phenology ###i think there isnt enough levels for each factor
#flower
flo.phen<-filter(berries, respvar.simple=="daystoflower")
flo.phen$forcetemp <- as.numeric(flo.phen$forcetemp)
flo.phen$photoperiod_day <- as.numeric(flo.phen$photoperiod_day)
flowers<-lm(response.time~forcetemp+photoperiod_day+chilldays, data=flo.phen)
summary(flowers)
plot(flowers)

###now leaf
bud.phen<-filter(berries, respvar.simple=="daystobudburst")
bud.phen<- within(bud.phen, response.time[response=="no response" & response.time==""]<-"no response")

fix <- which(bud.phen$figure.table..if.applicable.=="fig 3")
bud.phen$response.time[fix] <- as.numeric(bud.phen$response[fix])
bud.phen<-filter(bud.phen, figure.table..if.applicable.!= "fig 4")

bud.phen$forcetemp <- as.numeric(bud.phen$forcetemp)
bud.phen$photoperiod_day <- as.numeric(bud.phen$photoperiod_day)
bud.phen$response.time <- as.numeric(bud.phen$response.time)

leaves<-lm(response.time~forcetemp+photoperiod_day, data=bud.phen)
summary(leaves)



###to do: add in chilling and ambients