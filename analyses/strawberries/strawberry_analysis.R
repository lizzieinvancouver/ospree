###run clean merge all up to line 6
rm(list=ls()) 
options(stringsAsFactors = FALSE)
library(ggplot2)
library(lme4)
library(dplyr)


# Set working directory: 
if(length(grep("Lizzie", getwd())>0)) {setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
} else if
(length(grep("ailene", getwd()))>0) {setwd("/Users/aileneettinger/git/ospree/analyses")
}else 
  setwd("~/Documents/git/ospree/analyses")


####Question does selectting on flowering time (ever bearing, June or Day Neutral) influence the control of the leaves? http://strawberryplants.org/2010/05/strawberry-varieties/
#1 classify varieties as midseaon, everbearing, or daylength neutral
#2 mixed model for flowering
#3mixed model for leafing

berries<-read.csv("output/strawberries.csv")
###what are the varieties
table(berries$varetc)

###what are the respvars
table(berries$respvar.simple)

berries$forcetemp <- as.numeric(berries$forcetemp)
berries$photoperiod_day <- as.numeric(berries$photoperiod_day)
berries$Total_Utah_Model<-as.numeric(berries$Total_Utah_Model)
berries$Total_Chilling_Hours<-as.numeric(berries$Total_Chilling_Hours)
berries$Total_Chill_portions<-as.numeric(berries$Total_Chill_portions)
berries$responsedays <- as.numeric(berries$response.time)
berries$response <- as.numeric(berries$response)

condition1<-c("percentbudburst","percentflower")
straw <- filter(berries, respvar.simple %in% condition1)
table(straw$varetc)
###assign them to varclass
#View(filter(straw, varetc==""))

straw$vartype<-NA
straw$vartype[straw$varetc == "Abundance"] <- "June"
straw$vartype[straw$varetc == "Alta"] <- "everbearing"
straw$vartype[straw$varetc == "As"] <- "everbearing"
straw$vartype[straw$varetc == "Florene"] <- "June"
straw$vartype[straw$varetc == "Frida"] <- "June"
straw$vartype[straw$varetc == "Grytoy"] <- "everbearing"
straw$vartype[straw$varetc == "Hardanger"] <- "everbearing"
straw$vartype[straw$varetc == "Haugastol"] <- "everbearing"
straw$vartype[straw$varetc == "Honeoye"] <- "June"
straw$vartype[straw$varetc == "Jonsok"] <- "June"
straw$vartype[straw$varetc == "Korona"] <- "June"
straw$vartype[straw$varetc == "Namsos"] <- "everbearing"
straw$vartype[straw$varetc == "Ostara"] <- "everbearing"
straw$vartype[straw$varetc == "Rabunda"] <- "everbearing"
straw$vartype[straw$varetc == "Revada"] <- "everbearing"
straw$vartype[straw$varetc == "Tribute"] <- "dayneutral"
straw$vartype[straw$varetc == "RH30"] <- "dayneutral"
straw$vartype[straw$varetc == "Senga Sengana"] <- "June"
straw$vartype[straw$datasetID == "verheul07"] <- "June"
#8 June 
#9 everbearing (5 vesca)
#2 day neutral (1 virginiana)
#hypoth June are short day
# Everbearing longday
#dayneutal

table(straw$vartype)
table(straw$species)        
#mixed<-lmer(response~responsedays+responsedays:forcetemp+responsedays:photoperiod_day+(1|vartype), data=straw)
#summary(mixed)
#coef(mixed)
###i Think this is wrong, but why


###filtering and cleaning

bud<-filter(straw, respvar.simple=="percentbudburst")
buddy<-filter(bud, response.time!="")

flo<-filter(straw, respvar=="flowers") ##what is this variable? there are 200 percent value and respvar simpl eis just flowers. Its actually "number not percent"


floy<-filter(flo, response.time!="")

##floy metircs
unique(floy$datasetID)
table(floy$varetc)
table(floy$forcetemp)
table(floy$photoperiod_day)

##bud metrics
unique(buddy$datasetID)
table(buddy$varetc)
table(buddy$forcetemp)
table(buddy$photoperiod_day)

### a few fun exploratory models with just forcing and photoperoid
#####%budburst
mod <-lmer(response~responsedays+responsedays:forcetemp+responsedays:photoperiod_day+(1+vartype|1), data=buddy)
summary(mod)
coef(mod)

###percent flower
mod2 <-lmer(response~responsedays+responsedays:forcetemp+responsedays:photoperiod_day+(1|vartype), data=floy)
table(floy$forcetemp) #Why is floy rank deficient in force temp, they dont work as a mixed model either

#### investigate mod and mod2

###look at jsut phenology ###i think there isnt enough levels for each factor
#flower
flo.phen<-filter(berries, respvar.simple=="daystoflower")

###now leaf
bud.phen<-filter(berries, respvar.simple=="daystobudburst")
bud.phen<- within(bud.phen, response.time[response=="no response" & response.time==""]<-"no response")

fix <- which(bud.phen$figure.table..if.applicable.=="fig 3")
bud.phen$response.time[fix] <- as.numeric(bud.phen$response[fix])
bud.phen<-filter(bud.phen, figure.table..if.applicable.!= "fig 4")


table(flo.phen$datasetID)
table(bud.phen$datasetID)


###to do: add in chilling and ambients