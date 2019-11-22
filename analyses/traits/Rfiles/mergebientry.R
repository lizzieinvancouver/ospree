rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 

#Darwin, could you add your info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {   setwd("~/Desktop/trait_analysis")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 
setwd("~/Desktop/trait_analysis/input")

biendat<-read.csv("newspp_BIEN_traitdata_Nov11.csv", header=TRUE)

length(unique(biendat$scrubbed_species_binomial))
#94

unique(biendat$trait_name)

biendat<-biendat[,2:25]
#Need to widen trydata so traits are columns
#First could remove rows with no value

trydat<-read.csv("input/try_cleaning_dl/try_cleanlong_dl_allcoln.csv", header=TRUE)

trydatfull<-subset(trydat, value!="NA") #has 32075 rows
head(trydatfull)

require(tidyr)
trywide<-spread(trydatfull, 
             Trait, 
             value)

head(trywide)

names(biendat)
names(trywide)            

trywide$UnitName

#to do:
#merge try pi first and last name
trywide$project_pi<-paste(trywide$FirstName, trywide$LastName)

#make lowercase so easier to merge with bien
names(trywide)<- tolower(names(trywide))

#change column names, which are the same and which are different?
require(stringr)
#First adding "." instead of spaces
names(trywide)<-str_replace_all(names(trywide), c(" " = "." , ";" = ".",":"="" ))
names(trywide)

#make the column names for the same traits the same
colnames(biendat)[colnames(biendat)=="scrubbed_species_binomial"] <- "speciesname"
colnames(biendat)[colnames(biendat)=="unit"] <- "unitname"
colnames(biendat)[colnames(biendat)=="maximum.whole.plant.height"] <- "maximum.plant.height"
colnames(biendat)[colnames(biendat)=="leaf.area.per.leaf.dry.mass"] <- "ldmc"
colnames(biendat)[colnames(biendat)=="leaf.carbon.content.per.leaf.nitrogen.content"] <- "leaf carbon/nitrogen (c/n) ratio"
colnames(biendat)[colnames(biendat)=="leaf.dry.mass.per.leaf.fresh.mass"] <-"leaf.dry.matter.content.per.leaf.water-saturated.mass.(ldmc)"
                  
colnames(trywide)[colnames(trywide)=="plant.developmental.status...plant.age...maturity...plant.life.stage"] <-"whole.plant.growth.form"
colnames(trywide)[colnames(trywide)=="wood density; stem specific density; wood specific gravity"] <-"stem.wood.density"
colnames(trywide)[colnames(trywide)=="leaf.lifespan..longevity"] <-"leaf.life.span"

#the new dataset should simply be a combination of both
2177892+13929
#2191821 columns

require(gtools)
test<-smartbind(biendat, trywide, fill=NA)
#get the right number of rows, but get a warning message that many of the column classes have been changed - longitude, leaf.life.span, ldmc, leaf.dry.matter.content.per.leaf.water-saturated.mass.(ldmc)

#For example longitude
unique(test$leaf.life.span) #does have "LL)" 
unique(test$longitude)
unique(test$ldmc)
unique(test$`leaf.dry.matter.content.per.leaf.water-saturated.mass.(ldmc)`)
str(test)

sort(names(trywide))
unique(trywide$Bedrock...geological.substrate)

require(dplyr)
trywide %>% 
  summarise_all((funs(sum(is.na(.))))) 

unique(trywide$Plant.developmental.status...plant.age...maturity...plant.life.stage)

