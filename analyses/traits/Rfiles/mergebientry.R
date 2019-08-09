rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 

#Darwin, could you add your info/path here
if(length(grep("deirdreloughnan", getwd())>0)) {   setwd("~/Desktop/trait_analysis")
} #else if
#(length(grep("XXX", getwd())>0)) {   setwd("XXX") 
#} 

biendat<-read.csv("input/Phylospp_BIEN_traitdata.csv", header=TRUE)

#Need to widen trydata so traits are columns
#First could remove rows with no value

trydat<-read.csv("input/try_cleaning_dl/try_cleanlong_dl.csv", header=TRUE)

#trydatfull<-na.omit(trydat) #now 30290 rows

trydatfull<-subset(trydat, value!="NA") #has 32075 rows
head(trydatfull)

require(tidyr)
wide<-spread(trydatfull, 
             Trait, 
             value)

head(wide)
            