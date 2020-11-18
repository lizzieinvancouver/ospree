### Started January 8 2019 ###

## DL getting to know the BIEN Data ##

#The aim of this code is to better understand the scope and completeness of the BIEN data downloaded in December 2018##

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/treegarden/budreview/ospree/bb_analysis") 
} else setwd("~/Documents/github/ospree/analyses/traits")

dat<-read.csv("input/Phylospp_BIEN_traitdata.csv", header=TRUE)
dat<-dat[,2:25] #remove the first col that is just numbers
names(dat)[1]<-"sp.name"
head(dat)

#Questions I want to ask:
#How many traits do we have for each species?
#How many studies are there for each trait? 
#Are there some lab groups that more exhaustively measure traits or focus on certain traits?


head(long.sub)
names(long)
unique(long.sub$trait.name)
la<-subset(long.sub, project_pi=="Royer DL");head(la)
unique(la$trait.name)
unique(long.sub$project_pi)

#############################################
#Could I write a loop to give me the above information?
#First I should put the data back into a long format
require(tidyr)
names(dat)
long<-gather(dat, trait.name, value, flower.pollination.syndrome:whole.plant.woodiness)

#Remove all the NA's that resulted from having so many studies with so few traits
long.sub<-subset(test, value!="NA")
names(long.sub)
head(long.sub)

require(plyr)
unique(long.sub$trait.name)
test3<-ddply(long.sub,c("sp.name","project_pi","id"), summarise, freq=count(unique(trait.name)))
             
test3<-ddply(la,c("sp.name","project_pi","id"), summarise, freq=length(unique(trait.name)))
test3



test4<-count(unique(la$trait.name)); test4
head(test3)
unique(long.sub$sp.name[1])

for (i in 1:(unique(long.sub$sp.name))){
  list(long.sub$trait.name[long.sub$sp.name[i]])
  traitadd<-rbind(traitadd, traittemp)
}


#######################################################
#In the interest of time, I am only going to focus on the traits we might actually use and do this the more hack way
names(long.sub)
unique(long.sub$trait.name)

#Flower. pollination. syndrome
fps<-subset(long.sub, trait.name=="flower.pollination.syndrome")
unique(fps$sp.name) 
#"Alnus glutinosa"      "Robinia pseudoacacia" "Quercus robur"  

fps$id
#15849688 15604796 15594500

####################################################################
#leaf area per leaf dry mass
lapdm<-subset(long.sub, trait.name=="leaf.area.per.leaf.dry.mass")
unique(lapdm$sp.name) 
# 55 species, the most of all of the traits

length(unique(lapdm$id))
#There are 2143 unique ID
unique(lapdm$project_pi) #there are 18 PI

####################################################################
#leaf area per leaf fresh mass
lapfm<-subset(long.sub, trait.name=="leaf.dry.mass.per.leaf.fresh.mass")
unique(lapfm$sp.name) 
# 26 spp

length(unique(lapfm$id))
#There are 237 unique ID
unique(lapfm$project_pi) #there are 2 PI

####################################################################
#leaf.area
la<-subset(long.sub, trait.name=="leaf.area")
unique(la$sp.name) 
#51 spp

length(unique(la$id))
#There are 1220 unique ID
unique(la$project_pi) #there are 14 PI

####################################################################
#whole.plant.height
wht<-subset(long.sub, trait.name=="whole.plant.height")
unique(wht$sp.name) 
# 52, again unclear how

length(unique(wht$id))
#There are 2170184 unique ID
unique(wht$project_pi) #there are 16 PI

####################################################################
#maximum.whole.plant.height
mht<-subset(long.sub, trait.name=="maximum.whole.plant.height")
unique(mht$sp.name) 
# 25 spp

length(unique(mht$id))
#There are 34 unique ID
unique(mht$project_pi) #only 1 pi

####################################################################
#stem.wood.density
wd<-subset(long.sub, trait.name=="stem.wood.density")
unique(wd$sp.name) 
# 46 spp

length(unique(wd$id))
#There are 216 unique ID
unique(wd$project_pi) #there are 7 PI

####################################################################
#leaf.carbon.content.per.leaf.nitrogen.content
lcc<-subset(long.sub, trait.name=="leaf.carbon.content.per.leaf.nitrogen.content")
unique(lcc$sp.name) 
# 33 species

length(unique(lcc$id))
#There are 146 unique ID
unique(lcc$project_pi) #there are 3 PI

####################################################################
#seed.mass
seed<-subset(long.sub, trait.name=="seed.mass")
unique(seed$sp.name) 
# 48

length(unique(seed$id))
#There are 529 unique ID
unique(seed$project_pi) #there are 5 PI

