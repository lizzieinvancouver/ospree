### Started 21 December 2015 ###
### By Lizzie ###
### Modified by Harold Eyster ###

## Things to do ##
# (1) Think on models, including population is tricky as it is often completely confounded with origin...
# (2) congenerics analysis
# (7) and more!

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## load libraries
library(ggplot2)
library(nlme)
library(plyr)
library(reshape2)
library(Rmisc)
library(lme4)
library(scales)

## point to files on computer
setwd("C:/Users/Harold/Documents/github/eysterthesis")

## Get files
height<- read.csv("input/height_data_2-13-16.csv", header=TRUE) #Read the height file
germ <- read.csv("input/Germ_Data_2-9-16.csv", header=TRUE) #read the germination file
id<- read.csv("input/id_Data-2-4-16.csv", header=TRUE) #read the id file

## basic data cleaning
#rename the columns
names(height)<- c("trayloc", "substrate", "measuredate", "plantheight" ) 
names(germ) <- c("trayloc", "substrate", "germdate")
names(id) <- c("trayloc", "sp", "strat", "temp", "individual", "location", "substrate")

#reformat the dates 
germ$germdate <- as.Date(germ$germdate, format="%m/%d/%Y")
germ$doy <- as.numeric(format(germ$germdate, "%j"))
height$measuredate <- as.Date(height$measuredate, format="%m/%d/%Y") #this line doesn't work -- won't save in correct format
#height$heightdate <-format(height$measuredate, "%b/%d/%Y")
germ$daysfromstart <- ifelse(germ$doy<100, (germ$doy)+38, (germ$doy)-327) # use start date of 23 Nov 2015, and account for 2016 vs. 2015 date
#create unique identifiers for each seed
germ$uniqueid<- NA
germ$uniqueid<-do.call(paste, c(germ[c("trayloc", "substrate")], sep=""))
id$uniqueid<- NA
id$uniqueid<-do.call(paste, c(germ[c("trayloc", "substrate")], sep=""))
height$uniqueid<- NA
height$uniqueid<-do.call(paste, c(height[c("trayloc", "substrate")], sep="")) 
height$plantheight<-as.numeric(height$plantheight) # make the plant height measurements numerical 

#merge the germ and id data frames
germid <- merge(germ,id,by=("uniqueid"))
colnames(germid)[2]<-"trayloc" 
colnames(germid)[3]<-"substrate"
germid$substrate.y<-NULL #remove duplicate columns 
germid$trayloc.y<-NULL #remove duplicate columns 

#create column for continental origin 
germid$origin[which(grepl("EU", germid$individual)==TRUE)] <- "Europe"
germid$origin[which(grepl("US", germid$individual)==TRUE)] <- "USA"
germid$location<-do.call(paste, c(germid[c("origin", "location")], sep="--"))

## Assign the treatments an average temp
# 18/8, 22.67/12.67C, 27.33/17.33, and 32/22
germid$temp[germid$temp=="LL"] <- (16*8+18*8)/24
germid$temp[germid$temp=="LM"] <- (16*12.67+23.67*8)/24
germid$temp[germid$temp=="HM"] <- (16*27.33+17.33*8)/24
germid$temp[germid$temp=="HH"] <- (16*22+32*8)/24

germid$temp <- as.numeric(germid$temp)


##adding in a step here to round the average temp since you are likely using it as a factor in your analysis, not a continuous variable##EJF
germid$temp <- round(germid$temp,digits=0)

#add column to for current germ rate, setting germinated seeds to 1, ungerminated to 0
germid$germinated <- germid$doy
germid$germinated[is.na(germid$germinated)==TRUE] <- 0
germid$germinated[germid$germinated>0] <- 1

#subset the data by substrate 
germs<-subset(germid, substrate=="Soil", select=uniqueid:germinated) #subset with data from seeds planted in soil
germf<-subset(germid, substrate=="Filter", select=uniqueid:germinated) #subset with data from seeds planted on filter paper

#hist(germs$daysfromstart, breaks=10)
#hist(germf$daysfromstart, breaks=10)
#hist(subset(germid, substrate="Soil", select=daysfromstart), breaks=10 )
#germid$soil[which(grepl("Soil", germid$uniqueid)==TRUE)]<- "10"
#table(germid[which(germid$substrate=="Filter"),],germid$daysfromstart, germid$sp, germid$origin, useNA="always")

table(germs$daysfromstart, germs$sp, useNA="always") #table to view germination date data for soil seeds
table(germf$daysfromstart, germf$sp, useNA="always") #table to view germination date data for filter paper seeds

germcheckf <- table(germf$germinated, germf$sp, useNA="always") 
germchecks <-table(germs$germinated, germs$sp, useNA="always")
# calculate percent germinated
percentgermf<-germcheckf[2,]/(germcheckf[1,]+germcheckf[2,]) #percent germinated for filter paper 
percentgermf
percentgerms<-germchecks[2,]/(germchecks[1,]+germchecks[2,]) #percent germinated for soil
percentgerms



##Plots!  

pd<-position_dodge(.4)

# comparing day of germination for filter/soil and Europe/USA
ggplot(germid,aes(x=sp,y=daysfromstart, fill=substrate))+geom_boxplot() # Compare germination rates between the substrates
ggplot(germid[which(germid$sp!= "PLAMED" & germid$sp!="PLACOR" & germid$substrate=="Soil"),],aes(x=sp,y=daysfromstart, fill=origin))+ylab("Germination Date for Soil")+geom_boxplot() #Compare germination rates between continents for soil plantings
ggplot(germid[which(germid$sp!= "PLAMED" & germid$sp!="PLACOR" & germid$substrate=="Filter"),],aes(x=sp,y=daysfromstart, fill=origin))+ylab("Germination Date for filter")+geom_boxplot() #Compare germination rates between continents for filter plantings
#Comparing congenerics: 

ggplot(germid,aes(x=sp,y=daysfromstart, fill=location, colour=origin))+geom_boxplot() #compare day of germination for each species across locations 



##calculate germination percents for each species by continent
#for soil: 
germinatedsummarys<-
  ddply(subset(germs,sp!="PLAMED" & sp!="PLACOR"), c( "origin", "sp" ), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

#for filter:
germinatedsummaryf<-
  ddply(subset(germs,sp!="PLAMED" & sp!="PLACOR"), c( "origin", "sp" ), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

# plot germination percentages vs. species by continent for soil 
plot2b <-ggplot(germinatedsummarys, aes(x=sp,y=mean, color=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(position=pd, size=3.6)+
  ylab("percent germ")+
  xlab("species")+
  ggtitle("percent germ vs. species by continent")


##Looking more closely at location 
germinatedlocsummarys<-
  ddply(subset(germs, sp!="PLAMED", sp!="PLACOR"), c( "location", "origin" , "sp" ), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

#for filter:
germinatedlocsummaryf<-
  ddply(subset(germs, sp!="PLAMED", sp!="PLACOR"), c( "location", "origin" , "sp" ), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

#plot of germ day vs. species by populaiton
plot1b <-ggplot(germinatedlocsummarys, aes(x=sp,y=mean, color=location, shape=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(position=pd, size=3.6)+
  ylab("percent germ")+
  xlab("species")+
  ggtitle("Percent germination vs. species by population")

# plot germination percentages by location for soil
ggplot(germinatedlocsummarys,aes(x=sp,y=mean, colour=location))+ 
  xlab("species") + ylab("mean percent germination")+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6) + ggtitle("Germination percent vs. location vs. species in soil")

#and for filter paper:
ggplot(germinatedlocsummaryf,aes(x=sp,y=mean, colour=location))+ 
  xlab("species") + ylab("mean percent germination")+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6) + ggtitle("Germination percent vs. location vs. species on paper")


##now look at aggregated species for each continent at each of the eight treatments
germs$treatment<-do.call(paste, c(germs[c("temp", "strat")], sep="_")) #combine strat and temperature into one treatment variable
germf$treatment<-do.call(paste, c(germf[c("temp", "strat")], sep="_")) #combine strat and temperature into one treatment variable
germid$treatment<-do.call(paste, c(germid[c("temp", "strat")], sep="_")) #combine strat and temperature into one treatment variable

germcomps<-subset(germs, sp!="PLAMED" | sp!="PLACOR") #create frame without Europe congenerics
germcompf<-subset(germf, sp!="PLAMED" | sp!="PLACOR") #create frame without Europe congenerics

#creating summaries for treatment and origin for soil:
germinatedsummaryags<-
  ddply(germcomps, c( "origin", "treatment"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

#for filter:
germinatedsummaryagf<-
  ddply(germcompf, c( "origin", "treatment"), summarise,
        mean=mean(germinated), sd=sd(germinated), 
        sem=sd(germinated)/sqrt(length(germinated)))

##plot comparing percent germ rates for continents x  treatments

#plot showing germination response to treatments in the soil seeds
ggplot(germinatedsummaryags,aes(x=as.factor(treatment),y=mean, colour=origin))+ 
  xlab("treatment (temp_strat)") + ylab("mean percent germination")+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6) + ggtitle("Effect of treatment on percent germination of seeds in soil")

#plot showing germination response to treatments in the filter paper seeds
ggplot(germinatedsummaryagf,aes(x=as.factor(treatment),y=mean, colour=origin))+ 
  xlab("treatment (temp_strat)") + ylab("mean percent germination")+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6) + ggtitle("Effect of treatment on percent germination of seeds on filter paper")


### Next section: closer analysis of each species:
spp <- unique(germid$sp)

##new summaries with sp, origin, and treatment 
#for soil: 
germinatedsotsummarys<-
  ddply(germs, c( "sp", "origin", "strat", "temp"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

#for filter:
germinatedsotsummaryf<-
  ddply(germf, c( "sp", "origin", "treatment"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

##plots looking at just one species
#plot of germination percent against treatment, origin, for each species 

ggplot(subset(germinatedsotsummarys, sp=="PLALAN"),aes(x=(as.factor(treatment)),y=mean, color=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6)+
  ylab("PLALAN germ rate")+
  xlab("treatment (temp_strat)")+
  ggtitle("Effect of treatment on percent germination of seeds on soil")


#set up multipanel plot to look at germination date for each species  
plot3b<-ggplot(subset(germinatedsotsummarys, sp!="PLAMED" & sp!="PLACOR"), aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
  geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  facet_grid(sp~strat)+
  ylab("percent germ")+
  xlab("mean temp (C)")+
  ggtitle("percent germ vs. temp by continent and strat for each sp")


#For loop doesn't work: 
pdf(file="germ rate by species.pdf")
for(i in 1:length(spp)){
  print(spp[i])
  #set up multipanel plot -- look at Beth's comments 
  ggplot(subset(germinatedsotsummarys, sp==spp[i]),aes(x=(as.factor(treatment)),y=mean, color=origin))+
  ylab("germ rate")+
  xlab("treatment (temp_strat)")+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6)+
  ggtitle("Effect of treatment on percent germination of seeds on soil ")}
  
  dev.off()

#new calculations to look at individuals:
germs$uniqind<- NA
germs$uniqind<- do.call(paste, c(germs[c("sp", "individual")], sep="_")) #combine sp and individual to obtain a column for unique individual

germinatedindsummarys<-
  ddply(germs, c( "uniqind", "treatment"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

#now plotting this for a unique individuals:
p1<-ggplot(subset(germinatedindsummarys, uniqind=="PLAMAJ_US205"),aes(x=(as.factor(treatment)),y=mean))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6)+
  ylab("PLAMAJ_US205 germ rate")+
  xlab("treatment (temp_strat)")+
  ggtitle("Effect of treatment on germ rate of PLAMAJ_US205 on soil")

p2<-ggplot(subset(germinatedindsummarys, uniqind=="PLALAN_EU35"),aes(x=(as.factor(treatment)),y=mean))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6)+
  ylab("PLALAN_EU35 germ rate")+
  xlab("treatment (temp_strat)")+
  ggtitle("Effect of treatment on germ rate of PLALAN_EU35 on soil")

p3<-ggplot(subset(germinatedindsummarys, uniqind=="TAROFF_EU11"),aes(x=(as.factor(treatment)),y=mean))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6)+
  ylab("TAROFF_EU11 germ rate")+
  xlab("treatment (temp_strat)")+
  ggtitle("Effect of treatment on germ rate of TAROFF_EU11 on soil")

p4<-ggplot(subset(germinatedindsummarys, uniqind=="DACGLO_EU06"),aes(x=(as.factor(treatment)),y=mean))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6)+
  ylab("DACGLO_EU06 germ rate")+
  xlab("treatment (temp_strat)")+
  ggtitle("Effect of treatment on germ rate of DACGLO_EU06 on soil")

multiplot(p1, p2, p3, p4, cols=2)



##Now looking at Germination Date 
germsn<-subset(germs, germinated==1) #first subset data to only look at seeds that germinated
#calculations for date of germination across sp by location
germinatedsummarydatesloc<-
  ddply(subset(germsn, sp!="PLAMED" & sp!="PLACOR"), c( "origin", "sp", "location"), summarise,
        mean=mean(daysfromstart), sd=sd(daysfromstart),
        sem=sd(daysfromstart)/sqrt(length(daysfromstart)))

#plot of germ day vs. species by populaiton

plot1a <-ggplot(germinatedsummarydatesloc, aes(x=sp,y=mean, color=location, shape=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(position=pd, size=3.6)+
  ylab("germ date")+
  xlab("species")+
  ggtitle("Germ date vs. species by population")

#calculations to collapse population into continent
germinatedsummarydates<-
  ddply(subset(germsn, sp!="PLAMED" & sp!="PLACOR"), c( "origin", "sp"), summarise,
        mean=mean(daysfromstart), sd=sd(daysfromstart),
        sem=sd(daysfromstart)/sqrt(length(daysfromstart)))

#plot for germ day vs. species by continent 
plot2a <-ggplot(germinatedsummarydates, aes(x=sp,y=mean, color=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(position=pd, size=3.6)+
  ylab("germ date")+
  xlab("species")+
  ggtitle("Germ date vs. species by continent")


#Calculations for date of germination for each species 

germinatedsummarydatests<-
  ddply(germsn, c( "sp", "origin", "temp" , "strat"), summarise,
        mean=mean(daysfromstart), sd=sd(daysfromstart),
        sem=sd(daysfromstart)/sqrt(length(daysfromstart)))

#plot for each species germination date against treatment, origin
 
ggplot(subset(germinatedsummarydatests,sp==spp[1]), aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line(size=.8) +geom_point(position=pd, size=3.6)+ 
  facet_wrap(~strat, nrow=1)+
  ylab("germ date")+
  xlab("mean temp (C)")+
  ggtitle("Germ date vs. temp by continent and strat")


  #set up multipanel plot to look at germination date for each species  
plot3a<-ggplot(subset(germinatedsummarydatests, sp!="PLAMED" & sp!="PLACOR"), aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
    geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
    facet_grid(sp~strat)+
    ylab("germ date")+
    xlab("mean temp (C)")+
    ggtitle("Germ date vs. temp by continent and strat for each sp")


#Now aggregating the species 
germinatedsummarydates<-
  ddply(subset(germsn, sp!="PLAMED" | sp!="PLACOR"), c( "origin", "treatment"), summarise,
        mean=mean(daysfromstart), sd=sd(daysfromstart),
        sem=sd(daysfromstart)/sqrt(length(daysfromstart)))

#now plotting 
ggplot(germinatedsummarydates,aes(x=(as.factor(treatment)),y=mean, color=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_line() +geom_point(position=pd, size=2.6)+
  ylab("mean germ date for all spp")+
  xlab("treatment (temp_strat)")+
  ggtitle("Effect of treatment on germ date of all  seeds on soil")

asimplemodel <- lm(daysfromstart~origin*temp, data=subset(germs, germinated==1))

mod2<-lmer(daysfromstart~origin*temp*strat +(1|sp/location), data=subset(germs, germinated==1 & sp!="PLAMED" & sp!="PLACOR"))
save(mod2, file="mod2.Rdata")
mod3<-lmer(daysfromstart~origin*temp +(1|sp/location), data=subset(germs, germinated==1 & sp!="PLAMED" & sp!="PLACOR"))

summary(mod3)
#plot(asimplemodel) # some model diagnostics
anova(mod2, mod3)#compares models. Loglik: more negative, better. AIC: higher = better. Takes fewer expl. variables into account 




 

# Height ------------------------------------------------------------------



germsheightna <- merge(germs,height,by=("trayloc")) #merging the germination and height data sets. Including non-gemrinating seeds
germsheight<-germsheightna[which(germsheightna$germinated==1),] #removing the seeds that didn't germinate 

#data cleaning 
germsheight$substrate.y=NULL 
germsheight$uniqueid.y=NULL
names(germsheight)[names(germsheight)=="uniqueid.x"] <- "uniqueid"
names(germsheight)[names(germsheight)=="substrate.x"] <- "substrate"
#Relating measure date and start date 
germsheight$dom <- as.numeric(format(germsheight$measuredate, "%j"))
germsheight$mdaysfromstart <- ifelse(germsheight$dom<100, (germsheight$dom)+38, (germsheight$dom)-327) # use start date of 23 Nov 2015, and account for 2016 vs. 2015 date
germsheight$mdaysfromgerm <- germsheight$mdaysfromstart-germsheight$daysfromstart #this creates a column for how old the seedling was when measured. Seeds that haven't germinated yet will yeild negative values 
ghc<-subset(germsheight, mdaysfromgerm>=0) #subset data to remove false height measurements for seeds that hadn't yet germinated
gh<-subset(ghc, sp!="PLAMED" & sp!="PLACOR" & plantheight!="NA" ) #removing the european congenerics 


#plot of plantheight vs. days from germ for all of the seeds
ggplot(gh,aes(x=as.factor(mdaysfromgerm), y=plantheight))+ 
  ylab("plant height for all spp")+
  xlab("days since germination")+
  geom_boxplot()+
  ggtitle("Plant height vs. days since germination")

#Now for one species 
ggplot(subset(gh, sp=="RUMCRI"),aes(x=(as.factor(mdaysfromgerm)),y=plantheight, color=trayloc))+ 
  ylab("RUMCRI height")+
  xlab("days since germination")+
  geom_point()+
  geom_smooth(method=lm, aes(group=1))+
  ggtitle("RUMCRI height vs. days since germination")


#now for one seedling: 
ggplot(subset(gh, uniqueid=="12f5Soil") ,aes(x=(as.factor(mdaysfromgerm)),y=plantheight))+
  ylab("plant height for all RUMCRI 12f5Soil")+
  xlab("days since germination")+
  geom_point()+
  geom_smooth(method=lm, aes(group=1))+
  ggtitle("Plant height vs. days since germination")

ggplot(subset(gh, uniqind=="PLAMAJ_EU01"), aes(x=(as.factor(mdaysfromgerm)),y=plantheight, group=trayloc))+  
  #geom_boxplot+
  geom_path(size=.6) +
  geom_point(position=pd, size=1.6)+ 
  #facet_grid(sp~strat)+
  ylab("germ date")+
  xlab("mean temp (C)")+
  ggtitle("Germ date vs. temp by continent and strat for each sp")


n<-unique(gh$trayloc)
hlm=data.frame(n)

#run a for loop to calculate lm coef for height vs. time 
for(i in 1:length(n)){
  print(n[i])
  hlm$m[i]<-lm(plantheight~mdaysfromgerm, data=subset(gh, trayloc==n[i]))
}
hlm$b <- lapply(strsplit(as.character(hlm$m), ","), "[", 2)  #split the intercept and slope 
hlm$b<-gsub("\\)","",as.character(hlm$b)) #remove the paranthesis
hlm$gr<-as.numeric(hlm$b) #this columm now has the slope coef in numeric form
hlmn<-subset(hlm, gr!="NA") #removes NA values (i.e. where there is only one height measurment, hence no growth rate could be calulated)
hlms<-merge(hlmn, germs, by.x=("n"), by.y=("trayloc"), all.y=FALSE)
#print(model$n[model$c<0],) # tests for negative values

germinatedsummarygrloc<-
  ddply(hlms, c( "origin", "sp", "location"), summarise,
        mean=mean(gr), sd=sd(gr),
        sem=sd(gr)/sqrt(length(gr)))

#plot of growth rate vs. species by populaiton
plot1c <-ggplot(germinatedsummarygrloc, aes(x=sp,y=mean, color=location, shape=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(position=pd, size=3.6)+
  ylab("growth rate")+
  xlab("species")+
  ggtitle("growth rate vs. species by population")

germinatedsummarygr<-
  ddply(hlms, c( "origin", "sp"), summarise,
        mean=mean(gr), sd=sd(gr),
        sem=sd(gr)/sqrt(length(gr)))

#plot for germ day vs. species by continent 
plot2c <-ggplot(germinatedsummarygr, aes(x=sp,y=mean, color=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(position=pd, size=3.6)+
  ylab("growth rate")+
  xlab("species")+
  ggtitle("growth rate vs. species by continent")

germinatedsummarygrts<-
  ddply(hlms, c( "origin", "sp", "temp", "strat"), summarise,
        mean=mean(gr), sd=sd(gr),
        sem=sd(gr)/sqrt(length(gr)))

#set up multipanel plot to look at growth rate for each species  
plot3c<-ggplot(germinatedsummarygrts, aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
  geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  facet_grid(sp~strat)+
  ylab("growth rate")+
  xlab("mean temp (C)")+
  ggtitle("growth rate vs. temp by continent and strat for each sp")

germinatedsummaryheight<-
  ddply(gh, c( "origin", "sp", "temp", "strat" ,"mdaysfromgerm"), summarise,
        mean=mean(plantheight), sd=sd(plantheight),
        sem=sd(plantheight)/sqrt(length(plantheight)))

germinatedsummaryheight$ots<- do.call(paste,c(germinatedsummaryheight[c("strat", "sp" , "origin", "temp", "mdaysfromgerm")], sep="_"))
hm<-data.frame(germinatedsummaryheight$ots, germinatedsummaryheight$mean)
names(hm)=c("ots", "heightmean")
gh$ots<-do.call(paste, c(gh[c("strat", "sp" ,"origin", "temp", "mdaysfromgerm")], sep="_"))

mgh<-merge(hm, gh, by="ots", all.y=TRUE)


species<-unique(gh$sp)
pdf(file="heightbyspecies.pdf")
for(i in 1:length(species)){
  a<-ggplot(subset(gh, sp==species[i]), aes(x=(mdaysfromgerm),y=plantheight, color=origin, group=trayloc))+  
    geom_line(size=.6, alpha=.2) +geom_point(position=pd, size=1.6, alpha=.2)+ 
    geom_smooth(aes(group = origin), size = 1, method = "lm", se = FALSE, alpha=1)+
    facet_grid(temp~strat)+
    ylab("height (cm)")+
    xlab("days since germination")+
    ggtitle(paste(species[i], " height vs. age by  column=strat(days), row=temp(C)"))
  print(a)
  #ggsave("heightbyspecies.pdf")
}
dev.off()

plot4b<-ggplot(subset(mgh, sp=="DACGLO"), aes(x=(mdaysfromgerm),y=plantheight, color=origin, group=trayloc))+  
  geom_line(size=.6, alpha=.2) +geom_point(position=pd, size=2, alpha=.2)+ 
  #geom_smooth(aes(group = origin), size = 1, method = "lm", se = FALSE, alpha=1)+
  geom_line(aes(x=(mdaysfromgerm),y=heightmean, color=origin, group=origin), size=1, alpha=1)+
  facet_grid(temp~strat)+
  ylab("height (cm)")+
  xlab("mdaysfromgerm")+
  ggtitle("DACGLO height vs. days since germ  by color=continent, column=strat(days), row=temp(C)")

#now with a linear model
plot4a<-ggplot(subset(gh, sp=="DACGLO"), aes(x=(mdaysfromgerm),y=plantheight, color=origin, group=trayloc))+  
  geom_line(size=.6, alpha=.2) +geom_point(position=pd, size=1.6, alpha=.2)+ 
  geom_smooth(aes(group = origin), size = 1, method = "lm", se = FALSE, alpha=1)+
  #geom_line(aes(y=mean(group)), size = 1, method = "lm", se = FALSE, alpha=1)+
  facet_grid(temp~strat)+
  ylab("height (cm)")+
  xlab("mdaysfromgerm")+
  ggtitle("DACGLO height vs. days since germ  by color=continent, column=strat(days), row=temp(C)")
ggsave("heightbyspecies.pdf")
