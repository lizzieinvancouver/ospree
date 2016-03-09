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
#library(lme4)
library(scales)
library(nlme)
#library(lmerTest)
## point to files on computer
setwd("C:/Users/Harold/Documents/github/eysterthesis")

## Get files
height<- read.csv("input/height_data_2-13-16.csv", header=TRUE) #Read the height file
germ <- read.csv("input/Germ_Data_2-9-16.csv", header=TRUE) #read the germination file
id<- read.csv("input/id_Data-2-4-16.csv", header=TRUE) #read the id file
lab1<-read.csv("label.csv", header=TRUE) #read in the graph labeling file


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
germid$temp[germid$temp=="LL"] <- round((16*8+18*8)/24, digits=1)
germid$temp[germid$temp=="LM"] <- round((16*12.67+22.67*8)/24, digits=1)
germid$temp[germid$temp=="HM"] <- round((16*17.33+27.33*8)/24, digits=1)
germid$temp[germid$temp=="HH"] <- round((16*22+32*8)/24, digits=1)

germid$temp <- as.numeric(germid$temp)


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


# Germ Rate ---------------------------------------------------------------



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
  ddply(subset(germs, sp!="PLAMED" & sp!="PLACOR"), c( "location", "origin" , "sp" ), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

#for filter:
germinatedlocsummaryf<-
  ddply(subset(germs, sp!="PLAMED" & sp!="PLACOR"), c( "location", "origin" , "sp" ), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

germinatedsummarys$location<-5
plot12bsumm<-rbind(germinatedsummarys, germinatedlocsummarys)

#plot of germ day vs. species by populaiton
plot1b <-ggplot(germinatedlocsummarys, aes(x=sp,y=mean, group=location, shape=origin))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(position=pd, size=3.6)+
  ylab("percent germ")+
  xlab("species")+
  ggtitle("Percent germination vs. species by population")

#combining plot 1b and 2b 
plot12b <-ggplot(plot12bsumm, aes(x=sp,y=mean, group=location, shape=origin))+
  geom_errorbar(data=subset(plot12bsumm, location!="5"),aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8, alpha=.4) + 
  geom_point(data=subset(plot12bsumm, location!="5"), position=pd, size=3.6, alpha=.4)+
  geom_point(data=subset(plot12bsumm, location=="5"), aes(color=origin), position=pd, size=4.5)+
  geom_errorbar(data=subset(plot12bsumm, location=="5"), aes(ymin=mean-sem, ymax=mean+sem, color=origin), width=.2, position=pd, size=1.5)+ 
  ylab("Percent germination")+
  xlab("Species")
  #ggtitle("percent germination vs. species. Local populations in gray, continental mean in color")

#Printing figure 4a
pdf("figure4.pdf", width=11, height=8)
print(plot12b)
dev.off()

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
  ddply(subset(germs, sp!="PLACOR" & sp!="PLAMED"), c( "sp", "origin", "strat", "temp"), summarise,
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

#setting up facit grid labelling

mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="strat") { 
    value[value=="30"] <- "30 days"
    value[value=="60"]   <- "60 days"
  }
  if (var=="temp") { 
    value[value=="11.3"] <- "11.3 °C"
    value[value=="16"]   <- "16 °C"
    value[value=="20.7"]   <- "20.7 °C"
    value[value=="25.3"]   <- "25.3 °C"
  }
  return(value)
}

#set up multipanel plot to look at percent germination for each species  
plot3b<-ggplot(germinatedsotsummarys, aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
  geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  facet_grid(sp~strat, scale="free_y")+theme_bw()+
  ylab("Percent germination")+
  xlab("mean temp (C)")+
  ggtitle("Stratification length (days)") +theme(plot.title=element_text(size=10))+
  geom_text(aes(x, y, label=labs, group=NULL, color=NULL),data=lab1)
  #ggtitle("percent germ vs. temp by continent and strat for each sp")
pdf("figure5.pdf", width=8, height=11)
print(plot3b)
dev.off()

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


# Germ Date ---------------------------------------------------------------

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

#Combining plot1a and 2a:
germinatedsummarydates$location<-5
plot12asumm<-rbind(germinatedsummarydates, germinatedsummarydatesloc)

plot12a <-ggplot(plot12asumm, aes(x=sp,y=mean, shape=origin, group=location))+
  geom_errorbar(data=subset(plot12asumm, location!="5"), aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8, alpha=.4) + 
  geom_point(data=subset(plot12asumm, location!="5"), position=pd, size=3.6, alpha=.4)+
  geom_point(data=subset(plot12asumm, location=="5"), aes(color=origin), position=pd, size=4.5, alpha=.8)+
  geom_errorbar(data=subset(plot12asumm, location=="5"), aes(ymin=mean-sem, ymax=mean+sem, color=origin), alpha=.8, width=.2, position=pd, size=.8)+
  ylab("Days since end of stratification")+
  xlab("Species")
  #ggtitle("Germ date vs. species. Local population in gray, continental averages in color")

#printing figure 
pdf("figure6.pdf", width=11, height=8)
print(plot12a)
dev.off()

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
lab2<-lab1
lab2$y<-20
lab2[13:14, 2] <- 11
lab2[3:4, 2] <- 40
lab2[5:6, 2]<-11
lab2[11:12, 2]<-28
lab2[1:2, 2]<-25
plot3a<-ggplot(subset(germinatedsummarydatests, sp!="PLAMED" & sp!="PLACOR"), aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
    geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
    facet_grid(sp~strat, scales="free_y")+theme_bw()+
    ylab("Days to germination")+
    xlab("mean temp (C)")+
    ggtitle("Stratification length (days)") +theme(plot.title=element_text(size=10))+
    geom_text(aes(x, y, label=labs, group=NULL, color=NULL),data=lab2)
    #ggtitle("Germ date vs. temp by continent and strat for each sp")
pdf("figure7.pdf", width=8, height=11)
print(plot3a)
dev.off()
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

#combining continental averages and location graphs: 
germinatedsummarygr$location<-5
plot12csumm<-rbind(germinatedsummarygr, germinatedsummarygrloc)

plot12c <-ggplot(plot12csumm, aes(x=sp,y=mean, shape=origin, group=location))+
  geom_errorbar(data=subset(plot12csumm, location!="5"), aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8, alpha=.4) + 
  geom_point(data=subset(plot12csumm, location!="5"), position=pd, size=3.6, alpha=.4)+
  geom_point(data=subset(plot12csumm, location=="5"), aes(color=origin), position=pd, size=4.5, alpha=.8)+
  geom_errorbar(data=subset(plot12csumm, location=="5"), aes(ymin=mean-sem, ymax=mean+sem, color=origin), alpha=.8, width=.2, position=pd, size=1)+
  ylab("growth rate (cm/day)")+
  xlab("species")
  #ggtitle("Growth rate vs. species. Local population in gray, continental averages in color")
pdf("figure8.pdf", height=8, width=11)
print(plot12c)
dev.off()

germinatedsummarygrts<-
  ddply(hlms, c( "origin", "sp", "temp", "strat"), summarise,
        mean=mean(gr), sd=sd(gr),
        sem=sd(gr)/sqrt(length(gr)))
lab3<-lab2
lab3$y<-c(0.08)
lab3[5:6, 2] <- .4
#set up multipanel plot to look at growth rate for each species  
plot3c<-ggplot(germinatedsummarygrts, aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
  geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  facet_grid(sp~strat, scale="free_y")+theme_bw()+
  ylab("growth rate (cm/day)")+
  xlab("Mean temperature (C)")+
  ggtitle("Stratification length (days)") +theme(plot.title=element_text(size=10))+
  geom_text(aes(x, y, label=labs, group=NULL, color=NULL),data=lab3)
  #ggtitle("growth rate vs. temp by continent and strat for each sp")

pdf("figure9.pdf", width=8, height=11)
print(plot3c)
dev.off()

germinatedsummaryheight<-
  ddply(gh, c( "origin", "sp", "temp", "strat" ,"mdaysfromgerm"), summarise,
        mean=mean(plantheight), sd=sd(plantheight),
        sem=sd(plantheight)/sqrt(length(plantheight)))

germinatedsummaryheight$ots<- do.call(paste,c(germinatedsummaryheight[c("strat", "sp" , "origin", "temp", "mdaysfromgerm")], sep="_"))
hm<-data.frame(germinatedsummaryheight$ots, germinatedsummaryheight$mean)
names(hm)=c("ots", "heightmean")
gh$ots<-do.call(paste, c(gh[c("strat", "sp" ,"origin", "temp", "mdaysfromgerm")], sep="_"))

mgh<-merge(hm, gh, by="ots", all.y=TRUE)


mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="strat") { 
    value[value=="30"] <- "30 days"
    value[value=="60"]   <- "60 days"
  }
  if (var=="temp") { 
    value[value=="11.3"] <- "11.3 °C"
    value[value=="16"]   <- "16 °C"
    value[value=="20.7"]   <- "20.7 °C"
    value[value=="25.3"]   <- "25.3 °C"
  }
  return(value)
}

species<-unique(gh$sp)
pdf(file="supplement.pdf")
for(i in 1:length(species)){
  a<-ggplot(subset(gh, sp==species[i]), aes(x=(mdaysfromgerm),y=plantheight, color=origin, group=trayloc))+  
    geom_line(size=.6, alpha=.2) +geom_point(position=pd, size=1.6, alpha=.2)+ 
    geom_smooth(aes(group = origin), size = 1, method = "lm", se = FALSE, alpha=1)+
    facet_grid(temp~strat, labeller=mf_labeller)+
    ylab("height (cm)")+
    xlab("days since germination")+
    ggtitle(paste(species[i], " height vs. age by stratification length and temperature"))
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

#----------Measuring Plasticity: coef of variation ------------------------------------


germindsummarys<-
  ddply(subset(germs, sp!="PLAMED" & sp!="PLACOR"), c( "uniqind", "origin" , "sp"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        cv=sd(germinated)/mean(germinated))

germindsummarygr<-
  ddply(hlms, c( "uniqind", "origin" , "sp"), summarise,
        mean=mean(gr), sd=sd(gr),
        cv=sd(gr)/mean(gr))

germindsummarydate<-
  ddply(subset(germsn, sp!="PLAMED" & sp!="PLACOR"), c( "uniqind", "origin" , "sp"), summarise,
        mean=mean(daysfromstart), sd=sd(daysfromstart),
        cv=sd(daysfromstart)/mean(daysfromstart))


# indparent<-unique(subset(germindsummarys$uniqind, germindsummarys$sd!="NA" ))
# pdf("plot5b.pdf")
# for(i in 1:length(indparent)){
# a<-ggplot(subset(germindsummarys, uniqind==indparent[i] & sd!="NA"), aes(x=mean,y=as.factor(cv), color=origin, group=uniqind))+  
#   geom_point(size=3, alhpa=.2)+
#   #geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
#   #facet_grid(sp~strat)+
#   #ylab(paste(indparent[i], " Coefficient of Variation of percent germination"))+
#   #xlab(paste(indparent[i]," mean germination rate for "))+
#   ggtitle("coef of variation vs. mean by continent for ind percent germination")
# print(a)
# }
# dev.off()

pdf("plot5.pdf")
plot5a<-ggplot(subset(germindsummarydate, sd!="NA" & sp!="PLAMEd", sp!="PLACOR"), aes(x=as.factor(mean),y=(cv), color=origin, group=uniqind))+  
  geom_point(size=3, alhpa=.2, position=pd)+
  #geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  #facet_grid(sp~strat)+
  #ylab(paste(indparent[i], " Coefficient of Variation of percent germination"))+
  #xlab(paste(indparent[i]," mean germination rate for "))+
  ggtitle("coef of variation vs. mean by continent for ind days to germination")
print(plot5a)

plot5b<-ggplot(subset(germindsummarys, sd!="NA"), aes(x=as.factor(mean),y=(cv), color=origin, group=uniqind))+  
    geom_point(size=3, alhpa=.2, position=pd)+
  #geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  #facet_grid(sp~strat)+
  #ylab(paste(indparent[i], " Coefficient of Variation of percent germination"))+
  #xlab(paste(indparent[i]," mean germination rate for "))+
  ggtitle("coef of variation vs. mean by continent for ind percent germination")
  print(plot5b)

plot5c<-ggplot(subset(germindsummarygr, sd!="NA"), aes(x=as.factor(mean),y=(cv), color=origin, group=uniqind))+  
  geom_point(size=3, alhpa=.2, position=pd)+
  #geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  #facet_grid(sp~strat)+
  #ylab(paste(indparent[i], " Coefficient of Variation of percent germination"))+
  #xlab(paste(indparent[i]," mean germination rate for "))+
  ggtitle("coef of variation vs. mean by continent for ind growth rate")
print(plot5c)
dev.off()

##averaging across each species

#germindsummarys$cvn<-ifelse(germindsummarys$cv!="NaN", germindsummarys$cv, 0) #setting undefined cv values (resulting from mean==0) to zero


germsummarysp<-
  ddply(subset(germindsummarys, cv!="NaN"), c("origin" , "sp" ), summarise,
        mean=mean(cv), sd=sd(cv),
        sem=sd(cv)/sqrt(length(cv)))
        

germsummarygrp<-
  ddply(subset(germindsummarygr, cv!="NaN"), c( "origin" , "sp"), summarise,
        mean=mean(cv), sd=sd(cv),
        sem=sd(cv)/sqrt(length(cv)))

germsummarydatep<-
  ddply(subset(germindsummarydate, cv!="NaN"), c("origin" , "sp"), summarise,
        mean=mean(cv), sd=sd(cv),
        sem=sd(cv)/sqrt(length(cv)))


plastica<-ggplot(germsummarydatep, aes(x=as.factor(sp),y=(mean), color=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8, alpha=.5) + 
  geom_point(size=3, alpha=.7, position=pd)+
  geom_line(size=.6) + theme_bw()+
  ylab("Mean CV of days to germination")+
  xlab("Species")+
  ggtitle("B")

plasticb<-ggplot(germsummarysp, aes(x=as.factor(sp),y=(mean), color=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8, alpha=.5) + 
  geom_point(size=3, alpha=.7, position=pd)+
  geom_line(size=.6) + theme_bw()+
  ylab("Mean CV of percent germination")+
  xlab("Species")+
  ggtitle("A")

plasticc<-ggplot(germsummarygrp, aes(x=as.factor(sp),y=(mean), color=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8, alpha=.5) + 
  geom_point(size=3, alpha=.7, position=pd)+
  geom_line(size=.6) + theme_bw()+
  ylab("Mean CV  of growth rate")+
  xlab("Species")+
  ggtitle("C")

pdf("figure10.pdf", width=8, height=11)
multiplot(plasticb, plastica, plasticc)
dev.off()
# Models ------------------------------------------------------------------

germs$uniqind<-NA
germs$uniqind<- do.call(paste, c(germs[c("sp", "individual")], sep="_")) #creates a column that defines the individual 

#asimplemodel <- lm(daysfromstart~origin*temp, data=subset(germs, germinated==1))

#------------------------------DATE OF GERMINATION MODELs-------

# #global model with species as fixed effect:
# moddatesf<-lme(daysfromstart~origin*temp*strat*sp, random=~1|location/uniqind, data=subset(germs, germinated==1 & sp!="PLAMED" & sp!="PLACOR"))
# 
# 
# #making a global model with species, location, and individual:
# moddategsli<-lmer(daysfromstart~origin*temp*strat +(1|sp/location/uniqind), data=subset(germs, germinated==1 & sp!="PLAMED" & sp!="PLACOR"))
# save(moddategsli, file="moddategsli.Rdata")
# #global model with species and location as random effects, but removing the individual:
# moddategsl<-lmer(daysfromstart~origin*temp*strat +(1|sp/location), data=subset(germs, germinated==1 & sp!="PLAMED" & sp!="PLACOR"))
# save(moddategsl, file="moddatesgsl.Rdata")
# #global model with just sp as the random effect:
# moddategs<-lmer(daysfromstart~origin*temp*strat +(1|sp), data=subset(germs, germinated==1 & sp!="PLAMED" & sp!="PLACOR"))
# save(moddategs,  file="moddatesgsl.Rdata")

#Modelling each species separately, with temp as factor, and the log of days from start 
moddateliPLALAN<-lme(log(daysfromstart)~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(germs, germinated==1 & sp=="PLALAN" & sp!="PLAMED" & sp!="PLACOR"))
moddateliPLAMAJ<-lme(log(daysfromstart)~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(germs, germinated==1 & sp=="PLAMAJ" & sp!="PLAMED" & sp!="PLACOR"))
moddateliRUMCRI<-lme(log(daysfromstart)~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(germs, germinated==1 & sp=="RUMCRI" & sp!="PLAMED" & sp!="PLACOR"))
moddateliDACGLO<-lme(log(daysfromstart)~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(germs, germinated==1 & sp=="DACGLO" & sp!="PLAMED" & sp!="PLACOR"))
moddateliCAPBUR<-lme(log(daysfromstart)~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(germs, germinated==1 & sp=="CAPBUR" & sp!="PLAMED" & sp!="PLACOR" & temp!="11.3"))
moddateliCHEMAJ<-lme(log(daysfromstart)~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(germs, germinated==1 & sp=="CHEMAJ" & sp!="PLAMED" & sp!="PLACOR"))
moddateliTAROFF<-lme(log(daysfromstart)~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(germs, germinated==1 & sp=="TAROFF" & sp!="PLAMED" & sp!="PLACOR"))

#creating qqplots
pdf("moddate_qqplot.pdf")
qqnorm(moddateliPLALAN, main="normal qq plot for PLALAN germ date")
qqnorm(moddateliPLAMAJ, main="normal qq plot for PLAMAJ germ date")
qqnorm(moddateliCAPBUR, main="normal qq plot for CAPBUR germ date")
qqnorm(moddateliCHEMAJ, main="normal qq plot for CHEMAJ germ date")
qqnorm(moddateliRUMCRI, main="normal qq plot for RUMCRI germ date")
qqnorm(moddateliTAROFF, main="normal qq plot for TAROFF germ date")
qqnorm(moddateliDACGLO, main="normal qq plot for DACGLO germ date")
dev.off()

# 
# #taking out individual, because doesn't significant help the model (alpha =.15)
# #But taking out location also doesn't significantly hurt the model (alpha=.15), and (it's actually marginally better without location)
# moddatelPLALAN<-lme(daysfromstart~origin*temp*strat, random=~1|location, data=subset(germs, germinated==1 & sp=="PLALAN" & sp!="PLAMED" & sp!="PLACOR"))
# save(moddatelPLALAN,  file="moddatelPLALAN.Rdata")
# moddatelPLAMAJ<-lme(daysfromstart~origin*temp*strat, random=~1|location, data=subset(germs, germinated==1 & sp=="PLAMAJ" & sp!="PLAMED" & sp!="PLACOR"))
# save(moddatelPLAMAJ,  file="moddatelPLAMAJ.Rdata")
# moddatelRUMCRI<-lme(daysfromstart~origin*temp*strat, random=~1|location, data=subset(germs, germinated==1 & sp=="RUMCRI" & sp!="PLAMED" & sp!="PLACOR"))
# save(moddatelRUMCRI,  file="moddatelRUMCRI.Rdata")
# moddatelDACGLO<-lme(daysfromstart~origin*temp*strat, random=~1|location, data=subset(germs, germinated==1 & sp=="DACGLO" & sp!="PLAMED" & sp!="PLACOR"))
# save(moddatelDACGLO,  file="moddatelDACGLO.Rdata")
# moddatelCAPBUR<-lme(daysfromstart~origin*temp*strat, random=~1|location, data=subset(germs, germinated==1 & sp=="CAPBUR" & sp!="PLAMED" & sp!="PLACOR"))
# save(moddatelCAPBUR,  file="moddatelCAPBUR.Rdata")
# moddatelCHEMAJ<-lme(daysfromstart~origin*temp*strat, random=~1|location, data=subset(germs, germinated==1 & sp=="CHEMAJ" & sp!="PLAMED" & sp!="PLACOR"))
# save(moddatelCHEMAJ,  file="moddatelCHEMAJ.Rdata")
# moddatelTAROFF<-lme(daysfromstart~origin*temp*strat, random=~1|location, data=subset(germs, germinated==1 & sp=="TAROFF" & sp!="PLAMED" & sp!="PLACOR"))
# save(moddatelTAROFF,  file="moddatelTAROFF.Rdata")
# 
# summary(moddatelPLAMAJ)

#------------------------------Germ Rate Models---------------------------------------------------------------

# #germ rate with species as fixed effect: 
#modratesf<-lme(germinated~origin*temp*strat*sp, random=~1|location/uniqind, data=subset(germs, sp!="PLAMED" & sp!="PLACOR"))
# 
# modrategsli<-lmer(germinated~origin*temp*strat +(1|sp/location/uniqind), data=subset(germs, sp!="PLAMED" & sp!="PLACOR"))
# save(modrategsli,  file="modrategsli.Rdata")
# 
# modrategsl<-lmer(germinated~origin*temp*strat +(1|sp/location), data=subset(germs, sp!="PLAMED" & sp!="PLACOR"))
# save(modrategsl,  file="modrategsl.Rdata")
# 
# modrategs<-lmer(germinated~origin*temp*strat +(1|sp), data=subset(germs, sp!="PLAMED" & sp!="PLACOR"))
# save(modrategs,  file="modrategs.Rdata")

##Modeling with GLM:
#first averaging for each population
germrateloc<-
  ddply(germs, c( "sp", "origin", "strat", "temp", "location"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

germrateind<-
  ddply(germs, c( "sp", "origin", "strat", "temp", "uniqind"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))

lmratePLALAN<-lm(mean~origin*as.factor(temp)*strat, data=subset(germrateloc, sp=="PLALAN"))
lmratePLAMAJ<-lm(mean~origin*as.factor(temp)*strat, data=subset(germrateloc, sp=="PLAMAJ"))
lmrateCAPBUR<-lm(mean~origin*as.factor(temp)*strat, data=subset(germrateloc, sp=="CAPBUR"))
lmrateCHEMAJ<-lm(mean~origin*as.factor(temp)*strat, data=subset(germrateloc, sp=="CHEMAJ"))
lmrateDACGLO<-lm(mean~origin*as.factor(temp)*strat, data=subset(germrateloc, sp=="DACGLO"))
lmrateRUMCRI<-lm(mean~origin*as.factor(temp)*strat, data=subset(germrateloc, sp=="RUMCRI"))
lmrateTAROFF<-lm(mean~origin*as.factor(temp)*strat, data=subset(germrateloc, sp=="TAROFF"))

anova(lmratePLAMAJ)
#qqplot is light-tailed 
plot(lmratePLALAN)

ggplot(subset(germrateloc, sp=="PLALAN"), aes(mean)) + 
    geom_histogram()

#Now modelling the individual: 
modrateliPLALAN<-lme(germinated~origin*temp*strat, random=~1|location/uniqind, data=subset(germs, sp=="PLALAN" & sp!="PLAMED" & sp!="PLACOR"))
save(modrateliPLALAN,  file="modrateliPLALAN.Rdata") # sig better than model w/o ind or loc 
modrateliDACGLO<-lme(germinated~origin*temp*strat, random=~1|location/uniqind, data=subset(germs, sp=="DACGLO" & sp!="PLAMED" & sp!="PLACOR"))
save(modrateliDACGLO,  file="modrateliDACGLO.Rdata") #not sig better than model w/ ind and loc removed (p=1)
modrateliPLAMAJ<-lme(germinated~origin*temp*strat, random=~1|location/uniqind, data=subset(germs, sp=="PLAMAJ" & sp!="PLAMED" & sp!="PLACOR"))
save(modrateliPLAMAJ,  file="modrateliPLAMAJ.Rdata") # not sig better than model w/o ind or loc (p>.5)
modrateliRUMCRI<-lme(germinated~origin*temp*strat, random=~1|location/uniqind, data=subset(germs, sp=="RUMCRI" & sp!="PLAMED" & sp!="PLACOR"))
save(modrateliRUMCRI,  file="modrateliRUMCRI.Rdata") # not sig better than model w/o ind or loc (p>.7)
modrateliTAROFF<-lme(germinated~origin*temp*strat, random=~1|location/uniqind, data=subset(germs, sp=="TAROFF" & sp!="PLAMED" & sp!="PLACOR"))
save(modrateliTAROFF,  file="modrateliTAROFF.Rdata") #not sig better than model w/ ind and loc removed (p=1)
modrateliCAPBUR<-lme(germinated~origin*temp*strat, random=~1|location/uniqind, data=subset(germs, sp=="CAPBUR" & sp!="PLAMED" & sp!="PLACOR"))
save(modrateliCAPBUR,  file="modrateliCAPBUR.Rdata") #not sig better when loc is included (p=.067), and p=1 when ind is removed
modrateliCHEMAJ<-lme(germinated~origin*temp*strat, random=~1|location/uniqind, data=subset(germs, sp=="CHEMAJ" & sp!="PLAMED" & sp!="PLACOR"))
save(modrateliCHEMAJ,  file="modrateliCHAMAJ.Rdata") #not sig better than model w/ ind and loc removed (p=1)

#lmer:
modrateliPLALANrb<-lmer(germinated~origin*as.factor(temp)*strat+(1|location/uniqind), family=binomial(link="logit"), data=subset(germs, sp=="PLALAN" & sp!="PLAMED" & sp!="PLACOR"))
modrateliPLALANr<-lmer(germinated~origin*as.factor(temp)*strat+(1|location/uniqind), data=subset(germs, sp=="PLALAN" & sp!="PLAMED" & sp!="PLACOR"))

library(MASS)
library(lmerTest)
mod<-glm(germinated~origin*as.factor(temp)*strat, family=binomial(link="logit"), data=subset(germs, sp=="CAPBUR"))

modrateliPLALANg<-glmmPQL(germinated~origin*as.factor(temp)*strat, random=~1|location/uniqind, family=binomial(link="logit"), data=subset(germs, sp=="PLALAN" & sp!="PLAMED" & sp!="PLACOR"))

#using type 1. see https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/ 
#Kenward-Roger and Satterthwaite give nearly identicall DenDF, but perhaps KR is better. see: Schaalje, G., McBride, J. and Fellingham, G. (2002). Journal of Agricultural, Biological, and Enviromental Statistics 7, 512-524. 
#Anova of nlme model produces nearly identical DenDfs 
anova(modrateliPLALANrb, ddf = "Kenward-Roger", type=1)
anova(modrateliPLALANrb)
anova(modrateliPLALAN)
#creating qqplots
pdf("modrate_qqplot.pdf")
qqnorm(modrateliPLALAN, main="normal qq plot for PLALAN germ rate")
qqnorm(modrateliPLAMAJ, main="normal qq plot for PLAMAJ germ rate")
qqnorm(modrateliCAPBUR, main="normal qq plot for CAPBUR germ rate")
qqnorm(modrateliCHEMAJ, main="normal qq plot for CHEMAJ germ rate")
qqnorm(modrateliRUMCRI, main="normal qq plot for RUMCRI germ rate")
qqnorm(modrateliTAROFF, main="normal qq plot for TAROFF germ rate")
qqnorm(modrateliDACGLO, main="normal qq plot for DACGLO germ rate")
dev.off()


#-----------------------Growth Rate Models ---------------------

# #growth rate response model with speceis as fixed:
#subsetting to remove variables that have missing data 
hlmss1<-subset(hlms, (sp=="CHEMAJ" & temp!="25.3") | sp=="DACGLO" | sp=="PLALAN" | sp=="PLAMAJ" | sp=="RUMCRI" | sp=="TAROFF" | (sp=="CAPBUR" & temp!="11.3" & temp!="25.3"))
modgrowthsf1<-lme(gr~origin*as.factor(temp)*strat*sp, random=~1|location/uniqind, data=subset(hlms, temp!=11.3 & temp!=25.3)   )
modgrowthsf2<-lme(gr~origin*as.factor(temp)*strat*sp, random=~1|location/uniqind, data=subset(hlms, sp!="CAPBUR" & sp!="CHEMAJ")   )
anova.lme(modgrowthsf2)
anova.lme(modgrowthsf1)
sink(file="modgrowthsf2")
summary(modgrwothsf2)
sink()
dev.off()
# modgrowthgsli<-lmer(gr~origin*temp*strat +(1|sp/location/uniqind), data=subset(hlms, sp!="PLAMED" & sp!="PLACOR"))
# save(modgrowthgsli,  file="modgrowthgsli.Rdata")
# 
# modgrowthgsl<-lmer(gr~origin*temp*strat +(1|sp/location), data=subset(hlms, sp!="PLAMED" & sp!="PLACOR"))
# save(modgrowthgsl,  file="modgrowthgsl.Rdata")
# 
# modgrowthgs<-lmer(gr~origin*temp*strat +(1|sp), data=subset(hlms, sp!="PLAMED" & sp!="PLACOR"))
# save(modgrowthgs,  file="modgrowthgs.Rdata")

modgrowthsliPLALAN<-lme(gr~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(hlms, sp=="PLALAN"))
save(modgrowthsliPLALAN,  file="modgrowthsliPLALAN.Rdata") # modgrowthsliPLALAN is worse when loc removed (p=.001) and worse when ind removed (.0036)
modgrowthsliPLAMAJ<-lme(gr~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(hlms, sp=="PLAMAJ"))
save(modgrowthsliPLAMAJ,  file="modgrowthsliPLAMAJ.Rdata") # this model is the same as model w/o ind (p=1), or when loc is removed (p=.3307) (but actually slightly better w/o ind)
modgrowthsliDACGLO<-lme(gr~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(hlms, sp=="DACGLO"))
save(modgrowthsliDACGLO,  file="modgrowthsliDACGLO.Rdata") # not different from model without loc or ind (p=1)
modgrowthsliCAPBUR<-lme(gr~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(hlms, sp=="CAPBUR" & temp!="11.3" & temp!="25.3" ))
save(modgrowthsliCAPBUR,  file="modgrowthsliCAPBUR.Rdata")# not different from model without loc or ind (p=1)
modgrowthsliTAROFF<-lme(gr~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(hlms, sp=="TAROFF"))
save(modgrowthsliTAROFF,  file="modgrowthsliTAROFF.Rdata")  # not different from model without loc or ind (p=1)
modgrowthsliCHEMAJ<-lme(gr~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(hlms, sp=="CHEMAJ" & temp!=11.3))
save(modgrowthsliCHEMAJ,  file="modgrowthsliCHEMAJ.Rdata") # not different from model without loc or ind (p=1)
modgrowthsliRUMCRI<-lme(gr~origin*as.factor(temp)*strat, random=~1|location/uniqind, data=subset(hlms, sp=="RUMCRI"))
save(modgrowthsliRUMCRI,  file="modgrowthsliRUMCRI.Rdata") #not different from model without ind (p=1), or loc (p=.2166)

#qqplots:
pdf("modgrowth_qqplot.pdf")
qqnorm(modgrowthsliPLALAN, main="normal qq plot for PLALAN growth rate")
qqnorm(modgrowthsliPLAMAJ, main="normal qq plot for PLAMAJ growth rate")
qqnorm(modgrowthsliCAPBUR, main="normal qq plot for CAPBUR growth rate")
qqnorm(modgrowthsliCHEMAJ, main="normal qq plot for CHEMAJ growth rate")
qqnorm(modgrowthsliRUMCRI, main="normal qq plot for RUMCRI growth rate")
qqnorm(modgrowthsliTAROFF, main="normal qq plot for TAROFF growth rate")
qqnorm(modgrowthsliDACGLO, main="normal qq plot for DACGLO growth rate")
dev.off()


#--------------------------------------------------------------------------------------------------------------
#using lme:
lmemodel<-lme(germinated~origin*temp*strat, data=subset(germs, germinated==0 & sp!="PLAMED" & sp!="PLACOR"), random=~1|sp/location/uniqind)

lmemodel2<-lme(daysfromstart~origin*temp*strat, data=subset(germs, germinated==1, sp!="PLAMED" & sp!="PLACOR"), random=~1|sp/location/uniqind)
datagermrate<-subset(germs, sp!="PLAMED" & sp!="PLACOR")
summary(lmemodel2)
#plot(asimplemodel) # some model diagnostics
anova(mod2, mod3)#compares models. Loglik: more negative, better. AIC: lower = better. Takes fewer expl. variables into account 



ggplot(subset(germindsummarys, sd!="NA"), aes(x=as.factor(mean),y=(sd), color=origin, group=uniqind))+  
  geom_point(size=3, alhpa=.2, position=pd)+
  #geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  #facet_grid(sp~strat)+
  #ylab(paste(indparent[i], " Coefficient of Variation of percent germination"))+
  #xlab(paste(indparent[i]," mean germination rate for "))+
  ggtitle("coef of variation vs. mean by continent for ind percent germination")
print(plot5b)


# Congenerics -------------------------------------------------------------
germcon<- subset(germs, location=="Europe--Le Tretien, Switzerland" | location=="Europe--Vlieland, The Netherlands")


conrate<-
  ddply(germcon, c( "uniqind", "location" , "sp"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        cv=sd(germinated)/mean(germinated))

congr<-
  ddply(hlms, c( "uniqind", "location" , "sp"), summarise,
        mean=mean(gr), sd=sd(gr),
        cv=sd(gr)/mean(gr))

condate<-
  ddply(subset(germcon, germinated==1), c( "uniqind", "location" , "sp"), summarise,
        mean=mean(daysfromstart), sd=sd(daysfromstart),
        cv=sd(daysfromstart)/mean(daysfromstart))

condatecv<-
  ddply(subset(condate, cv!="NaN"), c("location" , "sp" ), summarise,
        mean=mean(cv), sd=sd(cv),
        sem=sd(cv)/sqrt(length(cv)))


congrcv<-
  ddply(subset(congr, cv!="NaN"), c( "location" , "sp"), summarise,
        mean=mean(cv), sd=sd(cv),
        sem=sd(cv)/sqrt(length(cv)))

condatecv<-
  ddply(subset(condate, cv!="NaN"), c("location" , "sp"), summarise,
        mean=mean(cv), sd=sd(cv),
        sem=sd(cv)/sqrt(length(cv)))

pdf("plasticity.pdf")

conplastica<-ggplot(condatecv, aes(x=as.factor(sp),y=(mean), color=location))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(size=3, alhpa=.2, position=pd)+
  geom_line(size=.6) +
  ylab("mean coefficient of Variation of individual germ date")+
  xlab("species")+
  ggtitle("mean coef of variation of germ date vs. species by continent")
print(plastica)

plasticb<-ggplot(germsummarysp, aes(x=as.factor(sp),y=(mean), color=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(size=3, alhpa=.2, position=pd)+
  geom_line(size=.6) +
  ylab("mean coefficient of Variation of individual percent germination")+
  xlab("species")+
  ggtitle("mean coef of variation of percent germination vs. species by continent")
print(plasticb)

plasticc<-ggplot(germsummarygrp, aes(x=as.factor(sp),y=(mean), color=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.4, position=pd, size=.8) + 
  geom_point(size=3, alhpa=.2, position=pd)+
  geom_line(size=.6) +
  ylab("mean coefficient of Variation of individual growth rate (cm/day)")+
  xlab("species")+
  ggtitle("mean coef of variation of growth rate vs. species by continent")
print(plasticc)

dev.off()