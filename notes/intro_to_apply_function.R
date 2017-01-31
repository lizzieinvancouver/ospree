#Short demo on how to use apply() functions in R
#Ailene Ettinger
#January 31, 2017
#There are a suite of commands related to apply: tapply, lapply, sapply.
#We'll start with the basic apply, and i'll show you a few others that i use alot.
options(stringsAsFactors = FALSE)
setwd("~/git/ospree")
library(dplyr)

#First, create a matrix of random numbers
set.seed(1)  # Makes the call to rnorm generate the same numbers every time
( mymat <- matrix(round(rnorm(16,10),2),4,4) )#rnorm generations random numbers (16 numbers in this case, with a mean of 10).

#apply allows you to apply some function to the rows or columns of matrix
#Say you wanted to know the means of all columns in mymat.
#you could do this one column at a time:
mean(mymat[,1]);mean(mymat[,2]);mean(mymat[,3]);mean(mymat[,4])

# but with apply, you can do this all at once:
apply(mymat, 2, mean)

#You can also do this for by row:
apply(mymat, 1, mean)

#you can write your own functions and apply them, too. 
#For example, say we wanted to know the first instance in a row when a value is less than 10
get.first<-function(x) min(which(x<10))
f<-apply(mymat,1,get.first)
f
#Now lets use the OSPREE database to try a few other things with apply and its related functions
d <- read.csv("analyses/output/ospree_master_clean.csv") #should use the cleaned data file created from Lizzie's "cleanmerge_all.R"" code, after the chilltemps and chilldays have been cleaned with cleaning_chilltemp.R" code
#use only woody species
d2 <- subset(d, woody=="yes")



#Now Let's look at just days to budburst:
d2.daysbb <- d2[which(d2$respvar.simple=="daystobudburst"),]

#one way that i use apply a lot is to remove nas. For example, here is some code from the fieldchillcalc_latlong.R file:
#selecting out european studies using the dplyr package
eur <- d2 %>% # start with the data frame
  distinct(datasetID, .keep_all = TRUE) %>% # establishing grouping variables
  filter(continent == 'europe' & year >= 1950) %>%#select out europe
  dplyr::select(datasetID, provenance.lat, provenance.long, year,fieldsample.date)
eur <- eur[apply(eur, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

#I use tapply a lot which is useful for making summary tables.
#Let's make a table to see how mean days to budburst varies by species
d2.daysbb$gensp<-paste(d2.daysbb$genus,d2.daysbb$species,sep=".")
d2.daysbb$response.time<-as.numeric(as.character(d2.daysbb$response.time))
(sp.means<-tapply(d2.daysbb$response.time,d2.daysbb$gensp,mean,na.rm=T))
#standard deviation:
(sp.sd<-tapply(d2.daysbb$response.time,d2.daysbb$gensp,sd,na.rm=T))

#You can use more than one index to summarize a vector
#For example, let's see how days to budburst varies by continunent and material
x<-tapply(d2.daysbb$response.time,list(d2.daysbb$continent,d2.daysbb$material),mean, na.rm=T)

#I use the sapply and lapply fuctions less often, but are some examples of them:
x<-subset(d2.daysbb,select=chilltemp:forcetemp)
x$chilltemp<-as.numeric(x$chilltemp);x$chillphotoperiod<-as.numeric(x$chillphotoperiod);
x$chilldays<-as.numeric(x$chilldays);x$forcetemp<-as.numeric(x$forcetemp)

lapply(x,mean, na.rm=T)#get means of the columns, in the form of a list
sapply(x,mean, na.rm=T)#get means of the columns, as a vector

