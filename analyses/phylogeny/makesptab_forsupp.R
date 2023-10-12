
library(tidyr)

d = read.csv("input/datasetforphyloms.csv")
d$spname<-paste(d$genus,d$species,sep=" ")
spnames<-sort(unique(d$spname))

#now create lookup table for datasetID and refs
datasetIDs<-sort(unique(d$datasetID))
refs<-c("Basler:2012","Basler:2014aa","Caffarra:2011a","Caffarra:2011b","Calme:1994aa",
        "Falusi:2003aa","Falusi:1990aa","Falusi:1996aa",
        "Falusi:1997aa","flynn2018","Ghelardini:2010aa","Heide:1993","Heide:1993a",
        "Laube:2014a","Laube:2014b",
        "Li:2005aa","Linkosalo:2006aa","malyshev2018","Man:2010aa",
        "Morin:2010aa","Myking:1995","Myking:1997aa",
        "Myking:1998aa","Pagter:2015","Rinne:1994",
        "Rinne:1997aa","Sanz-Perez:2009aa","Sanz-Perez:2010aa",
        "Skuterud:1994aa","Thielges:1976aa","ettinger2020","Webb:1977","zohner2016")
reftable<-cbind(datasetIDs,refs)
colnames(reftable)<-c("datasetID","ref")
#merge in refs
drefs<-left_join(d,reftable, copy=TRUE)
#get studies that go with the refnames
drefs$ref_sp<-paste(drefs$ref,drefs$spname,sep="_")
sp.st <- drefs %>% # start with the data frame
  distinct(ref_sp, .keep_all = TRUE) %>% # establishing grouping variables
  dplyr::select(ref, spname)
sp.st$study<-"study"
studynum<-rowSums(table(sp.st$spname,sp.st$ref))
spreftab<-table(sp.st$spname,sp.st$ref)
sp.df<-as.data.frame(matrix(ncol = 3, nrow = length(spnames)))
#provide column names
colnames(sp.df) <- c('spname', 'numstuds', 'ref')
for (i in 1:length(spnames)){
  x<-spreftab[i,]
  sp.df$spname[i]<-spnames[i]
  sp.df$ref[i]<-paste(names(x[which(x>0)]), collapse=",")
  sp.df$numstuds[i]<-length(x[which(x>0)])
}

head(sp.df)
sp.df$refxtab<-paste("\\citep{",sp.df$ref,"}", sep="")
reftable<-subset(sp.df,select=c(spname, numstuds,refxtab))
