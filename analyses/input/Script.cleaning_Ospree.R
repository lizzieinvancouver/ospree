###Script.cleaning_Ospree
##'
##'1/6/2017
##'Ignacio Morales-Castilla
##'


## to start

rm(list=ls())
options(stringsAsFactors = FALSE)


## read in file
#setwd("C:/Users/Ignacio/Documents/MEGA/Work_Harvard_postdoc/Ospree/")
setwd("~/your.wd/")

ospree<-read.csv("ospree_clean_respvar.csv")
head(ospree)

## fill blanks with NAs
ospree[ospree==""]<-NA

## look at the structure of variables within
str(ospree)


## function to get information from each column and clean afterwards
##'
##' input is the dataset we want information for
##' output:
##' A) the dataset with characters as numericals and constrains NAs to 
##' character strings in those vectors [[1]]
##' B) object "needs.cleaning" indicating whether action is needed in a given vector [[2]]
##'


clean.dataset<-function(x){
  
xdat<-x
need.action<-array(NA,dim=c(ncol(xdat),1))
row.names(need.action)<-colnames(xdat)
for(i in 1:ncol(xdat)){
  print(i)
  
  ## logical indicating if variable is a factor
  isfac<-is.factor(xdat[,i])
  
  if(isfac){
    xdat[,i]<-xdat[,i]
  } else {
    
    ## logical indicating if variable is character
    ischr<-is.character(xdat[,i])
    isint<-is.integer(xdat[,i])
    
  if(ischr){
    tem<-as.numeric(xdat[,i])
    
    if(length(tem)==sum(is.na(tem))){
      xdat[,i]<-xdat[,i]
    } else {
      xdat[,i]<-ifelse(is.na(tem)&is.na(xdat[,i]),NA,
                   ifelse(is.na(tem)&!is.na(xdat[,i]),-9999,tem))
    }
    
  }
    
  ## identifying which columns need action/cleaning of text
    
    
  }
  need.action[i,1]<-ifelse(-9999%in%xdat[,i],"yes","no")
}

return(list(xdat,need.action))
}

ospree.cleaning<-clean.dataset(ospree)



