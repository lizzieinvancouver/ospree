## Started 29 March 2021 ##
## By Lizzie ##

## Makes reference table for limitingcues ms ##
## Was in countinxns.R but moved here for ease. ##

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("Lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses") 
 } else if(length(grep("ailene", getwd())>0)){  setwd("/Users/aileneettinger/Documents/GitHub/ospree/analyses")
} else setwd("~/Documents/git/ospree/analyses")

###################
# All OSPREE data #
###################
dat <- read.csv("output/ospree_clean.csv", header = TRUE)
dat <- dat[dat$woody=="yes",]
dat <- dat[which(dat$datasetID!="spann04"),] # seems only to treatments during bud formation (I think)
dat$fieldsample.date <- as.Date(dat$fieldsample.date, format="%d-%b-%Y")
dat$doy <- format(dat$fieldsample.date, "%j")
dat$latbi <- paste(dat$genus, dat$species)


## Ref list for papers in the manusrcipt
datasetIDs <- sort(unique(tolower(dat$datasetID)))

refs <- c("\\citep{Ashby:1962aa}","\\citep{Basler:2012}","\\citep{Basler:2014aa}",
        "\\citep{Biasi:2012}", "\\citep{Boyer:1986}",
        "\\citep{Caffarra:2011a}","\\citep{Caffarra:2011b}",
        "\\citep{Calme:1994aa}",
        "\\citep{Campbell:1975aa}", "\\citep{Cannell:1983}",
        "\\citep{Charrier:2011aa}",
        "\\citep{Chavarria:2009aa}",
        "\\citep{Cook:2000aa}",
        "\\citep{Cook:2005aa}",
        "\\citep{Cronje:2003aa}",  "\\citep{Dantec:2014aa}",
        "\\citep{DeVries:1982aa}",     
        "\\citep{Falusi:2003aa}","\\citep{Falusi:1990aa}","\\citep{Falusi:1996aa}",
        "\\citep{Falusi:1997aa}",
        "\\citep{Fu:2013aa}","\\citep{Gansert:2002aa}",
        "\\citep{Ghelardini:2010aa}","\\citep{Gianfagna:1985aa}",
        "\\citep{Gomory:2015aa}",
        "\\citep{Granhus:2009aa}",
        "\\citep{Guak:1998aa}","\\citep{guerriero:1990}",
        "\\citep{Gunderson:2012aa}",
        "\\citep{Hawerroth:2013aa}",
        "\\citep{Hawkins:2012}",
        "\\citep{Heide:2003aa}",
        "\\citep{Heide:2005aa}",
        "\\citep{Heide:2008aa}",
        "\\citep{Heide:2011aa}",
        "\\citep{Heide:2012aa}",
         "\\citep{Heide:2015aa}",
        "\\citep{Heide:1993}","\\citep{Heide:1993a}",
        "\\citep{Howe:1995aa}",
        "\\citep{Jones:2012}",
        "\\citep{Junttila:2012aa}",
        "\\citep{Karlsson:2003aa}",
        "\\citep{Lamb:1948aa}",
        "\\citep{Laube:2014a}","\\citep{Laube:2014b}",
        "\\citep{Li:2005aa}","\\citep{Linkosalo:2006aa}","\\citep{Man:2010aa}",
        "\\citep{Manson:1991aa}",
        "\\citep{Morin:2010aa}","\\citep{Myking:1995}","\\citep{Myking:1997aa}",
        "\\citep{Myking:1998aa}",
        "\\citep{Nienstaedt:1966aa}","\\citep{Nishimoto:1994aa}",
        "\\citep{Okie:2011aa}", "\\citep{Pagter:2015}","\\citep{Partanen:2001aa}", "\\citep{Partanen:2005aa}",
        "\\citep{Partanen:1998aa}","\\citep{Pettersen:1972aa}","\\citep{Pop:2000aa}", "\\citep{ramos:1999}",
        "\\citep{Rinne:1994}",
        "\\citep{Rinne:1997aa}","\\citep{Ruesink:1998aa}","\\citep{Sanz-Perez:2009aa}","\\citep{Sanz-Perez:2010aa}",
        "\\citep{Schnabel:1987aa}",
        "\\citep{Skre:2008aa}",
        "\\citep{Skuterud:1994aa}",
        "\\citep{Sogaard:2008aa}","\\citep{Sonsteby:2013aa}",
        "\\citep{Sonsteby:2014aa}",
        "\\citep{Spiers:1974aa}","\\citep{Swartz:1981aa}",
        "\\citep{Thielges:1976aa}",
        "\\citep{Vihera-Aarnio:2006aa}",
        "\\citep{Webb:1977}","\\citep{Worrall:1967aa}",
        "\\citep{Yazdaniha:1967aa}",
        "\\citep{zohner2016}")


reftable <- data.frame(Dataset=as.character(datasetIDs), Reference=refs)

## End of ref list for papers in the manusrcipt
