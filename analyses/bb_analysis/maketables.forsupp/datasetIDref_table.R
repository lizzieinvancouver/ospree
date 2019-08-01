#Make table summarizing datasetID and the reference it goes with
# housekeeping
options(stringsAsFactors = FALSE)

#read in file with different lat/longs from PEP and chiling estimates
datasetIDs<-sort(unique(moddat$datasetID))
refs<-c("\\citep{Basler:2012}","\\citep{Basler:2014aa}","\\citep{Biasi:2012}",
        "\\citep{Caffarra:2011a}","\\citep{Caffarra:2011b}","\\citep{Calme:1994aa}",
        "\\citep{Campbell:1975aa}","\\citep{Chavarria:2009aa}","\\citep{Cook:2000aa}",
        "\\citep{Falusi:2003aa}","\\citep{Falusi:1990aa}","\\citep{Falusi:1996aa}",
        "\\citep{Falusi:1997aa}","\\citep{Ghelardini:2010aa}","\\citep{Gianfagna:1985aa}",
        "\\citep{Gomory:2015aa}","\\citep{Guak:1998aa}","\\citep{guerriero:1990}",
        "\\citep{Heide:2012aa}","\\citep{Heide:1993}","\\citep{Heide:1993a}",
        "\\citep{Jones:2012}","\\citep{Laube:2014a}","\\citep{Laube:2014b}",
        "\\citep{Li:2005aa}","\\citep{Linkosalo:2006aa}","\\citep{Man:2010aa}",
        "\\citep{Morin:2010aa}","\\citep{Myking:1995}","\\citep{Myking:1997aa}",
        "\\citep{Myking:1998aa}","\\citep{Pagter:2015}","\\citep{Partanen:2001aa}",
        "\\citep{Partanen:1998aa}","\\citep{ramos:1999}","\\citep{Rinne:1994}",
        "\\citep{Rinne:1997aa}","\\citep{Sanz-Perez:2009aa}","\\citep{Sanz-Perez:2010aa}",
        "\\citep{Schnabel:1987aa}","\\citep{Skuterud:1994aa}","\\citep{Sonsteby:2014aa}",
        "\\citep{Spann:2004aa}","\\citep{Spiers:1974aa}","\\citep{Swartz:1981aa}",
        "\\citep{Thielges:1976aa}","\\citep{Webb:1977}","\\citep{Worrall:1967aa}",
        "\\citep{zohner2016}")
reftable<-cbind(datasetIDs,refs)
