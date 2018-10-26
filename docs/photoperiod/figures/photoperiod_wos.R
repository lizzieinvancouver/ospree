#Make a bar graph of percent of studies including photoperiod or daylength
#From WOS search on October 25, 2018:

#TOPIC: ("climate change" AND forecast*) AND TOPIC: ("range shift" OR distribution) 
#Results: 1,474
#Of these, 4 contained photoperiod or daylength in title, abstract or keywords
#whereas 601 contained temperature
#TOPIC: ("climate change" AND forecast*) AND TOPIC: (phenolog*) 
#Results: 249
#Of these, 19 contained photoperiod or daylength as in title, abstract or keywords
#169 contained temperature
phen<-c((19/249)*100, (169/249)*100, 100-(((169+19)/249)*100))
range<-c((4/1474)*100,(601/1474)*100, 100-((605/1474)*100))
wos<-cbind(phen,range)
quartz()
barplot(as.matrix(wos), ylab="% Studies", ylim=c(0,100), col=c("goldenrod","darkred","darkblue"),names.arg = c("phenology", "distributions"))
