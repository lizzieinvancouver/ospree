###what does the photoperiod response curve look like?
## look at viheraaarnio06
#partenen98
#schnabel87
#howe95
# read in data
#plot responsetime ~ photoperiod
#see if it is linear
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(tidyverse)
library(ggplot2)
library(ggthemes)
setwd("~/Documents/git/ospree/analyses")


bb.all<- read.csv("output/ospree_clean_withchill_BB.csv", header=TRUE)


bb.some <- subset(bb.all, respvar.simple=="daystobudburst"|respvar.simple=="percentbudburst")
bbdat <- subset(bb.some, response.time!="")

colnames(bbdat)
#To Use
# heide93
#ashby62
#cafarra11b

###plots
hei<-subset(bbdat,datasetID=="heide93a"& study=="exp3")
hei<-subset(hei,species=="sylvatica")
hei$days_cent<-hei$response.time-(mean(hei$response.time)/sd(hei$response.time))
#ggplot(hei,aes(as.numeric(photoperiod_day),days_cent))+geom_line(aes(color=population))

hei$thingy[hei$population=="As.Norway"]<-9
hei$thingy[hei$population=="As.Norway (assuming)"]<-10
hei$thingy[hei$population=="Basel"]<-11
hei$thingy[hei$population=="Carpathia Mtns"]<-12
hei$thingy[hei$population=="Copenhagen"]<-13

ash1<-subset(bbdat,datasetID=="ashby62"&population=="Southwestern Michigan")
ash2<-subset(bbdat,datasetID=="ashby62"&population=="Central Wisconsin")
sh1<-subset(ash1,response.time<999)
ash2<-subset(ash2,response.time<999)

ash1$thingy[ash1$fieldsample.date=="11-Dec-1956"]<-"Mich1"
ash1$thingy[ash1$fieldsample.date=="8-Jan-1957"]<-"Mich2" 
ash1$thingy[ash1$fieldsample.date=="5-Feb-1957"]<-"Mich3"   
ash1$thingy[ash1$fieldsample.date=="4-Mar-1957"]<-"Mich4"
ash2$thingy[ash2$fieldsample.date=="11-Dec-1956"]<-"Wisc1"
ash2$thingy[ash2$fieldsample.date=="8-Jan-1957"]<-"Wisc2" 
ash2$thingy[ash2$fieldsample.date=="5-Feb-1957"]<-"Wisc3"   
ash2$thingy[ash2$fieldsample.date=="4-Mar-1957"]<-"Wisc4"  
ash<-rbind(ash2,ash1)
ash$days_cent<-ash$response.time-mean(ash$response.time)/sd(ash$response.time)


caf<-subset(bbdat,datasetID=="caffarra11b"& study=="exp2"& response.time!=999.0)

table(caf$photoperiod_day)
table(caf$forcetemp)
caf$thingy[caf$chilldays==0]<-5
caf$thingy[caf$chilldays==30]<-6
caf$thingy[caf$chilldays==55]<-7
caf$thingy[caf$chilldays==95]<-8

caf$days_cent<-caf$response.time-(mean(caf$response.time)/sd(caf$response.time))

ha<-rbind(caf,hei,ash)

ha$chill_level[ha$Total_Chill_portions<1] <- "None"
ha$chill_level[ha$Total_Chill_portions> 1 & ha$Total_Chill_portions < 44] <- "Low"
ha$chill_level[ha$Total_Chill_portions> 44 & ha$Total_Chill_portions < 69] <- "Med"
ha$chill_level[ha$Total_Chill_portions> 69 & ha$Total_Chill_portions < 106] <- "High"
ha$chill_level[ha$Total_Chill_portions> 106] <- "Very High"
table(ha$Total_Chill_portions)


ha$chill_level <- factor(ha$chill_level, levels=c("None", "Low", "Med","High","Very High"))
library(grid)
ha<-unite(ha,lat.long,provenance.lat,provenance.long,sep=",")
ha<-unite(ha,GENSPA,genus,species,sep=" ")
ha2<-filter(ha,response.time<80)
library(egg)

colorz<-c("red","orange","dark yellow","light blue","purple")

linez<-c("solid","dashed","dotted")
plotx<-ggplot(ha,(aes(as.numeric(photoperiod_day),response.time)))+geom_rect(aes(xmin=8,xmax=16,ymin=0,ymax=55),fill="grey98",color="grey44",linetype="dotted", alpha = .1)+geom_line(aes(color=chill_level,linetype=GENSPA,group=thingy))+ggtitle("A")+ylab("Days to budburst")+xlab("Photoperiod (hours)")+scale_color_manual(values=c("red","dark goldenrod1","darkseagreen4","deepskyblue3","purple"),name="Chill Level")+scale_linetype_manual(values=c("solid","dashed","dotted"),name="Taxa",guide=guide_legend(label.theme = element_text(angle = 0, face = "italic")))
ploty<-ggplot(ha2,(aes(as.numeric(photoperiod_day),response.time)))+xlim(8,16)+geom_line(aes(color=chill_level,linetype=GENSPA,group=thingy))+theme_base()+theme(legend.position = "none",axis.title.x=element_blank(),axis.title.y=element_blank())+ggtitle("B")+scale_color_manual(values=c("red","dark goldenrod1","darkseagreen4","deepskyblue3","purple"))+scale_linetype_manual(values=c("solid","dashed","dotted"),name="Taxa",guide=guide_legend(label.theme = element_text(angle = 0, face = "italic")))

quantile(ha$Total_Chill_portions)

vp <- viewport(width = 0.4, height = 0.6, x = 0.45, y = .95,just=c("left","top"))
plotx+theme_base()
print(ploty, vp = vp, )
  

