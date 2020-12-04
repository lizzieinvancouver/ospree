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

#library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
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

ha$chill_level[ha$Total_Chill_portions<1] <- "None (<1)"
ha$chill_level[ha$Total_Chill_portions> 1 & ha$Total_Chill_portions < 44] <- "Low (1-44)"
ha$chill_level[ha$Total_Chill_portions> 44 & ha$Total_Chill_portions < 69] <- "Med (45-69)"
ha$chill_level[ha$Total_Chill_portions> 69 & ha$Total_Chill_portions < 106] <- "High (70-106)"
ha$chill_level[ha$Total_Chill_portions> 106] <- "Very High (>106)"
table(ha$Total_Chill_portions)


ha$chill_level <- factor(ha$chill_level, levels=c("None (<1)", "Low (1-44)", "Med (45-69)","High (70-106)","Very High (>106)"))
library(grid)
ha<-unite(ha,lat.long,provenance.lat,provenance.long,sep=",")
ha<-unite(ha,GENSPA,genus,species,sep=" ")
ha2<-filter(ha,response.time<80)
library(egg)

colorz<-c("red","orange","dark yellow","light blue","purple")

linez<-c("solid","dashed","dotted")
plotx<-ggplot(ha,(aes(as.numeric(photoperiod_day),response.time)))+
  geom_segment(aes(y=90,yend=350,x=8,xend=16.4),size=.3,alpha=0.3)+
  geom_segment(aes(y=-15,yend=75,x=16,xend=24),size=.3,alpha=0.3)+
  #geom_segment(aes(y=-15,yend=66,x=8,xend=16.5),size=2,alpha=0.3)+
  geom_rect(aes(xmin=8,xmax=16,ymin=-15,ymax=90),fill="grey98",color="grey44", alpha = .1)+

  geom_line(aes(color=chill_level,linetype=GENSPA,group=thingy),size=1.1)+ylab("Days to budburst")+xlab("Photoperiod (hours)")+scale_color_manual(values=c("red","sienna2","darkseagreen4","deepskyblue3","purple"),name="Chill Level")+scale_linetype_manual(values=c("solid","dotted","twodash"),name="Taxa",guide=guide_legend(label.theme = element_text(angle = 0, face = "italic")))+theme_base()+theme(legend.key.width = unit(1.5,"cm"))
ploty<-ggplot(ha2,(aes(as.numeric(photoperiod_day),response.time)))+xlim(8,16)+geom_line(aes(color=chill_level,linetype=GENSPA,group=thingy),size=1.1)+theme_base()+theme(legend.position = "none",axis.title.x=element_blank(),axis.title.y=element_blank())+ggtitle("zoom")+scale_color_manual(values=c("red","sienna2","darkseagreen4","deepskyblue3","purple"))+scale_linetype_manual(values=c("solid","dotted","twodash"),name="Taxa",guide=guide_legend(label.theme = element_text(angle = 0, face = "italic")))+theme(panel.background = element_rect(fill = "grey98",color="grey44"))

quantile(ha$Total_Chill_portions)
jpeg("photoperiod/figures/Photo_curv_version3.jpeg",height=1900, width=2500,res=75)
vp <- viewport(width = 0.4, height = 0.7, x = 0.5, y = .99,just=c("left","top"))
plotx
print(ploty, vp = vp, )
dev.off()  

jpeg("photoperiod/figures/Photo_curv_version1.jpeg",height=1000, width=700,res=100)
ggplot(ha,(aes(as.numeric(photoperiod_day),response.time)))+
  geom_line(aes(color=chill_level,linetype=GENSPA,group=thingy),size=1.1)+
ylab("Days to budburst")+xlab("Photoperiod (hours)")+
  scale_color_manual(values=c("red","sienna2","darkseagreen4","deepskyblue3","purple"),name="Chill Level")+scale_linetype_manual(values=c("solid","dotted","twodash"),name="Taxa",guide=guide_legend(label.theme = element_text(angle = 0, face = "italic")))+theme_base()+theme(legend.key.width = unit(1.5,"cm"))
dev.off()
jpeg("photoperiod/figures/Photo_curv_version2.jpeg",height=1000, width=900,res=100)
ggplot(ha,(aes(as.numeric(photoperiod_day),response.time)))+
  geom_line(aes(color=chill_level,linetype=GENSPA,group=thingy),size=1.1)+
  ylab("Days to budburst")+xlab("Photoperiod (hours)")+xlim(8,16)+
  scale_color_manual(values=c("red","sienna2","darkseagreen4","deepskyblue3","purple"),name="Chill Level")+scale_linetype_manual(values=c("solid","dotted","twodash"),name="Taxa",guide=guide_legend(label.theme = element_text(angle = 0, face = "italic")))+theme_base()+theme(legend.key.width = unit(1.5,"cm"))
dev.off()
#adding a new version with differentcolors
jpeg("photoperiod/figures/Photo_curv_version2blue.jpeg",height=1000, width=900,res=100)
blus<-colorRampPalette(brewer.pal(9,"Blues"))(7)[3:7]
ggplot(ha,(aes(as.numeric(photoperiod_day),response.time)))+
  geom_line(aes(color=chill_level,linetype=GENSPA,group=thingy),size=1.1)+
  ylab("Days to budburst")+xlab("Photoperiod (hours)")+xlim(8,16)+
  scale_color_manual(values=alpha(blus, 0.9),name="Chill Level")+scale_linetype_manual(values=c("longdash","dotted","solid"),name="Taxa",guide=guide_legend(label.theme = element_text(angle = 0, face = "italic")))+theme_base()+theme(legend.key.width = unit(1.5,"cm"))
dev.off()

