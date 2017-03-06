#Cleaning columns for chilling calculations:fieldsample.dat, chilltemp, and chilldays###############################################
#Started by Ailene January 30, 2017

d<- subset(d, woody=="yes")#not necessary anymore, i think?
# See cleanmerge_all.R for started text #
unique(d$chilltemp)#Need to fix several things here- go back to the papers to figure these out
d[which(d$datasetID=="skre08" & d$chilltemp=="High"),]$chilltemp<-"ambient + 4"#this is skre08 + 4 degrees C
d[which(d$datasetID=="skre08" & d$chilltemp=="Low"),]$chilltemp<-"ambient"#this is skre08
d[which(d$datasetID=="skuterud94" & d$chilltemp=="mean of 0, 3, 6"),]$chilltemp<-mean(c(0,3))#this is skuterud94, and there is a mistake in the way the data were entered- these data are from figure 5 in the paper, which is just m ean of 0 and 3 C treatments, so i list the mean of these two
d[which(d$chilltemp=="negative 23 to 13 degrees Celsius"),]$chilltemp<-"ambient"#partanen01, after re-reading paper, the plants were just exposed to ambient conditions (which ranged from negative 23 to 13 degrees Celsius)
d[which(d$chilltemp=="elevated"),]$fieldchill<-"yes"#pagter15, this 
d[which(d$chilltemp=="elevated"),]$chilltemp<-"ambient + 0.76"#pagter15, amount of air warming reported in paper
#d[which(d$chilltemp=="ambient plus days at 4C"),]#okie11 exp2
#d[which(d$chilltemp=="neg 3,2"),]#man10
#d[which(d$chilltemp=="6,.5"),]#li05
#d[which(d$chilltemp=="-3, 3"),]#lamb37
#d[which(d$chilltemp=="8, 8, -4"),]#jones12
#d[which(d$chilltemp=="8, 4, 0"),]#jones12
#d[which(d$chilltemp=="4, 0, -4"),]#jones12
#d[which(d$chilltemp=="0, 4, 8"),]#jones12
#d[which(d$chilltemp=="-4, 8, 8"),]#jones12
#d[which(d$chilltemp=="-4, 0, 4"),]#jones12
#d[which(d$chilltemp=="-4, 0, 4"),]#jones12
#d[which(d$chilltemp=="Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C"),]#granhus09
#d[which(d$chilltemp=="<16"),]#falusi90

stop("Not an error, just stopping here to say we're now done cleaning the chilltemp column. The d item in your workspace is now all cleaned up and ready to pull climate data in to estimate field chilling. Yay!")
