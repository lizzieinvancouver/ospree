#Cleaning columns for chilling calculations:fieldsample.dat, chilltemp, and chilldays###############################################
#Started by Ailene January 30, 2017

d<- subset(d, woody=="yes")#not necessary anymore, i think?
# See cleanmerge_all.R for started text #
unique(d$chilltemp)#Need to fix several things here- go back to the papers to figure these out
d[which(d$datasetID=="skre08" & d$chilltemp=="High"),]$chilltemp<-"ambient + 4"#this is skre08 + 4 degrees C; does not specify
d[which(d$datasetID=="skre08" & d$chilltemp=="Low"),]$chilltemp<-"ambient"#this is skre08
d[which(d$datasetID=="skuterud94" & d$chilltemp=="mean of 0, 3, 6"),]$chilltemp<-mean(c(0,3))#this is skuterud94, and there is a mistake in the way the data were entered- these data are from figure 5 in the paper, which is just m ean of 0 and 3 C treatments, so i list the mean of these two
d[which(d$chilltemp=="negative 23 to 13 degrees Celsius"),]$chilltemp<-"ambient"#partanen01, after re-reading paper, the plants were just exposed to ambient conditions (which ranged from negative 23 to 13 degrees Celsius)
d[which(d$chilltemp=="elevated"),]$fieldchill<-"yes"#pagter15, this 
d[which(d$chilltemp=="elevated"),]$chilltemp<-"ambient + 0.76"#pagter15, amount of air warming reported in paper
d[which(d$chilltemp=="ambient plus days at 4C"),]$chilltemp<-4#okie11 exp2
d[which(d$chilltemp=="neg 3,2"),]$chilltemp<--0.5 #man10 - can we change to -0.5? seedlings were chilled for one month at -3, and one month at 2, took the average
d[which(d$chilltemp=="6,.5"),]$chilltemp<-3.25 #li05 - can we change to 3.25? seedlings were chilled for 3 weeks at 6, and 3 weeks at 0.5, took the average
d[which(d$chilltemp=="-3, 3"),]$chilltemp<-1 #lamb37 - 26.6F for 8 hr and 37.4 for 16 hr - take average (33.8F) converted - 1degC
d[which(d$chilltemp=="8, 8, -4"),]$chilltemp <- 4 #jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
d[which(d$chilltemp=="8, 4, 0"),]$chilltemp <- 4 #jones12
d[which(d$chilltemp=="4, 0, -4"),]$chilltemp <- 0 #jones12
d[which(d$chilltemp=="0, 4, 8"),]$chilltemp <- 4 #jones12
d[which(d$chilltemp=="-4, 8, 8"),]$chilltemp <- 4 #jones12
d[which(d$chilltemp=="-4, 0, 4"),]$chilltemp <- 0 #jones12
#d[which(d$chilltemp=="Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C"),]#granhus09
#d[which(d$chilltemp=="<16"),]#falusi90

stop("Not an error, just stopping here to say we're now done cleaning the chilltemp column. The d item in your workspace is now all cleaned up and ready to pull climate data in to estimate field chilling. Yay!")
