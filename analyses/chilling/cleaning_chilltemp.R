#Cleaning columns for chilling calculations:fieldsample.dat, chilltemp, and chilldays###############################################
#Started by Ailene January 30, 2017

d<- subset(d, woody=="yes")
# See cleanmerge_all.R for started text #
unique(d$chilltemp)#Need to fix several things here- go back to the papers to figure these out
#d[which(d$chilltemp=="High"),]#this is skre08
#d[which(d$chilltemp=="Low"),]#this is skre08
#d[which(d$chilltemp=="mean of 0, 3, 6"),]#skuterud94
#d[which(d$chilltemp=="negative 23 to 13 degrees Celsius"),]#partanen01
#d[which(d$chilltemp=="elevated"),]#pagter15
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
