#Cleaning columns for chilling calculations:fieldsample.dat, chilltemp, and chilldays###############################################
#Started by Ailene January 30, 2017
dat <- read.csv("analyses/output/ospree_master_clean.csv") #should use the cleaned data file created from Lizzie's "cleanmerge_all.R"" code, after the chilltemps and chilldays have been cleaned with cleaning_chilltemp.R" code
colnames(dat)[17]<-"fsdate_tofix"#the date format in this new file needs to be changed, for the chilling code to work
dat$fieldsample.date<-strptime(strptime(dat$fsdate_tofix, format = "%m/%d/%Y"),format = "%Y-%m-%d")

#use only woody species
dat2 <- subset(dat, woody=="yes")

unique(dat2$chilltemp)#Need to fix
dat2[which(dat2$chilltemp=="High"),]#this is skre08
dat2[which(dat2$chilltemp=="Low"),]#this is skre08
dat2[which(dat2$chilltemp=="mean of 0, 3, 6"),]#skuterud94
dat2[which(dat2$chilltemp=="negative 23 to 13 degrees Celsius"),]#partanen01
dat2[which(dat2$chilltemp=="elevated"),]#pagter15
dat2[which(dat2$chilltemp=="ambient plus days at 4C"),]#okie11 exp2
dat2[which(dat2$chilltemp=="neg 3,2"),]#man10
dat2[which(dat2$chilltemp=="6,.5"),]#li05
dat2[which(dat2$chilltemp=="-3, 3"),]#lamb37
dat2[which(dat2$chilltemp=="8, 8, -4"),]#jones12
dat2[which(dat2$chilltemp=="8, 4, 0"),]#jones12
dat2[which(dat2$chilltemp=="4, 0, -4"),]#jones12
dat2[which(dat2$chilltemp=="0, 4, 8"),]#jones12
dat2[which(dat2$chilltemp=="-4, 8, 8"),]#jones12
dat2[which(dat2$chilltemp=="-4, 0, 4"),]#jones12
dat2[which(dat2$chilltemp=="-4, 0, 4"),]#jones12
           
#chilldat$chilldays<-as.numeric(chilldat$chilldays)
#write.csv(dat4,"analyses/output/ospree_clean_withchill.csv",row.names=FALSE, eol="\r\n")
           