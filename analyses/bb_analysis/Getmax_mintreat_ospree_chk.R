
# Getting max-min treatments from ospree dataset
# By Nacho

# read in data for plots

#write.csv(jointdata,"~/MEGA/Work_Harvard_postdoc/Ospree/bb manuscript/jointdatamap.csv")
#write.table(jointdata,"~/MEGA/Work_Harvard_postdoc/Ospree/bb manuscript/jointdatamap.txt",
 #           col.names = T,sep="\t")

jointdata <- read.csv("output/jointdatamap.csv")


# remove S.Hemisphere points
jointdataplots<-jointdata[-which(jointdata$lat<0),]

# make plots
plot(jointdataplots$lat,jointdataplots$minforce,
     xlab="latitude",ylab="minimum forcing",pch=19,cex=1.2,
     col=adjustcolor("black",0.4),cex.lab=1.35,yaxt="n")
axis(2,seq(-5,25,5),seq(-5,25,5),las=2)

plot(jointdataplots$lat,jointdataplots$maxforce,
     xlab="latitude",ylab="maximum forcing",pch=19,cex=1.2,
     col=adjustcolor("black",0.4),cex.lab=1.35,yaxt="n")
axis(2,seq(5,35,5),seq(5,35,5),las=2)

plot(jointdataplots$lat,jointdataplots$minchill,
     xlab="latitude",ylab="minimum chilling",pch=19,cex=1.2,
     col=adjustcolor("black",0.4),cex.lab=1.35
     ,yaxt="n"
)
axis(2,seq(-1000,1500,500),seq(-1000,1500,500),las=2)

plot(jointdataplots$lat,jointdataplots$maxchill,
     xlab="latitude",ylab="maximum chilling",pch=19,cex=1.2,
     col=adjustcolor("black",0.4),cex.lab=1.35
     ,yaxt="n"
)
axis(2,seq(0,4000,1000),seq(0,4000,1000),las=2)

plot(jointdataplots$lat,jointdataplots$maxphoto,
     xlab="latitude",ylab="maximum photoperiod",pch=19,cex=1.2,
     col=adjustcolor("black",0.4),cex.lab=1.35
     ,yaxt="n"
)
axis(2,seq(12,24,2),seq(12,24,2),las=2)



### end

