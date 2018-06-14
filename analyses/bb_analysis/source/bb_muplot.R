par(mar=c(5,7,3,2))
plot(x=NULL,y=NULL,xlim=c(-50,150),yaxt='n',ylim=c(0,8),
     xlab="Model estimate change in days to BB",ylab="",main="M1_daysBB_2level.stan")
axis(2, at=1:7, labels=rownames(summary(m1.bb)$summary)[7:1],las=1)
abline(v=0,lty=2,col="darkgrey")
for(i in 1:7){
  pos.y<-(7:1)[i]
  pos.x<-summary(m1.bb)$summary[i,"mean"]
  lines(summary(m1.bb)$summary[i,c("25%","75%")],rep(pos.y,2),col="darkgrey")
  points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
for(spsi in 1:22){
  pos.sps.i<-which(grepl(paste("[",spsi,"]",sep=""),rownames(summary(m1.bb)$summary),fixed=T))[1:7]
  jitt<-runif(1,0.05,0.4)
  pos.y.sps.i<-pos.y-jitt
  pos.x.sps.i<-summary(m1.bb)$summary[pos.sps.i[i],"mean"]
  lines(summary(m1.bb)$summary[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),col=cols)
  points(pos.x.sps.i,pos.y.sps.i,cex=0.8,pch=19,col=cols)
  
}
}
