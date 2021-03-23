###Figuring out how interactions work
### March 23,2021

pdf("basicinxnsplot.pdf",height = 12, width =15)
#windows()
par(mfrow=c(3,3), mar=c(5, 8, 4, 2) + 0.1)
x1<-seq(1,20,by=1)
x2<-seq(1,20, by = 1)
b1<- 2
b2<- 2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)
plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),xlim=range(x1),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="Both coefs positive")
text(x1[length(x1)]-2,ypos[length(ypos)],"+x1,+x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"+x1,+x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"+x1,+x2, no intxn", col = "black")
mtext("Preds +", side = 2,line = 8, adj = 0, padj =1, las =1)

#x1<-seq(-20,-1, by = 1)
#x2<-seq(-20,-1, by = 1)
b1<- -2
b2<- -2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)

plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="Both coefs negative")
text(x1[length(x1)]-2,ypos[length(ypos)],"+x1,+x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"+x1,+x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"+x1,+x2, no intxn", col = "black")


#x1<-seq(1,20,by=1)
#x2<-seq(-20,-1, by = 1)
b1<- -2
b2<- 2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)

plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="1 positive,1 negative coef")
text(x1[length(x1)]-2,ypos[length(ypos)],"+x1,+x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"+x1,+x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"+x1,+x2, no intxn", col = "black")

#now with two negative predictors
x1<-seq(-20,-1, by = 1)
x2<-seq(-20,-1, by = 1)

b1<- 2
b2<- 2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)
plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),xlim=range(x1),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="Both coefs positive")
text(x1[length(x1)]-2,ypos[length(ypos)],"-x1,-x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"-x1,-x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"-x1,-x2, no intxn", col = "black")
mtext("Preds -", side = 2,line = 8, adj = 0, padj =1, las =1)


b1<- -2
b2<- -2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)

plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="Both coefs negative")
text(x1[length(x1)]-2,ypos[length(ypos)],"-x1,-x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"-x1,-x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"-x1,-x2, no intxn", col = "black")

b1<- -2
b2<- 2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)

plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="1 positive,1 negative coef")
text(x1[length(x1)]-2,ypos[length(ypos)],"-x1,-x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"-x1,-x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"-x1,-x2, no intxn", col = "black")

#one negative one positive predictor
x1<-seq(1,20,by=1)
x2<-seq(-20,-1, by = 1)
b1<- 2
b2<- 2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)
plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),xlim=range(x1),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="Both coefs positive")
text(x1[length(x1)]-2,ypos[length(ypos)],"+x1,-x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"+x1,-x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"+x1,-x2, no intxn", col = "black")
mtext("Preds + and -", side = 2,line = 8, adj = 0, padj =1, las =1)


b1<- -2
b2<- -2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)

plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="Both coefs negative")
text(x1[length(x1)]-2,ypos[length(ypos)],"+x1,-x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"+x1,-x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"+x1,-x2, no intxn", col = "black")



b1<- -2
b2<- 2
b3pos<- .5
b3neg<- -.5
int<- 100
ynoint<-int + b1*x1 + b2*x2 
ypos<-int + b1*x1 + b2*x2 + b3pos*(x1*x2)
yneg<-int + b1*x1 + b2*x2 + b3neg*(x1*x2)

plot(x1,ypos, ylim=c(min(c(ypos,yneg,ynoint)),max(c(ypos,yneg,ynoint))),type = "l", col="darkblue",xlab="x",ylab="y", bty="l", main ="1 positive,1 negative coef")
text(x1[length(x1)]-2,ypos[length(ypos)],"+x1,-x2,+intxn", col = "darkblue")
lines(x1,yneg,col = "darkred")
text(x1[length(x1)]-2,yneg[length(yneg)],"+x1,-x2,-intxn", col = "darkred")
lines(x1,ynoint,col = "black")
text(x1[length(x1)]-2,ynoint[length(ynoint)],"+x1,-x2, no intxn", col = "black")
