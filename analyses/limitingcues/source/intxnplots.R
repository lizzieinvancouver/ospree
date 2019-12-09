## Used in nonlinearities_intxns.R ##

# f(x) to make a plot with vary F x P
intxnplotmefp <- function(df, intxnsvector, intercept, ylim, xlim, xcol,  xlab, ylab, maintext, cexmain){
    df[["bb.nointxn"]] <- intercept + feff*df[["force"]] + peff*df[["photo"]] + ceff*df[["chill"]]
    df[["bb"]] <- intercept+ feff*df[["force"]] + peff*df[["photo"]] + ceff*df[["chill"]] +
        fpeff*(df[["force"]]*df[["photo"]]) +
        fceff*(df[["force"]]*df[["chill"]]) + pceff*(df[["chill"]]*df[["photo"]])
plot(df[["bb"]]~df[[xcol]], data=df, type="l", col=colz[1], ylim=ylim, xlim=xlim, main=maintext, cex.main=cexmain, xlab=xlab, ylab=ylab)
lines(df[["bb.nointxn"]]~df[[xcol]], data=df, col=colz[3])
text(df[[xcol]][length(df[[xcol]])]+1,df[["bb"]][length(df[["bb"]])], labels="All 2-way intxns",col=colz[1], cex=0.5)
text(df[[xcol]][length(df[[xcol]])]+1,df[["bb.nointxn"]][length(df[["bb.nointxn"]])], labels="bb.nointxn",col=colz[3], cex=0.5)
ncols<-dim(df)[2]
for(i in 1:length(intxnsvector)){
    newcol <- intercept + feff*df[["force"]] + peff*df[["photo"]] + ceff*df[["chill"]] + intxnsvector[i]*(df[["force"]]*df[["photo"]]) + 
        fceff*(df[["force"]]*df[["chill"]]) + pceff.alt*(df[["chill"]]*df[["photo"]])
    df <- cbind(df,newcol)
    colnames(df)[ncols+i]<-paste("bb.alt", intxnsvector[i], sep="_")
    lines(newcol~df[[xcol]], col=colz[2])
    text(df[[xcol]][length(df[[xcol]])]+1,newcol[length(newcol)], labels=paste("f*p=",intxnsvector[i]),col=colz[2], cex=0.5)
}
}

# f(x) to make a plot with vary F x C
intxnplotmefc <- function(df, intxnsvector, intercept, ylim, xlim, xcol,  xlab, ylab, maintext, cexmain){
    df[["bb.nointxn"]] <- intercept + feff*df[["force"]] + peff*df[["photo"]] + ceff*df[["chill"]]
    df[["bb"]] <- intercept + feff*df[["force"]] + peff*df[["photo"]] + ceff*df[["chill"]] +
        fpeff*(df[["force"]]*df[["photo"]]) +
        fceff*(df[["force"]]*df[["chill"]]) + pceff*(df[["chill"]]*df[["photo"]])
plot(df[["bb"]]~df[[xcol]], data=df, type="l", col=colz[1], ylim=ylim, xlim=xlim, main=maintext, cex.main=cexmain, xlab=xlab, ylab=ylab)
lines(df[["bb.nointxn"]]~df[[xcol]], data=df, col=colz[3])
text(df[[xcol]][length(df[[xcol]])]+1,df[["bb"]][length(df[["bb"]])], labels="All 2-way intxns",col=colz[1], cex=0.5)
text(df[[xcol]][length(df[[xcol]])]+1,df[["bb.nointxn"]][length(df[["bb.nointxn"]])], labels="bb.nointxn",col=colz[3], cex=0.5)
ncols<-dim(df)[2]
for(i in 1:length(intxnsvector)){
    newcol <- intercept + feff*df[["force"]] + peff*df[["photo"]] + ceff*df[["chill"]] + fpeff*(df[["force"]]*df[["photo"]]) + 
        intxnsvector[i]*(df[["force"]]*df[["chill"]]) + pceff.alt*(df[["chill"]]*df[["photo"]])
    df <- cbind(df,newcol)
    colnames(df)[ncols+i]<-paste("bb.alt", intxnsvector[i], sep="_")
    lines(newcol~df[[xcol]], col=colz[2])
    text(df[[xcol]][length(df[[xcol]])]+1,newcol[length(newcol)], labels=paste("f*c=",intxnsvector[i]),col=colz[2], cex=0.5)
}
}
