
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Utility function to plot 'random effects' from stan output - used now mostly in Fig 3.
# Taken from Pheno Budburst analysis. R
# Updates by Lizzie to show 50% credible intervals 
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

plotlet <- function(x, y, x.scale, y.scale, xlab=NULL, ylab=NULL, data, groups = NULL, ...){
  if(is.null(xlab)) xlab = x; if(is.null(ylab)) ylab = y
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
    else {
      colz = c("brown", "blue3")
      ccolz = rep(colz[1], length(groups))
      ccolz[groups == 2] = colz[2]
      col.pch = ccolz
      col.lines = alpha(ccolz, 0.4)
    }
  
  plot(
  data[grep(paste(x,"\\[",sep=""), rownames(data)),1]*x.scale,
  data[grep(paste(y,"\\[",sep=""), rownames(data)),1]*y.scale,
  pch = "+",
  ylab = ylab,
  xlab = xlab,
  col = col.pch,
  ...
  )

  abline(h=0, lty = 3, col = "grey60")
  abline(v=0, lty = 3, col = "grey60")
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"]*x.scale,
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"25%"]*y.scale,
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"]*x.scale,
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"75%"]*y.scale,
    length = 0, col = col.lines)
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"25%"]*x.scale,
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"]*y.scale,
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"75%"]*x.scale,
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"]*y.scale,
    length = 0, col = col.lines)
  
  # match with species names
  text( data[grep(paste(x,"\\[",sep=""), rownames(data)),1]*x.scale,
        data[grep(paste(y,"\\[",sep=""), rownames(data)),1]*y.scale,
        sort(unique(traitors.sp)),
        cex = 0.5, 
        pos = 3,
        col = col.pch)
}
