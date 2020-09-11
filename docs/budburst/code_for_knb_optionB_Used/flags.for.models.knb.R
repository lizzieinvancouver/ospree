if(length(which(c(use.flags.for.mainmodel,use.flags.for.allspp) =="TRUE"))>1) 
  print("ALERT! You have set too many master model flags to true, you must pick only one!")

if(length(which(c(use.flags.for.mainmodel,use.flags.for.allspp) =="TRUE"))==0) 
  print("ALERT! You have not set any master model flags; you must pick one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp =FALSE 
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}


if(use.flags.for.allspp) {
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}
