if(length(which(c(use.flags.for.mainmodel,use.flags.for.spcomp.cp,use.flags.for.allspp.utah, 
                  use.flags.for.spcomp.utah.nonz,use.flags.for.spcomp.cp.nonz,use.flags.for.allspp.utah.nonz,
                  use.yourown.flagdesign) =="TRUE"))>1) 
  print("ALERT! You have set too many master flags to true, you must pick only one!")

if(length(which(c(use.flags.for.mainmodel,use.flags.for.spcomp.cp,use.flags.for.allspp.utah, 
                  use.flags.for.spcomp.utah.nonz,use.flags.for.spcomp.cp.nonz,use.flags.for.allspp.utah.nonz,
                  use.yourown.flagdesign) =="TRUE"))==0) 
  print("ALERT! You have not set any master flags; you must pick one!")

if(use.flags.for.mainmodel){#centered predictors, spcomplex with utah units. Fig 2 in main text of budburst ms
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp =FALSE 
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.spcomp.cp) {#centered predictors, spcomplex with chill portions units. Table ms
  use.chillports = TRUE
  use.zscore = TRUE
  use.allspp =FALSE 
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}


if(use.flags.for.allspp.utah) {
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.spcomp.utah.nonz) {
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp =FALSE 
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = FALSE 
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.spcomp.cp.nonz) {
  use.chillports = TRUE
  use.zscore = FALSE
  use.allspp =FALSE 
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE 
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.allspp.utah.nonz){
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp = TRUE
  use.multcuespp = FALSE
  use.cropspp = TRUE
  use.expramptypes.fp = FALSE 
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = TRUE 
  use.zscore = TRUE 
  use.allspp = FALSE
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE 
}