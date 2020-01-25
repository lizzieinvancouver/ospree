if(length(which(c(use.flags.for.mainmodel,use.flags.for.spcomp.cp,use.flags.for.spcomp.cp,use.flags.for.allspp.utah, 
                  use.flags.for.spcomp.utah.nonz,use.flags.for.spcomp.cp.nonz,use.flags.for.allspp.utah.nonz,
                  use.yourown.flagdesign) =="TRUE"))>1) 
  print("ALERT! You have set too many master flags to true, you must pick only one!")

if(use.flags.for.mainmodel){
  use.chillports = FALSE
  use.zscore = TRUE
  use.allspp =FALSE # for the main model this is false
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE # Default is species complex use  alltypes of designs
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.spcomp.cp) {
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
  use.expramptypes.fp = TRUE # Default is species complex use  alltypes of designs
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.flags.for.spcomp.utah.nonz) {
  use.chillports = FALSE
  use.zscore = FALSE
  use.allspp =FALSE 
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE 
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
  use.expramptypes.fp = TRUE 
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE
}

if(use.yourown.flagdesign){
  use.chillports = TRUE # change to false for using utah instead of chill portions (most models use chill portions z)
  use.zscore = TRUE # change to false to use raw predictors
  use.allspp = FALSE
  use.multcuespp = FALSE
  use.cropspp = FALSE
  use.expramptypes.fp = TRUE
  use.exptypes.fp = FALSE
  use.expchillonly = FALSE # change to true for only experimental chilling 
  #note: with only exp chilling, there is only exp photo and force too.
  #also: subsetting to exp chill only reduces dataset to 3 species, <9 studies
}