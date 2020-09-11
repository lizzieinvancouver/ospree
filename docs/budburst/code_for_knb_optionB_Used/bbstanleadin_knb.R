#################################################
############### Data wrangling needed ###########
####### prior to fitting bb model in stan #######
#################################################
# adjust chilling (if needed)
# here we are transforming Utah chilling to have it in a scale more similar to the rest of variables and so that 
bb$chill <- bb$chill/240

# Subset data based on different types of photoperiod treatment
bb.expphoto <- subset(bb, photo_type=="exp")
bb.rampphoto <- subset(bb, photo_type=="ramped")
bb.exprampphoto <- subset(bb, photo_type=="exp" | photo_type=="ramped")
bb.ambphoto <- subset(bb, photo_type=="amb" | photo_type=="none")

# Subset data based on different types of forcing treatment
bb.exprampphotoforce <- subset(bb.exprampphoto, force_type=="exp"|force_type=="ramped")
bb.expphotoforce <- subset(bb.expphoto, force_type=="exp")

# Subset data based on experimental vs field chilling treatment
bb.exprampphotoforceexpch <- subset(bb.exprampphotoforce, chill_type=="exp")
bb.allexpch <- subset(bb, chill_type=="exp")

# set up data for when using all types of designs (exp, ramped, amb)
if (use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == FALSE){
    bb.stan.alltypes <- sppcomplexfx(bb.all) 
    bb.stan.alltypes.multcue <- sppcomplexfx.multcue(bb.all) 
    bb.stan.alltypes.nocrops <- sppcomplexfx.nocrops(bb.all)
}


# set up data for when using exp AND ramped for photo + forceexp for photo + force
if (use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE){
    bb.stan.expramptypes <- sppcomplexfx(bb.exprampphotoforce) 
    bb.stan.expramptypes.multcue <- sppcomplexfx.multcue(bb.exprampphotoforce) 
    bb.stan.expramptypes.nocrops <- sppcomplexfx.nocrops(bb.exprampphotoforce)
}


# set up data for when using exp for photo + force
if (use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE & use.expchillonly == FALSE){
    bb.stan.exptypes <- sppcomplexfx(bb.expphotoforce) 
    bb.stan.exptypes.multcue <- sppcomplexfx.multcue(bb.expphotoforce) 
    bb.stan.exptypes.nocrops <- sppcomplexfx.nocrops(bb.expphotoforce)
}

# set up data for when using only experimental chilling
if (use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == TRUE){
  bb.stan.exprampphotoforceexpch<- sppcomplexfx(bb.exprampphotoforceexpch) 
  bb.stan.exprampphotoforceexpch.multcue <- sppcomplexfx.multcue(bb.exprampphotoforceexpch) 
  bb.stan.exprampphotoforceexpch.nocrops <- sppcomplexfx.nocrops(bb.exprampphotoforceexpch)
}
if (use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == TRUE){
  bb.stan.alltypesexpch<- sppcomplexfx(bb.allexpch) 
  bb.stan.alltypesexpch <- sppcomplexfx.multcue(bb.allexpch) 
  bb.stan.alltypesexpch <- sppcomplexfx.nocrops(bb.allexpch)
}
##################################
## Prep the data for Stan model ##
##################################
# ALL species (no species complex used) & z-scored

if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & 
    use.expchillonly == FALSE & use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- addcomplex.allspp(bb.all)
  source("bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}


# Main Model: Species complex, exo and photo and forcing and using utah & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.expramptypes.nocrops
  source("bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}




## AT THE END ...
str(datalist.bb)
print("Unique forcing types are ...")
print(unique(bb.stan$force_type))
print("Unique photo types are ...")
print(unique(bb.stan$photo_type))
print("Unique chilling types are ...")
print(unique(bb.stan$chill_type))
print("Range of chilling is ...")
print(range(datalist.bb$chil))



