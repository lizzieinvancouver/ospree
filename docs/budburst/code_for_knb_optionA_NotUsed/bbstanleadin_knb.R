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

##################################################
# Set the data you want to use deal with species #
##################################################

bb.all <- bb

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

# making some list out of the processed data, which will be input for the model ...

##############################################
#     ALL TYPES (amb, ramped, exp etc.)      #
##############################################

# This is ALL species (no species complex used) and all types of forcing/photo & z-scored
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

# ALL species (no species complex used) and all types & NOT z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & 
    use.expchillonly == FALSE & use.chillports == FALSE & use.zscore == FALSE){
  bb.stan <- addcomplex.allspp(bb.all)
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

# Species complex and all types & z-scored, chill ports
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- bb.stan.alltypes.nocrops
    source("bb_zscorepreds.R")
    
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

##############################################
## EXP and RAMPED TYPES for photo and force ##
##############################################

# ALL species (no species complex used) with only exp & ramped photoforce & z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- addcomplex.allspp(bb.exprampphotoforce)
    source("bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}


# ALL species (no species complex used) with only exp & ramped photoforce & NOT z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == TRUE & use.zscore == FALSE){
    bb.stan <- addcomplex.allspp(bb.exprampphotoforce)
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# Species complex with only exp & ramped photoforce & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- bb.stan.expramptypes.nocrops
    source("bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# Species complex with only exp & ramped photoforce WITH crops & z-scored #

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- bb.stan.expramptypes
    source("bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# Species complex with only exp & ramped photoforce & NOT z-scored #

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == TRUE & use.zscore == FALSE){
  bb.stan <- bb.stan.expramptypes.nocrops
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.ports, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

# Species complex with only exp & ramped photoforce WITH crops & NOT z-scored #

if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == TRUE & use.zscore == FALSE){
  bb.stan <- bb.stan.expramptypes
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill.ports, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

##########################################
## EXP only as TYPE for photo and force ##
##########################################

# Species complex with only exp photoforce & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE & use.expchillonly == FALSE &
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- bb.stan.exptypes.nocrops
    source("bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y=resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}

# ALL species with only exp photoforce & z-scored
if (use.allspp==TRUE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==TRUE & use.expchillonly == FALSE  &
    use.chillports == TRUE & use.zscore == TRUE){
    bb.stan <- addcomplex.allspp(bb.expphotoforce)
    source("bb_zscorepreds.R")
    datalist.bb <- with(bb.stan, 
                    list(y=resp, 
                         chill = chill.ports.z, 
                         force = force.z, 
                         photo = photo.z,
                         sp = complex,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex))
                    )
 )
}


##############
## Chilling ##
##############

# Species complex, default photo and forcing and ONLY exp chill & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
  use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == TRUE &
  use.chillports == TRUE & use.zscore == TRUE){
  bb.stan <- bb.stan.exprampphotoforceexpch.nocrops
  source("bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill.ports.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}
# Species complex, ALL photo and forcing and ONLY exp chill & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==FALSE & use.exptypes.fp==FALSE & use.expchillonly == TRUE &
    use.chillports == TRUE & use.zscore == TRUE){
  bb.stan <- bb.stan.alltypesexpch.nocrops
  source("bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill.ports.z, 
                           force = force.z, 
                           photo = photo.z,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}
# Species complex, exo and photo and forcing and using utah & z-scored
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

# Species complex, exp and photo and forcing and using utah WITH CROPS & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.expramptypes
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

# Species complex with only exp & ramped photoforce using Utah & NOT z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==FALSE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE &
    use.chillports == FALSE & use.zscore == FALSE){
  bb.stan <- bb.stan.expramptypes.nocrops 
  datalist.bb <- with(bb.stan, 
                      list(y = resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}
# Species complex, exp and photo and forcing and using utah WITH CROPS & z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == FALSE & use.zscore == TRUE){
  bb.stan <- bb.stan.expramptypes
  source("bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
                           sp = complex,
                           N = nrow(bb.stan),
                           n_sp = length(unique(bb.stan$complex))
                      )
  )
}

# Species complex, exp and photo and forcing and using utah WITH CROPS & NOT z-scored
if (use.allspp==FALSE & use.multcuespp==FALSE & use.cropspp==TRUE &
    use.expramptypes.fp==TRUE & use.exptypes.fp==FALSE & use.expchillonly == FALSE & 
    use.chillports == FALSE & use.zscore == FALSE){
  bb.stan <- bb.stan.expramptypes
  
  source("bb_zscorepreds.R")
  datalist.bb <- with(bb.stan, 
                      list(y=resp, 
                           chill = chill, 
                           force = force, 
                           photo = photo,
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



