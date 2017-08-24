## Code started on 23 August 2017 ##
## By Lizzie ##
## Built off bbmodels_stan.R and bbmodels_stanplotting.R ##
## Runs a stan model with datasetID on the intercept, not species ##

## Must run start of bbmodels_stan.R before below #

bb.stan <- subset(bb, select=c("datasetID", "resp", "chill", "photo", "force", "complex", "type", "datasetID"))
bb.stan$complex <- as.numeric(as.factor(bb.stan$complex))
bb.stan$ datasetID <- as.numeric(as.factor(bb.stan$datasetID))

# remove the two values above 600
bb.stan <- subset(bb.stan, resp<600)

# adjust chilling (if needed)
# here we are transforming chilling to have it in a scale more similar to the rest of variables and so that 
# it can be interpreted as 10 days (so the coefficient will tell us change in BB every 10 days of chilling)
bb.stan$chill <- bb.stan$chill/240

length(unique(bb.stan$datasetID))

# Fairly strict rules of inclusion in this analysis: manipulation of forcing temperature, 
# photoperiod, and where we have a response in days and total chilling. 

## Prep the data for Stan model
# making a list out of the processed data. It will be input for the model
datalist.bb <- with(bb.stan, 
                    list(y = resp, 
                         chill = chill, 
                         force = force, 
                         photo = photo,
                         sp = complex,
                         dat = datasetID,
                         N = nrow(bb.stan),
                         n_sp = length(unique(bb.stan$complex)),
                         n_dat = length(unique(bb.stan$datasetID))
                    )
)
## real data with only experimental chilling (no field chilling)
#osp.td3 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td,
 #              iter = 2000,warmup=1500,control=list(adapt_delta=0.95))


########################################################
# real data on 2 level model (sp on intercept only) with no interactions 
# Note the notation: M1_daysBBnointer_2level_datintercept.stan: goo
########################################################
goo = stan('stan/bb/M1_daysBBnointer_2level_datintercept.stan', data = datalist.bb,
               iter = 2500)

doop <- summary(goo)$summary
doop[grep("mu_", rownames(doop)),] 

save(goo, file="stan/bb/output/M1_daysBBnointer_2level_datintercept.Rda")
# a: 78; f: -1.6; p: -0.2; c: -1.1

##
##
##

## scale up: plot each species with slopes from no interaction and with interactions model
whichmodel <- sumer.ni
othermodel <- sumer.wi
pdf(file.path(figpath, "M1intergoo.pdf"), width = 7, height = 3.5)
spp <- unique(bb.stan$complex)
for (sp in c(1:length(spp))){
    par(mfrow=c(1,3))
    subby <- subset(bb.stan, complex==spp[sp])
    # chilling
    plot(resp~chill, data=subby, main=subby$complex.wname[1]) 
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_chill", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_d", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_chill", rownames(othermodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col="blue")
    # forcing 
    plot(resp~force, data=subby) # should add color coding by datasetID
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_force", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_d", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_force", rownames(othermodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col="blue")
    # photo
    plot(resp~photo, data=subby) # should add color coding by datasetID
    intercepthere <- whichmodel[grep("a_sp", rownames(whichmodel)),1][spp[sp]+2]
    slopehere <- whichmodel[grep("b_photo", rownames(whichmodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere)
    intercepthere <- othermodel[grep("a_d", rownames(othermodel)),1][spp[sp]+2]
    slopehere <- othermodel[grep("b_photo", rownames(othermodel)),1][spp[sp]+2]
    abline(intercepthere, slopehere, col="blue")
}
dev.off()
