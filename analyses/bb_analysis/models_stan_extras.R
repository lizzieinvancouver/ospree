#########################
## For PEP 725 species ##
#########################
if(use.pep){
getspp <- subset(bb.stan, select=c("complex", "complex.wname"))
allspp <- getspp[!duplicated(getspp), ]
allspp <- allspp[order(allspp$complex),]
pepspp <- c("Acer_pseudoplatanus", "Aesculus_hippocastanum", "Betula_pendula", "Corylus_avellana",
    "Fagus_sylvatica", "Larix_decidua", "Picea_abies", "Populus_tremula",
    "Prunus_padus","Quercus_robur", "Syringa_vulgaris")
# gymnastics to renumber species
somespp <-  allspp[which(allspp$complex.wname %in% pepspp),]
somespp$complex <- NULL
somespp$complex <- seq(1:nrow(somespp))

bb.stan <- bb.stan[which(bb.stan$complex.wname %in% pepspp),] 
bb.stan$complex <- NULL
dim(bb.stan)
bb.stan <- merge(bb.stan, somespp, by="complex.wname")
dim(bb.stan)

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





##############################
## Code below not updated! ##
### But I think we will neeed it ##

########## SIDE BAR ##########
## Compare R2 today ##
observed.here <- bb.stan$resp

m2lni.sum <- summary(m2l.ni)$summary
m2lni.sum[grep("mu_", rownames(m2lni.sum)),] 

# getting predicted values if needed
preds.m2lni.sum <- m2lni.sum[grep("yhat", rownames(m2lni.sum)),]
preds.m2lnistudy.sum <- m2lnistudy.sum[grep("yhat", rownames(m2lnistudy.sum)),]

m2lni.R2 <- 1- sum((observed.here-preds.m2lni.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)
m2lnistudy.R2 <- 1- sum((observed.here-preds.m2lnistudy.sum[,1])^2)/sum((observed.here-mean(observed.here))^2)

summary(lm(preds.m2lni.sum[,1]~observed.here)) # Multiple R-squared:  0.6051 (Chill Portions)
summary(lm(preds.m2lnistudy.sum[,1]~observed.here)) # Multiple R-squared:  0.7158 (Chill Portions)

# try the w/ interaction
preds.m2l.winsp.sum <- m2l.winsp.sum[grep("yhat", rownames(m2l.winsp.sum)),]
summary(lm(preds.m2l.winsp.sum[,1]~observed.here)) #Multiple R-squared:  0.6122

########## END SIDE BAR ##########
