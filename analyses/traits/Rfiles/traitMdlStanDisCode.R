
# Part 1
Nrep <- 10 # rep per trait
Nstudy <- 20 # number of studies w/ traits 
Nspp <- 20 # number of species 

Ntrt <- Nspp * Nstudy * Nrep 

mu.grand <- 15 
sigma.species <- 5 
sigma.study <- 2
sigmaTrait_y <- 2

trt.dat <- data.frame(matrix(NA, Ntrt, 1))
names(trt.dat) <- c("rep")
trt.dat$rep <- c(1:Nrep)
trt.dat$study <- rep(rep(c(1:Nstudy), each = Nspp), each = Nrep)
trt.dat$species <- rep(rep(c(1:Nspp), times = Nstudy), each = Nrep)

alphaTraitSp <- rnorm(Nspp, 0, sigma.species)
trt.dat$alphaTraitSp <- rep(rep(alphaTraitSp, times = Nstudy), each = Nrep) 

muStudy <- rnorm(Nstudy, 0, sigma.study) 
#trt.dat$muStudy <- rep(rep(muStudy, times = Nspp), each = Nstudy) 
trt.dat$muStudy <- rep(muStudy, each = Nrep*Nspp) 

sigmaTraity <- 5 
trt.dat$sigmaTraity <- rnorm(Ntrt, 0, sigmaTraity)

for (i in 1:Ntrt){
  trt.dat$mu_grand_sp[i] <-  trt.dat$alphaTraitSp[i] +  mu.grand
}

for (i in 1:Ntrt){
  trt.dat$yTraiti[i] <-  trt.dat$alphaTraitSp[i] + trt.dat$muStudy[i] +  mu.grand + trt.dat$sigmaTraity[i]
}

#################################################################
# Part 2
n_spec <-Nspp
nRep <- 20
Ncue <- n_spec * nRep

cue.dat <- data.frame(matrix(NA, Ncue, 2))
names(cue.dat) <- c("rep","species")
cue.dat$rep <- c(1:Ncue)
cue.dat$species <- rep(c(1:n_spec), each = nRep)

cue.dat$cuei <- rnorm(Ncue, 1, 1)

sigmaRespSp <- 20
muRespSp <- 80
alphaRespSp <- rnorm(n_spec, muRespSp, sigmaRespSp)
cue.dat$alphaRespSp <- rep(alphaRespSp, each = nRep)

betaTraitxCue <- 0.3 #Cue effects

muCueSp <- -15
sigmaCueSp <- 4
alphaCueSp <- rnorm(n_spec, muCueSp, sigmaCueSp)
cue.dat$alphaCueSp <- rep(alphaCueSp, each = nRep)

sigmaResp_y <- 5
cue.dat$eResp <- rnorm(Ncue, 0, sigmaResp_y)

cueTrtDat <- merge(cue.dat, unique(trt.dat[,c("species","mu_grand_sp")]), by = "species")

for (i in 1:Ncue){
  cueTrtDat$betaCueSp[i] <-  cueTrtDat$alphaCueSp[i] + (betaTraitxCue * cueTrtDat$mu_grand_sp[i])
}
for (i in 1:Ncue){
  cueTrtDat$yMu[i] <-  cueTrtDat$alphaRespSp[i] +  cueTrtDat$betaCueSp[i] * cueTrtDat$cuei[i] 
}

cueTrtDat$yRespi <- cueTrtDat$yMu + cueTrtDat$eResp

sim.data <- list(yTraiti = trt.dat$yTraiti,
                 N = Ntrt,
                 n_spec = Nspp,
                 trait_species = as.numeric(as.factor(trt.dat$species)),
                 n_study = Nstudy,
                 study = as.numeric(as.factor(trt.dat$study )),
                 prior_mu_grand_mu = 20,
                 prior_mu_grand_sigma = 10,
                 prior_sigma_sp_mu = 4,
                 prior_sigma_sp_sigma = 5,
                 prior_sigma_study_mu = 2,
                 prior_sigma_study_sigma = 5,
                 prior_sigma_traity_mu = 3,
                 prior_sigma_traity_sigma = 5,
                 Ncue = nrow(cue.dat),
                 cue_species = as.numeric(as.factor(cueTrtDat$species )),
                 yRespi = cueTrtDat$yRespi,
                 cuei = cueTrtDat$cuei,
                 prior_muCueSp_mu = -15,
                 prior_muCueSp_sigma = 10, 
                 prior_muRespSp_mu = 40,
                 prior_muRespSp_sigma = 10,
                 prior_sigmaCueSp_mu = 5,
                 prior_sigmaCueSp_sigma = 5,
                 prior_sigmaRespSp_mu = 5,
                 prior_sigmaRespSp_sigma = 5, 
                 prior_betaTraitxCue_mu = 0,
                 prior_betaTraitxCue_sigma = 1,
                 prior_sigmaRespy_mu = 10,
                 prior_sigmaRespy_sigma = 5 
)

mdl2 <- stan("stan/traitMdlStanDis.stan",
                  data = sim.data,
                  iter = 4000,
                  warmup = 2000,
                  chains = 4,
                  cores = 4,
                  include = FALSE, pars = c("y_hat"))

sum.jfcp <- summary(mdl2)$summary

save(mdl2, file = "output/standiscouseOutput_fixed.RData")

mu_grand <- sum.jfcp[grep("mu_grand", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigma_sp <- sum.jfcp[grep("sigma_sp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigma_studyesti <- sum.jfcp[grep("sigma_study", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigmaTrait_y <- sum.jfcp[grep("sigma_traity", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]

mu_CueSp <- sum.jfcp[grep("muCueSp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
mu_RespSp <- sum.jfcp[grep("muRespSp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
alpha.CueSp <- sum.jfcp[grep("alphaCueSp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigma_CueSp <- sum.jfcp[grep("sigmaCueSp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigma_RespSp <- sum.jfcp[grep("sigmaRespSp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigma_Respy <- sum.jfcp[grep("sigmaResp_y", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
beta_tf <- sum.jfcp[grep("betaTraitxCue", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]

mdl.out <- data.frame(
"Parameter" = c("mu_grand","sigma_sp","sigma_study", "sigmaResp_y", 
                "muCueSp","muResSp","sigmaCuesp", "sigmaRespSp", 
                 "betaTraitCue"),
"param" = c(mu.grand, sigma.species, sigma.study, sigmaResp_y,  muCueSp,  muRespSp, sigmaCueSp, sigmaRespSp, betaTraitxCue),
"esti" = c(mu_grand[1,1], sigma_sp[1], sigma_studyesti[1], sigma_Respy[1], mu_CueSp[1], mu_RespSp[1], sigma_CueSp[1], sigma_RespSp[1],  beta_tf[1]),"2.5" = c(mu_grand[1,2], sigma_sp[2], sigma_studyesti[2], sigma_Respy[2], mu_CueSp[2], mu_RespSp[2], sigma_CueSp[2], sigma_RespSp[2],  beta_tf[2]),"97.5" = c(mu_grand[1,3], sigma_sp[3], sigma_studyesti[3], sigma_Respy[3], mu_CueSp[3], mu_RespSp[3], sigma_CueSp[3], sigma_RespSp[3], beta_tf[3]))
          
  
                  

mdl.out
names(mdl.out) <- c("Variable", "Parameter", "Estimate", "2.5", "97.5")


png("simulatedPairs.png")
pairs(mdl2, pars = c("mu_grand", "sigma_sp", "sigma_study","sigma_traity","muCueSp", "betaTraitxCue", "lp__")) 
dev.off()

muRespSp <- sum.jfcp[grep("alphaRespSp", rownames(sum.jfcp))]
muStudyEsti <- sum.jfcp[grep("muStudy", rownames(sum.jfcp))]

pdf("muRespSp_study_esti_discourse.pdf",width = 10, height = 5)
par(mfrow = c(1,2))
plot(muRespSp ~ alphaRespSp, xlab = "simulated muRespSp", ylab = "mdl estimated muRespSp")
 abline(0,1)
plot(muStudyEsti ~ muStudy, xlab = "simulated muStudy", ylab = "mdl estimated muStudy")
abline(0,1)
dev.off()