
# Part 1
Nrep <- 10 # rep per trait
Nstudy <- 50 # number of studies w/ traits 
Nspp <- 50 # number of species 

Ntrt <- Nspp * Nstudy * Nrep 

mu.grand <- 15 
sigma.species <- 5 
sigma.study <- 5


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

sigmaTraity <- 2 
trt.dat$sigmaTraity <- rnorm(Ntrt, 0, sigmaTraity)

for (i in 1:Ntrt){
  trt.dat$mu_grand_sp[i] <-  trt.dat$alphaTraitSp[i] +  mu.grand
}

for (i in 1:Ntrt){
  trt.dat$yTraiti[i] <-  trt.dat$alphaTraitSp[i] + trt.dat$muStudy[i] +  mu.grand + trt.dat$sigmaTraity[i]
}

sim.data <- list(yTraiti = trt.dat$yTraiti,
                 N = Ntrt,
                 n_spec = Nspp,
                 trait_species = as.numeric(as.factor(trt.dat$species)),
                 n_study = Nstudy,
                 study = as.numeric(as.factor(trt.dat$study ))
)

mdl2 <- stan("stan/traitMdlStanDis_traitOnly.stan",
                  data = sim.data,
                  iter = 4000,
                  warmup = 2000,
                  chains = 4,
                  cores = 4,
                  include = FALSE, pars = c("y_hat"))

sum.jfcp <- summary(mdl2)$summary

#save(mdl2, file = "output/standiscouseOutput_fixed.RData")

mu_grand <- sum.jfcp[grep("mu_grand", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigma_sp <- sum.jfcp[grep("sigma_sp", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigma_studyesti <- sum.jfcp[grep("sigma_study", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]
sigmaTrait_y <- sum.jfcp[grep("sigma_traity", rownames(sum.jfcp)),c("mean","2.5%","97.5%")]

mdl.out <- data.frame(
"Parameter" = c("mu_grand","sigma_sp","sigma_study", "sigmaTrait_y" ),
"param" = c(mu.grand, sigma.species, sigma.study, sigmaTraity),
"esti" = c(mu_grand[1,1], sigma_sp[1], sigma_studyesti[1], sigmaTrait_y[1]),"2.5" = c(mu_grand[1,2], sigma_sp[2], sigma_studyesti[2], sigmaTrait_y[2]),"97.5" = c(mu_grand[1,3], sigma_sp[3], sigma_studyesti[3], sigmaTrait_y[3]))
          
mdl.out
names(mdl.out) <- c("Variable", "Parameter", "Estimate", "2.5", "97.5")


png("simulatedPairs.png")
pairs(mdl2, pars = c("mu_grand", "sigma_sp", "sigma_study","sigma_traity", "lp__")) 
dev.off()

muSpEsti <- sum.jfcp[grep("muSp", rownames(sum.jfcp))]
muStudyEsti <- sum.jfcp[grep("muStudy", rownames(sum.jfcp))]

#pdf("muRespSp_study_esti_discourse.pdf",width = 10, height = 5)
par(mfrow = c(1,2))
#plot(muRespSp ~ alphaRespSp, xlab = "simulated muRespSp", ylab = "mdl estimated muRespSp")
plot(muSpEsti ~ alphaTraitSp, xlab = "simulated muSpecies", ylab = "mdl estimated muSpecies")
 abline(0,1)
plot(muStudyEsti ~ muStudy, xlab = "simulated muStudy", ylab = "mdl estimated muStudy")
abline(0,1)
#dev.off()
