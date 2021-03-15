intercept<-50
sigma<-2
nsp<-1:20
intercept_sp<- rnorm(length(nsp),0,sigma)
mean(intercept_sp)
b_climvar_force<-.4
b_climva_chill<--.6
b_clim_var_photo<--.5

alpha_force<- rnorm(length(nsp),-2,0.5)
alpha_chill<- rnorm(length(nsp),-4,.5)
alpha_photo<- rnorm(length(nsp),-.5,.5)


b_force<-alpha_force*b_climvar_force
b_chill<- alpha_chill*b_climva_chill
b_photo<- alpha_photo*b_clim_var_photo

#data
Chill<-rnorm(1000,-5,10)
Photo<--rnorm(1000,1,10)
Force<-rnorm(1000,-3,10)
climvar<-rnorm(1000,-.5,.002)
species=rep(nsp,50)
      
y<-b_chill*Chill+ b_force*Force+b_photo*Photo  ##no error

fak.dat<-data.frame(y=y,Chill=Chill,Photo=Photo,Force=Force, species=species,climvar=climvar)

faker<- with(fak.dat, 
                  list(yPhenoi = y, 
                       forcingi = Force,
                       photoi = Photo,
                       chillingi = Chill,
                       species = species,
                       N = nrow(fak.dat),
                       n_spec = length(unique(fak.dat$species)),
                       climvar=climvar
                  ))

fake_jnt = stan('stan/joint_climvar_3param.stan', data = faker,
                      iter = 6000, warmup=4000)
