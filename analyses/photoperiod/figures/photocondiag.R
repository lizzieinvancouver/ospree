## Make conceptual diagram about importance of 
## including photoperiod in forecasting
#############################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries

# set working directory
setwd("~/git/ospree/analyses/photoperiod/figures")

# 1. Make a reasonable observational dataset,
# with effect size similar to what Wolkovich et al found
# observational:  -6.4 days/C
# experimental:  -1.6 days/C 
n_sp=3#number of species or populations
obs=30#number of obs (plots, years) per species or population
N=n_sp*obs#total sample size
sp<-rep(seq(1:n_sp), each=obs)#species ids

#set up distribution parameters
mu_a<-120#grand mean mean of bb doy
sigma_a<-5
mu_b_temp<- -6
sigma_b_temp<-.5
mu_b_ph<- -0.1
sigma_b_ph<-.01

a_sp<-as.integer(rnorm(n_sp,mu_a,sigma_a))#species specific day of year for bb

b_temp<-rnorm(n_sp,mu_b_temp,sigma_b_temp)#species specific effects of temp
b_ph<-rnorm(n_sp,mu_b_ph,sigma_b_ph)#species specific effects of mois

#create explanatory variables
#try using centered predictors
temp<-rep(NA, N)
for(i in 1:n_sp){
  temp[which(sp==i)]<-rnorm(obs,0,1)#right now this is set up for each species but it doesn't need to be right?
}
ph<-rep(NA, N)
for(i in 1:n_sp){
  ph[which(sp==i)]<-rnorm(obs,0,1)#
}

#interaction
mu_b_tp_sp<--.5
sigma_b_tp_sp<-.005
b_tp<-rnorm(n_sp,mu_b_tp_sp,sigma_b_tp_sp)#species specific interaction

#generate response variable, ypred
sigma_y<-.5
ypred<-c()
for(i in 1:N){
  ypred[i] = a_sp[sp[i]] + b_temp[sp[i]] * temp[i] + b_ph[sp[i]] * ph[i]+ b_tp[sp[i]]*temp[i] * ph[i]
}

y<-rnorm(N,ypred,sigma_y)
#check that test data look ok
quartz()
plot(ph,y)
hist(ph)
#try model in lmer
testm2.lmer<-lmer(y~temp * ph +(1|sp))#
summary(testm2.lmer)

testm2<-lm(y~temp)
summary(testm2)#looks good!
#plot data

pdf("obstemplm.pdf")
plot(temp,y,xaxt="n",yaxt="n", xlab="",ylab="",pch=21, bg="gray", bty="l",xlim=c(-2,3),ylim=c(100,135))
abline(testm2,lwd=2, lty=3)
dev.off()

#now add a forecasting line
pdf("obstemplm_forcast.pdf")
#quartz()
plot(temp,y,xaxt="n",yaxt="n", xlab="",ylab="",pch=21, bg="gray", bty="l",xlim=c(-2,5),ylim=c(70,135))
#arrows(min(temp),max(y),max(temp),min(y),length=0, lwd=2)
abline(testm2, lwd=2, lty=3)

dev.off()

#now add data for shorter daylength, with weaker effect
n_sp=3#number of species or populations
obs=30#number of obs (plots, years) per species or population
N=n_sp*obs#total sample size
sp<-rep(seq(1:n_sp), each=obs)#species ids

#set up distribution parameters
mu_a<-110#grand mean mean of bb doy
sigma_a<-5
mu_b_temp<- -2
sigma_b_temp<-.5
mu_b_ph<- -0.1
sigma_b_ph<-.01

a_sp<-as.integer(rnorm(n_sp,mu_a,sigma_a))#species specific day of year for bb

b_temp<-rnorm(n_sp,mu_b_temp,sigma_b_temp)#species specific effects of temp
b_ph<-rnorm(n_sp,mu_b_ph,sigma_b_ph)#species specific effects of mois

#create explanatory variables
#try using centered predictors
temp<-rep(NA, N)
for(i in 1:n_sp){
  temp[which(sp==i)]<-rnorm(obs,2,1)#right now this is set up for each species but it doesn't need to be right?
}
ph<-rep(NA, N)
for(i in 1:n_sp){
  ph[which(sp==i)]<-rnorm(obs,0,1)#
}

#interaction
mu_b_tp_sp<--1
sigma_b_tp_sp<-.005
b_tp<-rnorm(n_sp,mu_b_tp_sp,sigma_b_tp_sp)#species specific interaction

#generate response variable, ypred
sigma_y<-.5
ypred<-c()
for(i in 1:N){
  ypred[i] = a_sp[sp[i]] + b_temp[sp[i]] * temp[i] + b_ph[sp[i]] * ph[i]+ b_tp[sp[i]]*temp[i] * ph[i]
}

y<-rnorm(N,ypred,sigma_y)

#try model in lmer
testm2.lmer<-lmer(y~temp * ph +(1|sp))#
summary(testm2.lmer)

testm2<-lm(y~temp)
summary(testm2)#looks good!
#plot data


#pdf("obstempph.pdf")
#quartz()
points(temp,y,pch=21, bg="black")
#arrows(min(temp),max(y),max(temp),min(y),length=0, lwd=2)
abline(testm2, lwd=2, lty=1)
#add points for
dev.off()