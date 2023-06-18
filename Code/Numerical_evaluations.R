require(colorRamps)
library("cowplot")       
require(MASS)
rm(list=ls())

###Simulation
n.ind<-10000
n.pops<-500
v <-matrix(NA, n.ind, n.pops)
delta_n<-delta_N<-Covzg<-Covzr<-vv<-COV<-COR<-vr_0<-vgamma<-mgamma<-delta_v<-delta_v_s<-delta_v_ed<-delta_g<-delta_r0<-mv<-dv<-mr_0<-z_b<-n<-N<-rep(NA,n.pops)
N[1]<-100
n[1]<-log(N[1])
z_b[1]<-3.4
vz<-0.05

Br<-2
ar<--0.4
Br/-(2*ar)

cr<--1
Bg<--1
cg<-2

dl<-list()

for(i in 1:n.pops){
  if(i==1){
    z<-rnorm(n.ind, z_b[i], sqrt(vz))
    d<-data.frame(ID=paste(1:n.ind, i, sep=" "), z=z)
    d$r_0<-cr + d$z*Br + ar*d$z^2
    d$gamma<-exp(cg + d$z*Bg)
    d$n<-n[i]
    d$v<-(d$r_0) - (d$gamma*n[i]) + rnorm(1,0,sqrt(0.005))
  } else{
    db<-as.data.frame(dl[[i-1]])
    db2<-db[sample(1:n.ind, n.ind/2),]
    z<-rnorm(n.ind/2, z_b[i], sqrt(vz))
    da<-data.frame(ID=paste(1:(n.ind/2), i, sep=" "), z=z)
    d<-rbind(db2[,c("ID", "z")],da)
    d$r_0<-cr + d$z*Br + ar*d$z^2
    d$gamma<-exp(cg + d$z*Bg)
    d$n<-n[i]
    d$v<-(d$r_0) - (d$gamma*n[i]) + rnorm(1,0,sqrt(0.005))
  }
  
  dl[[i]]<-d
  mgamma[i]<-mean(d$gamma)
  mr_0[i]<- mean(d$r_0)
  vgamma[i]<-var(d$gamma)
  vr_0[i]<-var(d$r_0)
  COV[i]<-cov(d$r_0,d$gamma)
  COR[i]<-cor(d$r_0,d$gamma)
  Covzr[i]<-cov(d$r_0,d$z)
  Covzg[i]<-cov(d$gamma,d$z)
  vv[i]<-var(d$v)
  mv[i]<-mean(d$v)
  z_b[i+1]<-mean(d$z) + cov(d$v,d$z)
  n[i+1]<-exp(mean(d$v))*n[i]
  N[i+1]<-exp(n[i+1])
  delta_N[i]<-N[i+1]-N[i]
  delta_n[i]<-n[i+1]-n[i]
  delta_v[i]<- cov(d$r_0,d$v) - cov(d$gamma, d$v)*n[i] - mgamma[i]*delta_n[i] 
  delta_v_ed[i]<- - mgamma[i]*delta_n[i]
}

dI<-do.call(rbind.data.frame, dl)
dI$ID<-as.numeric(as.factor(dI$ID))
rownames(dI)<-1:nrow(dI)

save.image("Code/Num_ev.RData")
