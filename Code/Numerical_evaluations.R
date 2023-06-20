require(colorRamps)
library("cowplot")       
require(MASS)
rm(list=ls())

###Simulation
n.ind<-100
n.years<-2000
v <-matrix(NA, n.ind, n.years)
delta_n<-delta_N<-Covzg<-Covzr<-vv<-COV<-COR<-vr_0<-vgamma<-mgamma<-vvz<-mv<-dv<-mr_0<-z_b<-n<-N<-rep(NA,n.years)
N[1]<-n.ind
n[1]<-log(N[1])
z_b[1]<-3
vz<-0.2

Br<-2
ar<--0.4
Br/-(2*ar)
cr<--1

cg<- 1.5
Bg<--1

dl<-list()

for(i in 1:n.years){
  if(i==1){
    z<-rnorm(n.ind, z_b[i], sqrt(vz))
    d<-data.frame(ID=paste(1:n.ind, i, sep=" "), z=z)
    d$r_0<-cr + d$z*Br + ar*d$z^2
    d$gamma<-exp(cg + d$z*Bg)
    d$n<-n[1]
    d$v<-(d$r_0) - (d$gamma*d$n) + rnorm(1,0,sqrt(0.01))
    d$w<-rpois(nrow(d), exp(d$v))
    d$r=d$s=d$w
    d$s[d$w>0]<-sample(c(0,1), length(d$w[d$w>0]), replace=TRUE)
    d$r<-d$w-d$s
    
        } else{
    db<-as.data.frame(dl[[i-1]])
    db2<-db[db$s==1,]
    zI<-rep(db$z, times=db$r) 
    z<-zI + rnorm(length(zI), 0, sqrt(vz-var(c(zI, db2$z))))
    da<-data.frame(ID=paste(1:length(z), i), z=z) 
    d<-rbind(db2[,c("ID", "z")],da)
    d$r_0<-cr + d$z*Br + ar*d$z^2
    d$gamma<-exp(cg + d$z*Bg)
    d$n<-log(nrow(d))
    d$v<-(d$r_0) - (d$gamma*d$n) + rnorm(1,0,sqrt(0.01))
    d$w<-rpois(nrow(d), exp(d$v))
    d$r<-d$w
    d$r=d$s=d$w
    d$s[d$w>0]<-sample(c(0,1), length(d$w[d$w>0]), replace=TRUE)
    d$r<-d$w-d$s
    
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
  z_b[i]<-mean(d$z)
  vvz[i]<-var(d$z)
  n[i]<-d$n[1]
  N[i]<-exp(d$n[1])
 }

dI<-do.call(rbind.data.frame, dl)
dI$ID<-as.numeric(as.factor(dI$ID))
rownames(dI)<-1:nrow(dI)

save.image("Code/Num_ev.RData")
