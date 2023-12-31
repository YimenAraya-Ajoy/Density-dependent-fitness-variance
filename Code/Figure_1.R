##Variance in fitness as a function of population size
load("Code/Num_ev.RData")

z<-rnorm(n.ind, mean(z_b), sqrt(vz))
ns<-seq(3,9,by=0.1)
v<-matrix(NA,length(z), length(n))
r_0<-cr + z*Br + ar*z^2
gamma<-exp(cg + z*Bg)
v=r_0 - gamma*mean(n)

m<-matrix(NA,2,2)
m[1,1]<-var(r_0)
m[2,2]<-var(log(gamma))
m[1,2]<-m[2,1]<-cov(r_0, log(gamma))

n.ind<-1000
rg<-mvrnorm(n.ind,c(mean(r_0),mean(log(gamma))), m)
mmv<-mean(rg[,1]) - mean(exp(rg[,2]))*ns
vvv<-matrix(NA, n.ind, length(ns))

for(i in 1:n.ind){
  vvv[i,]<-rg[i,1] - exp(rg[i,2])*ns
}

mmmv<-tapply(mv,round(n,4),mean)
nns<-as.numeric(names(mmmv))
cs<-coef(lm(mmmv~nns))
mmmmv<-cs[1] + cs[2]*nns[6<nns]


pdf("Figures/Figure_1.pdf", height= 6.5, width=6.5)
par(mfrow=c(2,2), mar=c(5,5,2,1))
plot(mmv~ns, type="l", ylim=c(-2,2), xlab="Log population size (n)", ylab="Log fitness (v)")
mtext("(A)", 3, 0.1, adj=0)

for(i in 1:n.ind){
  points(vvv[i,]~ns, type="l", col="gray")
}

points(mmv~ns, type="l")

plot(apply(vvv,2,var)~ns, xlab="Log population size (n)", ylab=expression(paste("Log fitness variance (", G[v], ")")), col="gray", type="l")
mtext("(B)", 3, 0.1, adj=0)
Evw<-var(rg[,1]) + var(exp(rg[,2]))*ns^2 - 2*ns*cov(rg[,1],exp(rg[,2]))

plot(NA, xlim=c(6,8), ylim=c(-0.5, 0.3), type="l", ylab="Log fitness (v)", xlab="Log population size (n)")
mtext("(C)", 3, 0.1, adj=0)

s<-sample(dI$ID, 500)

for(i in s){
  dtmp<-dI[dI$ID==i,]
  if(nrow(dtmp)>1){
    dtmp$vtmp<-dtmp$r_0 -(dtmp$gamma*dtmp$n)
    points(dtmp$vtm~dtmp$n, type="l", col="gray")
  }
} 

points(mmmmv~nns[6<nns],type="l")


plot(vv[1000:2000]~n[1000:2000], xlab="Log population size (n)", ylab=expression(paste("Log fitness variance (", G[v], ")")), col="gray", xlim=c(6,8), ylim=c(0.02,0.1))
n2<-seq(6,8, by=0.01)
Evw1<-mean(vr_0) + 2*n2*-mean(COV) + n2^2*mean(vgamma)
mmmvmv<-tapply(vv[1000:2000],round(n[1000:2000],2),mean)
points(mmmvmv~as.numeric(names(mmmvmv)), col="red")
points(Evw1~n2, type="l")

mtext("(D)", 3, 0.1, adj=0)

dev.off()
