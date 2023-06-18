setwd("/home/yi/Dropbox/DensityDependentFitnessVariance")
load("Code/Num_ev.RData")

z<-seq(2,5, by=0.005)
r_0<-cr + z*Br + ar*z^2 
gamma<-exp(cg + z*Bg )
v<-r_0-gamma*mean(n)


pdf("Figures/Figure_2.pdf", height= 6.5, width=6.5)
par(mfrow=c(2,2), mar=c(5,5,2,1))
plot(r_0~z, xlab="Phenotype (z)", ylab=expression(paste("Intrinsic rep rate (", r[0], ")")), type="l")
mtext("(A)", 3, 0.1, adj=0)

plot(gamma~z, xlab="Phenotype (z)", ylab=expression(paste("Sucept to comp (", gamma,")")), type="l")
mtext("(B)", 3, 0.1, adj=0)

plot(v~z, xlab="Phenotype (z)", ylab="Log fitness (v)", type="l", lwd=2, ylim=c(-4,1))
colfunc <- colorRampPalette(c("blue", "red"))
col=colfunc(length(ns))
mtext("(C)", 3, 0.1, adj=0)

for(i in 1:length(ns)){
  v=r_0 - gamma*ns[i]
  points(v~z, col=col[i],type="l")
}

v=r_0 - gamma*mean(n)
points(v~z, type="l", lwd=2)
abline(v=mean(z_b),lty=2)
abline(h=0,lty=2)
for(i in 1:length(ns)){
  v=r_0 - gamma*ns[i]
  points(max(v)~z[v==max(v)], pch=19, cex=0.01)
}

n.ind<-10000000
z<-rnorm(n.ind, mean(z_b), sqrt(0.1))
r_0<-cr + z*Br + ar*z^2 
gamma<-exp(cg + z*Bg )

n_hat<-cov(z,r_0)/cov(z,gamma)
v<-r_0 - gamma*n_hat

z<-seq(3.2,4.2, by=0.005)
ns<-seq(3,7,by=0.05)
v<-matrix(NA,length(z), length(n))

mr_0<-cr + z*Br + ar*(z^2 + 0.1)
mgamma<-exp(cg + z*Bg + (Bg^2*0.1)/2)
mv=mr_0 - mgamma*n_hat

exp_n_hat<-mr_0/mgamma
eqz<-z[exp_n_hat==max(exp_n_hat)]
plot(exp_n_hat~z, xlab=expression(paste("Mean phenotype (", bar(z), ")")), ylab=expression(paste("Eq log population size (", hat(n), ")")), type="l", lwd=2)
abline(v=mean(z_b),lty=2)
abline(h=max(exp_n_hat),lty=2)
mtext("(D)", 3, 0.1, adj=0)

dev.off()
