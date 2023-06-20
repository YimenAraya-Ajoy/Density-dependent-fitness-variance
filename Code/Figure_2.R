load("Code/Num_ev.RData")

z<-seq(3,4, by=0.001)
r_0 <- cr + z*Br + ar*(z^2 + mean(vvz[1000:2000])) 
gamma<- exp(cg + z*Bg + (Bg^2*mean(vvz[1000:2000]))/2)
v<-r_0-gamma*mean(mr_0/mgamma)
ns<-seq(5,9,by=0.01)

pdf("Figures/Figure_2.pdf", height= 6.5, width=6.5)
par(mfrow=c(2,2), mar=c(5,5,2,1))
plot(r_0~z, xlab="Phenotype (z)", ylab=expression(paste("Intrinsic rep rate (", r[0], ")")), type="l")
mtext("(A)", 3, 0.1, adj=0)

plot(gamma~z, xlab="Phenotype (z)", ylab=expression(paste("Sucept to comp (", gamma,")")), type="l")
mtext("(B)", 3, 0.1, adj=0)

plot(v~z, xlab=expression(paste("Mean phenotype (", bar(z), ")")), ylab="Log fitness (v)", type="l", lwd=2, ylim=c(-1,0.5), xlim=c(3,4))
colfunc <- colorRampPalette(c("blue", "red"))
col=colfunc(length(ns))
mtext("(C)", 3, 0.1, adj=0)

for(i in 1:length(ns)){
  v1=r_0 - gamma*ns[i]
  points(v1~z, col=col[i],type="l")
}
points(v~z, type="l", lwd=2)
abline(v=mean(z_b),lty=2)
abline(h=0,lty=2)


for(i in 1:length(ns)){
  v=r_0 - gamma*ns[i]
  points(max(v)~z[v==max(v)], pch=19, cex=0.01)
}



exp_n_hat<-r_0/gamma
plot(exp_n_hat~z, xlab=expression(paste("Mean phenotype (", bar(z), ")")), ylab=expression(paste("Eq log population size (", hat(n), ")")), type="l", lwd=2)
abline(v=mean(z_b),lty=2)
abline(h=max(exp_n_hat),lty=2)
mtext("(D)", 3, 0.1, adj=0)

z[exp_n_hat==max(exp_n_hat)]

dev.off()
