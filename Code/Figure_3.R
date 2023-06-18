setwd("/home/yi/Dropbox/DensityDependentFitnessVariance")
load("Code/Num_ev.RData")

pdf("Figures/Figure_3.pdf", height= 7.5, width=6.5)
par(mfrow=c(3,2), mar=c(5,5,2,1))
plot(n, type="l", xlab="Time steps", ylab="Log population size (n)", xlim=c(0,500))
mtext("(A)", 3, 0.1, adj=0)
abline(h=mean(Covzr[400:500])/mean(Covzg[400:500]),col="red")
abline(h=mean(mr_0[400:500])/mean(mgamma[400:500]),col="black")
abline(h=mean(n[400:500]),col="blue")

plot(mv,  ylab=expression(paste("Mean log fitness (", bar(v), ")")), xlab="Time steps", type = "l", xlim=c(0,500))
mtext("(B)", 3, 0.1, adj=0)

plot(z_b, type="l", xlab="Time steps", ylab=expression(paste("Mean phenotype (", bar(z), ")")), xlim=c(0,500))
((mean(z_b[400:500])-(-Br/(2*ar)))*2*ar)/(mean(mgamma[400:500])*Bg*mean(n[400:500]))
mtext("(C)", 3, 0.1, adj=0)

plot(mr_0, type="l", xlab="Time steps", ylab=expression(paste("Intrinsic rep rate (",bar(r)[0] ,")")), xlim=c(0,500))
mtext("(D)", 3, 0.1, adj=0)
plot(mgamma, type="l", xlab="Time steps", ylab=expression(paste("Sucept to comp (", bar(gamma),")")), xlim=c(0,500))
mtext("(E)", 3, 0.1, adj=0)

plot(vv, type="l", xlab="Time steps", ylab=expression(paste("Variance in fitness (", G[v],")")), xlim=c(0,500))
mtext("(F)", 3, 0.1, adj=0)

dev.off()
