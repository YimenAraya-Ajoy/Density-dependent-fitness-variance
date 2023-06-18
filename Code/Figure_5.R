###Relation between the mean phenotype and the r_0 and gamma
setwd("/home/yi/Dropbox/DensityDependentFitnessVariance")
load("Code/Num_ev.RData")

pdf("Figures/Figure_5.pdf", height= 6.5, width=6.5)
par(mfrow=c(2,2))
plot(vr_0~z_b[-501], ylab=expression(G[r[0]]), xlab="Mean phenotype")
abline(v=mean(z_b[400:500]), lty=2)
mtext("(A)", 3,adj=0)

plot(vgamma~z_b[-501], ylab=expression(G[gamma]), xlab="Mean phenotype")
abline(v=mean(z_b[400:500]), lty=2)
mtext("(B)", 3,adj=0)

plot(COV/sqrt(vr_0*vgamma)~z_b[-501], ylab=expression(G[paste(r[0],",",gamma)]), xlab="Mean phenotype")
abline(v=mean(z_b[400:500]), lty=2)
mtext("(C)", 3,adj=0)

plot(vv~z_b[-501], ylab=expression(G[v]), xlab="Mean phenotype")
abline(v=mean(z_b[400:500]), lty=2)
mtext("(D)", 3,adj=0)
dev.off()
