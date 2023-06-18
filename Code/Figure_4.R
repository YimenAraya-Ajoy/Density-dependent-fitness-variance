setwd("/home/yi/Dropbox/DensityDependentFitnessVariance")
load("Code/Num_ev.RData")

pdf("Figures/Figure_4.pdf", height= 5, width=6.5)
par(mfrow=c(2,2), mar=c(5,5,2,1))
delta_v<-mv[-1]-mv[-c(n.pops,n.pops+1)]
delta_g<-(mgamma[-1]-mgamma[-c(n.pops,n.pops)])
delta_r<-(mr_0[-1]-mr_0[-c(n.pops,n.pops)])

plot(delta_g~delta_r,  ylab=expression(paste("Sucept to comp change (", Delta, bar(gamma),")")),  
     xlab=expression(paste("Intrinsic rep. change (", Delta, bar(r)[0],")")), ylim=c(-0.003,0.003))
abline(v=0, lty=2)
abline(h=0, lty=2)
mtext("(A)", 3, 0.1, adj=0)

zb2<-seq(min(z_b), max(z_b), by=0.001)
plot(vv~z_b[-n.pops+1], xlab=expression(paste("Mean Phenotype (", bar(z), ")")), ylab=expression(paste("Variance in fitness (" ,G[v], ")")))
abline(v=mean(z_b[50:100]), lty=2)
n2<-mean(n)
Evw2<-(2*ar*zb2 + Br - (Bg*n2*exp(cg + Bg*zb2)))^2*vz + (((2*ar-Bg^2*n2*exp(cg + Bg*zb2))^2)/2)*vz^2
points(Evw2~zb2, type="l")
mtext("(B)", 3, 0.1, adj=0)

plot(delta_v[-1]~vv[-c(n.pops-1,n.pops)],  
     ylab=expression(paste("Mean fitness change (", Delta, bar(v),")")),  
     xlab=expression(paste("Variance in fitness (" ,G[v], ")")))
abline(lm(delta_v[-1]~vv[-c(n.pops-1,n.pops)]))
mtext("(C)", 3, 0.1, adj=0)

plot(delta_v[-1]~delta_v_ed[-c(n.pops-1,n.pops)],  
     ylab=expression(paste("Mean fitness change (", Delta, bar(v),")")), 
     xlab=expression(paste("Change in pop. size (", gamma,Delta,"n)")))

abline(lm(delta_v[-1]~delta_v_ed[-c(n.pops-1,n.pops)]))
mtext("(D)", 3, 0.1, adj=0)

dev.off()
