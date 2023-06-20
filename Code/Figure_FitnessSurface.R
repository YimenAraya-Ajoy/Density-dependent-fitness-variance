require(lattice)
setwd("/home/yi/Dropbox/DensityDependentFitnessVariance")
load("Code/Num_ev.RData")
pdf("Figures/FitnessSurface.pdf", height= 5, width=6.5)

z<-seq(2.5,5, by=0.05)
r_0<-cr + z*Br + ar*z^2 
gamma<-exp(cg + z*Bg )

vmat<-matrix(NA,length(z), length(ns))
for(i in 1:length(ns)){
  vmat[,i]=r_0 - gamma*ns[i]
}


dvv<-data.frame(v=matrix(vmat,,1),z=rep(z,length(ns)), n=rep(ns, each=length(z)))
wireframe(dvv$v~dvv$z+dvv$n, zlab="v", xlab="z", ylab="n",
          screen = list(z = -35, x = -70), 
          scales = list( arrows = FALSE, col="black", cex.axis=0.8), 
          aspect = c(1.1, 1),
          drape = FALSE, par.settings = list(axis.line = list(col = "transparent")),
          par.box = c(col = "black") )


z<-seq(3.2,4.2, by=0.1)
ns<-seq(3,7,by=0.05)
v<-matrix(NA,length(z), length(n))

mr_0<-cr + z*Br + ar*(z^2 + 0.1)
mgamma<-exp(cg + z*Bg + (Bg^2*0.1)/2)
vmat2<-matrix(NA,length(z), length(ns))
for(i in 1:length(ns)){
  vmat2[,i]=mr_0 - mgamma*ns[i]
}



dvv2<-data.frame(v=matrix(vmat2,ncol(vmat2)*nrow(vmat2),1),z=rep(z,length(ns)), n=rep(ns, each=length(z)))
wireframe(dvv2$v~dvv2$z+dvv2$n, zlab="v", xlab="z", ylab="n",
          screen = list(z = -35, x = -70), 
          scales = list( arrows = FALSE, col="black", cex.axis=0.8), 
          aspect = c(1.1, 1),
          drape = FALSE, par.settings = list(axis.line = list(col = "transparent")),
          par.box = c(col = "black") )


dev.off()
