# Lyon A2017
#
# Copule Clayton
#
# Simulation de réalisations de la copule normale (gaussienne)
#
# Paramètre de dépendance : rho
alp<-2
alp2<-1/alp
#
set.seed(2017)
nsim<-10000
#
matV<-matrix(runif(3*nsim),nsim,3,byrow=TRUE)
Theta<-qgamma(matV[,1],alp2,1)
vY1<-qexp(matV[,2],Theta)
vY2<-qexp(matV[,3],Theta)
vU1<-(1/(1+vY1))^alp2
vU2<-(1/(1+vY2))^alp2

plot(vU1,vU2)

#cbind(vU1,1-vU2)

cor(qexp(vU1),qlnorm(vU2),method="kendall")
cor(vU1,vU2,method="kendall")

set.seed(2017)
vU3<-runif(nsim)
vU4<-runif(nsim)
cor(qexp(vU3),qlnorm(vU4),method="kendall")
