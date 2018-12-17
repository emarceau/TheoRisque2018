# Lyon A2017
#
# Copule Clayton
#
# Simulation de réalisations de la copule normale (gaussienne)
#
# Paramètre de dépendance : rho
alp<-5
alp2<-1/alp
#
set.seed(2017)
nsim<-100000
#
matV<-matrix(runif(3*nsim),nsim,3,byrow=TRUE)
Theta<-qgamma(matV[,1],alp2,1)
vY1<-qexp(matV[,2],Theta)
vY2<-qexp(matV[,3],Theta)
vU1<-(1/(1+vY1))^alp2
vU2<-(1/(1+vY2))^alp2
#par(mfrow=c(1,2))
#plot(vU1,vU2)
#plot(1-vU1,1-vU2)
#par(mfrow=c(1,2))
#plot(1-vU1,vU2)
#plot(vU1,1-vU2)

cor(vU1,vU2)
cor(1-vU1,1-vU2)

vX1<-qexp(vU1)
vX2<-qexp(vU2)

vX3<-qexp(1-vU1)
vX4<-qexp(1-vU2)

cor(vX1,vX2)


cor(vX3,vX4)

S1<-vX1+vX2
S2<-vX3+vX4

vkap<-(9000:9999)/10000
VaRS1<-quantile(S1,probs=vkap)
VaRS2<-quantile(S2,probs=vkap)
par(mfrow=c(1,1))
matplot(vkap,cbind(VaRS1,VaRS2),type="l")

# Paramètre de dépendance : rho
alp<-1
alp2<-1/alp
#
set.seed(2017)
nsim<-100000
#
matV<-matrix(runif(3*nsim),nsim,3,byrow=TRUE)
Theta<-qgamma(matV[,1],alp2,1)
vY1<-qexp(matV[,2],Theta)
vY2<-qexp(matV[,3],Theta)
vU1<-(1/(1+vY1))^alp2
vU2<-(1/(1+vY2))^alp2
#par(mfrow=c(1,2))
#plot(vU1,vU2)
#plot(1-vU1,1-vU2)
#par(mfrow=c(1,2))
#plot(1-vU1,vU2)
#plot(vU1,1-vU2)

cor(vU1,vU2)
cor(1-vU1,1-vU2)

vX5<-qexp(vU1)
vX6<-qexp(vU2)

vX7<-qexp(1-vU1)
vX8<-qexp(1-vU2)

cor(vX5,vX6)

cor(vX7,vX8)

S3<-vX5+vX6
S4<-vX7+vX8

vkap<-(9000:9999)/10000
VaRS3<-quantile(S3,probs=vkap)
VaRS4<-quantile(S4,probs=vkap)
par(mfrow=c(1,1))

matplot(vkap,cbind(VaRS1,VaRS4),type="l")

cor(vX1,vX2)
cor(vX7,vX8)

cor(vX1,vX2,method="spearman")
cor(vX3,vX4,method="spearman")
cor(vX5,vX6,method="spearman")
cor(vX7,vX8,method="spearman")








