# Lyon A2017
#
# Copule Normale
#
# Simulation de réalisations de la copule normale (gaussienne)
#
# Paramètre de dépendance : rho
rho<-0.7
#
set.seed(2017)
#
nsim<-10000
matV<-matrix(runif(2*nsim),nsim,2,byrow=TRUE)
matZ<-qnorm(matV)
# Loi de (Y1,Y2) : normale bivariée avec coefficient Pearson = rho
Y1<-matZ[,1]
Y2<-rho*matZ[,1]+((1-rho^2)^0.5)*matZ[,2]
#
plot(Y1,Y2)
#
# (U_1,U_1) : copule de v.a. de loi uniforme standard
# F_{U_1,U_2} : copule normale
# Application de la partie #1 du théorème de Sklar
U1<-pnorm(Y1)
U2<-pnorm(Y2)
#
plot(U1,U2)
#
#
# Loi de (X1,X2) : loi définie par F_{X_1,X_2}
# F_{X_1,X_2} définie par la copule normale et F_1 et F_2
# F_1 : fn répartition de la loi gamma
# F_2 : fn répartition de la loi lognormale
# Application de la partie #2 du théorème de Sklar
#
a1<-2
b1<-1/10
mu2<-log(20)-0.5
sig2<-1
# 
X1<-qgamma(U1,a1,b1)
X2<-qlnorm(U2,mu2,sig2)
#
plot(X1,X2)
#
cov(X1,X2)
cor(X1,X2)
S<-X1+X2
vkap<-(1:999)/1000
VaRS<-quantile(S,probs=vkap)
plot(vkap,VaRS,type="l")
# 
R1<-rank(X1)
R2<-rank(X2)
#
plot(R1/(nsim+1),R2/(nsim+1))

