# Lyon A2017
# jeudi 9 novembre 2017
#
# methode allocation Euler 
# simulation
# TVaR et VaR
# X1,...,Xn = indépendantes

set.seed(2017)

nsim<-100000
nrisk<-3
matU<-matrix(runif(nrisk*nsim),nsim,nrisk)
X1<-qgamma(matU[,1],2,1/10)
X2<-qlnorm(matU[,2],log(20)-0.5,1)
X3<-qgamma(matU[,3],0.5,1/40)
S<-X1+X2+X3
#cbind(1:nsim,X1,X2,X3,S)
kap<-0.99
VaRS<-quantile(S,probs=kap,type=1)
VaRS
TVaRS<-sum(S*(1*(S>VaRS)))/nsim/(1-kap)
TVaRS

C1<-sum(X1*(1*(S>VaRS)))/nsim/(1-kap)
C2<-sum(X2*(1*(S>VaRS)))/nsim/(1-kap)
C3<-sum(X3*(1*(S>VaRS)))/nsim/(1-kap)

VaRX1<-quantile(X1,probs=kap,type=1)
TVaRX1<-sum(X1*(1*(X1>VaRX1)))/nsim/(1-kap)
VaRX2<-quantile(X2,probs=kap,type=1)
TVaRX2<-sum(X2*(1*(X2>VaRX2)))/nsim/(1-kap)
VaRX3<-quantile(X3,probs=kap,type=1)
TVaRX3<-sum(X3*(1*(X3>VaRX3)))/nsim/(1-kap)


c(C1,C2,C3,C1+C2+C3,TVaRS)
c(TVaRX1,TVaRX2,TVaRX3)
mean(X1)
mean(X2)
mean(X3)

TVaRX1








