# Lyon A2018
# Date : Lundi 2018-11-05
# Mutualisation
#
#
# Contexte : portefeuille de 2 risques X1 et X2
# 
# Loi de X1 : Gamma
a1<-2
b1<-1/10
EX1<-a1/b1
EX1
# Loi de X2 : Gamma
a2<-5
b2<-1/10
EX2<-a2/b2
EX2
# Loi de S
ES<-EX1+EX2
ES

# Methode MC
nsim<-5
set.seed(2018)
U<-matrix(runif(2*nsim),nsim,2,byrow=T)
X1<-qgamma(U[,1],a1,b1)
X2<-qgamma(U[,2],a2,b2)
S<-X1+X2
cbind(1:5,U,X1,X2,S)

nsim<-1000
set.seed(2018)
U<-matrix(runif(2*nsim),nsim,2,byrow=T)
X1<-qgamma(U[,1],a1,b1)
X2<-qgamma(U[,2],a2,b2)
S<-X1+X2
X1t<-sort(X1)
X2t<-sort(X2)
St<-sort(S)

kap<-0.8
VaRX1<-X1t[kap*nsim]
VaRX2<-X2t[kap*nsim]
VaRS<-St[kap*nsim]
cbind(kap,VaRX1,VaRX2,VaRX1+VaRX2,VaRS)
TVaRX1<-mean(X1*(X1>VaRX1))/(1-kap)
TVaRX2<-mean(X2*(X2>VaRX2))/(1-kap)
TVaRS<-mean(S*(S>VaRS))/(1-kap)
cbind(kap,TVaRX1,TVaRX2,TVaRX1+TVaRX2,TVaRS)









