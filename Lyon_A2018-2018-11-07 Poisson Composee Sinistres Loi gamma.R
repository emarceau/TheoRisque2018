# Lyon A2018
# Date : 2018-11-07

# Exemple en classe
# Loi Poisson composée

# ---- Paramètres de la loi de M (Poisson)

lam<-2
EM<-lam
VarM<-lam

# ---- Paramètres de la loi de B (Gamma)
alpha<-2.5
beta<-1/1000
EB<-alpha/beta
VarB<-alpha/(beta^2)

# ---- Espérance de X
EX<-EM*EB

# ---- Variance de X
VarX<-EM*VarB+VarM*(EB^2)

c(EM,VarM)
c(EB,VarB)
c(EX,VarX)

# ---- Fonction de répartition de X

k0<-1000
vk<-1:k0
fM0<-dpois(0,lam)
vfM<-dpois(vk,lam)
sum(vfM)+fM0
xx<-20000

Fx<-fM0+sum(vfM*pgamma(xx,vk*alpha,beta))

c(xx,Fx)
xx
Fx

# ---- Construction d'une fonction R pour Fx 

FnX.poiscompgamma<-function(x,lambda,alpha,beta,k0=1000)
{
  v.k<-0:k0
  v.k2<-1:k0
  v.prob<-dpois(v.k,lambda)
  v.prob2<-c(1,pgamma(x,v.k2*alpha,beta))
  Fn<-sum(v.prob*v.prob2)
  return(Fn)
}

FnX.poiscompgamma(x=0,lambda=lam,alpha=alpha,beta=beta)
FnX.poiscompgamma(x=3000,lambda=lam,alpha=alpha,beta=beta)
FnX.poiscompgamma(x=10000,lambda=lam,alpha=alpha,beta=beta)
FnX.poiscompgamma(x=20000,lambda=lam,alpha=alpha,beta=beta)

# ---- VaR et TVaR de X

# -- les calculs fonctionnent pour toute valeur de kappa qui est dans [0,1[
kappa<-0.99
FX0<-dpois(0,lam)

# -- Construction d'une fonction R pour être utilisée dans "optimize"
f <- function(x) abs(FnX.poiscompgamma(x,lam=lam,alpha=alpha,beta=beta)-kappa)

# -- VaR de X
VaRX<-0

if (kappa>FX0) 
{
  res<-optimize(f, c(100,30000))
  VaRX<-res$minimum
}
VaRX

# -- Vérification de la VaR de X
FnX.poiscompgamma(x=VaRX,lam=lam,alpha=alpha,beta=beta)

# -- Construction d'une fonction R pour espérance tronquée à droite de X

EtrX.poiscompgamma<-function(x,lambda,alpha,beta,k0=1000)
{
  vk<-1:k0
  vfM<-dpois(1:k0,lambda)
  E<-sum(vfM*(alpha*vk/beta)*(1-pgamma(VaRX,vk*alpha+1,beta)))
  return(E)
}


# -- TVaR de X

EtronqX<-EtrX.poiscompgamma(VaRX,lambda=lam,alpha=alpha,beta=beta)
TVaRX<-EtronqX/(1-kappa)

# -- Résultats pour VaR et TVaR
c(kappa,VaRX,TVaRX)
FnX.poiscompgamma(x=VaRX,lam=lam,alpha=alpha,beta=beta)
FX0
EtronqX

# -- Courbe de Fx

vx<-(0:25)*1000
long<-length(vx)
vFx<-rep(0,long)
for (i in 1:long)
{
vFx[i]<-FnX.poiscompgamma(x=vx[i],lam=lam,alpha=alpha,beta=beta)
}
plot(c(0,vx),c(0,vFx),type="l",xlab="x",ylab="FX(x)",main="loi Poisson composée avec sinistres de loi gamma")



