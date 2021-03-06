# Lyon A2018
#
# Exemple : Algo Panjer + discrétisation
#
# Cas particulier : Algo Panjer Poisson composée

panjer.poisson<-function(lam,fB,xmax)
{
  # Algorithme de Panjer
  # Cas Poisson
  # Loi discrete pour B
  aa<-0
  bb<-lam
  ll<-length(fB)
  fX<-exp(lam*(fB[1]-1))
  fB<-c(fB,rep(0,xmax-ll+1))
  for (i in 1:xmax)
  {
    j<-i+1
    fX<-c(fX,(1/(1-aa*fB[1]))*sum(fB[2:j]*fX[i:1]*(bb*(1:i)/i+aa)))    
  }
  
  return(fX)
}


# loi de M: Poisson
lambda<-2
EM<-lambda

# loi de B: lognormale 
mu<-2
sig<-1
EB<-exp(mu+(sig^2)/2)

# EX
EX<-EM*EB

# discretisation upper
vk<-1:20000
hh<-0.1
vkh<-vk*hh
vfBup<-plnorm(vkh,mu,sig)-plnorm(vkh-hh,mu,sig)
vfXup<-panjer.poisson(lam=lambda,fB=vfBup,xmax=21000)
nup<-length(vfXup)

sum(vfXup)
EXup<-sum((0:(nup-1))*hh*vfXup)
c(EX,EXup)

# discretisation lower
vk<-1:20000
hh<-0.1
vkh<-vk*hh
vfBlow<-c(0,plnorm(vkh,mu,sig)-plnorm(vkh-hh,mu,sig))
vfXlow<-panjer.poisson(lam=lambda,fB=vfBlow,xmax=21000)
nlow<-length(vfXlow)

sum(vfXlow)
EXlow<-sum((0:(nlow-1))*hh*vfXlow)
c(EX,EXlow)

# Calcul de la VaR
kappa<-0.99
vFXlow<-cumsum(vfXlow)
VaRXlow<-sum(vFXlow<kappa)
VaRXlow

kappa<-0.99
vFXup<-cumsum(vfXup)
VaRXup<-sum(vFXup<kappa)
VaRXup

c(VaRXup*hh,VaRXlow*hh)


# Calcul de la TVaR avec espérance tronquée

long<-length(fX1)-1
vj<-0:long
EX1tronq<-sum(vj*fX1*(vj>VaRX1))
TVaRX1<-(EX1tronq+VaRX1*(FX1[VaRX1+1]-kappa))/(1-kappa)
TVaRX1

