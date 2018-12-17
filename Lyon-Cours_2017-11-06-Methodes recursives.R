# Lyon
# A2017
# Cours Mardi 2017-11-06
# 
# Algorithme de Panjer
#
# Fonction
#
panjer.poisson<-function(lam,ff,smax)
{
  aa<-0
  bb<-lam
  ll<-length(ff)
  ffs<-exp(lam*(ff[1]-1))
  ff<-c(ff,rep(0,smax-ll+1))
  for (i in 1 :smax)
  {
    j<-i+1
    ffs<-c(ffs,(1/(1-aa*ff[1]))*sum(ff[2 :j]*ffs[i :1]*(bb*(1 :i)/i+aa)))
  }
  return(ffs)
}
#
# Pareto

ppareto<-function(x,aa,la)
{
  FF<-1-((la/(la+x))^aa)
  return(FF)
}

ppareto(0:10,aa=2,la=5)


# Utilisation de l'algo de Panjer
#
# loi de X : Poisson composée 
# loi de M : Poisson
# paramètre de la loi de Poisson : lambda
# représentation de X : X = \sum_{k=1}^M B_k
# fmp de B : fB
# fmp de X : fX
#
#
alphaP<-3
lambdaP<-20
vk<-1:10000
fB<-c(0,ppareto(vk,aa=alphaP,la=lambdaP)-ppareto(vk-1,aa=alphaP,la=lambdaP))
sum(fB)
lambda<-2
EM<-lambda
EB<-sum(fB*c(0,vk))
EB2<-sum(fB*c(0,vk^2))
EX<-EM*EB
VarX<-lambda*EB2
EM
EB
EX
VarX
#
fX<-panjer.poisson(lam=lambda,ff=fB,smax=18000)
# 
# Vérifications
sum(fX)
# 
EXv<-sum((0:18000)*fX)
EX2v<-sum(((0:18000)^2)*fX)
VarXv<-EX2v-(EXv^2)
c(EX,EXv)
c(VarX,VarXv)
# FX
FX<-cumsum(fX)
plot(0:1000,FX[1:1001],type="l")
# prime stop-loss
long<-length(FX)-1
vk<-0:long
k<-100
SL<-sum(pmax(vk-k,0)*fX)
SL
#
# VaR
#
FX[1:10]

kap<-0.00001
kap
VaRX.1<-sum((FX<kap)*1)
VaRX.2<-min(vk[(FX>=kap)])

VaRX.1
VaRX.2

VaRX<-VaRX.2
EXtron<-sum(vk*fX*(vk>VaRX))
TVaRX<-(EXtron+VaRX*(FX[1+VaRX]-kap))/(1-kap)

c(kap,VaRX,TVaRX)

c(EX,VarX)






