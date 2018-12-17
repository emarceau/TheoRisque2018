# Lyon
# A2017
# Cours Mercredi 2017-11-08
#
# But : calculer Pr(N=k) où N=M1+M2
# (M1,M2) obéit à une loi Poisson bivariée Teicher
# 2 options : FFt ou Panjer
# important : les valeurs calculées sont exactes
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
#
#
# Loi Poisson Bivariée Teicher
#
la1<-2
la2<-3
al0<-1
mm<-2^10
EN<-la1+la2
CovM1M2<-al0
VarN<-la1+la2+2*CovM1M2

# option #1 : FFT

fB<-rep(0,mm)
fB[2]<-1
fBt<-fft(fB)
fNt<-exp((la1-al0)*(fBt-1))*exp((la2-al0)*(fBt-1))*exp(al0*(fBt^2-1))
fN<-Re(fft(fNt,inverse=TRUE)/mm)
sum(fN)
vk<-0:(mm-1)
ENv<-sum(vk*fN)
EN2v<-sum((vk^2)*fN)
VarNv<-EN2v-(ENv^2)
c(EN,ENv)
c(VarN,VarNv)

# option #2 : Panjer
laN<-la1+la2-al0
fC1<-(la1+la2-2*al0)/laN
fC2<-al0/laN
fC<-c(0,fC1,fC2)
fNpanjer<-panjer.poisson(lam=laN,ff=fC,smax=1000)
sum(fNpanjer)
vk<-0:(mm-1)
ENw<-sum(vk*fN)
EN2w<-sum((vk^2)*fN)
VarNw<-EN2w-(ENw^2)
c(EN,ENv,ENw)
c(VarN,VarNv,VarNw)

round(cbind(0:30,fN[1:31],fNpanjer[1:31]),6)



