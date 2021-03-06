# Lyon
# A2017
# Cours Mardi 2017-11-06
# 
# FFT = Transform�e de Fourier rapide
#
# ------ Exercice de r�chauffement - Approche na�ve -------
f1<-c(0.3,0.4,0.2,0.1)
nbim<-1i
vk<-0:3
f1
sum(f1)
#
f1t<-rep(0,4)
# construction
for (j in 0:3)
{
  f1t[j+1]<-sum(exp(nbim*2*pi*vk*j/4)*f1)
}

f1t

f1v<-rep(0,4)
# inversion
for (k in 0:3)
{
  f1v[k+1]<-(1/4)*sum(exp(-nbim*2*pi*vk*k/4)*f1t)
}

Re(f1v)

# ----- Exercice pour s'amuser un peu  -------

f1<-c(0.3,0.4,0.2,0.1)
f2<-c(0.2,0.5,0.25,0.05)
nn<-8
f1c<-c(f1,rep(0,4))
f2c<-c(f2,rep(0,4))
nbim<-1i
vk<-0:(nn-1)
f1c
sum(f1c)
f2c
sum(f2c)
#
f1t<-rep(0,nn)
f2t<-rep(0,nn)
# construction
for (j in 0:(nn-1))
{
  f1t[j+1]<-sum(exp(nbim*2*pi*vk*j/nn)*f1c)
  f2t[j+1]<-sum(exp(nbim*2*pi*vk*j/nn)*f2c)
}

fst<-f1t*f2t

cbind(f1t,f2t,fst)

fsv<-rep(0,nn)
# inversion
for (k in 0:(nn-1))
{
  fsv[k+1]<-(1/nn)*sum(exp(-nbim*2*pi*vk*k/nn)*fst)
}


fs<-rep(0,nn)
for (k in 1:nn)
{
  fs[k]<-sum(f1c[1:k]*f2c[k:1])
}

cbind(0:(nn-1),round(Re(fsv),6),fs)

2^15

# ----------- Exercice Poisson compos�e -----------
#
# Pareto continue

ppareto<-function(x,aa,la)
{
  FF<-1-((la/(la+x))^aa)
  return(FF)
}

ppareto(0:10,aa=2,la=5)


#
# loi de X : Poisson compos�e 
# loi de M : Poisson
# param�tre de la loi de Poisson : lambda
# repr�sentation de X : X = \sum_{k=1}^M B_k
# fmp de B : fB
# fmp de X : fX
#
#
alphaP<-3
lambdaP<-20
vk<-1:10000
# d�finition du vecteur fB (fonction de masses de prob de la v.a. B)
fB<-c(0,ppareto(vk,aa=alphaP,la=lambdaP)-ppareto(vk-1,aa=alphaP,la=lambdaP))
sum(fB)
# param�tre de la loi Poisson
lambda<-2
# calculs
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
# - On utilise FFT
nn<-2^15
nn
long<-length(fB)
# On ajoute des "0"
fBc<-c(fB,rep(0,nn-long))
# On utilise fft pour calculer les valeurs de la fn caract�ristique de B
fBt<-fft(fbt)
# on calculer les valeurs de la fn caract�ristique de X 
fXt<-exp(lambda*(fBt-1))
# on inverse avec fft pour calculer les valeurs de fX
fX<-Re(fft(fXt,inverse=TRUE)/nn)
# 
# V�rifications
sum(fX)
# 
EXv<-sum((0:18000)*fX)
EX2v<-sum(((0:18000)^2)*fX)
VarXv<-EX2v-(EXv^2)
c(EX,EXv)
c(VarX,VarXv)

