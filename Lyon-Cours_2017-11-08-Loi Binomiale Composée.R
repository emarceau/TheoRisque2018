# Lyon
# A2017
# Cours Mercredi 2017-11-08
#
# Loi de (X_1,X_2) : binomialecomposée bivariée
# Loi de B1 : discrètes
# Loi de B2 : discrètes
# S = X_1 + X_2
# But : calculer Pr(S=k) où N=M1+M2
# 1 option présentée : FFt
# important : les valeurs calculées sont exactes
#
p00<-0.7
p10<-0.15
p01<-0.05
p11<-0.1
p00+p01+p10+p11
nn<-10
q1<-p10+p11
q2<-p01+p11
q1
q2
EM1<-nn*q1
EM2<-nn*q2
mm<-2^10
vk<-0:(mm-1)
fB1<-dpois(vk,2)
fB2<-dnbinom(vk,1.5,1/3)
EB1<-2
EB2<-1.5*(1-1/3)/(1/3)
EX1<-EM1*EB1
EX2<-EM2*EB2
ES<-EX1+EX2
ES
fB1t<-fft(fB1)
fB2t<-fft(fB2)
fSt<-(p00+p10*fB1t+p01*fB2t+p11*fB1t*fB2t)^nn
fS<-Re(fft(fSt,inverse=TRUE)/mm)
sum(fS)
ESv<-sum(vk*fS)
ES
  ESv
  plot(0:40,fS[1:41],type="h")

