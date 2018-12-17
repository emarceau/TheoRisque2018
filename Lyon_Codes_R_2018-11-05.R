# Lyon A2018
#
# Séance : lundi 2018-11-05
#
# Loi exponentielle
bet<-1/10
EX<-1/bet
EX
# Fonction de répartition
x<-0:100
Fx<-pexp(x,bet)
Fx
plot(x,Fx,type="l",xlab="x",ylab="Fx")

# Fonction de densité
x<-0:100
fx<-dexp(x,bet)
fx
plot(x,fx,type="l",xlab="x",ylab="fx")

#Fonction quantile
u<-(1:999)/1000
Qu<-qexp(u,bet)
Qu
plot(u,Qu,type="l",xlab="u",ylab="Qu")

#VaR et TVaR
kap<-(1:9999)/10000
VaRX<-qexp(kap,bet)
TVaRX<-VaRX+1/bet
matplot(kap,cbind(VaRX,TVaRX,rep(1/bet,9999)),type="l",xlab="kappa",ylab="VaR et TVaR")

#Fonction stop-losss
x<-0:100
SLx<-(1/bet)*exp(-bet*x)
SLx
plot(x,SLx,type="l",xlab="x",ylab="SLx")

#Methode MC
set.seed(2018)
nsim<-20000
U<-runif(nsim)
X<-qexp(U,bet)
#X<--log(1-U)/bet
#cbind(1:nsim,U,X)
EX<-mean(X)
EX
cEX<-cumsum(X)/(1:nsim)
plot(1:nsim,cEX,type="l",xlab="m",ylab="EX")

plot.ecdf(X)
points(x,Fx,type="l")

dd<-60
SLdd<-mean(pmax(X-dd,0))
SLdd
(1/bet)*exp(-bet*dd)
1-pexp(dd,bet)


