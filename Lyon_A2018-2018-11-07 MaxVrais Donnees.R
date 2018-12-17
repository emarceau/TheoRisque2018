#Installation des packages
if (!"CASdatasets" %in% installed.packages()){
  install.packages(c("xts","sp"))
  install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/R/", type="source")
}
install.packages("actuar")
install.packages("ReIns")
##activation des packages
library("ReIns")
library("actuar")
library("CASdatasets")
###Charger les 3 bases de donn?es
data("secura")
data("norwegianfire")
data("danishuni")

###attribution ? des nouvelles variables (pour rendre le code plus lisible)
##Montants
Montants_NF <- norwegianfire$size
Montants_S <- secura$size
Montants_D <- danishuni$Loss

##Dates
annee_NR <- norwegianfire$year
annee_S <- secura$year
Dates_DF <- danishuni$Date

###Nombre de sinistres
##Nowergian Fire
n_NF <- sapply(72:92,function(t){
  sum(annee_NR==t)  
})#nombre d observation par ann?e

##Secura
n_S <- sapply(1990:2001,function(t){
  sum(annee_S==t)  
})#nombre d observation par ann?e

##Danish Fire
jours <- seq(as.Date('1980-01-01'), as.Date('1990-12-01'), by="days")
nombres_jours <- sapply(jours,function(t){
  sum(Dates_DF==t)  
})#nombre d observation par jour (il y a surement une methode plus ?fficace)

###Statistiques
summary(Montants_NF)
summary(Montants_S)
summary(Montants_D)
length(Montants_NF)
length(Montants_S)
length(Montants_D)
summary(n_NF)
summary(n_S)
summary(nombres_jours)

###Histogramme
hist(Montants_NF[Montants_NF<10000])
hist(Montants_S)
hist(Montants_D[Montants_D<25])

plot(72:92,n_NF,xlab="Ann?es",ylab="Fr?quence",main = "Norwegian Fire fr?quence annulle",type="l")
plot(1990:2001,n_S,xlab="Ann?es",ylab="Fr?quence",main = "Secura fr?quence annulle",type="l")
plot(jours,nombres_jours,xlab="Jours",ylab="Fr?quence",main = "Danish Fire fr?quence par jour",type="l")

plot(72:92,cumsum(n_NF),xlab="Ann?es",ylab="Fr?quence cumul?e",main = "Norwegian Fire fr?quence cumul?e",type="l")
plot(1990:2001,cumsum(n_S),xlab="Ann?es",ylab="Fr?quence cumul?e",main = "Secura fr?quence cumul?e",type="l")
plot(jours,cumsum(nombres_jours),xlab="Jours",ylab="Fr?quence cumul?e",main = "Danish Fire fr?quence cumul?e",type="l")

plot(ecdf(log(Montants_NF)),main = "cdf empirique du log de Nowergian fire") ## ecdf du log de X car les valeurs extr?mes sont trops importantes
plot(ecdf(Montants_S),main = "cdf empirique de secura") ## ecdf simple
plot(ecdf(log(Montants_D)),main = "cdf empirique du log de Danish fire") ###ecdf du log de X

plot(ecdf(Montants_NF),main = "cdf empirique du log de Nowergian fire") ## ecdf du log de X car les valeurs extr?mes sont trops importantes
plot(ecdf(Montants_S),main = "cdf empirique de secura") ## ecdf simple
plot(ecdf(Montants_D),main = "cdf empirique du log de Danish fire") ###ecdf du log de X

# -----------------

# données

donnees<-Montants_D

# On calcule l'esp?rance et la variance empiriques
#
moy<-mean(donnees)
var<-var(donnees)
moy
var
# 
#
# 2. Estimation
#
# 2.1 Construction du log de la fonction de vraisemblance
#



logvrais<-function(theta,donnees)
{
  alpha<-theta[1]
  beta<-theta[2]
  dum<-sum(-log(dgamma(donnees,alpha,beta)))
}

# 2.2 Valeurs initiales
# les valeurs initiales correspondent aux estimateurs des param?tres 
# selon la m?thode des moments
#
alpha.initial<-(moy^2)/var
beta.initial<-alpha.initial/moy
theta.initial<-c(alpha.initial,beta.initial)
alpha.initial
beta.initial

# 2.3 utilisation de la fonction nlm ou optim

res<-nlm(logvrais,theta.initial,donnees=data1,hessian=TRUE)

#res<-optim(logvrais,theta.initial,donnees=data1,hessian=TRUE)

# Avertissement: il est possible que la fonction nlm produise quelques messages de warnings
# Il ne faut pas s'en pr?occupper si la fonction produit les r?sultats

# 2.4 On s'amuse avec les estimations

theta.chap<-res$estimate
mat.fisher<-res$hessian
mat.varcovar<-solve(mat.fisher)

theta.chap
mat.fisher
mat.varcovar

alpha.chap<-theta.chap[1]
beta.chap<-theta.chap[2]
alpha.chap/beta.chap
alpha.chap/(beta.chap^2)


# 2.5 Intervalle de confiance pour les parametres

nivconf<-0.95 
quant.z<-qnorm((1+nivconf)/2)

int.alphachap<-alpha.chap+c(-1,1)*quant.z*(mat.varcovar[1,1]^0.5)
int.betachap<-beta.chap+c(-1,1)*quant.z*(mat.varcovar[2,2]^0.5)
int.alphachap
int.betachap

# 2.6 Comparaison graphique de la fonction de r?partition empirique et 
# de la fonction de r?partition param?trique avec les param?tres estim?s
plot.ecdf(data1)
points(data1,pgamma(data1,alpha.chap,beta.chap),type="l")


