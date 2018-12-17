# Act-2001
# H2016
# Date: 2016-04-06
# On illustre la m?thode du maximum de vraisemblance 
# Application avec des donn?es continues
# Application avec la loi gamma
# 
# On utilise une fonction d'optimisation en R
# Pourquoi ? Pour estimer les param?tres du log de la fonction de vraisemblance
# 
# On peut utiliser deux fonctions R: nlm ou optim
#
# Pour l'illustration, on utilise des donn?es simul?es de la loi gamma
#
# 1. Simulation des donne?s (loi gamma)
# Param?tre de la loi gamma
alpha<-2
beta<-0.1
EX<-alpha/beta
VarX<-EX/beta
EX
VarX
# Nombre d'observations (e.g. sinistres) 
#(suggestion: varier le nombre d'observations 
# et voir l'impact sur les intervalles de confiance
# et la comparaison graphique)
nn<-841
# Initialisation
set.seed(20160406)
# Donn?es simul?es
data1<-sort(rgamma(nn,alpha,beta))
#
# On calcule l'esp?rance et la variance empiriques
#
moy<-mean(data1)
var<-var(data1)
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
res

res<-optim(logvrais,theta.initial,donnees=data1,hessian=TRUE)
res

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

# 88. Note pour les amateurs de R
# voir fonction de Christophe Dutang pour une fonction plus g?n?rale
# fonction = mledist
