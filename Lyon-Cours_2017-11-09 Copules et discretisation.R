# Lyon A2017
#
#
clayton<-function(u1,u2,alpha)
{
  a1<--alpha
  a2<-1/a1
  C<-(u1^a1+u2^a1-1)^a2
  return(C)
}
#
#
ppareto<-function(x,aa,ll)
{
  FF<-1-((ll/(ll+x))^aa)
  return(FF)
}
# Clayton
alpC<-5

# Pareto

lam<-30
alp<-4
EX<-lam/(alp-1)
nmax<-5000
vk<-1:nmax
ppareto(nmax,aa=alp,ll=lam)
vF<-ppareto(vk,aa=alp,ll=lam)
F12<-matrix(0,nmax+1,nmax+1)
for (k in 0:nmax)
{
  F12[k+1,]<-clayton(ppareto(k,alp,lam),c(0,vF),alpC)
}
dim(F12)
F12[1001,5001]
f12<-matrix(0,nmax+1,nmax+1)
f12[1,1]<-F12[1,1]
f12[2:5001,1]<-F12[2:5001,1]-F12[1:5000,1]
f12[1,2:5001]<-F12[1,2:5001]-F12[1,1:5000]
f12[2:5001,2:5001]<-F12[2:5001,2:5001]-F12[1:5000,2:5001]-F12[2:5001,1:5000]+F12[1:5000,1:5000]
sum(f12)
fS<-rep(0,2*nmax+1)
fS<-rep(0,2*nmax+1)
#
for (k in 0:nmax)
{
  for (j in 0:k)
  {
    fS[k+1]<-fS[k+1]+f12[j+1,k-j+1]    
  }
}

for (k in nmax2:(2*nmax))
{
  for (j in (k-nmax):nmax)
  {
    fS[k+1]<-fS[k+1]+f12[j+1,k-j+1]    
  }
}



# -- Exemple pour se faire le main
# Lyon A2017
#
#
clayton<-function(u1,u2,alpha)
{
  a1<--alpha
  a2<-1/a1
  C<-(u1^a1+u2^a1-1)^a2
  return(C)
}
#
#
# Clayton
alpC<-5
nmax<-10
vF<-(1:nmax)/nmax
F12<-matrix(0,nmax+1,nmax+1)
for (k in 0:nmax)
{
  F12[k+1,]<-clayton(k/nmax,c(0,vF),alpC)
}
#
round(F12,3)

#
dim(F12)
#
nmax2<-nmax+1
f12<-matrix(0,nmax+1,nmax+1)
f12[1,1]<-F12[1,1]
f12[2:nmax2,1]<-F12[2:nmax2,1]-F12[1:nmax,1]
f12[1,2:nmax2]<-F12[1,2:nmax2]-F12[1,1:nmax]
f12[2:nmax2,2:nmax2]<-F12[2:nmax2,2:nmax2]-F12[1:nmax,2:nmax2]-F12[2:nmax2,1:nmax]+F12[1:nmax,1:nmax]
sum(f12)
round(f12,3)

fS<-rep(0,2*nmax+1)
#
for (k in 0:nmax)
{
  for (j in 0:k)
  {
    fS[k+1]<-fS[k+1]+f12[j+1,k-j+1]    
}
}

for (k in nmax2:(2*nmax))
{
  for (j in (k-nmax):nmax)
  {
    fS[k+1]<-fS[k+1]+f12[j+1,k-j+1]    
  }
}

cbind(0:(2*nmax),fS)
round(f12,4)
sum(fS)
vk<-0:(nmax*2)

EX<-sum((1:10)*(1/10))
ES<-EX+EX
ESv<-sum(vk*fS)
ESv
ES


