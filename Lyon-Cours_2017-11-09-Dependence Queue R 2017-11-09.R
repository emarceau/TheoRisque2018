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

# Pr(U1)

coeffLclayton<-function(kap,alpha)
{
  coeff<-clayton(kap,kap,alpha)/kap
  return(coeff)
}

coeffUclayton<-function(kap,alpha)
{
  coeff<-(1-2*kap+clayton(kap,kap,alpha))/(1-kap)
  return(coeff)
}

coeffLclayton(0.000001,5)
coeffUclayton(0.999999,5)


