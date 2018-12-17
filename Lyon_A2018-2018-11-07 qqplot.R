# Lyon A2018
#
# Qqplot

set.seed(2018)

nsim<-1432

#x<-sort(rexp(nsim,1/10))
x<-sort(rlnorm(nsim,2,0.9))
u<-(1:nsim)/(nsim+1)
  
plot(x,qexp(u))

#plot(x,qlnorm(u,0,1))
