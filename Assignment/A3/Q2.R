n<-3
Y<-c(12,10,22)
sigma<-diag(3,3,10)
library(mvtnorm)


density<-function(x)
{
  d<-dmvnorm(x, mean = Y, sigma = SIGMA)
  return(d)
}

#a

integrate(density,-1e6,1e6)


exp(-(sum((Y-x)/sigma)^2)/2)

prod(dnorm(x,Y,sigma))
