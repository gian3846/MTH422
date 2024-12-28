mu1<-1
mu2<-1.5
sigma<-2
m<-25
n<-30

X<-rnorm(m,mean=mu1,sd=sigma)
Y<-rnorm(n,mean=mu2,sd=sigma)

a<-0.01
b<-0.01

s2.i<-rgamma(1,a,b)

s2<-1/s2.i

A<-m+n+1
B<-(((sum(X-mu1)^2)/2)+((sum(X-mu1)^2)/2))

func1<-function(sigma)
{
  s<-sigma^2
  return((s^(-A-1))*exp(-B/s))
}

kernel_density<-integrate(func1,0,Inf)
