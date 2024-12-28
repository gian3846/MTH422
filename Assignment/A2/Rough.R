#Q2
mu1<-1
mu2<-1.5
sigma<-2
m<-25
n<-30

X<-rnorm(m,mean=mu1,sd=sigma)
Y<-rnorm(n,mean=mu2,sd=sigma)

a<-0.01
b<-0.01
c<-1e6

A<-(m+n+1)/2+a
B<-(((sum(X-mu1)^2)/2)+((sum(Y-mu2)^2)/2)+((mu1-mu2)^2)/(4*c)+b)

func1<-function(sigma)
{
  s<-sigma^2
  return((s^(-A-1))*exp(-B/s))
}

kernel_density<-integrate(func1,0,Inf)
#Q3
m<-10
n<-15
lambda1<-2
lambda2<-2.5

X<-rpois(m,lambda1)
Y<-rpois(n,lambda2)


a<-0.01
b<-0.01
c<-1e6


theta<-rbeta(1e4,a+sum(X),a+sum(Y))
plot(density(theta))
mean(theta)

sig_level<-0.95

lower_quantile <- qbeta((1-sig_level)/2, a+sum(X), a+sum(Y))
upper_quantile <- qbeta((sig_level)/2, a+sum(X), a+sum(Y))

cat("95% HPD Credible Interval for θ:", lower_quantile, "to", upper_quantile, "\n")

#Calculate probability that theta=1/2

p<-dbeta(0.5,a+sum(X), a+sum(Y))


p>sig_level
