m<-10
n<-15
lambda1<-2
lambda2<-2

X<-rpois(m,lambda1)
Y<-rpois(n,lambda2)

a<-1
b<-1

l1<-rgamma(a+sum(X),b+m)
l2<-rgamma(a+sum(Y),b+n)

plot(density(l1))
plot(density(l2))

mean(l1)
mean(l2)
var(l1)
var(l2)

theta<-l1/(l1+l2)

mean(theta)
var(theta)

plot(density(theta))
