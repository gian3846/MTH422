Y<-c(12,90,80,5,63,15,67,22,56,33)
X<-c(50,150,63,10,63,8,56,19,63,19)
N<-X+Y
n<-length(Y)

#a

alpha<-Y+1/2
beta<-N-Y+1/2

theta.Y<-rbeta(n,alpha,beta)

# Set the confidence level
cl <- 0.95

# Calculate the lower and upper quantiles
lower_quantile <- qbeta((1 - cl) / 2, alpha, beta)
upper_quantile <- qbeta(1 - (1 - cl) / 2, alpha, beta)
CI<-cbind(lower_quantile,upper_quantile)

#b
p<-Y/N

mean.p<-mean(p)
var.p<-var(p)

M<-mean.p
V<-var.p

a<-(((M^2)*(1-M))-M*V)/V
b<-(M*(1-M)^2-(1-M)*V)/V

#c

alpha2<-a+Y
beta2<-N-Y+b

# Set the confidence level
cl <- 0.95

# Calculate the lower and upper quantiles
lower_quantile2<- qbeta((1 - cl) / 2, alpha2, beta2)
upper_quantile2 <- qbeta(1 - (1 - cl) / 2, alpha2, beta2)
