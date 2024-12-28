#Q1
n<-6
Y<-c(64, 13, 33, 18, 30, 20)
t <- 1:n

alpha<-dnorm(0,10000)
beta<-dnorm(0,10000)
lambda<-exp(alpha+beta*t)
data<-list(Y=Y,n=n)

library(rjags)


model_string<-textConnection("model{

 #Likelihood
 for(t in 1:n){
 Y[t]~dpois(lambda[t])
 }
 
 #Priors
 alpha~dnorm(0,10000)
 beta~dnorm(0,10000)
 for(t in 1:n){
 lambda[t]=exp(alpha+beta*t)
 }
}")

#inits <- list(alpha=alpha, beta=beta)
model <- jags.model(model_string, data = data, quiet = TRUE, n.chains = 2) 

update(model, 2000, progress.bar = "none")

params <-c("alpha","beta")
samples <- coda.samples(model,
                        variable.names = params,
                        n.iter = 5000, progress.bar = "none")

plot(samples)
summary(samples)


library(coda)

# Low autocorrelation indicates convergence
autocorr.plot(samples)


autocorr(samples, lag = 1)

# high ESS indicates convergence
effectiveSize(samples)

# R less than 1.1 indicates convergence
gelman.diag(samples)

# /z/ less than 2 indicates convergence
geweke.diag(samples[[1]])

t<-1:n

a<-summary(samples)$statistics[1,1]
b<-summary(samples)$statistics[2,1]

lambda2<-exp(a+b*t)

plot(lambda2)

n<-6
Y<-c(64, 13, 33, 18, 30, 20)
t <- 1:n

beta<-c(2,0)

iter<-25000
par<-matrix(0,iter,2)

cand<-c(0.2,0.05)

posterior<-function(Y,t,beta,sd=10){
  l<-exp(beta[1]+t*beta[2])
  lambda<-prod(dpois(Y,l))
  p<-prod(dnorm(beta,0,sd))
  return(lambda*p)}
#can<-rnorm(1,beta[1],cand[1])
#posterior(Y,t,can)

for(i in 1:iter)
{
  for(j in 1:2)
  {
    can<-beta
    can[j]<-rnorm(1,beta[j],cand[j])
    R<-posterior(Y,t,can)/posterior(Y,t,beta)
    
    if(runif(1)<R)
    {
      beta<-can
    }
  }
  
  par[i,]<-beta
}

plot(par[,1],type="l",ylab=expression(aplha),xlab="Iteration")
plot(par[,2],type="l",ylab=expression(beta),xlab="Iteration")

acc_rate <- colMeans(par[-1,]!=par[-iter,])
acc_rate


#Q2
set.seed(27695)
theta_true <- 4
n          <- 30
B          <- rbinom(n,1,0.5)
Y          <- rnorm(n,B*theta_true,1)
hist(Y,breaks=25)

#b

dens<-function(y)
{
  return(exp(-(y^2)/2)/sqrt(2*pi))
}
func<-function(y,theta)
{
  return(dens(y)+dens(y-theta))
}

y<-seq(-3,10,0.1)

plot(func(y,2),type="l",ylab="PDF")
lines(func(y,4),col=2)
lines(func(y,6),col=3)
legend("topright",c("theta=2","theta=4","theta=6"),lty=1,col=1:3,bty="n")

#c
Y1<-Y[1]

library(stats4)
nlp <- function(theta,Y){
  like<- 0.5*dnorm(Y,0,1) + 0.5*dnorm(Y,theta,1)
  prior<- dnorm(theta,0,10)
  neg_log_post <--sum(log(like))- log(prior)
  return(neg_log_post)}

map_est <- mle(nlp,start=list(theta=1), fixed=list(Y=Y))
sd<- sqrt(vcov(map_est))
map_est; sd


nlp <- function(theta,Y){
  like<- 0.5*dnorm(Y,0,1) + 0.5*dnorm(Y,theta,1)
  prior<- dnorm(theta,0,10)
  neg_log_post <--sum(log(like))- log(prior)
  return(neg_log_post)}
#map_est <- mle(nlp,start=list(theta=1), fixed=list(Y=Y))
#sd<- sqrt(vcov(map_est))
out <- suppressWarnings(optim(par = 1, nlp, Y = Y, hessian = T))

map_est <- out$par
sd <- sqrt(1 /c(out$hessian))
map_est; sd

#d

posterior <- function(theta,Y,k){
  post <- dnorm(theta,0,sqrt(10^k))
  for(i in 1:length(Y)){
    post<-post*(0.5*dnorm(Y[i],0,1)+
                  0.5*dnorm(Y[i],theta,1))
  }
  return(post/sum(post))}

theta <- seq(2,6,0.01)
plot(theta,posterior(theta,Y,0),col=2,type="l",ylab="Posterior")
lines(theta,posterior(theta,Y,0),col=2)
lines(theta,posterior(theta,Y,1),col=3)
lines(theta,posterior(theta,Y,2),col=4)
lines(theta,posterior(theta,Y,3),col=5)
legend("topright",c("MAP","k=0","k=1","k=2","k=3"),
       col=1:5,lty=1,bty="n")


#e

set.seed(27695)
theta_true <- 4
n          <- 30
B          <- rbinom(n,1,0.5)
Y          <- rnorm(n,B*theta_true,1)
hist(Y,breaks=25)


data<-list(Y=Y,n=n)

library(rjags)


model_string<-textConnection("model{

 #Likelihood
 for(i in 1:n){
 Y[i]~dnorm(B[i]*theta,1)
 }
 
 #Priors
 for(i in 1:n){
 B[i]~dbern(0.5)
 }
 theta~dnorm(0,10^2)
}")

#inits <- list(alpha=alpha, beta=beta)
model <- jags.model(model_string, data = data, quiet = TRUE, n.chains = 2) 

update(model, 2000, progress.bar = "none")

params <-c("theta")
samples <- coda.samples(model,
                        variable.names = params,
                        n.iter = 5000, progress.bar = "none")

plot(samples)
summary(samples)



#Q3
Y<-10

data<-list(Y=Y)


library(rjags)


model_string<-textConnection("model{

 #Likelihood
 Y~dbin(p,n)
 
 #Priors
 lambda<-10
  a<-10
  b<-10
 n~dpois(lambda)
 p~dbeta(a,b)
 np<-n*p
}")

#inits <- list(n=20, p=0.5)
model <- jags.model(model_string, data = data, quiet = TRUE, n.chains = 2) 

update(model, 2000, progress.bar = "none")

params <-c("n","p","np")

samples <- coda.samples(model,
                        variable.names = params,
                        n.iter = 5000, progress.bar = "none")
samples_n <- coda.samples(model,
                          variable.names = "n",
                          n.iter = 5000, progress.bar = "none")
samples_p <- coda.samples(model,
                          variable.names = "p",
                          n.iter = 5000, progress.bar = "none")
samples_np <- coda.samples(model,
                           variable.names = "np",
                           n.iter = 5000, progress.bar = "none")

plot(samples_n)
plot(samples_p)
plot(samples_np)
summary(samples)

library(coda)

# Low autocorrelation indicates convergence
autocorr.plot(samples)


autocorr(samples, lag = 1)

# high ESS indicates convergence
effectiveSize(samples)

# R less than 1.1 indicates convergence
gelman.diag(samples)

# /z/ less than 2 indicates convergence
geweke.diag(samples[[1]])


#Q4
n<-12
n1<-6
n2<-6
Y1<-c(2.0,-3.1,-1.0,0.2,0.3,0.4)
Y2<-c(-3.5,-1.6,-4.6,-0.9,-5.1,0.1)

Y1_m<-mean(Y1)
Y2_m<-mean(Y2)

sigma1<-mean((Y1_m-Y1)^2)
sigma2<-mean((Y2_m-Y2)^2)
sig_rt<-sqrt(sigma1/2+sigma2/2)

p_mean<-Y2_m-Y1_m
sigma_hat<-(sig_rt)*sqrt(1/n1+1/n2)


cred_set <- p_mean+sigma_hat*qt(c(0.025,0.975),df=n1+n2)


p_mean;sigma_hat;cred_set


library(rjags)
data <- list(n=6,Y1=Y1,Y2=Y2)

model_string <- textConnection("model{

   # Likelihood
   for(i in 1:n){
     Y1[i] ~ dnorm(mu,tau)
     Y2[i] ~ dnorm(mu+delta,tau)
   }

   # Priors
   mu    ~  dnorm(0, 0.0001)
   delta ~  dnorm(0, 0.0001)
   tau   ~  dgamma(0.1, 0.1)
   sigma <- 1/sqrt(tau)
 }")

model <- jags.model(model_string,data = data, n.chains=2,quiet=TRUE)
update(model, 10000, progress.bar="none")
params  <- c("delta")
samples <- coda.samples(model, 
                        variable.names=params, 
                        n.iter=50000, progress.bar="none")
plot(samples)
summary(samples)

mu1 <- Y1_m + sqrt(sigma1/n1)*rt(1000000,df=n1)
mu2 <- Y2_m + sqrt(sigma2/n2)*rt(1000000,df=n2)
delta <- mu2-mu1

delta <- mu2 - mu1

plot(density(delta), main = "Posterior distribution of the difference in means")
quantile(delta, c(0.025, 0.975)) # 95% credible set

#Q5

library(MASS)
data(Boston)
n = nrow(Boston)
p = ncol(Boston)
X = cbind(1, Boston[, -p])

#part a

library(rjags)

data = list(Y = Boston$medv, n = n, p = p, X = X)

model_string_1 = "
model{

  #Likelihood
  for(i in 1:n){
    Y[i] ~ dnorm(inprod(X[i, ], beta[1:p]), tau)
  }

  #Priors
  beta[1] ~ dnorm(0,1e-4)
  for(i in 2:p){
    beta[i] ~ dnorm(0, 1e-4)
  }
  tau ~ dgamma(0.1, 0.1)
  sigma2 = 1 / tau
}"

model1 = jags.model(textConnection(model_string_1), data = data, quiet = TRUE)

#burn-in
update(model1, 5e3, progress.bar = 'none')

params = c('beta')
samples1 = coda.samples(model1,
                        variable.names = params,
                        n.iter = 2e4, progress.bar = 'none')
summary(samples1)
library(coda)

# Trace plot for beta[1] and beta[2]
par(mfrow=c(2,1))
plot(samples1[[1]][, "beta[1]"], type="l", main="Trace Plot for beta[1]")
plot(samples1[[1]][, "beta[2]"], type="l", main="Trace Plot for beta[2]")

# Density plot for beta[1] and beta[2]
par(mfrow=c(2,1))
densplot(samples1[[1]][, "beta[1]"], main="Density Plot for beta[1]")
densplot(samples1[[1]][, "beta[2]"], main="Density Plot for beta[2]")

# b
fit_least_sq = lm(medv~., data = Boston)
summary(fit_least_sq)

#From table both estimates of regression tables are similar.
#part c
library(rjags)

data = list(Y = Boston$medv, n = n, p = p, X = X)

model_string_2 = textConnection("model{

    #Likelihood
    for(i in 1:n){
    Y[i] ~ dnorm(inprod(X[i, ], beta[]), tau)
    }

    #Priors
    beta[1] ~ ddexp(0,1e-4)
    for(i in 2:p){
    beta[i] ~ ddexp(0, 1e-4)
    }
    tau ~ dgamma(0.1, 0.1)
    sigma2 = 1 / tau
}")

model2 = jags.model(model_string_2, data = data, quiet = TRUE)

#burn-in
update(model2, 5e3, progress.bar = 'none')

params = c('beta')
samples2 = coda.samples(model2,
                        variable.names = params,
                        n.iter = 2e4, progress.bar = 'none')
summary(samples2)
#estimate values are more or less similar.