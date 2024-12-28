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


