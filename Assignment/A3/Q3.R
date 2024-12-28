sigmaSq.update <- function(n,Y, a, b)
{
  sigmaSq.inv<-rgamma(n,a+1/2,b+(Y^2)/2)
  sigmaSq<-sigmaSq.inv
  return(sigmaSq)
}

b.update <- function(sigmaSq)
{
  b<-rgamma(1,1,b+sum(1/sigmaSq))
  return(b)
}

MCMC <- function(n,Y, b.init, sigmaSq.init, a, iters){
  # chain initiation
  b<-b.init
  sigmaSq <- sigmaSq.init
  # define chains
  b.chain <- rep(NA, iters)
  sigmaSq.chain <- matrix(NA, iters,n)
  # start MCMC
  for(i in 1:iters){
    b <- b.update(sigmaSq)
    sigmaSq <- sigmaSq.update(n,Y, a, b)
    b.chain[i] <- b
    sigmaSq.chain[i,] <- sigmaSq
  }
  # return chains
  out <- list(b.chain = b.chain,
              sigmaSq.chain = sigmaSq.chain)
  return(out)}

N<-10
Y<-seq(1:10)
a<-10

MCMC.out <- MCMC(n=N,Y = Y,
                 b.init = 0.1,
                 sigmaSq.init = var(Y),
                 a = a,
                 iters = 30000)
plot(MCMC.out$b.chain, xlab = "Iteration", ylab = "b", type = "l")
plot(MCMC.out$sigmaSq.chain[,1], xlab = "Iteration", ylab = "sigmaSq", type = "l")

N<-10
Y<-seq(1:10)
a<-1

MCMC.out <- MCMC(n=N,Y = Y,
                 b.init = 0.1,
                 sigmaSq.init = var(Y),
                 a = a,
                 iters = 30000)
plot(MCMC.out$b.chain, xlab = "Iteration", ylab = "b", type = "l")
plot(MCMC.out$sigmaSq.chain, xlab = "Iteration", ylab = "sigmaSq", type = "l")


library(rjags)

n<-10
Y<-seq(1:10)
a<-10

data <- list(Y = Y, n = n)

model_string <- textConnection("model{

   # Likelihood
   for(i in 1:n){
     Y[i] ~ dnorm(0,sigma_sq[i])
   }

   # Priors
   for(i in 1:n){
     sigma_sq[i] ~ dgamma(10,b)
   }
   b ~ dgamma(1,1)

 }")

model <- jags.model(model_string, data = data,
                    n.chains = 2, quiet = TRUE)

update(model, 10000, progress.bar = "none")

samples <- coda.samples(model, variable.names = "sigma_sq", 
                        n.iter = 30000, thin = 2, progress.bar = "none")

summary(samples)

plot(samples[,1])
