m<-rnorm(1,0,10)
m<-0
theta<-rbeta(10,Y+exp(m)*Q,N-Y+exp(m)*(1-Q))
theta.chain <- matrix(NA, 10,10)
theta.chain[1,] <- theta


theta.update <- function(n,Y,Q,N,m)
{
  theta<-rbeta(n,Y+exp(m)*Q,N-Y+exp(m)*(1-Q))
  return(theta)
}

m.update <- function()
{
  m<-rnorm(1,0,10)
  return(m)
}

MCMC <- function(n,Y,Q,N, m.init, theta.init, iters){
  # chain initiation
  m<-m.init
  theta <- theta.init
  # define chains
  m.chain <- rep(NA, iters)
  theta.chain <- matrix(NA, iters,n)
  # start MCMC
  for(i in 1:iters){
    m <- m.update()
    theta <- theta.update(n,Y,Q,N, m)
    m.chain[i] <- m
    theta.chain[i,] <- theta
  }
  # return chains
  out <- list(m.chain = m.chain,
              theta.chain = theta.chain)
  return(out)}

n<-10
Q<-c(0.845, 0.847, 0.880, 0.674, 0.909, 0.898, 0.770, 0.801, 0.802, 0.875)
Y<-c(64, 72, 55, 27, 75, 24, 28, 66, 40, 13)
X<-c(75, 95, 63, 39, 83, 26, 41, 82, 54, 16)
N<-Y+X

MCMC.out <- MCMC(n=n,Y = Y,
                 Q=Q,
                 N=N,
                 m.init = 0,
                 theta.init = 1-var(Y)/mean(Y),
                 iters = 30000)
plot(MCMC.out$m.chain, xlab = "Iteration", ylab = "m", type = "l")
plot(MCMC.out$theta.chain[,1], xlab = "Iteration", ylab = "theta1", type = "l")

confidence_interval<-matrix(NA,2,11)

for(i in 1:10)
{
  confidence_interval[,i] <- quantile(MCMC.out$theta.chain[,i], c(0.025, 0.975))
}
confidence_interval[,11]<-quantile(MCMC.out$m.chain, c(0.025, 0.975))



library(rjags)

n<-10
Q<-c(0.845, 0.847, 0.880, 0.674, 0.909, 0.898, 0.770, 0.801, 0.802, 0.875)
Y<-c(64, 72, 55, 27, 75, 24, 28, 66, 40, 13)
X<-c(75, 95, 63, 39, 83, 26, 41, 82, 54, 16)
N<-Y+X

data <- list(Y = Y,Q=Q,N=N, n = n)

model_string <- textConnection("model{

   # Likelihood
   for(i in 1:n){
     Y[i] ~ dbin(theta[i],N[i])
   }

   # Priors
   for(i in 1:n){
     theta[i] ~ dbeta(exp(m)*Q[i],exp(m)*(1-Q[i]))
   }
   m ~ dnorm(0,10)

 }")

model <- jags.model(model_string, data = data,
                    n.chains = 2, quiet = TRUE)

update(model, 10000, progress.bar = "none")

samples <- coda.samples(model, variable.names = "theta", 
                        n.iter = 30000, thin = 2, progress.bar = "none")
samples2 <- coda.samples(model, variable.names = "m", 
                         n.iter = 30000, thin = 2, progress.bar = "none")

summary(samples)[2]

plot(samples[,1])


summary(samples)[2]
