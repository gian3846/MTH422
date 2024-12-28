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


