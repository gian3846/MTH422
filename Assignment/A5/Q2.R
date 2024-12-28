#Question 2
library (MASS)
data("galaxies")
Y = galaxies
hist(Y, breaks = 25)
n = length(Y)

data= list(Y = Y, N=n, K = 3, alpha = rep(1, 3))

library(rjags)
model_string = textConnection("model{

# Likelihood
for (i in 1:N) {
Y[i] ~ dnorm(mu[Z[i]], tau[Z[i]])

Z[i] ~ dcat(theta[])
}

for (j in 1:K) {
mu[j]~ dnorm(0, 1e-8) 
tau[j]~ dgamma (0.01, 0.01)
}
theta[1:K] ~ ddirch(alpha[])
}")



params = c('mu', 'tau', 'theta')

model =  jags.model(model_string, data =data, quiet = TRUE)
update(model, 2e4)

samples = coda.samples (model, variable.names = params, n.iter = 1e4) 
plot(samples[[1]][,1])
plot(samples[[1]][,2])
plot(samples[[1]][,3])

y = seq(5e3, 4e4, 100)

mu.post = samples[[1]][,1:3]

tau.post = samples[[1]][,4:6]

theta.post = samples[[1]][,7:9]

S = 1e4

post_density = matrix (NA, nrow = S, ncol = 351)

for(i in 1:S) {
  mu = as.numeric(mu.post[i,])
  sigma = as.numeric(1/ sqrt(tau.post[i,]))
  theta = as.numeric(theta.post[i, ])
  
  
  
  mix_gauss = function(x) {
    
    theta [1] * dnorm(x, mean = mu[1], sd =sigma[1])
    +
      theta [2] * dnorm(x, mean = mu[2], sd = sigma[2]) + 
      theta[3] * dnorm(x, mean = mu[3], sd = sigma[3]) }
  
  post_density[i, ] <- sapply(y, mix_gauss)
  
  print(paste0('Done:', i))
}

post_median <- apply(post_density, 2, median)
post_2.5.quantile <- apply(post_density, 2, quantile, probs = 0.025)
post_97.5.quantile <- apply(post_density, 2, quantile, probs =  0.975)

par(mfrow = c(1,1))


library(ggplot2)

ggplot()+
  geom_histogram(aes(x = Y, y =after_stat(density)), col = 'black')+
  geom_line(aes(y, post_median, col ='Median'), size = 1)+
  geom_line(aes(y, post_2.5.quantile, col='Quantile: 0.025'), linetype = 'dashed',size = 1)+
  geom_line(aes(y, post_97.5.quantile, col ='Quantile: 0.975'), linetype = 'dashed', size = 1)+
  labs(col='Index')

