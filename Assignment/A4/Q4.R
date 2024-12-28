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
