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
