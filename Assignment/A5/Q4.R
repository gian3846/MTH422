library(rjags)

# Load the data
library(geoR)

gambia

Y<-gambia$pos
X<-gambia[,4:8]
X<-scale(X)
X<-X[1:500,]

# Fit logistic model
model <- textConnection("model{
 for(i in 1:n){
  Y[i] ~ dbern(pi[i])
  logit(pi[i]) <- beta[1] + X[i,1]*beta[2] +
                  X[i,2]*beta[3] + X[i,3]*beta[4] +
                  X[i,4]*beta[5] + X[i,5]*beta[6]
 }
 for(j in 1:6){beta[j] ~ dnorm(0,0.01)}
 
 for(i in 1:n){
  Y2[i] ~ dbern(pi[i])
  
 }
 
 D[1] <- min(Y2[])
 D[2] <- mean(Y2[])
 D[3] <- median(Y2[])
 D[4] <- sd(Y2[])
 D[5] <- max(Y2[])
 
}")


data  <- list(Y=Y,X=X,n=length(Y))
model <- jags.model(model,data = data, n.chains=2,quiet=F)
update(model, 5000, progress.bar="text")
samps<-coda.samples(model,variable.names=c("D"),
                     n.iter=20000,thin=5,progress.bar="text")
plot(samps[1][,1])
plot(samps[1][,2])
plot(samps[1][,3])
plot(samps[1][,4])
plot(samps[1][,5])
plot(samps[1][,6])


D.m1<-samps[[1]]

D0<-c(min(Y),mean(Y),sd(Y),max(Y))
Dnames<-c("MinY","MeanY","SDY","MaxY")
#Computetheteststatsforthemodels
pval1<-pval2<-rep(NA,4)
names(pval1)<-names(pval2)<-c("MinY","MeanY","SDY","MaxY")
for(j in 1:4){
  plot(density(D.m1[,j]),xlim=range(c(D0[j],D.m1[,j])),
       xlab ="D",ylab="Posteriorprobability",
       main =Dnames[j],ylim=c(0,3))
  #lines(density(D.m2[,j]),col=2)
  abline(v=D0[j],col=2)
  legend("topleft",c("Logistic Regression","Data"),
         lty=1,col=1:2,bty= "n")
  pval1[j]<-mean(D.m1[,j]>D0[j])
}

print(pval1)

