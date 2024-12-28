set.seed(100)

Y1<-563
Y2<-10

N1<-2820
N2<-27

Y<-c(Y1,Y2)
N<-c(N1,N2)

library(rjags)

#M1
model_string1<-"model{
 #Likelihood
 for(i in 1:length(N))
 {
  Y[i]~dpois(N[i]*lambda[i])
 }
 
 #Priors
 c<-1
 lambda1~dunif(0,c)
 lambda2~dunif(0,c)
 
 lambda<-c(lambda1, lambda2)
 }"

#M2
model_string2<-"model{
 #Likelihood
 for(i in 1:length(N))
 {
  Y[i]~dpois(N[i]*lambda0)
 }
 
 #Priors
 c<-1
 lambda0~dunif(0,c)
 lambda<-lambda0
 }"

data<-list(Y=Y,N=N)
model1<-jags.model(textConnection(model_string1),
                   data=data,n.chains=1,quiet=TRUE)
update(model1,10000,progress.bar ="none")
sample1<-coda.samples(model1,variable.names=c("lambda"),
                     n.iter=20000,thin=5,progress.bar="none")
lambda.m1<-sample1[[1]]

model2<-jags.model(textConnection(model_string1),
                   data=data,n.chains=1,quiet=TRUE)
update(model1,10000,progress.bar ="none")
sample2<-coda.samples(model2,variable.names=c("lambda"),
                      n.iter=20000,thin=5,progress.bar="none")

lambda.m2<-sample2[[1]]

#after thinning,4Kpost-burn-insamplesleft
loglike.m1<-sapply(1:4000,function(iter){
  sum(dpois(Y1,lambda=lambda.m1[iter,1],log=T)+dpois(Y2,lambda.m1[iter,2],log=T))})
loglike.m2<-sapply(1:4000,function(iter){
  sum(dpois(Y1,lambda=lambda.m2[iter,1],log=T)+dpois(Y2,lambda.m2[iter,1],log=T))})

deviance.m1<--2*loglike.m1
deviance.m2<--2*loglike.m2
#DIC
Dbar.m1<-mean(deviance.m1)
Dbar.m2<-mean(deviance.m2)

D.thetahat.m1<-sum(dpois(Y1,lambda=mean(lambda.m1[,1]),log=T)+dpois(Y2,lambda=mean(lambda.m1[,2]),log=T))
                        
D.thetahat.m2<-sum(dpois(Y1,lambda=mean(lambda.m2[,1]),log=T)+dpois(Y2,lambda=mean(lambda.m2[,1]),log=T))

pD.m1<-Dbar.m1-D.thetahat.m1
pD.m2<-Dbar.m2-D.thetahat.m2
DIC.m1<-pD.m1+Dbar.m1
DIC.m2<-pD.m2+Dbar.m2
DIC.m1
DIC.m2


#WAIC
loglike1.m1<-sapply(1:4000,function(iter){
  dpois(Y1,lambda=lambda.m1[iter,1],log=T)})
loglike2.m1<-sapply(1:4000,function(iter){
  dpois(Y2,lambda=lambda.m1[iter,2],log=T)})
loglike1.m2<-sapply(1:4000,function(iter){
  dpois(Y1,lambda=lambda.m2[iter,1],log=T)})
loglike2.m2<-sapply(1:4000,function(iter){
  +dpois(Y2,lambda.m2[iter,1],log=T)})

posmeans.m1<-c(mean(loglike1.m1),mean(loglike2.m1))
posmeans.m2<-c(mean(loglike1.m2),mean(loglike2.m2))
posvars.m1<-c(var(loglike1.m1),var(loglike2.m1))
posvars.m2<-c(var(loglike1.m2),var(loglike2.m2))
pW.m1<-sum(posvars.m1)
pW.m2<-sum(posvars.m2)
sum.means.m1<-sum(posmeans.m1)
sum.means.m2<-sum(posmeans.m2)
WAIC.m1<--2*sum.means.m1+2 * pW.m1
WAIC.m2<--2*sum.means.m2+2 * pW.m2
WAIC.m1
WAIC.m2

OUT<-rbind(c(DIC.m1,WAIC.m1), c(DIC.m2,WAIC.m2))
OUT<-round(OUT,2)
rownames(OUT)<-c("Model1","Model2")
colnames(OUT)<-c("DIC","WAIC")
library(kableExtra)
kable(OUT)
