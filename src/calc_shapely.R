library(sensitivity)
Xall = function(n) X[sample(nrow(X),size=n,replace=F),] #should this be w or w.o replacement?
Xset <- function(n, Sj, Sjc, xjc) matrix(runif(n*length(Sj),-pi,pi),nc=length(Sj))
set.seed(139)
Xset(10,c(1,2,4,5,67,7),NULL,NULL)


#### the subset shit????
p=20 #dimension
A=matrix(rnorm(p^2,5),nrow=p,ncol=p)
ASigma=t(A)%*%A # it means t(A)%*%A
C=chol(Sigma)
n=2000 #sample size
Z=matrix(rnorm(p*n),nrow=n,ncol=p)
X=Z%*%C # X is a gaussian vector with zero mean and covariance Sigma
Y=agg(A,rep(.25, p), 'geom')
Shap=shapleySubsetMc(X=A,Y=Y,Ntot=5000)
plot(Shap)
system("say -v Victoria hey dumbass")
intToBits(2)


