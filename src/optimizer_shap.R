p=4 #dimension
A=matrix(rnorm(p^2),nrow=p,ncol=p)
Sigma=t(A)%*%A # it means t(A)%*%A
C=chol(Sigma)
n=2000 #sample size
Z=matrix(rnorm(p*n),nrow=n,ncol=p)
X=Z%*%C # X is a gaussian vector with zero mean and covariance Sigma


optim(par = rep(1/ncol(X),ncol(X)), fn = importance_diff, X=X, Ntot= 100)



optim(par = rep(1/ncol(X),ncol(X)), fn = importance_diff, X=X, Ntot= 250, method = 'L-BFGS-B')
system("say -v Victoria your code is done")
