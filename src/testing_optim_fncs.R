p=4 #dimension
A=matrix(rnorm(p^2),nrow=p,ncol=p)
Sigma=t(A)%*%A # it means t(A)%*%A
C=chol(Sigma)
n=2000 #sample size
Z=matrix(rnorm(p*n),nrow=n,ncol=p)
X=Z%*%C # X is a gaussian vector with zero mean and covariance Sigma

## testing optim
#
system.time(
  {
    optim(par = rep(1/ncol(X),ncol(X)), fn = importance_diff, X=X, Ntot= 250, method = 'L-BFGS-B')
  }
)
# takes like 329.719s

## testing optimparallel
cl <- makeCluster(detectCores()) # set the number of processor cores
setDefaultCluster(cl=cl)

clusterExport(cl = cl, varlist = list('X','agg','importance_diff', 'shapleySubsetMc'), envir = environment())
system.time({
  rep2 = optimParallel(par = rep(1/ncol(X),ncol(X)), fn = importance_diff, data=X, Ntot= 500)
})

setDefaultCluster(cl=NULL); stopCluster(cl)
# takes 72.128s w/ 40 clusters, ntot =250, 750s when ntot =500, not good results.


## testing hydropso
system.time({
  hydroPSO( fn = importance_diff, X=X, Ntot= 250, lower = rep(0,4), upper = rep(3, 4),control=list(write2disk=FALSE))
})


## testing pso, hasn't converged after 660 iterations
system.time({
  res4 = psoptim(par = rep(1/ncol(X),ncol(X)), fn = importance_diff, lower = rep(0,ncol(X)), data=X, Ntot= 250,
                 control = list(maxit = 100, maxit.stagnate=20))
})

## testins DEoptim, takes forever, but gives good results
system.time(
  {
    res5 = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(3, 4), data=X, Ntot= 250)
  }
)

# in parallel
cl <- makeCluster(39) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('X', 'agg','importance_diff', 'shapleySubsetMc'), envir = environment())
system.time(
  {
    res6 = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(3, 4),
                   control = list(cluster = cl, itermax = 10),
                   data=X, Ntot= 250)
  }
)
setDefaultCluster(cl=NULL); stopCluster(cl)
