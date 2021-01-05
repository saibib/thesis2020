
## an example of the command call: The Sobol model with heterogeneous inputs
Ni = 10
weight = NULL
noise = FALSE
Ntot=20000

p=8 #dimension
A=matrix(rnorm(p^2),nrow=p,ncol=p)
Sigma=t(A)%*%A
C=chol(Sigma)
n=5000 #sample size
Z=matrix(rnorm(p*n),nrow=n,ncol=p)
X=Z
#we create discrete and categorical variables
X[,1]=round(X[,1]/2)
X[,2]=X[,2]>2
X[,4]=-2*round(X[,4])+4
X[(X[,6]>0 &X[,6]<1),6]=1
cat=c(1,2) # we choose to take X1 and X2 as categorical variables (with the discrete distance)
discrete=c(4,6) # we indicate that X4 and X6 can take several times the same value
Y=sobol.fun(X)

Shap=shapleySubsetMc(X=X,Y=Y, cat=cat, discrete=discrete,Ntot=20000, Ni=10)
plot(Shap)


{
  dimen = dim(X)
  N = dimen[1]
  p = dimen[2]
  if (length(Y) != N) {
    stop("the inputs and the output do not have the same number of individuals")
  }
  if (length(weight) != 0 & length(weight) != length(cat)) {
    stop("'weight' must have the same length as 'cat'")
  }
  if (mode(X) != "numeric") {
    if (mode(X) == "list") {
      X = as.matrix(sapply(X, as.numeric))
    }
    else {
      stop("'X' is not a matrix or a dataframe")
    }
  }
  if (is.null(colnames(X))) {
    names = paste(rep("X", p), 1:p, sep = "")
  }
  else {
    names = colnames(X)
  }
  discr = union(cat, discrete) # takes union of indices of categorical and discrete columns
  EY <- mean(Y)
  VarY = var(Y)
  Varu = rep(0, 2^p) # empty vector of Variances 2^p long, basically the powerset of the indicators
  # matrix of 2^p-2 in binary, each digit is a cell
  U = t(sapply(1:(2^p - 2), function(x) {
    as.numeric(intToBits(x)[1:p])
  }))

  if (length(weight) == 0) {
    weight = rep(1, length(cat))
  }
  # distance function, euclidean for numerical points,
  norm_vec <- function(x1, x2, realvar, catvar) {
    dist_prod = (sum((x1[realvar] - x2[realvar])^2)) + sum((weight[is.element(cat,
                                                                              catvar)]) * (x1[catvar] != x2[catvar]))
    return(dist_prod)
  }

  VU = function(u) {
    cardu = sum(u)
    if (length(Ntot) == 0) {
      Nu = N
    }
    else {
      Nu = round(Ntot/choose(p, cardu)/(p - 1))
    }
    if (Nu == 0) {
      warning("Ntot is too small and some conditional elements have been estimated to 0")
      return(0)
    }
    else {
      Num = sample(1:N, Nu, replace = FALSE)
      if (Nu == 1) {
        X_unif = matrix(X[Num, ], nrow = 1)
      }
      else {
        X_unif = X[Num, ]
      }
      Su = which(u == 1)
      Smu = which(u == 0)
      cat_Smu = intersect(Smu, cat)
      real_Smu = setdiff(Smu, cat)
      V = NULL
      TF = (length(setdiff(Smu, discr)) > 0)
      if (TF) {
        for (n in 1:Nu) {
          xx = X_unif[n, ]
          dist = apply(X, 1, function(x) norm_vec(x,
                                                  xx, real_Smu, cat_Smu))
          num_cond = order(dist, decreasing = F)[1:Ni]
          Vn = var(Y[num_cond])
          V = c(V, Vn)
        }
      }
      else {
        for (n in 1:Nu) {
          xx = X_unif[n, ]
          samp = sample(1:N)
          XX = X[samp, ]
          YY = Y[samp]
          dist = apply(XX, 1, function(x) norm_vec(x,
                                                   xx, real_Smu, cat_Smu))
          num_cond = order(dist, decreasing = F)[1:Ni]
          Vn = var(YY[num_cond])
          V = c(V, Vn)
        }
      }
      return(mean(V))
    }
  }
  Varu[2:(2^p - 1)] = apply(U, 1, VU)
  Varu[2^p] = VarY
  if (noise) {
    Varu[1] = VU(rep(0, p))
    VarY = VarY - Varu[1]
  }
  if (length(Ntot) == 0) {
    if (noise) {
      cost = (2^p - 1) * N
    }
    else {
      cost = (2^p - 2) * N
    }
  }
  else {
    cost = sum(apply(U, 1, function(u) round(Ntot/choose(p,
                                                         sum(u))/(p - 1))))
    if (noise) {
      cost = cost + round(Ntot/choose(p, 0)/(p - 1))
    }
  }
  eta = rep(0, p)
  for (i in 1:p) {
    funci = function(k) {
      if (floor(k/(2^(i - 1)))%%2 == 0) {
        return((Varu[k + 1 + 2^(i - 1)] - Varu[k + 1])/choose(p -
                                                                1, sum(U[k, ])))
      }
      else {
        return(0)
      }
    }
    diffVa = apply(matrix(0:(2^p - 1), nrow = 1), 2, funci)
    eta[i] = sum(diffVa)/p/VarY
  }
  Shap = list(shapley = eta, cost = cost, names = names)
  class(Shap) <- "shapleySubsetMc"
  return(Shap)
}