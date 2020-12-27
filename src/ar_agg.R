ar_agg = function(X,wts){
  X = as.matrix(X)
  method = match.arg(method)
  if (!is.numeric(X)) stop('Must pass a numeric matrix')
  if (any(is.na(X)) ) message('Data has missing values')
  if (is.null(wts)){
    message('Weights not provided, using equal weights')
    wts = rep(1/ncol(X), ncol(X))
  }
  else{
    if (!is.numeric(wts)) stop('Must pass numeric weights')
    if (length(wts) != ncol(X)) stop('Mismatch between number of weights and predictors')
  }


  # X %*% diag(wts) multiply by column weights
  scores = apply(X %*% diag(wts), 1, function(x) sum(x, na.rm = T)) # row sum
}