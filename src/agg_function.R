agg = function(X, wts = NULL, method = c('ar', 'geom')){

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


  if(method == 'ar') {
    tmp = X %*% diag(wts) # multiply by column weights
    scores = apply(tmp, 1, function(x) sum(x, na.rm = T)) # row sum
  }
  if(method == 'geom'){
    tmp = X %*% diag(wts) # multiply by column weights
    scores = apply(tmp, 1, function(x) prod(x, na.rm = T)) # row product
  }
  return(scores)
}

