agg = function(X, var_wts = NULL, agg_method = c('ar', 'geom')){
  agg_method = match.arg(agg_method)
  X = as.matrix(X)
  if (!is.numeric(X)) stop('Must pass a numeric matrix')
  if (any(is.na(X)) ) message('Data has missing values')
  if (is.null(var_wts)){
    message('Weights not provided, using equal weights')
    var_wts = rep(1/ncol(X), ncol(X))
  }
  else{
    if (!is.numeric(var_wts)) stop('Must pass numeric weights')
    if (length(var_wts) != ncol(X)) stop('Mismatch between number of weights and predictors')
  }


  if(agg_method == 'ar') {
    tmp = X %*% diag(var_wts) # multiply by column weights
    scores = apply(tmp, 1, function(x) sum(x, na.rm = T)) # row sum
  }
  if(agg_method == 'geom'){
    tmp = X %*% diag(var_wts) # multiply by column weights
    scores = apply(tmp, 1, function(x) prod(x, na.rm = T)) # row product
  }
  return(scores)
}

