agg = function(X, wts, method = c('ar', 'geom')){
  X = as.matrix(X)
  method = match.arg(method)
  if (!is.numeric(X)) stop('Must pass a numeric matrix')
  if (any(is.na(X)) ) message('Data has missing values')

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
