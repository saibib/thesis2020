weights_shapley_diff = function(wts = NULL, impt = NULL, aggregation = c('ar','geom'),data, model = NULL, ...){
  # if (any(is.na(X)) ) message('Data has missing values')
  if (is.null(wts)){
    message('Weights not provided, using equal weights')
    wts = rep(1/ncol(X), ncol(X))
  }
  if (is.null(impt)){
    message('Importances not provided, assuming equal importance')
    impt = rep(1/ncol(X), ncol(X))
  }
  data = data
  aggregation = match.arg(aggregation)

  if (aggregation == 'ar'){
    model = function(data) apply(data %*% diag(wts), 1, function(x) sum(x, na.rm = T))
  }
  if (aggregation == 'geom'){
    model = function(data) apply(data %*% diag(wts), 1, function(x) prod(x,na.rm = T))
  }

  #calculating the shapely effects
  res = sobolshap_knn(model = model, X=data, ...)

  #distance between shapley effects and desried importances
  distance = stats::dist(rbind(res$shapley,impt))
  return(as.numeric(distance))
}
weights_shapley_diff(wts = importances, aggregation = 'ar', data = ghi)
