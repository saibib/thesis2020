weights_shapley_diff = function(wts = NULL, impt = NULL, model = NULL, data, id.cat = NULL,
                                U = NULL, method = "knn", n.knn = 2, return.shap = FALSE, randperm = FALSE,
                                n.perm = 10000, rescale = FALSE, n.limit = 2000, noise = FALSE, ...){
  # if (any(is.na(X)) ) message('Data has missing values')
  if (is.null(wts)){
    message('Weights not provided, using equal weights')
    wts = rep(1/ncol(X), ncol(X))
  }
  if (is.null(impt)){
    message('Importances not provided, assuming equal importance')
    impt = rep(1/ncol(X), ncol(X))
  }
  wts= wts/sum(wts)

  #calculating the shapely effects
  res = sobolshap_knn(model = model, X = data, id.cat = id.cat,
                      U = U, method = method, n.knn = n.knn, return.shap = return.shap, randperm = randperm,
                      n.perm = n.perm, rescale = rescale, n.limit = n.limit, noise = noise, var_wts = wts,...)

  #distance between shapley effects and desried importances
  distance = stats::dist(rbind(res$Shap,impt))
  return(as.numeric(distance))
}

