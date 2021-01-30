importance_diff = function(wts = NULL, impt = NULL, aggregation = c('ar','geom'),data, Ntot = NULL, Ni = 3,
                           weight = NULL, noise = FALSE, cat = NULL, discrete = NULL){
  # if (any(is.na(X)) ) message('Data has missing values')
  if (is.null(wts)){
    message('Weights not provided, using equal weights')
    wts = rep(1/ncol(X), ncol(X))
  }
  if (is.null(impt)){
    message('Importances not provided, assuming equal importance')
    impt = rep(1/ncol(X), ncol(X))
  }

  wts <- wts / sum(wts)
  Y = agg(data,wts = wts, method = match.arg(aggregation)) # aggregating the columns

  #calculating the shapely effects
  res = shapleySubsetMc(X=data,Y=Y, Ntot = Ntot, Ni = Ni, cat = cat, weight = weight,
                        noise = noise, discrete = discrete)

  #distance between shapley effects and desried importances
  distance = stats::dist(rbind(res$shapley,impt))
  return(as.numeric(distance))
}
