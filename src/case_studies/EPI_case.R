library(tidyverse)
library(readxl)
library(here)
library(igraph)

epi = read_xlsx(here('data','EPI', 'EPI_Issue_Category.xlsx'))
epi = epi %>% remove_rownames %>% column_to_rownames(var="country")
importances = c(.2, .16, .02, .02, .03, .03, .24, .03, .06, .06, .15)


importance_diff_mod = function(wts = NULL, impt = NULL, aggregation = c('ar','geom'),data, Ntot = NULL, Ni = 3,
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
  if (sum(wts[1:4])>.4) return(Inf) #constraint
  Y = agg(data,wts = wts, method = match.arg(aggregation)) # aggregating the columns

  #calculating the shapely effects
  res = shapleySubsetMc(X=data,Y=Y, Ntot = Ntot, Ni = Ni, cat = cat, weight = weight,
                        noise = noise, discrete = discrete)

  #distance between shapley effects and desried importances
  distance = stats::dist(rbind(res$shapley,impt))
  return(as.numeric(distance))
}

epi_scores = agg(epi, importances)


