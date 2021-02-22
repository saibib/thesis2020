

epi = read_xlsx(here('data','EPI', 'EPI_Issue_Category.xlsx'),trim_ws = T)
countries = epi$country
epi_impt = c(.2, .16, .02, .02, .03, .03, .24, .03, .06, .06, .15)

epi = epi %>% select(-country) %>%
  mutate(across(.cols = everything(), .fns = as.numeric)) %>%
  as.matrix()
rownames(epi) = countries


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
  if (sum(wts[1:4])>.4) return(1)
  Y = agg(data,var_wts = wts, agg_method = match.arg(aggregation)) # aggregating the columns

  #calculating the shapely effects
  res = shapleySubsetMc(X=data,Y=Y, Ntot = Ntot, Ni = Ni, cat = cat, weight = weight,
                        noise = noise, discrete = discrete)

  #distance between shapley effects and desried importances
  distance = stats::dist(rbind(res$shapley,impt))
  return(as.numeric(distance))
}

epi_scores = agg(epi, epi_impt,'ar')

system.time({
  orig_shap_epi = shapleySubsetMc(X=epi,Y=epi_scores, Ntot = 15000, Ni = 3)
})

stats::dist(rbind(orig_shap_epi$shapley,epi_impt))

desired_v_shapley(epi, epi_impt, orig_shap_epi$shapley) # plot the desired importances vs shapley effects


cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('epi', 'agg','importance_diff_mod', 'shapleySubsetMc'), envir = environment())

res_epi = DEoptim(fn = importance_diff_mod, lower = rep(0,ncol(epi)), upper = rep(1,ncol(epi)),
              control = list(cluster = cl),
              data=epi, Ntot= 2500, impt = epi_impt)
setDefaultCluster(cl=NULL); stopCluster(cl)
