spi = read_xlsx(here('data','SPI','us-social-progress-index-results-public-4.xlsx'))

spi = spi[,-1]
colnames(spi) = tolower(gsub(' ','_', colnames(spi)))
spi = spi %>% remove_rownames %>% column_to_rownames(var="code")
spi = spi[,-2]
spi_dims = spi[,1:3]
spi_vars = spi %>% select(c(nutrition_and_basic_medical_care:access_to_advanced_education))

agg(spi_vars, agg_method = 'geom')*ncol(spi_vars)

orig_shap_spi = sobolshap_knn(agg, spi_vars, method = 'knn', return.shap = T, n.knn=2,
                              randperm = T, n.perm=30, var_wts = rep(1,ncol(spi_vars)))

stats::dist(rbind(orig_shap_spi$Shap,rep(1,ncol(spi_vars)))) #euclidean distance difference in importances and shapley values

desired_v_shapley(spi_vars, rep(1/12,ncol(spi_vars)), as.numeric(orig_shap_spi$Shap))

cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('spi_vars', 'agg', 'sobolshap_knn',
                                      'weights_shapley_diff', 'importance_diff',
                                      'shapleySubsetMc'), envir = environment())

# res_svi = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(3, 4),
#                   control = list(cluster = cl),
#                   data=toi, Ntot= 7500, impt = toi_impt)

res_spi = DEoptim(fn = weights_shapley_diff, lower = rep(0,ncol(spi_vars)), upper = rep(3, ncol(spi_vars)),
                  control = list(cluster = cl),
                  impt = rep(1/12,ncol(spi_vars)), model =agg, data = spi_vars,
                  method = 'knn', randperm = T, n.perm=30, n.knn=2, agg_method = 'geom')
setDefaultCluster(cl=NULL); stopCluster(cl)