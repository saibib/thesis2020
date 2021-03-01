vir = read.csv(here('data','SVI','Virginia_COUNTY.csv'),colClasses=c("FIPS"="character"))
vir = na_if(vir,y = -999)
vir[is.na(vir)] = 0
# information about each census tract
tract_info = vir[,c(1:7)]

#the 15 variables of interests, after they've been standardized
voi_vir = vir %>%
  select(starts_with('EPL'))
rownames(voi_vir) = vir$FIPS
# the four themes of interest and the total composite score

toi1 = apply(voi_vir[,1:4], 1, sum)
toi2 = apply(voi_vir[,5:8], 1, sum)
toi3 = apply(voi_vir[,9:11],1,sum)
toi4 = apply(voi_vir[,12:15], 1, sum)

toi_vir = data.frame(ses = toi1, household = toi2, minority = toi3,
                 housing_transportation = toi4)

# toi = svi %>%
#   select(starts_with('SPL_THEME'))
# toi = toi[,-5]
rownames(toi_vir) = vir$FIPS

voi_impt = rep(1, ncol(voi_vir))
# toi_impt = c(4/15, 4/15,2/15,5/15)
toi_impt = rep(1/4,4)

vir_scores = agg(voi_vir, voi_impt)

# 15*agg(voi, voi_impt)['26065004492'] == svi_scores['26065004492']
#

orig_shap_vir = shapleySubsetMc(X=voi_vir,Y=vir_scores, Ntot = 10000, Ni = 3) # est orginal shapley values
orig_shap_vir = sobolshap_knn(agg, toi_vir, method = 'knn', return.shap = T, n.knn=2,
                              randperm = T, n.perm=20, var_wts = toi_impt)
stats::dist(rbind(orig_shap_vir$Shap,voi_impt)) #euclidean distance difference in importances and shapley values

desired_v_shapley(voi_vir, voi_impt, orig_shap_vir$Shap) # plot the desired importances vs shapley effects

cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('toi_vir', 'agg','importance_diff',
                                      'shapleySubsetMc','sobolshap_knn','weights_shapley_diff'), envir = environment())

res_vir = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(3, 4),
                  control = list(cluster = cl),
                  data=toi_vir, Ntot= 1500, impt = toi_impt)

res_vir = DEoptim(fn = weights_shapley_diff, lower = rep(0,4), upper = rep(2, 4),
                  control = list(cluster = cl),
                  impt = toi_impt, model =agg, data = toi_vir,
                  method = 'knn', return.shap = T, n.knn=5, randperm = T, n.perm = 20, agg_method = 'ar')
setDefaultCluster(cl=NULL); stopCluster(cl)

optim_wts_vir = res_vir$optim$bestmem/sum(res_vir$optim$bestmem)

wts_v_optim_wts(toi_vir,toi_impt,optim_wts_vir)

vir_optim_scores = agg(toi_vir, var_wts = 4*optim_wts_vir, agg_method = 'ar')
optim_shap_vir = sobolshap_knn(agg, toi_vir, method = 'knn', return.shap = T, n.knn=2,
                               randperm = T, n.perm=20, var_wts = optim_wts_vir)



