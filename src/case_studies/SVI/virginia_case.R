vir = read.csv(here('data','SVI','Virginia.csv'),colClasses=c("FIPS"="character"))
vir = na_if(vir,y = -999)
vir[is.na(vir)] = 0
# information about each census tract
tract_info = vir[,c(1:7)]

#the 15 variables of interests, after they've been standardized
voi = vir %>%
  select(starts_with('EPL'))
rownames(voi) = vir$FIPS
# the four themes of interest and the total composite score

toi1 = apply(voi[,1:4], 1, sum)
toi2 = apply(voi[,5:8], 1, sum)
toi3 = apply(voi[,9:11],1,sum)
toi4 = apply(voi[,12:15], 1, sum)

toi = data.frame(ses = toi1, household = toi2, minority = toi3,
                 housing_transportation = toi4)

# toi = svi %>%
#   select(starts_with('SPL_THEME'))
# toi = toi[,-5]
rownames(toi) = vir$FIPS

voi_impt = rep(1, ncol(voi))
# toi_impt = c(4/15, 4/15,2/15,5/15)
toi_impt = rep(1,4)

vir_scores = agg(voi, voi_impt)

# 15*agg(voi, voi_impt)['26065004492'] == svi_scores['26065004492']
#

orig_shap_svi = shapleySubsetMc(X=voi,Y=vir_scores, Ntot = 10000, Ni = 3) # est orginal shapley values
orig_shap_svi = sobolshap_knn(agg, voi, method = 'knn', return.shap = T, n.knn=2,
                              randperm = T, n.perm=10, var_wts = voi_impt)
stats::dist(rbind(orig_shap_svi$Shap,voi_impt)) #euclidean distance difference in importances and shapley values

desired_v_shapley(voi, voi_impt, orig_shap_svi$Shap) # plot the desired importances vs shapley effects

cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('ghi', 'agg','importance_diff',
                                      'shapleySubsetMc','sobolshap_knn','weights_shapley_diff'), envir = environment())

res_svi = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(3, 4),
                  control = list(cluster = cl),
                  data=toi, Ntot= 1500, impt = toi_impt)

res_svi = DEoptim(fn = weights_shapley_diff, lower = rep(0,4), upper = rep(1, 4),
                  control = list(cluster = cl, CR = .3,F=.3),
                  impt = toi_impt, model =agg, data = toi,
                  method = 'knn', return.shap = T, n.knn=5, agg_method = 'ar')
setDefaultCluster(cl=NULL); stopCluster(cl)

optim_wts_svi = res_svi$optim$bestmem/sum(res_svi$optim$bestmem)

wts_v_optim_wts(toi,toi_impt,optim_wts_svi)

svi_optim_scores = agg(vir, var_wts = optim_wts_svi, agg_method = 'ar')
optim_shap_svi = shapleySubsetMc(X=toi,Y=svi_optim_scores, Ntot = 1500, Ni = 3)



