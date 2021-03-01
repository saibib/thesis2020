svi = read.csv(here('data','SVI','SVI2018_US_COUNTY.csv'),colClasses=c("FIPS"="character"))
svi = na_if(svi,y = -999)
svi[is.na(svi)] = 0
# information about each census tract
tract_info = svi[,c(1:7)]

#the 15 variables of interests, after they've been standardized
voi = svi %>%
  select(starts_with('EPL'))
rownames(voi) = svi$FIPS
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
rownames(toi) = svi$FIPS

voi_impt = rep(1/ncol(voi), ncol(voi))
# toi_impt = c(4/15, 4/15,2/15,5/15)
toi_impt = rep(1,4)

svi_scores = agg(voi, voi_impt)

# 15*agg(voi, voi_impt)['26065004492'] == svi_scores['26065004492']
#

orig_shap_svi = shapleySubsetMc(X=voi,Y=svi_scores, Ntot = 15000, Ni = 3) # est orginal shapley values
orig_shap_svi = sobolshap_knn(agg, voi, method = 'knn', return.shap = T, n.knn=4,
                              randperm = T, n.perm=30, var_wts = voi_impt)
stats::dist(rbind(orig_shap_svi$shapley,toi_impt)) #euclidean distance difference in importances and shapley values

desired_v_shapley(toi, toi_impt, orig_shap_svi$shapley) # plot the desired importances vs shapley effects

cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('voi', 'agg', 'sobolshap_knn',
                                      'weights_shapley_diff'), envir = environment())

res_svi = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(3, 4),
              control = list(cluster = cl),
              data=toi, Ntot= 1500, impt = toi_impt)

res_svi = DEoptim(fn = weights_shapley_diff, lower = rep(0,ncol(voi)), upper = rep(1, ncol(voi)),
              control = list(cluster = cl),
              impt = voi_impt, model = agg, data = voi,
              method = 'knn', return.shap = T, n.knn=4,
              randperm = T, n.perm=30)
setDefaultCluster(cl=NULL); stopCluster(cl)

optim_wts_svi = res_svi$optim$bestmem/sum(res_svi$optim$bestmem)

wts_v_optim_wts(toi,toi_impt,optim_wts_svi)

svi_optim_scores = agg(svi, var_wts = optim_wts_svi, agg_method = 'ar')
optim_shap_svi = shapleySubsetMc(X=toi,Y=svi_optim_scores, Ntot = 1500, Ni = 3)



