gsmi = read_xlsx(here('data','GSMI', 'WEF_GSMI_2020_Dataset_2020.xlsx'),sheet = 'Data',trim_ws = T)


countries = as.data.frame(gsmi[2,4:ncol(gsmi)], row.names = NULL)
names(countries) = NULL

View(gsmi)
gsmi = gsmi[-c(1,3),-1]
gsmi = gsmi[grep('DTF', gsmi$...3),]
gsmi = gsmi[grep('Pillar', gsmi$...2),]
gsmi = gsmi[,-2]

gsmi = t(gsmi)

colnames(gsmi) = gsmi[1,]
gsmi = gsmi[-1,]
rownames(gsmi) = countries

colnames(gsmi) = gsub(".*:", "", colnames(gsmi))
colnames(gsmi) = gsub("\\s*\\([^\\)]+\\)","",colnames(gsmi))
colnames(gsmi) = trimws(colnames(gsmi))
colnames(gsmi) = gsub(" ", "_", trimws(colnames(gsmi)))
colnames(gsmi) = tolower(colnames(gsmi))
write.csv(gsmi, here('data','GSMI','GSMI_final.csv'))

gsmi = gsmi %>% as.data.frame() %>%
  mutate(across(.cols = everything(), .fns = as.numeric)) %>%
  as.matrix()
rownames(gsmi) = countries


gsmi_impt = rep(1/ncol(gsmi), ncol(gsmi))
gsmi_scores = agg(gsmi, var_wts = gsmi_impt,agg_method = 'ar') # calculating index score by arithmetic aggregation


orig_shap_gsmi = shapleySubsetMc(X=gsmi,Y=gsmi_scores, Ntot = 5000, Ni = 3) # est orginal shapley values
stats::dist(rbind(orig_shap_gsmi$shapley,importances)) #euclidean distance difference in importances and shapley values


sobolshap_knn(agg, gsmi, method = 'knn', return.shap = T, n.knn=2, randperm = T, n.perm=100, var_wts = gsmi_impt)

desired_v_shapley(gsmi, gsmi_impt, orig_shap_gsmi$shapley) # plot the desired importances vs shapley effects


cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('gsmi', 'agg','sobolshap_knn','weights_shapley_diff', 'shapleySubsetMc'), envir = environment())

gsmi_res = DEoptim(fn = importance_diff, lower = rep(0,10), upper = rep(1, 10),
              control = list(cluster = cl),
              data=gsmi, Ntot= 1500, impt = gsmi_impt)

# gsmi_res = DEoptim(fn = weights_shapley_diff, lower = rep(0,10), upper = rep(1, 10),
#               control = list(cluster = cl),
#               impt = gsmi_impt, model =agg, data = gsmi,
#               method = 'knn', return.shap = T, n.knn=5, agg_method = 'ar')
setDefaultCluster(cl=NULL); stopCluster(cl)

gsmi_optim_wts = gsmi_res$optim$bestmem/sum(gsmi_res$optim$bestmem)

wts_v_optim_wts(gsmi,gsmi_impt,gsmi_optim_wts)

gsmi_optim_scores = agg(gsmi, var_wts = gsmi_optim_wts, agg_method = 'ar')
optim_shap_gsmi = shapleySubsetMc(X=gsmi,Y=gsmi_optim_scores, Ntot = 1500, Ni = 3)


v1 = names(sort(gsmi_scores[gsmi_scores>0]))[1:25]
v2 = names(sort(gsmi_optim_scores[gsmi_optim_scores>0]))[1:25]

plotRanks(v1,v2,labels.offset = .5)

match(v1,v1)-match(v1,v2)
match(names(sort(gsmi_scores[gsmi_scores>0])),names(sort(gsmi_scores[gsmi_scores>0]))) -
  match(names(sort(gsmi_scores[gsmi_scores>0])),names(sort(gsmi_optim_scores[gsmi_optim_scores>0])))
