library(tidyverse)
library(readxl)
library(here)
library(ggplot2)
library(grid)
library(igraph)

# reading in the dataset and applying the preprocessing transformation to the indicators that the GHI does
ghi = read_xlsx(here('data','GHI', 'GHI_cleaned.xlsx'))
ghi = ghi %>% remove_rownames %>% column_to_rownames(var="country")
ghi[,1] = ghi[,1]/80*100
ghi[,2] = ghi[,2]/30*100
ghi[,3] = ghi[,3]/70*100
ghi[,4] = ghi[,4]/35*100
ghi = as.matrix(ghi)
write.csv(ghi, here('data','GHI','GHI_transformed.csv'))
colnames(ghi)
importances = c(1/3,1/3, 1/6, 1/6) # importances
ghi_scores = agg(ghi, var_wts = importances,agg_method = 'ar') # calculating index score by arithmetic aggregation
ghi_scores

orig_shap_ghi = shapleySubsetMc(X=ghi,Y=ghi_scores, Ntot = 1500, Ni = 3) # est orginal shapley values
stats::dist(rbind(orig_shap$shapley,importances)) #euclidean distance difference in importances and shapley values


desired_v_shapley(ghi, importances, orig_shap_ghi$shapley) # plot the desired importances vs shapley effects


cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('ghi', 'agg','importance_diff',
                                      'shapleySubsetMc','sobolshap_knn','weights_shapley_diff'), envir = environment())

res = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(1, 4),
              control = list(cluster = cl),
              data=ghi, Ntot= 1500, impt = importances)

res = DEoptim(fn = weights_shapley_diff, lower = rep(0,4), upper = rep(1, 4),
              control = list(cluster = cl, CR = .3,F=.3),
              impt = importances, model =agg, data = ghi,
              method = 'knn', return.shap = T, n.knn=5, agg_method = 'ar')
setDefaultCluster(cl=NULL); stopCluster(cl)

optim_wts = res$optim$bestmem/sum(res$optim$bestmem)

wts_v_optim_wts(ghi,importances,optim_wts)

ghi_optim_scores = agg(ghi, var_wts = optim_wts, agg_method = 'ar')
optim_shap_ghi = shapleySubsetMc(X=ghi,Y=ghi_optim_scores, Ntot = 1500, Ni = 3)

v1 = names(sort(ghi_scores[ghi_scores>0]))[1:25]
v2 = names(sort(ghi_optim_scores[ghi_optim_scores>0]))[1:25]

# - https://stackoverflow.com/questions/25781284/simplest-way-to-plot-changes-in-ranking-between-two-ordered-lists-in-r

plotRanks(v1,v2,labels.offset = .5)


match(v1,v1)-match(v1,v2)


match(names(sort(ghi_scores[ghi_scores>0])),names(sort(ghi_scores[ghi_scores>0]))) -
  match(names(sort(ghi_scores[ghi_scores>0])),names(sort(ghi_optim_scores[ghi_optim_scores>0])))

data.frame(countries = names(ghi_scores[ghi_scores>0]), old_scores = ghi_scores[ghi_scores>0],
           optimized_scores = ghi_optim_scores[ghi_optim_scores>0])%>%
  pivot_longer(-countries,names_to="scores", values_to="value") %>%
  ggplot(aes(value, fill=scores))+
  geom_boxplot()

data.frame(countries = names(ghi_scores[ghi_scores>0]), old_scores = ghi_scores[ghi_scores>0],
           optimized_scores = ghi_optim_scores[ghi_optim_scores>0])%>%
  pivot_longer(-countries,names_to="scores", values_to="value") %>%
  wilcox.test(value ~ scores, data = ., paired = TRUE)



SIGN.test(ghi_scores[ghi_scores>0],ghi_optim_scores[ghi_optim_scores>0])

