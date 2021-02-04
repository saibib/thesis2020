fnc = function(X){
  tmp = X %*% diag() # multiply by column weights
  scores = apply(tmp, 1, function(x) sum(x, na.rm = T)) # row sum
}


system.time({
  ahh = sobolshap_knn(agg, epi, method = 'knn', return.shap = T, n.knn=2,  wts = importances)
  ahh_samp =sobolshap_knn(agg, epi, method = 'knn', return.shap = T, n.knn=2, randperm = T, n.perm=200, wts = importances)
})
ahh$Shap
orig_shap_ghi$shapley
colnames(ghi)

weights_shapley_diff(wts = importances, impt = importances, model =agg, data = ghi,
                     method = 'knn', return.shap = T, n.knn=2, agg_method = 'ar')


