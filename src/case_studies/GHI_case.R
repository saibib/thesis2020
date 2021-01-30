library(tidyverse)
library(readxl)
library(here)




ghi = read_xlsx(here('data','GHI_cleaned.xlsx'))
ghi = ghi %>% remove_rownames %>% column_to_rownames(var="country")
ghi[,1] = ghi[,1]/80*100
ghi[,2] = ghi[,2]/30*100
ghi[,3] = ghi[,3]/70*100
ghi[,4] = ghi[,4]/35*100

write.csv(ghi, here('data','GHI_transformed.csv'))

importances = c(1/3,1/3, 1/6, 1/6)
ghi_scores = agg(ghi, wts = importances,method = 'ar')
ghi_scores

orig_shap = shapleySubsetMc(X=ghi,Y=ghi_scores, Ntot = 1500, Ni = 3)
stats::dist(rbind(orig_shap$shapley,importances))


res = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(1, 4),
              control = list(itermax = 3),
              data=ghi, Ntot= 1500, impt = importances)
system("say -v Victoria Your code is done fuck wad!")


