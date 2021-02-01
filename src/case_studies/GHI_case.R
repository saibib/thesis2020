library(tidyverse)
library(readxl)
library(here)
library(ggplot2)
library(grid)
library(igraph)


ghi = read_xlsx(here('data','GHI_cleaned.xlsx'))
ghi = ghi %>% remove_rownames %>% column_to_rownames(var="country")
ghi[,1] = ghi[,1]/80*100
ghi[,2] = ghi[,2]/30*100
ghi[,3] = ghi[,3]/70*100
ghi[,4] = ghi[,4]/35*100

write.csv(ghi, here('data','GHI_transformed.csv'))
colnames(ghi)
importances = c(1/3,1/3, 1/6, 1/6)
ghi_scores = agg(ghi, wts = importances,method = 'ar')
ghi_scores

orig_shap = shapleySubsetMc(X=ghi,Y=ghi_scores, Ntot = 1500, Ni = 3)
stats::dist(rbind(orig_shap$shapley,importances))

data.frame(variable = colnames(ghi),desired = importances, shapley = orig_shap$shapley) %>%
  pivot_longer( -variable, names_to="impt", values_to="value") %>%
  ggplot(aes(variable,value)) +
  geom_bar(aes(fill = impt),stat = "identity",position = "dodge")


cl <- makeCluster(39) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('ghi', 'agg','importance_diff', 'shapleySubsetMc'), envir = environment())

res = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(1, 4),
              control = list(cluster = cl, itermax = 10),
              data=ghi, Ntot= 1500, impt = importances)
setDefaultCluster(cl=NULL); stopCluster(cl)

optim_wts = res$optim$bestmem/sum(res$optim$bestmem)
data.frame(variable = colnames(ghi),old_weights = importances,
           optimized_weights = optim_wts) %>%
  pivot_longer( -variable, names_to="weight", values_to="value") %>%
  ggplot(aes(variable,value)) +
  geom_bar(aes(fill = weight),stat = "identity",position = "dodge")



ghi_optim_scores = agg(ghi, wts = optim_wts,method = 'ar')


v1 = names(sort(ghi_scores[ghi_scores>0]))[1:10]
v2 = names(sort(ghi_optim_scores[ghi_optim_scores>0]))[1:10]

# - https://stackoverflow.com/questions/25781284/simplest-way-to-plot-changes-in-ranking-between-two-ordered-lists-in-r
plotRanks <- function(a, b, labels.offset=0.1, arrow.len=0.1)
{
  old.par <- par(mar=c(1,1,1,1))

  # Find the length of the vectors
  len.1 <- length(a)
  len.2 <- length(b)

  # Plot two columns of equidistant points
  plot(rep(1, len.1), 1:len.1, pch=20, cex=0.8,
       xlim=c(0, 3), ylim=c(0, max(len.1, len.2)),
       axes=F, xlab="", ylab="") # Remove axes and labels
  points(rep(2, len.2), 1:len.2, pch=20, cex=0.8)

  # Put labels next to each observation
  text(rep(1-labels.offset, len.1), 1:len.1, a)
  text(rep(2+labels.offset, len.2), 1:len.2, b)

  # Now we need to map where the elements of a are in b
  # We use the match function for this job
  a.to.b <- match(a, b)

  # Now we can draw arrows from the first column to the second
  arrows(rep(1.02, len.1), 1:len.1, rep(1.98, len.2), a.to.b,
         length=arrow.len, angle=20)
  par(old.par)
}
plotRanks(v1,v2,labels.offset = .4)


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
  wilcox.test(value ~ scores, data = ., paired = TRUE, alternative = "greater")

