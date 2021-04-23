


library(corrplot)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat = cor.mtest(voi)

colnames(voi) = c("Below\nPoverty","Unemp-\nloyed", "Income", "No High\nSchool", "Age 65>", "Age <17",
"Age 5+\nw/\nDisability", "Single\nParent", "Minority", "Poor\nEnglish", "Multiunit\nStructures",
"Mobile\nHomes", "Crowding", "No\nVehicle", "Group\nQuarters")



png(height=1600, width=1600, filename="figs/SVI/voi_cor.png")
cor(voi, use = 'pairwise') %>%
  corrplot.mixed(lower = "number", upper = "ellipse",
                 tl.col = "black", tl.cex = 1.5,
                 cl.cex = 2, number.cex = 1.5)
dev.off()

colnames(toi) = c("Socioeconomic\nStatus", 'Household\nComposition', 'Minority Status\n& Language', 'Housing Type')
png(height=800, width=800, filename="figs/SVI/toi_cor.png")
cor(toi, use = 'pairwise') %>%
  corrplot.mixed(lower = "number", upper = "ellipse",
                 tl.col = "black", tl.cex = 1.5,
                 cl.cex = 2, number.cex = 1.5)
dev.off()

png(height=900, width=1600, file="figs/SVI/combined_cor.png")
par(mfrow = c(1,2))
cor(voi, use = 'pairwise') %>%
  corrplot(method = 'shade', tl.col ='black', tl.cex = 2, cl.cex = 1.75,
             mar=c(0,0,5,0),cl.pos="b")
title('Variable Correlations', cex.main =3)
cor(toi, use = 'pairwise') %>%
  corrplot(method = 'shade',tl.col = "black", tl.cex = 2, cl.cex = 1.5,
             mar=c(3,0,3,3), cl.pos="b")
title('Dimension Correlations', cex.main = 3)
dev.off()

colnames(voi) = c("Below Poverty","Unemployed", "Income", "No High School", "Age 65>", "Age <17",
                  "Age 5+ w/ Disability", "Single Parent", "Minority", "Poor English", "Multiunit Structures",
                  "Mobile Homes", "Crowding", "No Vehicle", "Group Quarters")
voi %>%
  tidyr::gather() %>%
  ggplot(aes(value)) +
  geom_histogram(bins=40)+
  geom_boxploth(aes(y = -30), width =30, color = "orange", lwd = 1, alpha = .5) +
  facet_wrap(~ key) +
  theme(axis.text.x = element_text(size = 15))+
  xlab('Percentile Rank')+
  ylab('Frequency ')+
  ggtitle('Distribution of SVI Variables')+
  theme_minimal()+
  ggsave('figs/SVI/svi_eda_hist.png')



df = cbind(toi,svi_scores,svi_optim_scores)
colnames(df) = c('Undernourishment', 'Child Wasting', 'Child Stunting', 'Child Mortality', 'Original Scores',
                 'Optimized Scores')
png(height=1000, width=1000, filename="figs/SVI/svi_scores_cor.png")
cor(df, use = 'pairwise') %>%
  corrplot(method = 'shade')
dev.off()


|>|
  # pairs(ghi)
  # library(GGally)
  # ggpairs(ghi, aes(alpha = 0.4),diag = list(discrete = 'barDiag'))


  #correlation matrix btw before and after scores
  library(naniar)
library(visdat)

miss_var_summary(ghi)
ghi %>%
  gg_miss_var()
ghi %>%
  gg_miss_upset(nsets = 7)



ghi %>%
  mutate(total_na = rowSums(is.na(ghi))) %>%
  ggplot() +
  geom_point(aes(x = 1:nrow(ghi), y = total_na), alpha = 0.3) +
  labs(y = "Number of NA's", x = "index") +
  theme_minimal()

ghi %>%
  mutate(total_na = rowSums(is.na(ghi))) %>%
  ggplot() +
  geom_hex(aes(x = 1:nrow(ghi), y = total_na), alpha = 1) +
  labs(y = "Number of NA's", x = "index") +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")+
  theme_minimal()


