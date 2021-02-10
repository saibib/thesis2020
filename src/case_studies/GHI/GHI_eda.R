ghi = read.csv( here('data','GHI','GHI_transformed.csv'))
ghi = ghi %>% remove_rownames %>% column_to_rownames(var="X")

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
p.mat = cor.mtest(ghi)

cor(ghi, use = 'pairwise') %>%
  corrplot.mixed(lower = "number", upper = "ellipse",
                 tl.col = "black",
                 p.mat = p.mat, sig.level = 0.01, insig = "blank")


ghi %>% tidyr::gather() %>% ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") + geom_histogram()+
  theme(axis.text.x = element_text(size = 15))

cor(cbind(ghi,ghi_scores), use = 'pairwise') %>%
  corrplot.mixed(lower = "number", upper = "ellipse",
                 tl.col = "black",
                 p.mat = cor.mtest(cbind(ghi,ghi_scores)), sig.level = 0.01, insig = "blank")
cor(cbind(ghi,ghi_optim_scores), use = 'pairwise') %>%
  corrplot.mixed(lower = "number", upper = "ellipse",
                 tl.col = "black",
                 p.mat = cor.mtest(cbind(ghi,ghi_optim_scores)), sig.level = 0.01, insig = "blank")


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
