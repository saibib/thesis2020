gsmi= read.csv(here('data','GSMI','GSMI_final.csv'))

gsmi = gsmi %>% remove_rownames %>% column_to_rownames(var="X")
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
p.mat = cor.mtest(gsmi)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
colnames(gsmi) = firstup(gsub("_", "\n", colnames(gsmi)))


png(height=1100, width=1100, filename="figs/GSMI/gsmi_cor.png")
cor(gsmi, use = 'pairwise') %>%
  corrplot.mixed(lower = "number", upper = "ellipse",
                 tl.col = "black",
                 p.mat = p.mat, sig.level = 0.01, insig = "blank",cl.cex = 2)
dev.off()


colnames(gsmi) = gsub("\n", " ", colnames(gsmi))

gsmi %>% as.data.frame() %>%
  tidyr::gather() %>%
  ggplot(aes(value)) +
  geom_histogram()+
  geom_boxploth(aes(y = -3), width =3, color = "orange", lwd = 1, alpha = .5) +
  facet_wrap(~ key) +
  theme(axis.text.x = element_text(size = 15))+
  xlab('Value')+
  ylab('Count')+
  ggtitle('Distribution of GSMI Dimension Scores')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))+
  ggsave('figs/GSMI/gsmi_eda_hist.png')


df = cbind(gsmi,gsmi_scores,gsmi_optim_scores)
colnames(df) = firstup(gsub("_", " ", colnames(df)))
colnames(df) = gsub(' ', '\n', colnames(df))

png(height=1200, width=1200, filename="figs/GSMI/gsmi_scores_cor.png")
cor(df, use = 'pairwise') %>%
  corrplot.mixed(lower = "number", upper = "ellipse",
                 tl.col = "black",
                 p.mat = cor.mtest(df), sig.level = 0.01, insig = "blank",cl.cex = 2)
dev.off()


cor(df, use = 'pairwise') %>%
  corrplot(method = 'sh',type ='lower')
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


