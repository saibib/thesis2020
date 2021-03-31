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
stats::dist(rbind(orig_shap_ghi$shapley,importances)) #euclidean distance difference in importances and shapley values



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

ghi_optim_scores = agg(ghi, var_wts = optim_wts, agg_method = 'ar')
optim_shap_ghi = shapleySubsetMc(X=ghi,Y=ghi_optim_scores, Ntot = 1500, Ni = 3)

wts_v_optim_wts(ghi,importances,optim_wts)+
  ggtitle('Dimension Weights for GHI')+
  theme(legend.position = 'bottom')+
  ggsave('figs/GHI/ghi_weights.png')

desired_v_shapley(ghi, importances, orig_shap_ghi$shapley,optim_shap_ghi$shapley)+
  ggtitle('Dimension Importances for GHI')+
  theme(legend.position = 'bottom')+
  ggsave('figs/GHI/ghi_dim_impt.png')

v1 = names(sort(ghi_scores[ghi_scores>0]))
v2 = names(sort(ghi_optim_scores[ghi_optim_scores>0]))

# - https://stackoverflow.com/questions/25781284/simplest-way-to-plot-changes-in-ranking-between-two-ordered-lists-in-r

# plotRanks(v1,v2,labels.offset = .5)


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

ks.test(ghi_scores[ghi_scores>0],ghi_optim_scores[ghi_optim_scores>0])

ghi_scores = ghi_scores[ghi_scores>0]
ghi_optim_scores = ghi_optim_scores[ghi_optim_scores>0]
rankshifts  = data.frame(country = as.factor(v1),  old_scores = ghi_scores[as.factor(v1)],
                         new_scores =  ghi_optim_scores[as.factor(v1)], change = match(v1,v1)-match(v1,v2))
# rankshifts %>%
#   ggplot(aes(x=reorder(country, -change), y=change)) +
#   geom_segment( aes(x=reorder(country, -change), xend=reorder(country, -change), y=0, yend=change), color="grey") +
#   geom_point( color="orange", size=2) +
#   theme_bw()+
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   coord_flip()+
#   ggtitle('Change in GHI Rank After Applying Optimized Weights')+
#   xlab("") +
#   ylab("Change in Rank")+
#   theme_light()


rankshifts %>%
  arrange(-old_scores)%>%
  mutate(country = factor(country, country)) %>%
  ggplot() +
  geom_segment( aes(x=country, xend=country, y=old_scores, yend=new_scores), color="grey") +
  geom_point( aes(x=country, y=old_scores), color='lightblue', size=3 ) +
  geom_point( aes(x=country, y=new_scores), color='orange', size=3 ) +
  coord_flip()+
  ggtitle('Change in GHI Scores After Applying Optimized Weights')+
  xlab("Country (in order of original rank)") +
  ylab("GHI Score")+
  annotate("text", y=28.5, x=115, label= "Blue: GHI Scores using Original Weights", size =4) +
  annotate("text", y=30, x=113, label= "Orange: GHI Scores using Optimized Weights", size = 4) +
  theme_light()+
  ggsave( width = 9, height = 14, dpi = 300, filename = "figs/GHI/ghi_scores_loli.png")


map_data = rankshifts

levels(map_data$country) <- c(levels(map_data$country),
                              "Slovakia" ,"Macedonia", "Bosnia and Herzegovina","Russia",
                              "Republic of Serbia", "Kyrgyzstan", "Trinidad and Tobago", 'Vietnam',
                              "Swaziland", "Ivory Coast", 'Laos', "United Republic of Tanzania",
                              "Guinea Bissau", "Republic of Congo", "East Timor")

map_data$country[map_data$country == "Côte d'Ivoire"        ] <- 'Ivory Coast'
map_data$country[map_data$country == "Lao PDR"              ] <- 'Laos'
map_data$country[map_data$country == "Viet Nam"             ] <- 'Vietnam'
map_data$country[map_data$country == "Serbia"               ] <- "Republic of Serbia"
map_data$country[map_data$country ==  "Russian Federation"  ] <- "Russia"
map_data$country[map_data$country ==  "Slovak Republic"     ] <- "Slovakia"
map_data$country[map_data$country ==  "North Macedonia"     ] <- "Macedonia"
map_data$country[map_data$country ==  "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
map_data$country[map_data$country ==  "Kyrgyz Republic"     ] <- "Kyrgyzstan"
map_data$country[map_data$country ==  "Trinidad & Tobago"   ] <- "Trinidad and Tobago"
map_data$country[map_data$country ==  "Eswatini"            ] <- "Swaziland"
map_data$country[map_data$country ==  "Tanzania"            ] <- "United Republic of Tanzania"
map_data$country[map_data$country ==  "Guinea-Bissau"       ] <- "Guinea Bissau"
map_data$country[map_data$country ==  "Congo, Rep."         ] <- "Republic of Congo"
map_data$country[map_data$country ==  "Timor-Leste"         ] <- "East Timor"


# colourPalette <- RColorBrewer::brewer.pal(10,'RdYlGn')
# sPDF <- joinCountryData2Map( map_data, joinCode = "NAME", nameJoinColumn = "country", verbose = T,)
# mapParams.shifts <- mapCountryData( sPDF, nameColumnToPlot="change", addLegend=FALSE,
#                                     missingCountryCol = gray(.9), colourPalette = colourPalette )
# do.call( addMapLegend, c( mapParams.shifts, legendLabels="all", legendWidth=0.5, legendIntervals="page", legendMar = 2 ) )


# map_data$ADMIN = map_data$country
# map_data = merge(map_data, countryRegions, by.x = 'ADMIN')

map_data$admin = map_data$country
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_dat <- merge(world, map_data, by.x="admin")


coolwarm_hcl <- diverging_hcl(11,h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))
pal <- function(col, border = "transparent") {
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

pal(coolwarm_hcl)
rwb <- colorRampPalette(colors = c("red", "white", "blue"))
sf_dat %>%
ggplot() +
  geom_map(dat=world_map, map = world_map,
           aes(map_id=region), fill="lightgray", color="black", alpha = .1)+
  geom_sf(aes(fill=change), color = 'black') +
  # scale_fill_binned(breaks = c(-15,-10,-5,0,5,10,15,20),low = 'red',high = 'blue')+
  scale_fill_stepsn(n.breaks = 9, colours = rwb(10),name = 'Shift')+
  # scale_fill_viridis(name = 'Shift',option = 'E',direction = -1, begin =.2, end =.8,breaks = c(-15,-10,-5,0,5,10,15,20))+
  guides(fill = guide_coloursteps(show.limits = TRUE, title ='Shift', title.position = 'top'))+
  theme_light()+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1.3, 'cm'))+ #change legend key width)+
  ggtitle('Map of GHI Rank Shifts')+
  ggsave('figs/GHI/ghi_rank_map.png')


sf_dat %>%
  mutate(category = cut(sf_dat$old_scores, breaks = c(-Inf, 10, 20, 35,50,Inf),
                        labels  = c('\u2264 9.9 Low', '10-19.9 Moderate',
                                    "20-34.9 Serious", "35-49.9 Alarming","\u2265 50 Extremly Alarming"))) %>%
  ggplot() +
  geom_map(dat=world_map, map = world_map,
           aes(map_id=region), fill="lightgray", color="black", alpha = .3)+
  geom_sf(aes(fill=category), color = 'black') +
  scale_fill_viridis(name = 'Category',discrete = TRUE, option = 'A', begin = .2, end = .8, direction = -1)+
  # guides(fill = guide_coloursteps(show.limits = F, title ='Score', title.position = 'top'))+
  theme_light()+
  theme(legend.position = 'bottom')+
  ggtitle('Map of GHI Scores')+
  ggsave('figs/GHI/ghi_old_score_map.png')

sf_dat %>%
  st_drop_geometry() %>%
  select(country, old_scores, new_scores, continent)%>%
  pivot_longer(cols = -c(country, continent), names_to = 'method', values_to = 'values') %>%
  filter(!continent %in% c('Oceania', 'Seven seas (open ocean)') ) %>%
  ggplot(aes(continent, values, fill = method ))+
  geom_split_violin(width =1.75,position = position_dodge(.5), color = 'lightgray')+
  geom_boxplot(width = .2, color = 'black',position = position_dodge(.3))+
  theme_light()+
  scale_fill_manual(values = c("orange","deepskyblue3"),
                    labels = c("Optimized", "Original"))+
  theme(legend.position = "none")+
  xlab('Continent')+
  ylab('GHI Score')+
  coord_flip()+
  annotate("text", x=5.15, y=28, label= "Blue: GHI Scores using Original Weights", size = 2.5) +
  annotate("text", x=4.85, y=29, label= "Orange: GHI Scores using Optimized Weights", size = 2.5) +
  ggtitle('GHI Scores Distributions by Weighting Scheme')+
  ggsave('figs/GHI/ghi_scores_violin.png')


d1 = sf_dat %>%
  st_drop_geometry() %>%
  select(country, old_scores, new_scores, continent)%>%
  pivot_longer(cols = -c(country, continent), names_to = 'method', values_to = 'values') %>%
  filter(!continent %in% c('Oceania', 'Seven seas (open ocean)') )

d2 = sf_dat %>%
  st_drop_geometry() %>%
  select(country, old_scores, new_scores, continent)%>%
  pivot_longer(cols = -c(country, continent), names_to = 'method', values_to = 'values') %>%
  mutate(continent = 'World')

p2 = d2 %>%
  ggplot(aes(continent, values, fill = method ))+
  geom_split_violin(color='lightgray')+
  geom_boxplot(width = .1, color = 'black',position = position_dodge(.3))+
  theme_light()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("orange","deepskyblue3"),name = "Weights",
                    labels = c("Optimized", "Original"))+
  xlab('')+
  ylab('GHI Score')+
  theme(plot.title = element_text(size = 25, face = "bold"),
        axis.text=element_text(size=16, face = "bold"),
        axis.title = element_text(size = 18, face = 'bold'))+
  ggtitle(' ')+
  coord_flip()


p1 = d1 %>%
  ggplot(aes(continent, values, fill = method ))+
  geom_split_violin(width =2,position = position_dodge(.5), color = 'lightgray')+
  geom_boxplot(width = .15, color = 'black',position = position_dodge(.3))+
  theme_light()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("orange","deepskyblue3"),name = "Weights",
                    labels = c("Optimized", "Original"))+
  xlab('Continent')+
  ylab('GHI Score')+
  coord_flip()+
  annotate("text", x=5.15, y=35, label= "Blue: GHI Scores using Original Weights", size = 6) +
  annotate("text", x=4.85, y=34, label= "Orange: GHI Scores using Optimized Weights", size = 6) +
  ggtitle('GHI Scores Distributions by Weighting Scheme')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text=element_text(size=16,face = "bold"),
        axis.title = element_text(size = 18, face = "bold"))+
  ggsave('figs/GHI/ghi_scores_violin.png')

png('figs/GHI/ghi_violins.png',width = 1600, height = 800  )
grid.arrange(p1,p2,nrow =1 )
dev.off()




tmp_df = sf_dat %>%
  st_drop_geometry() %>%
  select(country, old_scores, new_scores, continent, change)%>%
  filter(!continent %in% c('Seven seas (open ocean)') )
tmp2_df = tmp_df %>%
  group_by(continent) %>%
  summarize(old_avg = mean(old_scores),new_avg = mean(new_scores))

p1 = ggplot()+
  geom_point(data = tmp_df, aes(old_scores, new_scores, shape = continent, color = continent,fill = continent),
             size = 3, alpha = 1)+
  # geom_point(data = tmp2_df,
  #            aes(old_avg, new_avg, shape = continent, color = continent), size = 5)+
  geom_abline(slope=1, intercept = 0,linetype = 'dashed', alpha =.5)+
  theme_light()+
  scale_color_viridis(name = 'Continent' , option = 'A', discrete = T, end = .8, begin =.2)+
  scale_fill_viridis(name = 'Continent', option ='A', discrete = T, end = .8, begin = .2)+
  xlab('Original Scores')+
  ylab('Optimized Scores')+
  ggtitle('Optimized vs. Original GHI Scores\n')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text=element_text(size=16,face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))+
  theme(legend.position= c(.8,.15))+
  scale_shape_manual(name = 'Continent', values = c(16, 15,17,21,22,23))


p2 = tmp_df %>%
  mutate(diff = (new_scores - old_scores)/old_scores) %>%
  group_by(continent) %>%
  summarise(mean_diff = mean(diff), mean_old = mean(old_scores))%>%
  ggplot(aes(mean_old, mean_diff, color =continent, shape = continent, fill = continent))+
    geom_point(size = 4)+
  geom_hline(yintercept = 0, alpha =.5)+
  geom_hline(yintercept = .1, linetype = 'dashed', alpha =.5)+
  geom_hline(yintercept = -.1, linetype = 'dashed', alpha =.5)+
  xlab('Average Original Score')+
  ylab('Percent Difference')+
  theme_light()+
  scale_color_viridis(name = 'Continent' , option = 'A', discrete = T, end = .8, begin =.2, guide = F)+
  scale_fill_viridis(name = 'Continent', option ='A', discrete = T, end = .8, begin = .2, guide = F)+
  ggtitle('Average Percent Difference Between\nOptimized and Original Scores')+
  theme(plot.title = element_text(size = 24, face = "bold"),
        axis.text=element_text(size=16,face = "bold"),
        axis.title = element_text(size = 18, face = "bold"))+
  scale_shape_manual(guide = F, values = c(16, 15,17,21,22,23))

png('figs/GHI/ghi_scatter.png',width = 1200, height = 600  )
grid.arrange(p1,p2,nrow =1 )
dev.off()

tmp_df %>%
  ggplot(aes(x=reorder(country, -change), y=change, color = continent)) +
  geom_segment( aes(x=reorder(country, -change), xend=reorder(country, -change), y=0, yend=change), color="grey") +
  geom_point( size=2) +
  theme_bw()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  coord_flip()+
  ggtitle('Change in GHI Rank After Applying Optimized Weights')+
  xlab("") +
  ylab("Change in Rank")+
  theme_light()+
  scale_color_discrete(name = 'Continent')+
  ggsave( width = 8, height = 12, dpi = 300, filename = "figs/GHI/ghi_rank_loli.png")

ggplot(rankshifts, aes(x=old_scores, y=new_scores) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
