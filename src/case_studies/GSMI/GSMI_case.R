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

 # plot the desired importances vs shapley effects


cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('gsmi', 'agg','sobolshap_knn','weights_shapley_diff', 'shapleySubsetMc', "sobolshap_knn"), envir = environment())

# gsmi_res = DEoptim(fn = importance_diff, lower = rep(0,10), upper = rep(1, 10),
#               control = list(cluster = cl),
#               data=gsmi, Ntot= 1500, impt = gsmi_impt)

gsmi_res = DEoptim(fn = weights_shapley_diff, lower = rep(0,ncol(gsmi)), upper = rep(1, ncol(gsmi)),
              control = list(cluster = cl),
              impt = gsmi_impt, model =agg, data = gsmi,
              method = 'knn', randperm = T, n.perm=50, n.knn=5, agg_method = 'ar')
setDefaultCluster(cl=NULL); stopCluster(cl)

# gsmi_optim_wts = gsmi_res$optim$bestmem/sum(gsmi_res$optim$bestmem)

gsmi_optim_scores = agg(gsmi, var_wts = gsmi_optim_wts, agg_method = 'ar')
optim_shap_gsmi = shapleySubsetMc(X=gsmi,Y=gsmi_optim_scores, Ntot = 5000, Ni = 3)

wts_v_optim_wts(gsmi,gsmi_impt,gsmi_optim_wts)+
  ggtitle('Dimension Weights for GSMI')+
  ggsave('figs/GSMI/gsmi_weights.png')

desired_v_shapley(gsmi, gsmi_impt, orig_shap_gsmi$shapley, optim_shap_gsmi$shapley)+
  ggtitle('Dimension Importances for GSMI')+
  theme(legend.position = 'bottom')+
  ggsave('figs/GSMI/gsmi_dim_impt.png', height  = 5, width = 10, units = 'in')

v1 = names(sort(gsmi_scores))
v2 = names(sort(gsmi_optim_scores))

# plotRanks(v1,v2,labels.offset = .5)

SIGN.test(gsmi_scores,gsmi_optim_scores)

# match(names(sort(gsmi_scores[gsmi_scores>0])),names(sort(gsmi_scores[gsmi_scores>0]))) -
  # match(names(sort(gsmi_scores[gsmi_scores>0])),names(sort(gsmi_optim_scores[gsmi_optim_scores>0])))
rankshifts  = data.frame(country = as.factor(v1),  old_scores =gsmi_scores[as.factor(v1)],
                         new_scores =  gsmi_optim_scores[as.factor(v1)], change = match(v1,v1)-match(v1,v2))

# rankshifts %>%
# ggplot(aes(x=reorder(country, -change), y=change)) +
#   geom_segment( aes(x=reorder(country, -change), xend=reorder(country, -change), y=0, yend=change), color="grey") +
#   geom_point( color="orange", size=2) +
#   theme_bw()+
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   coord_flip()+
#   ggtitle('Change in GSMI Rank After Applying Optimized Weights')+
#   xlab("") +
#   ylab("Change in Rank")+
#   theme_light()+
#   ggsave(width = 8, height = 10, dpi = 300, filename = "figs/GSMI/gsmi_ranks_loli.png")

# rankshifts %>%
#   arrange(old_scores)%>%
#   mutate(country = factor(country, country)) %>%
#   ggplot() +
#   geom_segment( aes(x=country, xend=country, y=old_scores, yend=new_scores), color="grey") +
#   geom_point( aes(x=country, y=old_scores), color='lightblue', size=3 ) +
#   geom_point( aes(x=country, y=new_scores), color='orange', size=3 ) +
#   coord_flip()+
#   ggtitle('Change in GSMI Scores After Applying Optimized Weights')+
#   xlab("Country (in order of original rank)") +
#   ylab("Composite Score")+
#   theme_light()+
#   ggsave( width = 8, height = 10, dpi = 300, filename = "figs/GSMI/gsmi_scores_loli.png")


# install.packages('reactable')

# heat map 82x82
tbl = reactable(rankshifts, rownames = F, columns = list(
  country = colDef(name = "Country", align = 'center'),
  old_scores = colDef(name = "Original Scores",format = colFormat(digits = 2)),
  new_scores = colDef(name = 'Optimized Scores', format = colFormat(digits = 2)),
  change = colDef(name = 'Change in Rank',
    cell = function(value) {
      if (value > 0) paste0("+", value) else value
    },
    style = function(value) {
    if (value > 0) {
      color <- "#99FF79"
    } else if (value < 0) {
      color <- "#FF9179"
    } else {
      color <- "#DBDDDA"
    }
    list(background = color, fontWeight = "bold")
    }
  )
  ),
  defaultSorted = list(old_scores = 'desc'),
  bordered = TRUE, compact = TRUE,
  defaultPageSize=15)


map_data = rankshifts
levels(map_data$country) <- c(levels(map_data$country), "Ivory Coast", "United States of America" ,
                              'Laos', 'Vietnam', 'South Korea',"Republic of Serbia","Slovakia" ,
                              "Russia")
map_data$country[map_data$country == "Côte d'Ivoire"] <- 'Ivory Coast'
map_data$country[map_data$country == "Lao PDR"] <- 'Laos'
map_data$country[map_data$country == "Viet Nam" ] <- 'Vietnam'
map_data$country[map_data$country == "Serbia" ] <- "Republic of Serbia"
map_data$country[map_data$country == "Korea, Rep."   ] <- 'South Korea'
map_data$country[map_data$country == "United States"   ] <- "United States of America"
map_data$country[map_data$country ==  "Russian Federation" ] <- "Russia"
map_data$country[map_data$country ==  "Slovak Republic"  ] <- "Slovakia"


# colourPalette <- RColorBrewer::brewer.pal(9,'RdYlGn')
# sPDF <- joinCountryData2Map( map_data, joinCode = "NAME", nameJoinColumn = "country", verbose = T,)
# mapParams.shifts <- mapCountryData( sPDF, nameColumnToPlot="change", addLegend=F,
#                                     missingCountryCol = gray(.9), colourPalette = colourPalette)
# do.call( addMapLegend, c( mapParams.shifts, legendLabels="all", legendWidth=0.5, legendIntervals="data", legendMar = 2 ) )
#
# world_map<-map_data("world")
map_data$admin = map_data$country

world <- ne_countries(scale = "medium", returnclass = "sf")
sf_dat <- merge(world, map_data, by.x="admin")
# sf_dat$change = as.factor(sf_dat$change)

sf_dat %>%
ggplot() +
  geom_map(dat=world_map, map = world_map,
           aes(map_id=region), fill="lightgray", color="black", alpha = .3)+
  geom_sf(aes(fill=change), color = 'black') +
  scale_fill_binned(breaks = c(-4,-2,0,2,4),low = 'orange',high = 'blue')+
  guides(fill = guide_coloursteps(show.limits = TRUE, title ='Change in Rank', title.position = 'top'))+
  theme_light()+
  theme(legend.position="bottom")+
  ggtitle('Map of GSMI Rank Shifts')+
  ggsave('figs/GSMI/gsmi_rank_map.png')


sf_dat %>%
  ggplot() +
  geom_map(dat=world_map, map = world_map,
           aes(map_id=region), fill="lightgray", color="black", alpha = .3)+
  geom_sf(aes(fill=old_scores), color = 'black') +
  scale_fill_binned(low = 'orange',high = 'blue')+
  guides(fill = guide_coloursteps(title = 'GSMI Score', show.limits = TRUE,title.position = "top"))+
  theme_light()+
  theme(legend.position="bottom",
        legend.key.width = unit(1.3, 'cm'))+
  ggtitle('Map of GSMI Scores')+
  ggsave('figs/GSMI/gsmi_score_map.png')

map_data$ADMIN = map_data$country
map_data = merge(map_data, countryRegions, by.x = 'ADMIN')


d1 = sf_dat %>%
  st_drop_geometry() %>%
  select(country, old_scores, new_scores, continent)%>%
  pivot_longer(cols = -c(country, continent), names_to = 'method', values_to = 'values') %>%
  filter(!continent %in% c('Oceania') )

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
  scale_fill_manual(values = c("orange", "lightblue"),name = "Weights",
                    labels = c("Optimized", "Original"))+
  xlab('')+
  ylab('GSMI Score')+
  ggtitle(' ')+
  coord_flip()


p1 = d1 %>%
  ggplot(aes(continent, values, fill = method ))+
  geom_split_violin(color='lightgray')+
  geom_boxplot(width = .1, color = 'black',position = position_dodge(.3))+
  theme_light()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("orange", "lightblue"),name = "Weights",
                    labels = c("Optimized", "Original"))+
  xlab('Continent')+
  ylab('GSMI Score')+
  coord_flip()+
  annotate("text", x=5.25, y=40, label= "Blue: GSMI Scores\nusing Original Weights", size = 3) +
  annotate("text", x=4.75, y=40, label= "Orange: GSMI Scores\nusing Optimized Weights", size = 3) +
  ggtitle('GSMI Scores Distributions by Weighting Scheme')+
  ggsave('figs/GSMI/gsmi_scores_violin.png')

png('figs/GSMI/gsmi_violins.png',width = 1200, height = 600  )
grid.arrange(p1,p2,nrow =1 )
dev.off()

# map_data %>%
#   select(old_scores, new_scores, continent)%>%
#   group_by(continent)%>%
#   mutate(mean_old = mean(old_scores), mean_new = mean(new_score))
#   ggplot() +
#   geom_segment( aes(x=continent, xend=continent, y=mean(old_scores), yend=mean(new_scores)), color="grey") +
#   geom_point( aes(x=continent, y=mean(old_scores)), color='lightblue', size=3 ) +
#   geom_point( aes(x=continent, y=mean(new_scores)), color='orange', size=3 ) +
#   coord_flip()+
#   xlab("") +
#   ylab("Value of Y")

tmp_df = sf_dat %>%
  st_drop_geometry() %>%
  select(country, old_scores, new_scores, continent, change)%>%
  filter(!continent %in% c('Seven seas (open ocean)') )

p1 = ggplot()+
  geom_point(data = tmp_df, aes(old_scores, new_scores, shape = continent, color = continent),
             size = 3, alpha = .6)+
  # geom_point(data = tmp2_df,
  #            aes(old_avg, new_avg, shape = continent, color = continent), size = 5)+
  geom_abline(slope=1, intercept = 0,linetype = 'dashed', alpha =.5)+
  theme_light()+
  xlab('Original Scores')+
  ylab('Optimized Scores')+
  ggtitle('Optimized vs. Original GHI Scores\n')+
  theme(legend.position= c(.8,.15))+
  scale_color_discrete(name = 'Continent')+
  scale_shape_discrete(name = 'Continent')

p2 = tmp_df %>%
  mutate(diff = (new_scores - old_scores)/old_scores) %>%
  group_by(continent) %>%
  summarise(mean_diff = mean(diff), mean_old = mean(old_scores))%>%
  ggplot(aes(mean_old, mean_diff, color =continent, shape = continent))+
  geom_point(size = 4)+
  geom_hline(yintercept = 0, alpha =.5)+
  geom_hline(yintercept = .05, linetype = 'dashed', alpha =.5)+
  geom_hline(yintercept = -.05, linetype = 'dashed', alpha =.5)+
  xlab('Average Original Score')+
  ylab('Percent Difference')+
  ggtitle('Average Percent Difference Between\nOptimized and Original Scores')+
  theme_light()+
  scale_color_discrete(guide = F)+
  scale_shape_discrete(guide = F)

png('figs/GSMI/gsmi_scatter.png',width = 1200, height = 600  )
grid.arrange(p1,p2,nrow =1 )
dev.off()
