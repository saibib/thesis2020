svi = read.csv(here('data','SVI','SVI2018_US_COUNTY.csv'),colClasses=c("FIPS"="character"))
svi = na_if(svi,y = -999)
svi[is.na(svi)] = 0
# information about each census tract
tract_info = svi[,c(1:7)]

#the 15 variables of interests, after they've been standardized
voi = svi %>%
  select(starts_with('EPL'))
rownames(voi) = svi$FIPS
# the four themes of interest and the total composite score

toi1 = apply(voi[,1:4], 1, sum)
toi2 = apply(voi[,5:8], 1, sum)
toi3 = apply(voi[,9:11],1,sum)
toi4 = apply(voi[,12:15], 1, sum)

toi = data.frame(ses = toi1, household = toi2, minority = toi3,
                 housing_transportation = toi4)

# toi = svi %>%
#   select(starts_with('SPL_THEME'))
# toi = toi[,-5]
rownames(toi) = svi$FIPS

voi_impt = rep(1/ncol(voi), ncol(voi))
# toi_impt = c(4/15, 4/15,2/15,5/15)
toi_impt = rep(1/4,4)

svi_scores = 4*(agg(toi, toi_impt))

# 15*agg(voi, voi_impt)['26065004492'] == svi_scores['26065004492']
#

# orig_shap_svi = shapleySubsetMc(X=toi,Y=svi_scores, Ntot = 7500, Ni = 3) # est orginal shapley values
orig_shap_svi = sobolshap_knn(agg, toi, method = 'knn', return.shap = T, n.knn=2,
                              randperm = T, n.perm=30, var_wts = toi_impt)
stats::dist(rbind(orig_shap_svi$Shap,toi_impt)) #euclidean distance difference in importances and shapley values



cl <- makeCluster(detectCores()-1) # set the number of processor cores
setDefaultCluster(cl=cl)
clusterExport(cl = cl, varlist = list('toi', 'agg', 'sobolshap_knn',
                                      'weights_shapley_diff', 'importance_diff',
                                      'shapleySubsetMc'), envir = environment())

# res_svi = DEoptim(fn = importance_diff, lower = rep(0,4), upper = rep(3, 4),
#               control = list(cluster = cl),
#               data=toi, Ntot= 7500, impt = toi_impt)

res_svi2 = DEoptim(fn = weights_shapley_diff, lower = rep(0,ncol(toi)), upper = rep(2, ncol(toi)),
                  control = list(cluster = cl),
                  impt = toi_impt, model =agg, data = toi,
                  method = 'knn', randperm = T, n.perm=30, n.knn=10, agg_method = 'ar')
setDefaultCluster(cl=NULL); stopCluster(cl)

optim_wts_svi = res_svi2$optim$bestmem/sum(res_svi$optim$bestmem)

wts_v_optim_wts(toi,toi_impt,optim_wts_svi)

svi_optim_scores = 4*agg(toi, var_wts = optim_wts_svi, agg_method = 'ar')
optim_shap_svi = sobolshap_knn(agg, toi, method = 'knn', return.shap = T, n.knn=10,var_wts = optim_wts_svi)

wts_v_optim_wts(toi,toi_impt,optim_wts_svi)+
  ggtitle('Dimension Weights for SVI')+
  theme(legend.position = 'bottom')+
  ggsave('figs/SVI/svi_weights.png')

desired_v_shapley(toi, toi_impt, as.numeric(orig_shap_svi$Shap),
                  as.numeric(optim_shap_svi$Shap))+
  ggtitle('Dimension Importances for SVI')+
  theme(legend.position = 'bottom')+
  ggsave('figs/SVI/svi_dim_impt.png')# plot the desired importances vs shapley effects

##########

data.frame(counties = rownames(toi), old_scores = svi_scores,
           optimized_scores = svi_optim_scores)%>%
  pivot_longer(-counties,names_to="scores", values_to="value") %>%
  ggplot(aes(value, fill=scores))+
  geom_boxplot()








#### testing
data.frame(counties = names(svi_scores), old_scores = svi_scores,
           optimized_scores = svi_optim_scores)%>%
  pivot_longer(-counties,names_to="scores", values_to="value") %>%
  wilcox.test(value ~ scores, data = ., paired = TRUE)

SIGN.test(svi_scores,svi_optim_scores)
########


v1 = names(sort(svi_scores))
v2 = names(sort(svi_optim_scores))

rankshifts  = data.frame(FIPS = v1,  old_scores = svi_scores[v1],
                         new_scores =  svi_optim_scores[v1], change = match(v1,v1)-match(v1,v2))

##################### too many counties, maybe take a random sample of them?
# rankshifts %>%
#   ggplot(aes(x=reorder(FIPS, -change), y=change)) +
#   geom_segment( aes(x=reorder(FIPS, -change), xend=reorder(FIPS, -change), y=0, yend=change), color="grey") +
#   geom_point( color="orange", size=2) +
#   theme_bw()+
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   coord_flip()+
#   ggtitle('Change in SVI Rank After Applying Optimized Weights')+
#   xlab("") +
#   ylab("Change in Rank")+
#   theme_light()
#   ggsave( width = 8, height = 12, dpi = 300, filename = "figs/SVI/svi_rank_loli.png")


# rankshifts %>%
#   arrange(-old_scores)%>%
#   mutate(FIPS = factor(FIPS, FIPS)) %>%
#   ggplot() +
#   geom_segment( aes(x=FIPS, xend=FIPS, y=old_scores, yend=new_scores), color="grey") +
#   geom_point( aes(x=FIPS, y=old_scores), color='lightblue', size=3 ) +
#   geom_point( aes(x=FIPS, y=new_scores), color='orange', size=3 ) +
#   coord_flip()+
#   ggtitle('Change in SVI Scores After Applying Optimized Weights')+
#   xlab("County (in order of original rank)") +
#   ylab("SVI Score")+
#   theme_light()+
#   # ggsave( width = 9, height = 14, dpi = 300, filename = "figs/GHI/ghi_scores_loli.png")

map_data = merge(rankshifts, tract_info, by.x = 'FIPS') %>%
  mutate(region = tolower(STATE), subregion = tolower(COUNTY)) %>%
  select(-c(STATE,COUNTY))

dfips = maps::county.fips %>%
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$")

dall = map_data("county") %>%
  right_join(dfips) %>%
  right_join(map_data)


dall %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=change)) +
  geom_polygon( data=map_data("state"), aes(x=long, y=lat, group=group),
                color="black", fill=NA, size = .5)+
  scale_fill_binned(breaks = c(-500,-250,0,250,500,750,1000),low = 'orange',high = 'blue',
                    name = 'SVI Score')+
  guides(fill = guide_coloursteps(show.limits = F,title.position = 'top'))+
  theme_light()+
  theme(legend.position="bottom",
        legend.key.width = unit(1.3, 'cm'))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle('Map of SVI Rank Shifts')+
  ggsave('figs/SVI/svi_rank_map.png')

dall %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=old_scores)) +
  geom_polygon( data=map_data("state"), aes(x=long, y=lat, group=group),
                color="black", fill=NA, size = .5)+
  scale_fill_binned(low = 'orange',high = 'blue')+
  guides(fill = guide_coloursteps(show.limits = TRUE, title = 'SVI Score', title.position = 'top'))+
  theme_light()+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1.3, 'cm'))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle('Map of SVI Scores')+
  ggsave('figs/SVI/svi_score_map.png')


dall %>%
  select(long,lat, group,  old_scores, new_scores)%>%
  mutate(diff = (new_scores - old_scores)/old_scores) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=diff)) +
  geom_polygon( data=map_data("state"), aes(x=long, y=lat, group=group),
                color="black", fill=NA, size = .5)+
  scale_fill_binned(low = 'orange',high = 'blue')+
  guides(fill = guide_coloursteps(show.limits = TRUE, title = 'Percent Change', title.position = 'top'))+
  theme_light()+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1.3, 'cm'))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle('Map of Percent Difference in Optimized vs. Original Scores')+
  ggsave('figs/SVI/svi_percent_map.png')




state = data.frame(ST_ABBR = state.abb, region_of_us = levels(state.region)[state.region])

dall %>% right_join(state) %>%
  select(region, old_scores, new_scores, region_of_us)%>%
  pivot_longer(cols = -c(region, region_of_us), names_to = 'method', values_to = 'values') %>%
  filter(method == 'old_scores') %>%
  ggplot(aes(region_of_us, values))+
  geom_violin(width =1,position = position_dodge(.5), fill = 'lightblue')+
  geom_boxplot(width = .1,position = position_dodge(.3))+
  theme_light()+
  scale_fill_manual(values = c("orange", "lightblue"),name = "Weights",
                    labels = c("Optimized", "Original"))+
  xlab('Region of USA')+
  ylab('SVI Score')+
  ggtitle('SVI Scores Distributions by US Region')+
  scale_x_discrete() +
  ggsave('figs/SVI/svi_old_scores_violin.png')

p1 = dall %>% right_join(state) %>%
  select(region, old_scores, new_scores, region_of_us)%>%
  pivot_longer(cols = -c(region, region_of_us), names_to = 'method', values_to = 'values') %>%
  ggplot(aes(region_of_us, values, fill = method ))+
  geom_split_violin(width =1.7,position = position_dodge(.5),color ='lightgray')+
  geom_boxplot(width = .2, color = 'black',position = position_dodge(.3))+
  theme_light()+
  scale_fill_manual(values = c("orange", "lightblue"),name = "Weights",
                    labels = c("Optimized", "Original"))+
  coord_flip()+
  theme(legend.position = "none")+
  xlab('Region of USA')+
  ylab('SVI Score')+
  ylim(c(0,16))+
  annotate("text", x=4.3, y=14, label= "Blue: GHI Scores using\nOriginal Weights", size = 3) +
  annotate("text", x=3.8, y=14, label= "Orange: GHI Scores using\nOptimized Weights", size = 3) +
  ggtitle('SVI Scores Distributions by Weighting Scheme')+
  scale_x_discrete() +
  ggsave('figs/SVI/svi_split_scores_violin.png')
p2 = dall %>%
  mutate(where = 'USA') %>%
  select(where, old_scores, new_scores) %>%
  pivot_longer(-where, names_to = 'method', values_to = 'values') %>%
  ggplot(aes(where, values, fill = method ))+
  geom_split_violin(width =1.7,position = position_dodge(.5),color ='lightgray')+
  geom_boxplot(width = .2, color = 'black',position = position_dodge(.3))+
  theme_light()+
  scale_fill_manual(values = c("orange", "lightblue"),name = "Weights",
                    labels = c("Optimized", "Original"))+
  coord_flip()+
  theme(legend.position = "none")+
  ggtitle('')+
  xlab('')+
  ylab('SVI Score')+
  ylim(c(0,16))

png('figs/SVI/svi_violins.png',width = 1600, height = 800  )
grid.arrange(p1,p2,nrow =1 )
dev.off()
# map_data %>%
#   select(old_scores, new_scores, region) %>%
#   pivot_longer(-region, names_to = 'method', values_to = 'values') %>%
#   ggplot(aes(region, values, fill = method ))+
#   geom_split_violin(width =1.75,position = position_dodge(.5))+
#   geom_boxplot(width = .1, color = 'white',position = position_dodge(.3))+
#   theme_light()+
#   scale_fill_manual(values = c("orange", "lightblue"),name = "Weights",
#                     labels = c("Optimized", "Original"))+
#   xlab('Continent')+
#   ylab('GHI Score')+
#   ggtitle('GHI Scores Distributions by Weighting Scheme')+
#   scale_x_discrete() +
#   ggsave('figs/SVI/svi_scores_violin.png')