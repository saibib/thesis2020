library(naniar)
svi = read.csv(here('data','SVI','SVI2018_US.csv'),colClasses=c("FIPS"="character"))
svi = na_if(svi,y = -999)
svi[is.na(svi)] = 0
# information about each census tract
tract_info = svi[,c(1:7)]

#the 15 variables of interests, after they've been standardized
voi = svi %>%
  select(starts_with('EPL'))
rownames(voi) = svi$FIPS
# the four themes of interest and the total composite score
toi = svi %>%
  select(starts_with('SPL_THEME'))
toi = toi[,-5]
rownames(toi) = svi$FIPS

voi_impt = rep(1/ncol(voi), ncol(voi))
toi_impt = c(4/15, 4/15,2/15,5/15)

svi_scores = 15*agg(toi, toi_impt)

15*agg(voi, voi_impt)['26065004492'] == svi_scores['26065004492']

act_scores = svi %>%
  select('SPL_THEMES')
rownames(act_scores) = svi$FIPS
act_scores['26065004492',]
