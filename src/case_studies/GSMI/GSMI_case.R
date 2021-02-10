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

write.csv(gsmi, here('data','GSMI','GSMI_final.csv'))

