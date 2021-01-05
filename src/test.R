ahh = read.table("https://www.itl.nist.gov/div898/strd/anova/SmLs09t.dat",
           header=TRUE, skip=58)
dim(ahh)
y = geom_agg(ahh)

res = shapleySubsetMc(ahh,y, Ntot = 10000, Ni = 10)
plot(res)
system("say -v Victoria your code is done")

