rm(list = ls())
dt = "2019-03-10"
seed = c(1:10)

#--------------Monotone------------#
coef.mat = matrix(NA, nrow = 5)
for (i in seed) {
      file.name = paste("titanic/data/mono_bootstrap_Seed", i, "_", dt, ".RData", sep = "")
      load(file = file.name)
      coef.mat = cbind(coef.mat, mono.mat)
}
coef.mat = coef.mat[,-1]

# sd of the boostrap
mono.mean = apply(coef.mat, 1, mean)
mono.sd = apply(coef.mat, 1, sd)


#--------------GOP------------#
coef.mat = matrix(NA, nrow = 10)
for (i in seed) {
      file.name = paste("titanic/data/gop_bootstrap_Seed", i, "_", dt, ".RData", sep = "")
      load(file = file.name)
      coef.mat = cbind(coef.mat, gop.bootst.mat)
}
coef.mat = coef.mat[,-1]

# sd of the boostrap
gop.mean = apply(coef.mat, 1, mean)
gop.sd = apply(coef.mat, 1, sd)

#------------poisson----------
load("titanic/data/poisson_bootstrap_Seed1_2019-05-22.RData") #11 incovergence
poi.mean = apply(poi.bootst.mat, 1, function(x) mean(x, na.rm = T))
poi.sd = apply(poi.bootst.mat, 1, function(x) sd(x, na.rm = T))

tmp = rbind(mono.sd, gop.sd, poi.sd)
tmp[,c(3,4, 5, 8,9,10)] = tmp[,c(3,4, 5, 8,9,10)] *10
tmp[,c(4,9)] = tmp[,c(4,9)]*10
sd.tab = round(tmp, 3)

tmp = rbind(mono.mean, gop.mean, poi.mean)
tmp[,c(3,4, 5, 8,9,10)] = tmp[,c(3,4, 5, 8,9,10)] *10
tmp[,c(4,9)] = tmp[,c(4,9)]*10
mean.tab = round(tmp, 3)
save(sd.tab, mean.tab, file = "titanic/data/bootstrap_results.RData")
