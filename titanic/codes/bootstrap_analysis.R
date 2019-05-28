rm(list = ls())

library(tidyr)
library(dplyr)
dt = "2019-03-10"
seed = c(1:10)

#--------------Monotone------------#
coef.mat = NULL
for (i in seed) {
      file.name = paste("titanic/data/mono_bootstrap_Seed", i, "_", dt, ".RData", sep = "")
      load(file = file.name)
      coef.mat = cbind(coef.mat, mono.mat)
}

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

# made a new bootstrap
dt1 = "2019-05-22"
dt2 = "2019-05-23"
coef.mat = NULL

for (i in 1:200){
      f1 = paste("titanic/data/gop_bootstrap_Seed", i, "_", dt1, ".RData", sep = "")
      f2 = paste("titanic/data/gop_bootstrap_Seed", i, "_", dt2, ".RData", sep = "")

      if(file.exists(f1)) {
            load(f1)
            coef.mat = cbind(coef.mat, gop.bootst.mat)
      } else {
            load(f2)
            coef.mat = cbind(coef.mat, gop.bootst.mat)
      }
}
gop.mean = apply(coef.mat, 1, mean)
gop.sd = apply(coef.mat, 1, sd)

#---------GOP 5/24-------------
dt3 = "2019-05-24"
coef.mat = NULL

for (i in 1: 500) {
      f3 = paste("titanic/data/gop_bootstrap_Seed", i, "_", dt3, ".RData", sep = "")

      load(f3)
      coef.mat = cbind(coef.mat, gop.bootst.mat)
}

par(mfrow=c(3,5))
for (i in 1: 15) {
      hist(coef.mat[i,])
}

# convergence
mean(coef.mat[16,]) # 1
gop.mean = apply(coef.mat[1:10,], 1, mean)
gop.sd = apply(coef.mat[1:10,], 1, sd)

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
save(sd.tab, mean.tab, file = "titanic/data/bootstrap_results_2019-05-24.RData")
