#


rm(list = ls())
source("rr_monotone_modeling_simulation/codes/1_mono_model.R")
source(file = "functions/var.R")

sample_size.vec = c(100, 500, 1000)
date = "2019-02-27"
seed.vec = 1:5
alpha_true = c(0, 1)
beta_true = c(-0.5, 1)
nz = 3

accuracy.mat = matrix(NA,  ncol = 2, nrow = length(sample_size.vec))
cr.mat = matrix(NA,  ncol = 2, nrow = length(sample_size.vec))

for (n in 1: length(sample_size.vec)){
      mle_tmp_dat = matrix(NA, nrow = 8)
      manu_sd_tmp = matrix(NA, ncol = 2)
      for(seed in seed.vec){
            sample_size = sample_size.vec[n]
            file_name = paste("rr_monotone_modeling_simulation/data2/SampleSize", sample_size, "_Seed", seed,"_", date,".Rdata", sep = "")
            load(file = file_name)
            mle_tmp_dat = cbind(mle_tmp_dat, mle.mat)

            set.seed(seed)
            alpha.mat = mle.mat[c("alpha0","alpha1"),]
            beta.mat = mle.mat[c("beta0","beta1"),]


            for (j in 1:ncol(alpha.mat)) {
                  alpha = alpha.mat[,j]
                  beta = beta.mat[,j]

                  # recover the data
                  v1 = rep(1, sample_size)
                  v2 = runif(sample_size, -2, 2)
                  va = cbind(v1, v2)
                  vb = va
                  z = sample(x = c(0:(nz-1)), size = sample_size, replace = T)
                  z = as.factor(z)
                  ny = sample_size

                  Pzmin_Pzmax = t(mapply(getProbScalarRR, va %*% alpha * (nz-1), vb %*% beta))
                  P.mat = matrix(0, ncol = nz, nrow = ny)
                  P.mat[, c(1, nz)] = Pzmin_Pzmax
                  P.mat[, -c(1, nz)] = Pzmin_Pzmax[,1] * exp(va %*% alpha %*% t(c(1: (nz-2))))

                  y = rep(0, sample_size)

                  for(i in 1:nz){
                        y[z == i-1] = rbinom(length(which(z==i-1)), 1, P.mat[z==i-1,i])
                  }

                  manu_var = rr.mono.var(y, z, va, vb, gamma=alpha, beta)
                  manu_sd_tmp = rbind(manu_sd_tmp, sqrt(manu_var[c(1,2)]))
            }
      }

      manu_sd.mat = manu_sd_tmp[-1,]
      mle.mat = mle_tmp_dat[,-1]

      manu_sd = apply(manu_sd.mat, 2, mean)
      mcsd = apply(mle.mat[c("alpha0","alpha1"),],1, sd)

      accuracy.mat[n,] = round(manu_sd / mcsd, 3)
      # covarage rate
      lowerB = mle.mat[c("alpha0","alpha1"),] - 1.96 * manu_sd
      upperB = mle.mat[c("alpha0","alpha1"),] + 1.96 * manu_sd

      cr.mat[n, ] = c(sum((alpha_true[1] > lowerB[1,] & alpha_true[1] < upperB[1, ])),
                      sum((alpha_true[2] > lowerB[2,] & alpha_true[2] < upperB[2, ])))/ncol(mle.mat)
}

save(accuracy.mat, cr.mat, file = "rr_monotone_modeling_simulation/data2/acc_and_coverage_manual.RData")
