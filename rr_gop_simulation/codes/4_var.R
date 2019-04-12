# with close form for estiamtion variance
rm(list = ls())

library(xtable)
library(abind)
source(file = "functions/generalized_op.R")
source("functions/var.R")
source("functions/helper.R")

alpha_true = cbind(c(-0.5,1),c(0.5, 1.5))
beta_true = c(1, -0.5)
date = "2019-04-02"

sample_size.vec = c(100, 500, 1000)
seed.vec= 1:100
bias_se = list()
accuracy = list()
coverage = list()

for(i in 1: length(sample_size.vec)){
      mle_tmp_dat = array(0, c(2, 6))

      sample_size = sample_size.vec[i]
      for(seed in seed.vec){
            file_name = paste("rr_gop_simulation/data4/SampleSize", sample_size, "_Seed", seed,"_", date,".Rdata", sep = "")
            load(file = file_name)
            mle_tmp_dat = abind(mle_tmp_dat, mle.mat, along = 3)

      }
      mle_tmp_dat = mle_tmp_dat[,,-1]
      # bias
      tmp =  array(NA, c(2,2, 1000))
      for (j in 1:1000){
            tmp[,,j] = mle_tmp_dat[ ,c(1, 2), j]- alpha_true
      }
      bias = apply(tmp, c(1,2), mean)
      se = apply(tmp, c(1,2), sd) / sqrt(sample_size)
      bias_se[[i]]  = matrix(bias_sd(bias, se),2,2)

      # estimated SD
      alpha_sd = apply(mle_tmp_dat[ ,c(5, 6), ], c(1,2), mean)
      # Monte Carlo SD
      alpha_mcsd = apply(mle_tmp_dat[ ,c(1:2), ], c(1,2), sd)
      accuracy[[i]] = round(alpha_sd / alpha_mcsd, 3)
      #accuracy[[i]] = round(alpha_mcsd / alpha_sd, 3)
      alpha_est = mle_tmp_dat[ ,c(1:2), ]
      # covarage rate
      lowerB = alpha_est - 1.96 * mle_tmp_dat[ ,c(5:6), ]
      upperB = alpha_est + 1.96 * mle_tmp_dat[ ,c(5:6), ]

      tmp = array(NA, c(2,2, 1000))
      for (j in 1: 1000){
            tmp[,,j] = (alpha_true < upperB[,,j]) *  (alpha_true > lowerB[,,j])
      }

      coverage[[i]] = apply(tmp, c(1,2), mean)
}
save(bias_se, accuracy, coverage, file = paste("rr_gop_simulation/data4/simulation_rst_", date, ".RData",sep = ""))

