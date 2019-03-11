# data in file data2 are updated.
# estimated of sd from objective function neg.log.likelihood()

rm(list = ls())
source("rr_monotone_modeling_simulation/codes/1_mono_model.R")

sample_size.vec = c(50, 100, 500, 1000, 5000, 10000)
date = "2019-02-27"
seed.vec = 1:5
alpha_true = c(0, 1)
beta_true = c(-0.5, 1)

bias.mat = matrix(NA,  ncol = 2, nrow = length(sample_size.vec))
accuracy.mat = matrix(NA,  ncol = 2, nrow = length(sample_size.vec))
cr.mat = matrix(NA,  ncol = 2, nrow = length(sample_size.vec))
na.count = rep(NA, nrow = length(sample_size.vec))

for (i in 1: length(sample_size.vec)){
      mle_tmp_dat = matrix(NA, nrow = 8)
      for(seed in seed.vec){
            sample_size = sample_size.vec[i]
                  file_name = paste("rr_monotone_modeling_simulation/data2/SampleSize", sample_size, "_Seed", seed,"_", date,".Rdata", sep = "")
            load(file = file_name)
            mle_tmp_dat = cbind(mle_tmp_dat, mle.mat)
      }
      mle.mat = mle_tmp_dat[,-1]

      # estimated sd appear NA
      # c(alpha0_sd, alpha1_sd) both are NA or both not NA
      na_count_tmp = sum(is.na(mle.mat[c("alpha0_sd"), ]))/1000
      na.count[i] = na_count_tmp

      # alpha

      # bias of the estimation
      bias = apply(mle.mat[c("alpha0","alpha1"),]- c(alpha_true),1, mean)
      # se of bias
      se = apply(mle.mat[c("alpha0","alpha1"),]- c(alpha_true),1, sd) / sqrt(sample_size)
      bias.mat[i, ] = bias_sd(bias, se)

      # Monte Carlo SD
      mcsd = apply(mle.mat[c("alpha0","alpha1"),],1, sd)

      # estimated standard deviation
      ## note that there are NA for some runs
      if (na_count_tmp!=0) {
            mle.mat = mle.mat[,!is.na(mle.mat[c("alpha0_sd"), ])]
      }

      sd_tmp = apply(mle.mat[c("alpha0_sd","alpha1_sd"),], 1, function(x) mean(x))
      accuracy.mat[i,] = round(sd_tmp / mcsd, 3)

      # covarage rate
      lowerB = mle.mat[c("alpha0","alpha1"),] - 1.96 * mle.mat[c("alpha0_sd", "alpha1_sd"),]
      upperB = mle.mat[c("alpha0","alpha1"),] + 1.96 * mle.mat[c("alpha0_sd", "alpha1_sd"),]

      cr.mat[i, ] = c(sum((alpha_true[1] > lowerB[1,] & alpha_true[1] < upperB[1, ])),
                      sum((alpha_true[2] > lowerB[2,] & alpha_true[2] < upperB[2, ])))/ncol(mle.mat)
}



filename = paste("rr_monotone_modeling_simulation/data/anlysis_Seed", seed, "_", date,".RData", sep = "")
save(bias.mat, accuracy.mat, cr.mat, file = filename)


# latex output
library(xtable)
table = t(cbind(bias.mat, accuracy.mat, cr.mat))
rownames(table) = c("alpha0", "alpha1", "alpha0_acc", "alpha1_acc", "alpha0_cr", "alpha1_cr")
colnames(table) = sample_size.vec
print(xtable(table))



